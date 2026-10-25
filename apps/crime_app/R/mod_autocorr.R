# =====================================================================
# R/mod_autocorr.R  -  Autocorrelation tab (Phase 4)
# Reads  : data$districts  (sf; has local_I, p_fdr and cluster columns)
#          data$tables     (global_autocorrelation, moran_sensitivity,
#                           lisa_cluster_counts)
# Controls: LISA classes to show, significant districts only, results table
# Needs  : theme.R and helpers.R (lisa_cols, crime_cols, theme_crime,
#          page_head, what_this_shows)
# Nothing is recomputed: everything comes from the saved app data.
# Save this file as apps/crime_app/R/mod_autocorr.R
# (the app looks for mod_autocorr_ui / mod_autocorr_server).
# =====================================================================

# ---- Local constants (prefixed so they cannot clash with other tabs) -------
ac_levels <- c("High-High", "Low-Low", "High-Low", "Low-High",
               "Not significant", "No neighbours")

# Optional: text the tab owner adds to the end of the "What this shows" box
ac_owner_note <- NULL

# p-values such as 9.6e-73 would print as 0 in a table, so format them as text
ac_fmt_p <- function(p) {
  ifelse(is.na(p), "",
         ifelse(p < 0.05, formatC(p, format = "e", digits = 2),
                formatC(p, format = "f", digits = 4)))
}

ac_fmt_num <- function(x, digits = 4) {
  ifelse(is.na(x), "", formatC(x, format = "f", digits = digits))
}

# ---- UI ----------------------------------------------------------------------
mod_autocorr_ui <- function(id) {
  ns <- NS(id)

  div(class = "page-wrap",
      page_head("Autocorrelation",
                "Is violent crime clustered in space, and where are the hot and cold spots?"),

      layout_sidebar(
        fillable = FALSE,
        sidebar = sidebar(
          width = 300,
          checkboxGroupInput(ns("classes"), "LISA classes to show",
                             choices = ac_levels, selected = ac_levels),
          checkboxInput(ns("sig_only"), "Significant districts only", FALSE),
          radioButtons(ns("table_choice"), "Results table",
                       choices = c("Global tests" = "global",
                                   "Weights sensitivity (Moran's I)" = "sens"),
                       selected = "global"),
          helpText("Urban districts are small, so Gauteng hot spots look tiny ",
                   "at national scale. Zoom in to see them.")
        ),

        card(card_header("Local Moran's I (LISA) clusters"),
             leafletOutput(ns("map"), height = 520)),

        layout_columns(
          col_widths = 12,
          card(card_header("Districts per class"),
               plotOutput(ns("counts_plot"), height = 280)),
          card(card_header(textOutput(ns("table_title"))),
               tableOutput(ns("results_table")))
        ),

        layout_columns(
          col_widths = c(7, 5),
          card(card_header("Moran scatterplot: log crime rate"),
               plotOutput(ns("scatter"), height = 340)),
          card(card_header("Hot and cold spot districts"),
               tableOutput(ns("spots")))
        ),

        uiOutput(ns("note"))
      )
  )
}

# ---- Server --------------------------------------------------------------------
mod_autocorr_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    # One copy of the districts with a clean, complete cluster factor
    lisa <- local({
      x <- data$districts
      validate_cols <- c("police_district", "cluster")
      stopifnot(all(validate_cols %in% names(x)))
      x$cluster <- factor(as.character(x$cluster), levels = ac_levels)
      sf::st_transform(x, 4326)
    })

    bb <- sf::st_bbox(lisa)

    # Rows that match the controls
    shown <- reactive({
      cl <- as.character(lisa$cluster)
      keep <- cl %in% input$classes
      if (isTRUE(input$sig_only)) {
        keep <- keep & !(cl %in% c("Not significant", "No neighbours"))
      }
      lisa[keep & !is.na(keep), ]
    })

    # Base map: drawn once, never redrawn when the controls change
    output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 4)) |>
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })

    # Polygons and legend update through a proxy
    observe({
      x <- shown()
      proxy <- leafletProxy("map") |>
        clearShapes() |>
        clearControls()

      if (nrow(x) == 0) return(invisible(NULL))

      cl   <- as.character(x$cluster)
      fill <- unname(lisa_cols[cl])
      lab  <- sprintf(
        "<strong>%s</strong><br/>Class: %s<br/>Log crime rate: %s<br/>Local Moran's I: %s<br/>FDR p-value: %s",
        htmltools::htmlEscape(x$police_district), htmltools::htmlEscape(cl),
        ac_fmt_num(as.numeric(x$log_crime_rate), 2),
        ac_fmt_num(as.numeric(x$local_I), 3),
        ac_fmt_p(as.numeric(x$p_fdr))
      )

      show <- names(lisa_cols) != "No neighbours"
      proxy |>
        addPolygons(
          data = x,
          fillColor = fill, fillOpacity = 0.9,
          color = crime_cols$bg, weight = 0.3,
          label = lapply(lab, htmltools::HTML),
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2,
                                              bringToFront = TRUE)
        ) |>
        addLegend(position = "bottomright",
                  colors = unname(lisa_cols[show]),
                  labels = names(lisa_cols)[show],
                  title = "LISA cluster", opacity = 0.95)
    })

    # Bar chart of districts per class (counts follow the class tick-boxes)
    output$counts_plot <- renderPlot({
      cnt <- data$tables$lisa_cluster_counts
      validate(need(!is.null(cnt), "Cluster counts are not available."))
      cnt <- data.frame(cluster = as.character(cnt[[1]]), n = as.numeric(cnt[[2]]),
                        stringsAsFactors = FALSE)
      cnt <- cnt[cnt$cluster %in% input$classes, , drop = FALSE]
      validate(need(nrow(cnt) > 0, "Tick at least one LISA class."))
      cnt$cluster <- factor(cnt$cluster, levels = rev(ac_levels))

      ggplot(cnt, aes(x = cluster, y = n, fill = cluster)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = format(n, big.mark = ",", trim = TRUE)),
                  hjust = -0.15, size = 3.5, colour = crime_cols$text) +
        scale_fill_manual(values = lisa_cols) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.18))) +
        coord_flip(clip = "off") +
        labs(x = NULL, y = "Number of districts") +
        theme_crime()
    }, res = 96)

    # Moran scatterplot (Phase 4): standardised log crime rate against the
    # average of its queen neighbours. Built from the saved neighbour links.
    scatter_df <- reactive({
      x <- as.numeric(data$districts$log_crime_rate)
      lk <- sf::st_drop_geometry(data$links$queen)
      z <- as.numeric(scale(x))
      lag_z <- as.numeric(tapply(z[lk$j], lk$i, mean))
      idx <- as.integer(names(tapply(z[lk$j], lk$i, mean)))
      lagfull <- rep(NA_real_, length(z))
      lagfull[idx] <- lag_z
      quad <- ifelse(z >= 0 & lagfull >= 0, "High-High",
              ifelse(z <  0 & lagfull <  0, "Low-Low",
              ifelse(z >= 0 & lagfull <  0, "High-Low", "Low-High")))
      data.frame(z = z, lag_z = lagfull, quadrant = quad)
    })

    output$scatter <- renderPlot({
      df <- scatter_df()
      df <- df[is.finite(df$z) & is.finite(df$lag_z), , drop = FALSE]
      validate(need(nrow(df) > 5, "Not enough districts to draw the scatterplot."))
      slope <- unname(stats::coef(stats::lm(lag_z ~ z, data = df))[2])
      ggplot(df, aes(z, lag_z)) +
        geom_hline(yintercept = 0, linetype = "dashed", colour = crime_cols$muted) +
        geom_vline(xintercept = 0, linetype = "dashed", colour = crime_cols$muted) +
        geom_point(aes(colour = quadrant), alpha = 0.7, size = 1.4) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE,
                    colour = crime_cols$text, linewidth = 0.8) +
        scale_colour_manual(values = lisa_cols[c("High-High", "Low-Low", "High-Low", "Low-High")]) +
        labs(subtitle = sprintf("Slope = %.3f (queen weights)", slope),
             x = "Standardised log crime rate",
             y = "Average of neighbours (spatial lag)", colour = "Quadrant") +
        theme_crime()
    }, res = 96)

    # Names of the significant hot and cold spots
    output$spots <- renderTable({
      cl <- as.character(lisa$cluster)
      nm <- function(k) sort(as.character(lisa$police_district[cl == k]))
      hh <- nm("High-High"); ll <- nm("Low-Low")
      n <- max(length(hh), length(ll), 1)
      pad <- function(v) c(v, rep("", n - length(v)))
      out <- data.frame(`Hot spots (High-High)` = pad(hh),
                        `Cold spots (Low-Low)` = pad(ll),
                        check.names = FALSE, stringsAsFactors = FALSE)
      out
    }, striped = TRUE, spacing = "s", width = "100%")

    output$table_title <- renderText({
      if (identical(input$table_choice, "sens")) {
        "Moran's I under different weights"
      } else {
        "Global spatial autocorrelation tests"
      }
    })

    output$results_table <- renderTable({
      src <- if (identical(input$table_choice, "sens")) {
        data$tables$moran_sensitivity
      } else {
        data$tables$global_autocorrelation
      }
      validate(need(!is.null(src), "This table is not available."))
      tab <- as.data.frame(src, stringsAsFactors = FALSE)
      for (nm in intersect(c("statistic", "expected", "z"), names(tab))) {
        tab[[nm]] <- ac_fmt_num(as.numeric(tab[[nm]]))
      }
      if ("p_value" %in% names(tab)) tab$p_value <- ac_fmt_p(as.numeric(tab$p_value))
      names(tab) <- c(test = "Test", weights = "Weights", statistic = "Statistic",
                      expected = "Expected", z = "z", p_value = "p-value")[names(tab)]
      tab
    }, striped = TRUE, spacing = "s", width = "100%")

    output$note <- renderUI({
      m <- data$meta
      cl <- as.character(lisa$cluster)
      n_hh <- sum(cl == "High-High")
      n_ll <- sum(cl == "Low-Low")
      what_this_shows(
        sprintf("Global Moran's I is %s (p %s), so districts with similar violent crime rates tend to sit next to each other. ",
                ac_fmt_num(m$moran_I, 3),
                if (m$moran_p < 0.05) "< 0.05" else paste("=", ac_fmt_p(m$moran_p))),
        "The analysis variable is the log violent crime rate, log(rate + 1). Global G uses the raw rate with binary distance-band weights. ",
        sprintf("The map shows where: %d High-High districts are hot spots (high crime surrounded by high crime) and %d Low-Low districts are cold spots. ",
                n_hh, n_ll),
        "Classes use Local Moran's I with queen weights and FDR-adjusted p-values at the 5% level. ",
        "Most districts are not significant, which is normal for a local test.",
        ac_owner_note
      )
    })
  })
}

