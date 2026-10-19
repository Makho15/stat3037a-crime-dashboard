# =====================================================================
# R/mod_explore.R  -  Explore tab (Phase 2)
# Reads  : data$districts   (sf, one row per police district)
# Controls: variable, raw vs empirical Bayes crime rate, histogram scale
# Shows  : interactive map, histogram, summary table, correlation plot
# Needs  : theme.R and helpers.R (crime_choropleth, what_this_shows,
#          page_head, theme_crime, crime_cols, lisa_cols)
# Nothing is recomputed from raw data: everything comes from districts.rds.
# =====================================================================

# ---- Variables the user can pick ---------------------------------------
# id must match a column of data$districts. kind decides the scaling:
#   rate    crime rate per 100,000 (raw or smoothed)
#   pct     proportion stored as 0-1, shown as a percentage
#   density people per km2
# avg_hh_size is left out on purpose: in districts.rds its values look like
# household counts (hundreds to thousands), not an average. Add it back here
# once the values have been checked.
explore_catalogue <- data.frame(
  id    = c("crime_rate", "pct_informal", "pct_no_schooling", "pct_matric_plus",
            "pct_no_electricity", "pct_urban", "pct_no_piped_water",
            "pct_female", "pct_youth_15-34", "pop_density"),
  label = c("Violent crime rate (per 100,000)",
            "Informal dwellings (%)", "No schooling (%)", "Matric or higher (%)",
            "No electricity (%)", "Urban share (%)", "No piped water (%)",
            "Female (%)", "Youth aged 15-34 (%)", "Population density (people per km2)"),
  short = c("Crime rate", "Informal", "No schooling", "Matric+", "No electricity",
            "Urban", "No piped water", "Female", "Youth 15-34", "Density"),
  kind  = c("rate", rep("pct", 8), "density"),
  stringsAsFactors = FALSE
)

# Optional: text the tab owner adds to the end of the "What this shows" box
explore_owner_note <- NULL

# ---- Small helpers (prefixed so they cannot clash with other tabs) ---------
explore_skew <- function(x) {            # same formula as e1071::skewness, type 3
  x <- x[is.finite(x)]
  n <- length(x)
  if (n < 3) return(NA_real_)
  m <- mean(x)
  m2 <- mean((x - m)^2)
  if (m2 == 0) return(NA_real_)
  mean((x - m)^3) / m2^1.5 * ((n - 1) / n)^1.5
}

explore_num <- function(x, digits = 2) {
  ifelse(is.na(x), "", format(round(x, digits), big.mark = ",", nsmall = digits,
                              trim = TRUE, scientific = FALSE))
}

# ---- UI ----------------------------------------------------------------------
mod_explore_ui <- function(id) {
  ns <- NS(id)

  div(class = "page-wrap",
      page_head("Explore",
                "Look at violent crime and its covariates before any modelling"),

      layout_sidebar(
        fillable = FALSE,
        sidebar = sidebar(
          width = 300,
          selectInput(ns("var"), "Variable",
                      choices = setNames(explore_catalogue$id, explore_catalogue$label),
                      selected = "crime_rate"),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'crime_rate'", ns("var")),
            radioButtons(ns("rate_type"), "Crime rate version",
                         choices = c("Raw rate" = "raw",
                                     "Empirical Bayes (smoothed)" = "eb"),
                         selected = "raw"),
            helpText("The smoothed rate pulls the rates of very small districts ",
                     "towards the national pattern, so a few crimes in a tiny ",
                     "population do not look extreme.")
          ),
          radioButtons(ns("hist_scale"), "Histogram scale",
                       choices = c("Original scale" = "orig", "Log scale" = "log"),
                       selected = "orig"),
          helpText("Hover over a district on the map to see its name and value.")
        ),

        card(card_header(textOutput(ns("map_title"))),
             leafletOutput(ns("map"), height = 480)),

        layout_columns(
          col_widths = c(6, 6),
          card(card_header("Distribution"), plotOutput(ns("hist"), height = 320)),
          card(card_header("Correlation: log crime rate and covariates"),
               plotOutput(ns("corr"), height = 320))
        ),

        card(card_header("Summary statistics"), tableOutput(ns("summary"))),

        uiOutput(ns("note"))
      )
  )
}

# ---- Server --------------------------------------------------------------------
mod_explore_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {

    d <- data$districts

    # Only offer variables that exist in this version of the data
    catalogue <- explore_catalogue[explore_catalogue$id %in% names(d), , drop = FALSE]
    updateSelectInput(session, "var",
                      choices = setNames(catalogue$id, catalogue$label),
                      selected = "crime_rate")

    # Everything the outputs need about the chosen variable
    sel <- reactive({
      req(input$var)
      row <- catalogue[catalogue$id == input$var, , drop = FALSE]
      req(nrow(row) == 1)
      eb <- identical(input$rate_type, "eb")

      if (row$kind == "rate") {
        x <- if (eb) d$crime_rate_eb else d$crime_rate
        label <- if (eb) "Violent crime rate per 100,000 (smoothed, empirical Bayes)"
                 else    "Violent crime rate per 100,000 (raw)"
        legend <- "Violent crimes per 100,000"
        x_log <- if (eb) log(x) else log1p(x)
        log_label <- if (eb) "log(smoothed rate)" else "log(rate + 1)"
      } else if (row$kind == "pct") {
        x <- d[[row$id]] * 100                 # stored as 0-1
        label <- row$label
        legend <- row$label
        x_log <- log1p(x)
        log_label <- "log(percentage + 1)"
      } else {
        x <- d[[row$id]]
        label <- row$label
        legend <- "People per km2"
        x_log <- log1p(x)
        log_label <- "log(density + 1)"
      }
      list(x = x, x_log = x_log, label = label, legend = legend,
           log_label = log_label, kind = row$kind, eb = eb)
    })

    output$map_title <- renderText({ sel()$label })

    output$map <- renderLeaflet({
      s <- sel()
      validate(need(length(unique(stats::na.omit(s$x))) > 5,
                    "Not enough different values to draw a map."))
      md <- d
      md$value <- s$x
      crime_choropleth(md, "value", s$legend)
    })

    output$hist <- renderPlot({
      s <- sel()
      use_log <- identical(input$hist_scale, "log")
      x <- if (use_log) s$x_log else s$x
      validate(need(sum(is.finite(x)) > 5, "Not enough values to draw a histogram."))
      xlab <- if (use_log) s$log_label else s$label

      ggplot(data.frame(x = x), aes(x = x)) +
        geom_histogram(bins = 40, fill = crime_cols$muted,
                       colour = crime_cols$surface, na.rm = TRUE) +
        geom_vline(xintercept = stats::median(x, na.rm = TRUE),
                   colour = crime_cols$accent, linewidth = 1) +
        geom_vline(xintercept = mean(x, na.rm = TRUE),
                   colour = crime_cols$text, linewidth = 0.8, linetype = "dashed") +
        labs(title = "Distribution across districts",
             subtitle = "Amber line = median, dashed line = mean",
             x = xlab, y = "Number of districts") +
        theme_crime()
    }, res = 96)

    output$corr <- renderPlot({
      pcts <- catalogue[catalogue$kind == "pct", , drop = FALSE]
      validate(need(nrow(pcts) > 1, "Covariates are not available."))
      eb <- identical(input$rate_type, "eb")
      y <- if (eb) log(d$crime_rate_eb) else log1p(d$crime_rate)

      m <- data.frame(y, sf::st_drop_geometry(d)[, pcts$id, drop = FALSE],
                      check.names = FALSE)
      names(m) <- c("Log crime rate", pcts$short)
      cm <- stats::cor(m, use = "pairwise.complete.obs")

      long <- as.data.frame(as.table(cm), stringsAsFactors = FALSE)
      names(long) <- c("a", "b", "r")
      long$a <- factor(long$a, levels = colnames(cm))
      long$b <- factor(long$b, levels = colnames(cm))
      long$strong <- abs(long$r) > 0.8 & long$a != long$b
      long$txt_col <- ifelse(abs(long$r) > 0.55, crime_cols$bg, crime_cols$text)

      ggplot(long, aes(a, b, fill = r)) +
        geom_tile(colour = crime_cols$surface) +
        geom_text(aes(label = sprintf("%.2f", r), fontface = ifelse(strong, "bold", "plain")),
                  colour = long$txt_col, size = 3) +
        scale_fill_gradient2(low = lisa_cols[["Low-Low"]], mid = crime_cols$border,
                             high = crime_cols$accent, midpoint = 0,
                             limits = c(-1, 1), name = "r") +
        scale_y_discrete(limits = rev) +
        labs(title = "Pearson correlation",
             subtitle = "Bold = |r| above 0.8: avoid using both in one regression",
             x = NULL, y = NULL) +
        theme_crime() +
        theme(axis.text.x = element_text(angle = 40, hjust = 1),
              panel.grid = element_blank())
    }, res = 96)

    output$summary <- renderTable({
      s <- sel()
      one <- function(x, name) {
        data.frame(
          Scale    = name,
          Districts = sum(is.finite(x)),
          Missing  = sum(!is.finite(x)),
          Mean     = explore_num(mean(x[is.finite(x)])),
          SD       = explore_num(stats::sd(x[is.finite(x)])),
          Min      = explore_num(min(x[is.finite(x)])),
          Median   = explore_num(stats::median(x[is.finite(x)])),
          Max      = explore_num(max(x[is.finite(x)])),
          Skewness = explore_num(explore_skew(x)),
          check.names = FALSE, stringsAsFactors = FALSE
        )
      }
      rbind(one(s$x, "Original"), one(s$x_log, s$log_label))
    }, striped = TRUE, spacing = "s", width = "100%")

    output$note <- renderUI({
      s <- sel()
      x <- s$x[is.finite(s$x)]
      sk <- explore_skew(s$x)
      sk_log <- explore_skew(s$x_log)
      what_this_shows(
        sprintf("%s across %s districts: the median is %s and the mean is %s. ",
                s$label, format(length(x), big.mark = ","),
                explore_num(stats::median(x)), explore_num(mean(x))),
        sprintf("Skewness is %s on the original scale and %s on the log scale; values between -1 and 1 are close to symmetric. ",
                explore_num(sk), explore_num(sk_log)),
        "Recorded crime depends on reporting, and rates in very small districts are unstable.",
        explore_owner_note
      )
    })
  })
}
