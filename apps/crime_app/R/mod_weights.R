# =====================================================================
# R/mod_weights.R  -  Weights tab (owner: Phase 7 lead)
# Reads: data$districts, data$wdetail, data$counts, data$tables$weights_summary
# Shows how a neighbour rule becomes weights, and how weights make a spatial lag.
# =====================================================================

weights_choices <- c(
  "Queen (shared border or corner)" = "queen",
  "Rook (shared border only)"       = "rook",
  "5 nearest neighbours"            = "knn",
  "Distance band (about 102 km)"    = "dist"
)

weights_type_choices <- c(
  "Row-standardised (W)" = "W",
  "Binary (B)"           = "B"
)

weights_desc <- c(
  queen = "Districts are neighbours if they touch at a border or a corner. This is the default in the analysis tabs.",
  rook  = "Districts are neighbours only if they share part of a border.",
  knn   = "Each district is linked to its 5 nearest districts, so every district has exactly 5 neighbours.",
  dist  = "Districts are neighbours if their centres are within about 102 km, the smallest distance that gives every district at least one neighbour."
)

# Which weights each test uses (from scripts 03 and 04)
weights_use <- data.frame(
  Test    = c("Moran's I", "Geary's C", "Local Moran's I (LISA)", "Global G", "Join counts"),
  Weights = c("Row-standardised, queen", "Row-standardised, queen", "Row-standardised, queen",
              "Binary, distance band", "Binary, queen"),
  Why     = c("Compares each district with the average of its neighbours",
              "Compares each district with its neighbours one by one",
              "Same lag, calculated for each district separately",
              "Adds up neighbours' values, so weights must be 0 or 1",
              "Counts neighbouring pairs, so weights must be 0 or 1"),
  check.names = FALSE
)

mod_weights_ui <- function(id) {
  ns <- NS(id)
  div(class = "page-wrap",
    page_head("Spatial weights",
              "A neighbour rule says who is nearby. Weights say how much each neighbour counts."),

    card(card_header("From neighbours to weights"),
      card_body(layout_columns(
        col_widths = breakpoints(sm = 12, md = c(4, 4, 4)), fill = FALSE,
        div(div(class = "tour-title", "Binary weights (B)"),
            div(class = "tour-text", "Every neighbour gets weight 1 and everyone else gets 0. Used when a test adds up or counts neighbours.")),
        div(div(class = "tour-title", "Row-standardised weights (W)"),
            div(class = "tour-text", "A district with n neighbours gives each one weight 1/n, so its weights add up to 1. Districts with many and few neighbours are then treated alike.")),
        div(div(class = "tour-title", "Spatial lag"),
            div(class = "tour-text", "The weighted average of a district's neighbours' values. Comparing a district with its lag is the idea behind Moran's I."))
      ))),

    card(card_body(
      layout_columns(
        col_widths = breakpoints(sm = 12, md = c(4, 4, 4)), fill = FALSE,
        radioButtons(ns("w"), "Neighbour rule", choices = weights_choices, selected = "queen"),
        div(radioButtons(ns("type"), "Weights", choices = weights_type_choices, selected = "W"),
            selectizeInput(ns("dist"), "District", choices = NULL)),
        div(class = "text-muted", style = "padding-top:28px;", uiOutput(ns("desc")))
      )
    )),

    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6, xl = 3), fill = FALSE,
      uiOutput(ns("s_n")), uiOutput(ns("s_each")),
      uiOutput(ns("s_sum")), uiOutput(ns("s_lag"))
    ),

    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
      card(full_screen = TRUE,
           card_header(textOutput(ns("map_title"))),
           leafletOutput(ns("map"), height = 480)),
      card(card_header("The district's neighbours and their weights"),
           card_body(tableOutput(ns("nbtbl")), uiOutput(ns("nbnote"))))
    ),

    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(7, 5)), fill = FALSE,
      card(card_header("Each district against the weighted average of its neighbours"),
           plotOutput(ns("scatter"), height = 340)),
      what_this_shows("Each dot is a district: its own log crime rate against its spatial lag, the row-standardised average of its neighbours. The line slopes upward, so districts with high rates tend to have high-rate neighbours. The slope of this line is Moran's I. The highlighted dot is the district chosen above.")
    ),

    card(card_header("Which weights each test uses"),
         card_body(tableOutput(ns("use")))),

    accordion(id = ns("counts_acc"), open = FALSE,
      accordion_panel("How many neighbours do districts have?",
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = c(5, 7)), fill = FALSE,
          plotOutput(ns("hist"), height = 260),
          tableOutput(ns("tbl")))))
  )
}

mod_weights_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    d  <- data$districts
    nm <- as.character(d$police_district)
    x  <- d$log_crime_rate

    # District picker: names in title case; start on a district with 5 queen neighbours
    default <- if (!is.null(data$counts)) nm[which(data$counts$queen == 5)[1]] else nm[1]
    if (is.na(default)) default <- nm[1]
    updateSelectizeInput(session, "dist", server = TRUE, selected = default,
                         choices = setNames(nm, tools::toTitleCase(tolower(nm))))

    nb_all <- reactive({
      validate(need(!is.null(data$wdetail),
                    "Weights detail missing: rerun scripts/07_prepare_app_data.R"))
      data$wdetail$nb[[input$w]]
    })

    # Average of neighbours' values for every district (row-standardised lag)
    lag_all <- reactive({
      vapply(nb_all(), function(v) if (v[1] == 0L) NA_real_ else mean(x[v]), numeric(1))
    })

    sel <- reactive({
      req(input$dist)
      i  <- which(nm == input$dist)[1]
      nb <- nb_all()[[i]]
      if (length(nb) == 1L && nb == 0L) nb <- integer(0)
      list(i = i, nb = nb, n = length(nb))
    })

    output$desc <- renderUI(p(weights_desc[[input$w]]))

    each_w <- reactive(if (input$type == "W") 1 / sel()$n else 1)

    output$s_n <- renderUI(stat_card("Neighbours of this district", sel()$n, TRUE))
    output$s_each <- renderUI({
      s <- sel(); validate(need(s$n > 0, "No neighbours"))
      stat_card("Weight of each neighbour",
                if (input$type == "W") formatC(1 / s$n, digits = 3, format = "f") else "1")
    })
    output$s_sum <- renderUI({
      s <- sel(); validate(need(s$n > 0, "No neighbours"))
      stat_card("Weights add up to", if (input$type == "W") "1" else s$n)
    })
    output$s_lag <- renderUI({
      s <- sel(); validate(need(s$n > 0, "No neighbours"))
      v <- if (input$type == "W") mean(x[s$nb]) else sum(x[s$nb])
      stat_card(if (input$type == "W") "Spatial lag (neighbours' average)" else "Sum of neighbours' values",
                formatC(v, digits = 2, format = "f"))
    })

    output$map_title <- renderText({
      req(input$dist)
      paste0(tools::toTitleCase(tolower(input$dist)), ": neighbours and weights")
    })

    output$map <- renderLeaflet({
      s <- sel()
      fill <- rep(crime_cols$border, length(nm))
      fill[s$nb] <- crime_cols$accent
      fill[s$i]  <- "#FFFFFF"
      wtxt <- if (input$type == "W") formatC(1 / max(s$n, 1), digits = 3, format = "f") else "1"
      role <- rep("", length(nm)); role[s$nb] <- paste0("Neighbour, weight ", wtxt); role[s$i] <- "Selected district"
      labs <- sprintf("<strong>%s</strong>%s", htmlEscape(nm), ifelse(role == "", "", paste0("<br/>", role)))
      bb <- sf::st_bbox(d[c(s$i, s$nb), ])
      leaflet(d, options = leafletOptions(minZoom = 4)) |>
        addPolygons(fillColor = fill, fillOpacity = ifelse(role == "", 0.55, 0.95),
                    color = crime_cols$bg, weight = 0.3, label = lapply(labs, HTML),
                    highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2, bringToFront = TRUE)) |>
        fitBounds(bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]])
    })

    output$nbtbl <- renderTable({
      s <- sel()
      validate(need(s$n > 0, "This district has no neighbours under this rule, so it has no spatial lag."))
      k  <- head(s$nb, 12)
      tb <- data.frame(
        District = tools::toTitleCase(tolower(nm[k])),
        `Binary (B)` = "1",
        `Row-standardised (W)` = formatC(1 / s$n, digits = 3, format = "f"),
        `Log crime rate` = formatC(x[k], digits = 2, format = "f"),
        check.names = FALSE)
      tb
    }, align = "lccc")

    output$nbnote <- renderUI({
      s <- sel(); req(s$n > 0)
      p(class = "text-muted",
        if (s$n > 12) sprintf("Showing 12 of %d neighbours. ", s$n),
        sprintf("Own log crime rate: %.2f. ", x[s$i]),
        if (input$type == "W") sprintf("Average of neighbours (the spatial lag): %.2f.", mean(x[s$nb]))
        else sprintf("Sum of neighbours' values: %.2f.", sum(x[s$nb])))
    })

    output$scatter <- renderPlot({
      lg <- lag_all()
      df <- data.frame(x = x, lag = lg)
      df <- df[!is.na(df$lag), ]
      s  <- sel()
      pt <- data.frame(x = x[s$i], lag = lg[s$i])
      g <- ggplot(df, aes(x, lag)) +
        geom_point(colour = crime_cols$muted, alpha = 0.35, size = 1.2) +
        geom_smooth(method = "lm", formula = y ~ x, se = FALSE, colour = crime_cols$accent, linewidth = 1)
      if (!is.na(pt$lag)) g <- g + geom_point(data = pt, colour = "#FFFFFF", size = 3.5)
      g + labs(x = "District's log crime rate", y = "Average of neighbours") + theme_crime()
    }, bg = crime_cols$surface, res = 96)

    output$use <- renderTable(weights_use, align = "lll")

    output$hist <- renderPlot({
      validate(need(!is.null(data$counts), "Neighbour counts missing: rerun scripts/07_prepare_app_data.R"))
      n  <- data$counts[[input$w]]
      df <- data.frame(n = n)
      layer <- if (max(n) <= 30) geom_histogram(binwidth = 1, boundary = -0.5,
                                                fill = crime_cols$accent, colour = crime_cols$surface)
               else geom_histogram(bins = 30, fill = crime_cols$accent, colour = crime_cols$surface)
      ggplot(df, aes(n)) + layer +
        labs(x = "Number of neighbours", y = "Districts") +
        theme_crime()
    }, bg = crime_cols$surface, res = 96)

    output$tbl <- renderTable({
      tb <- data$tables$weights_summary
      validate(need(!is.null(tb), "weights_summary.csv not found: rerun scripts/07_prepare_app_data.R"))
      tb
    }, digits = 2, na = "")
  })
}
