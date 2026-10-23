# =====================================================================
# R/mod_weights.R  -  Weights tab (owner: Phase 3 owner)
# Reads: data$districts, data$counts, data$links, data$tables$weights_summary
# =====================================================================

weights_choices <- c(
  "Queen (shared border or corner)" = "queen",
  "Rook (shared border only)"       = "rook",
  "5 nearest neighbours"            = "knn",
  "Distance band (about 102 km)"    = "dist"
)

weights_desc <- c(
  queen = "Two districts are neighbours if they touch at a border or a corner. This is the default used in the analysis tabs.",
  rook  = "Two districts are neighbours only if they share part of a border, so corner-only contact does not count.",
  knn   = "Each district is linked to its 5 nearest districts, measured between centres. Every district therefore has exactly 5 neighbours, so even isolated districts get a lag.",
  dist  = "Districts are neighbours if their centres are within about 102 km, the smallest distance that gives most districts a neighbour. Links are not drawn on the map because there are too many."
)

# Quintile colours for neighbour counts; NULL when counts are (almost) constant
count_pal <- function(n) {
  brk <- unique(quantile(n, probs = seq(0, 1, 0.2), na.rm = TRUE))
  if (length(brk) < 3) return(NULL)
  colorBin(crime_ramp, domain = n, bins = brk, na.color = crime_cols$border)
}

mod_weights_ui <- function(id) {
  ns <- NS(id)
  div(class = "page-wrap",
    page_head("Spatial weights",
              "Spatial statistics need a rule for who counts as a neighbour. Compare four common rules."),

    card(card_body(
      layout_columns(
        col_widths = breakpoints(sm = 12, md = c(5, 7)), fill = FALSE,
        radioButtons(ns("w"), "Neighbour definition", choices = weights_choices, selected = "queen"),
        div(checkboxInput(ns("links"), "Draw neighbour links on the map", FALSE),
            uiOutput(ns("desc")))
      )
    )),

    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6, xl = 3), fill = FALSE,
      uiOutput(ns("s_mean")), uiOutput(ns("s_range")),
      uiOutput(ns("s_median")), uiOutput(ns("s_isolated"))
    ),

    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(8, 4)), fill = FALSE,
      card(full_screen = TRUE,
           card_header(textOutput(ns("map_title"))),
           leafletOutput(ns("map"), height = 560)),
      div(style = "display:flex; flex-direction:column; gap:16px;",
          card(card_header("Neighbours per district"),
               plotOutput(ns("hist"), height = 260)),
          what_this_shows("A district with more neighbours has its spatial lag averaged over more areas, which smooths the result. Districts with no neighbours get no spatial lag, so they are left out of the Moran scatterplot and LISA map."))
    ),

    card(card_header("Summary of all four structures"),
         card_body(tableOutput(ns("tbl"))))
  )
}

mod_weights_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    d <- data$districts

    n_nb <- reactive({
      validate(need(!is.null(data$counts),
                    "Neighbour counts missing: rerun scripts/07_prepare_app_data.R"))
      n <- data$counts[[input$w]]
      validate(need(!is.null(n), "No weights of this type in weights.rds"))
      n
    })

    output$desc <- renderUI(p(class = "text-muted", weights_desc[[input$w]]))

    output$s_mean     <- renderUI(stat_card("Average neighbours", formatC(mean(n_nb()), digits = 1, format = "f"), TRUE))
    output$s_range    <- renderUI(stat_card("Fewest to most", paste(min(n_nb()), "to", max(n_nb()))))
    output$s_median   <- renderUI(stat_card("Median neighbours", format(median(n_nb()))))
    output$s_isolated <- renderUI(stat_card("Districts with no neighbours", format(sum(n_nb() == 0), big.mark = ","), sum(n_nb() == 0) > 0))

    output$map_title <- renderText(paste("Neighbours per district:", names(weights_choices)[weights_choices == input$w]))

    output$map <- renderLeaflet({
      n    <- n_nb()
      pal  <- count_pal(n)
      fill <- if (is.null(pal)) crime_ramp[4] else pal(n)
      labs <- sprintf("<strong>%s</strong><br/>Neighbours: %s",
                      htmlEscape(d$police_district), n)

      m <- leaflet(d, options = leafletOptions(minZoom = 4)) |>
        addPolygons(
          fillColor = fill, fillOpacity = 0.85,
          color = crime_cols$bg, weight = 0.3,
          label = lapply(labs, HTML),
          highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2, bringToFront = TRUE)
        )
      if (!is.null(pal)) {
        m <- m |> addLegend(position = "bottomright", pal = pal, values = n,
                            title = "Neighbours", opacity = 0.95)
      }
      lk <- data$links[[input$w]]
      if (isTRUE(input$links) && !is.null(lk)) {
        m <- m |> addPolylines(data = lk, color = "#FFFFFF", weight = 0.7, opacity = 0.35)
      }
      m
    })

    output$hist <- renderPlot({
      n  <- n_nb()
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
