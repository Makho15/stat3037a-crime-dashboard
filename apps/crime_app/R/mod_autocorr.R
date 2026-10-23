# Module: Autocorrelation tab (Phase 4)
# Reads only saved results from apps/crime_app/data/. Nothing is recomputed
# except a cheap reprojection and filter for display.

library(shiny)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

# ---- Helpers (local to this module) ---------------------------------------
.ac_levels <- c("High-High", "Low-Low", "High-Low", "Low-High",
                "Not significant", "No neighbours")

# Loads the Phase 4 results from data/. Used when `data` is NULL.
.ac_load <- function(dir = "data") {
  list(
    lisa   = readRDS(file.path(dir, "lisa_results.rds")),
    global = read.csv(file.path(dir, "global_autocorrelation.csv")),
    sens   = read.csv(file.path(dir, "moran_sensitivity.csv")),
    counts = read.csv(file.path(dir, "lisa_cluster_counts.csv"))
  )
}

# ASSUMPTION: shared palette lives in crime_palettes$cluster (from helpers.R).
# Falls back to the Phase 4 colours if it isn't found.
.ac_palette <- function() {
  pal <- tryCatch(crime_palettes$cluster, error = function(e) NULL)
  if (is.null(pal)) {
    pal <- c("High-High" = "#d7191c", "Low-Low" = "#2c7bb6",
             "High-Low" = "#fdae61", "Low-High" = "#abd9e9",
             "Not significant" = "grey92", "No neighbours" = "white")
  }
  pal
}

# ---- UI --------------------------------------------------------------------
mod_autocorr_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 3,
      checkboxGroupInput(
        ns("classes"), "LISA classes to show",
        choices  = .ac_levels,
        selected = .ac_levels
      ),
      checkboxInput(ns("sig_only"), "Significant districts only", FALSE),
      radioButtons(
        ns("table_choice"), "Results table",
        choices = c("Global tests" = "global",
                    "Weights sensitivity (Moran's I)" = "sens")
      ),
      div(
        class = "well",
        h5("What this shows"),
        p("Global tests ask whether violent crime is clustered across the country.",
          "The map shows where: High-High districts are hot spots (high crime",
          "surrounded by high crime) and Low-Low districts are cold spots.",
          "Classes use Local Moran's I with queen weights and FDR-adjusted",
          "p-values (5% level)."),
        p("Reading the map: urban districts are small, so Gauteng hot spots look",
          "tiny at national scale. Zoom in to see them.")
      )
    ),
    mainPanel(
      width = 9,
      leafletOutput(ns("map"), height = 520),
      br(),
      fluidRow(
        column(5, plotOutput(ns("counts_plot"), height = 260)),
        column(7, tableOutput(ns("results_table")))
      )
    )
  )
}

# ---- Server ----------------------------------------------------------------
mod_autocorr_server <- function(id, data = NULL) {
  moduleServer(id, function(input, output, session) {
    
    # `data` may be NULL, a list, or a reactive returning a list.
    res <- reactive({
      d <- if (is.null(data)) NULL else if (is.function(data)) data() else data
      if (is.null(d)) d <- .ac_load()
      d
    })
    
    pal_vals <- .ac_palette()
    
    # Reproject once for leaflet (needs lon/lat)
    lisa_ll <- reactive({
      x <- res()$lisa
      x$cluster <- factor(as.character(x$cluster), levels = .ac_levels)
      st_transform(x, 4326)
    })
    
    shown <- reactive({
      x <- lisa_ll()
      keep <- as.character(x$cluster) %in% input$classes
      if (isTRUE(input$sig_only)) {
        keep <- keep & !(as.character(x$cluster) %in%
                           c("Not significant", "No neighbours"))
      }
      x[keep, ]
    })
    
    output$map <- renderLeaflet({
      x <- lisa_ll()
      leaflet(x) |>
        addProviderTiles(providers$CartoDB.Positron) |>
        fitBounds(st_bbox(x)[["xmin"]], st_bbox(x)[["ymin"]],
                  st_bbox(x)[["xmax"]], st_bbox(x)[["ymax"]])
    })
    
    observe({
      x <- shown()
      p <- colorFactor(unname(pal_vals[.ac_levels]), levels = .ac_levels)
      lab <- sprintf("<strong>%s</strong><br/>Log crime rate: %.2f<br/>Class: %s",
                     x$police_district, x$value, x$cluster) |>
        lapply(htmltools::HTML)
      
      leafletProxy("map", data = x) |>
        clearShapes() |>
        clearControls() |>
        addPolygons(
          fillColor = ~p(cluster), fillOpacity = 0.85,
          color = "grey50", weight = 0.4,
          label = lab,
          labelOptions = labelOptions(textsize = "13px"),
          highlightOptions = highlightOptions(weight = 2, color = "black",
                                              bringToFront = TRUE)
        ) |>
        addLegend("bottomright", pal = p, values = ~cluster,
                  title = "LISA cluster", opacity = 0.9)
    })
    
    output$counts_plot <- renderPlot({
      cnt <- res()$counts
      names(cnt)[1:2] <- c("cluster", "n")
      cnt <- cnt[cnt$cluster %in% input$classes, ]
      ggplot(cnt, aes(x = reorder(cluster, n), y = n, fill = cluster)) +
        geom_col(show.legend = FALSE) +
        geom_text(aes(label = n), hjust = -0.2, size = 3.5) +
        scale_fill_manual(values = pal_vals) +
        coord_flip(clip = "off") +
        labs(title = "Districts per class", x = NULL, y = "Districts") +
        theme_crime()
    })
    
    output$results_table <- renderTable({
      tab <- if (input$table_choice == "global") res()$global else res()$sens
      num <- vapply(tab, is.numeric, logical(1))
      tab[num] <- lapply(tab[num], signif, 4)
      tab
    }, digits = 4, striped = TRUE, spacing = "s")
  })
}

