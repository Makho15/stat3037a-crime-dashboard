# =====================================================================
# R/mod_home.R  -  Home tab (owner: Phase 7 lead)
# =====================================================================

mod_home_ui <- function(id) {
  ns <- NS(id)
  div(class = "page-wrap",
    page_head("Where violent crime concentrates",
              "An exploration of violent crime rates across South African police districts, using spatial statistics."),

    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6, xl = 3), fill = FALSE,
      uiOutput(ns("s_districts")), uiOutput(ns("s_median")),
      uiOutput(ns("s_moran")),     uiOutput(ns("s_hh"))
    ),

    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(8, 4)), fill = FALSE,
      card(full_screen = TRUE,
           card_header("Violent crime rate by police district, 2023/24"),
           leafletOutput(ns("map"), height = 560)),
      div(style = "display:flex; flex-direction:column; gap:16px;",
          card(card_header("Cluster types (local Moran's I)"),
               card_body(uiOutput(ns("clusters")))),
          what_this_shows("[Two plain-English lines for the Home tab, written by the lead.]"))
    )
  )
}

mod_home_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    d <- data$districts
    m <- data$meta

    output$s_districts <- renderUI(stat_card("Police districts", format(m$n_districts, big.mark = ","), TRUE))
    output$s_median    <- renderUI(stat_card("National median rate (per 100,000)", fmt_rate(m$median_rate)))
    output$s_moran     <- renderUI(stat_card("Global Moran's I", formatC(m$moran_I, digits = 3, format = "f"), TRUE))
    output$s_hh        <- renderUI(stat_card("High-High districts", format(m$n_hh, big.mark = ",")))

    output$map <- renderLeaflet(crime_choropleth(d, "crime_rate", "Violent crimes per 100,000"))

    output$clusters <- renderUI({
      cc <- data$tables$lisa_cluster_counts
      counts <- if (!is.null(cc)) setNames(cc$Freq, as.character(cc$cluster)) else NULL
      rows <- lapply(setdiff(names(lisa_cols), "No neighbours"), function(nm) {
        n <- if (!is.null(counts) && nm %in% names(counts)) format(counts[[nm]], big.mark = ",") else ""
        div(class = "legend-row",
            span(class = "legend-swatch", style = paste0("background:", lisa_cols[[nm]])),
            span(nm), span(class = "legend-count", n))
      })
      tagList(rows)
    })
  })
}
