# mod_sampling.R: Sampling tab (Phase 6 owner)
# Reads: data$sampling (sampling_results.rds contents), data$districts (sf, 1,160 rows)

mod_sampling_ui <- function(id) {
  ns <- NS(id)
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 300,
      radioButtons(ns("design"), "Sampling design",
                   choices = c("Simple random" = "random",
                               "Stratified (urbanisation)" = "stratified",
                               "Systematic (west to east)" = "systematic")),
      selectInput(ns("n"), "Sample size (districts)",
                  choices = c(25, 50, 100, 200), selected = 50),
      actionButton(ns("draw"), "Draw new sample", class = "btn-primary w-100")
    ),
    bslib::card(
      bslib::card_header("Sampled districts"),
      leaflet::leafletOutput(ns("map"), height = 420)
    ),
    bslib::layout_columns(
      col_widths = c(4, 4, 4),
      bslib::card(bslib::card_header("This sample"), tableOutput(ns("this_sample"))),
      bslib::card(bslib::card_header("1,000 repeated samples"), plotOutput(ns("hist"), height = 250)),
      bslib::card(bslib::card_header("Performance by design"), tableOutput(ns("perf")))
    ),
    bslib::card(
      bslib::card_header("What this shows"),
      p("Each design was repeated 1,000 times at each sample size and compared with the true mean ",
        "violent crime rate across all 1,160 districts (about 1,446 per 100,000). All three designs ",
        "were approximately unbiased, and the error fell as the sample size grew (simple random ",
        "sampling RMSE: 664 at n = 25, 256 at n = 200). At n = 50 the designs were almost identical ",
        "(RMSE 539 to 549), so none was clearly best, and stratified and systematic sampling were ",
        "worse than simple random sampling at n = 25. Crime rates are highly right-skewed ",
        "(skewness 21.47), so a few extreme districts drive most of the sampling error.")
    )
  )
}

mod_sampling_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    s  <- data$sampling
    d  <- data$districts
    dd <- sf::st_drop_geometry(d)
    N  <- nrow(dd)
    
    # Replace with the real constants from R/theme.R (names may differ)
    accent <- if (exists("COL_ACCENT")) COL_ACCENT else "#F5A524"
    labels <- c(random = "Simple random", stratified = "Stratified", systematic = "Systematic")
    
    # Ordering for systematic sampling: centroid longitude, west to east
    lon <- sf::st_coordinates(suppressWarnings(sf::st_centroid(sf::st_geometry(d))))[, 1]
    ord <- order(lon)
    
    # Three urbanisation strata of roughly equal size
    strata <- ceiling(3 * rank(dd$pct_urban, ties.method = "first") / N)
    
    draw_sample <- function(design, n) {
      if (design == "random") {
        sample(N, n)
      } else if (design == "stratified") {
        sizes <- round(n * table(strata) / N)
        sizes[which.max(sizes)] <- sizes[which.max(sizes)] + (n - sum(sizes))
        unlist(lapply(names(sizes), function(g) {
          pool <- which(strata == as.numeric(g))
          pool[sample.int(length(pool), sizes[[g]])]
        }))
      } else {
        k   <- N / n
        pos <- ceiling(runif(1, 0, k) + (0:(n - 1)) * k)
        ord[pos]
      }
    }
    
    sel <- reactive({
      input$draw                      # re-draw when the button is clicked
      draw_sample(input$design, as.numeric(input$n))
    })
    
    output$map <- leaflet::renderLeaflet({
      m <- d
      m$selected <- seq_len(N) %in% sel()
      lab <- sprintf("%s<br/>Crime rate: %s per 100,000%s",
                     m$police_district, format(round(m$crime_rate), big.mark = ","),
                     ifelse(m$selected, "<br/><b>In sample</b>", ""))
      leaflet::leaflet(m) |>
        leaflet::addProviderTiles("CartoDB.DarkMatter") |>   # use the same base layer as helpers.R
        leaflet::addPolygons(
          fillColor = ifelse(m$selected, accent, "#3A4450"),
          fillOpacity = ifelse(m$selected, 0.9, 0.5),
          color = "#222", weight = 0.3,
          label = lapply(lab, htmltools::HTML)
        ) |>
        leaflet::addLegend(position = "bottomright",
                           colors = c(accent, "#3A4450"),
                           labels = c("In sample", "Not sampled"),
                           title = paste0(labels[[input$design]], ", n = ", input$n))
    })
    
    output$this_sample <- renderTable({
      est <- mean(dd$crime_rate[sel()])
      tru <- s$true_mean
      data.frame(
        Measure = c("Sample mean", "True national mean", "Error", "Error (%)"),
        Value   = c(format(round(est), big.mark = ","),
                    format(round(tru), big.mark = ","),
                    format(round(est - tru), big.mark = ","),
                    sprintf("%.1f%%", 100 * (est - tru) / tru))
      )
    })
    
    output$hist <- renderPlot({
      r <- subset(s$results, design == input$design & n == as.numeric(input$n))
      ggplot2::ggplot(r, ggplot2::aes(estimate)) +
        ggplot2::geom_histogram(bins = 30, fill = accent, colour = "white") +
        ggplot2::geom_vline(xintercept = s$true_mean, linetype = "dashed", colour = "red") +
        ggplot2::geom_vline(xintercept = mean(dd$crime_rate[sel()]), colour = "white") +
        ggplot2::labs(subtitle = "Dashed = true mean, solid = this sample",
                      x = "Sample mean crime rate", y = "Samples") +
        theme_crime() 
    })
    
    output$perf <- renderTable({
      p <- subset(s$summary, n == as.numeric(input$n))
      data.frame(Design = labels[p$design], Bias = round(p$bias),
                 SE = round(p$se), RMSE = round(p$rmse))
    })
  })
}



