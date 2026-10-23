# =====================================================================
# R/mod_home.R  -  Home tab (owner: Phase 7 lead)
# An overview of the story. No maps here: the rate map is in Explore and
# the cluster map is in Autocorrelation.
# =====================================================================

tour_item <- function(title, text) {
  div(class = "tour-item", div(class = "tour-title", title), div(class = "tour-text", text))
}

# Full-width banner: a police van drives to the station and reveals the title.
# The animation is pure CSS (see the "Home hero" section of www/custom.css).
home_hero <- function() {
  div(class = "hero",
    div(class = "hero-glow"),
    div(class = "hero-title-wrap",
        div(class = "hero-title-inner",
            h1(class = "hero-title",
               "South African ", tags$span(class = "hl", "Violent Crime"),
               " Analysis through a Spatial Statistics Lens"),
            p(class = "hero-sub", "STAT3037A Introduction to Spatial Statistics  ·  1,160 police districts  ·  2023/24"))),
    tags$img(class = "hero-station", src = "img/station.png",
             alt = "A South African Police Service police station"),
    div(class = "hero-road"),
    div(class = "hero-van",
        tags$img(src = "img/van.png", alt = "A South African Police Service van driving to the station")))
}

mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
  home_hero(),
  div(class = "page-wrap",
    card(card_header("What the app does"),
         card_body(p("The app explores where violent crime is concentrated across South African police districts and tests whether that pattern is spatially clustered. Each tab follows one part of the course: exploratory analysis, spatial weights, spatial autocorrelation, spatial regression and sampling."))),

    card(card_header("How violent crime is measured"),
         card_body(
           p("Violent crime is the sum of seven SAPS categories, chosen so that no incident is counted twice:"),
           tags$ul(
             tags$li("Murder"), tags$li("Attempted murder"), tags$li("Common assault"),
             tags$li("Assault with intent to inflict grievous bodily harm"),
             tags$li("Sexual offences"), tags$li("Aggravated robbery"), tags$li("Common robbery"), tags$li("Kidnapping")),
           p("The crime rate is the violent crime total divided by the district population, multiplied by 100,000."))),

    layout_columns(
      col_widths = breakpoints(sm = 12, md = 6, xl = 3), fill = FALSE,
      uiOutput(ns("s_districts")), uiOutput(ns("s_median")),
      uiOutput(ns("s_moran")),     uiOutput(ns("s_hh"))
    ),

    card(card_header("Key findings"), card_body(uiOutput(ns("findings")))),

    layout_columns(
      col_widths = breakpoints(sm = 12, lg = c(6, 6)), fill = FALSE,
      card(card_header("Ten highest violent crime rates, 2023/24"),
           plotOutput(ns("top10"), height = 380)),
      card(card_header("Districts in each cluster type"),
           plotOutput(ns("clusters"), height = 380))
    ),

    what_this_shows("The left chart ranks districts by violent crimes per 100,000 residents. The right chart counts districts by the type of spatial cluster they belong to, from local Moran's I: High-High are hot spots and Low-Low are cold spots."),

    card(card_header("What is in this app"),
         card_body(div(class = "tour-grid",
           tour_item("Explore",         "Distributions and maps of the crime rate and its social and economic covariates."),
           tour_item("Weights",         "How districts are linked to their neighbours, with four neighbour definitions to compare."),
           tour_item("Autocorrelation", "Tests of whether violent crime is spatially clustered, and where the hot and cold spots are."),
           tour_item("Regression",      "Models of what is associated with violent crime, including spatial lag and error models."),
           tour_item("Sampling",        "How simple, stratified and systematic samples compare with the full set of districts."),
           tour_item("About",           "Data sources, limitations and the team.")
         )))
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

    output$findings <- renderUI({
      p_txt <- if (is.na(m$moran_p)) "" else if (m$moran_p < 0.05) ", p < 0.05" else paste0(", p = ", signif(m$moran_p, 2))
      tags$ul(
        tags$li(strong("Violent crime is spatially clustered. "),
                sprintf("Global Moran's I is %.3f%s, so districts with high rates tend to sit next to other high-rate districts.",
                        m$moran_I, p_txt)),
        tags$li(strong("Hot and cold spots. "),
                sprintf("%s districts form High-High clusters (hot spots) and %s form Low-Low clusters (cold spots).",
                        format(m$n_hh, big.mark = ","), format(m$n_ll, big.mark = ","))),
        tags$li(strong("Reading the rates. "),
                "Rates are violent crimes per 100,000 residents in 2023/24. In sparsely populated districts a few incidents can produce a very high rate.")
      )
    })

    output$top10 <- renderPlot({
      top <- d |>
        sf::st_drop_geometry() |>
        dplyr::arrange(dplyr::desc(crime_rate)) |>
        head(10)
      nm <- tools::toTitleCase(as.character(top$police_district))
      top$name <- factor(nm, levels = rev(nm))
      ggplot(top, aes(crime_rate, name)) +
        geom_col(fill = crime_cols$accent) +
        geom_text(aes(label = fmt_rate(crime_rate)), hjust = -0.1,
                  colour = crime_cols$text, size = 3.5) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.15)), labels = scales::comma) +
        labs(x = "Violent crimes per 100,000", y = NULL) +
        theme_crime() +
        theme(panel.grid.major.y = element_blank())
    }, bg = crime_cols$surface, res = 96)

    output$clusters <- renderPlot({
      cl <- as.data.frame(table(cluster = d$cluster), stringsAsFactors = FALSE)
      cl <- cl[cl$Freq > 0 & cl$cluster != "No neighbours", ]
      cl$cluster <- factor(cl$cluster, levels = rev(names(lisa_cols)))
      ggplot(cl, aes(Freq, cluster, fill = cluster)) +
        geom_col(colour = crime_cols$muted, linewidth = 0.2, show.legend = FALSE) +
        geom_text(aes(label = format(Freq, big.mark = ",")), hjust = -0.15,
                  colour = crime_cols$text, size = 3.5) +
        scale_fill_manual(values = lisa_cols) +
        scale_x_continuous(expand = expansion(mult = c(0, 0.15))) +
        labs(x = "Number of districts", y = NULL) +
        theme_crime() +
        theme(panel.grid.major.y = element_blank())
    }, bg = crime_cols$surface, res = 96)
  })
}
