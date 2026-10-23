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

# Methodology: collapsible panels. Fill in the [square brackets] before hand-in.
methodology_section <- function() {
  tagList(
    div(class = "section-head",
        h2("Methodology"),
        p(class = "lead-muted", "How the data were built and analysed. Click a heading to open or close it.")),
    accordion(
      id = "methodology", open = c("measure", "dataset"),

      accordion_panel("How violent crime is measured", value = "measure",
        p("Violent crime is the sum of seven SAPS categories, chosen so that no incident is counted twice:"),
        tags$ul(
          tags$li("Murder"), tags$li("Attempted murder"), tags$li("Common assault"),
          tags$li("Assault with intent to inflict grievous bodily harm"),
          tags$li("Sexual offences"), tags$li("Aggravated robbery"), tags$li("Common robbery")),
        p("The crime rate is the violent crime total divided by the district population, multiplied by 100,000."),
        p("Rates are strongly right-skewed, so the clustering tests use the logarithm of the rate. Maps and the hot-spot test use the raw rate.")),

      accordion_panel("Building the 1,160-district dataset", value = "dataset",
        p("Crime records are by police station, while the Census 2022 covariates and the boundary file are by police district. Names were matched with a crosswalk and joined to the boundaries. Units with no match in another source were dropped, so the analysis uses the 1,160 districts present in all three sources ([number] crime-only stations and [number]
          covariate-only districts were excluded; see the output of scripts/01_build_master.R)."),
        p("Results therefore describe these 1,160 districts and not every police district in South Africa.")),

      accordion_panel("Spatial weights: who counts as a neighbour", value = "weights",
        p("Spatial statistics need a rule for which districts are neighbours. Four rules were compared: queen (shared border or corner), rook (shared border only), the 5 nearest neighbours, and a distance band of about 102 km between district centres. Queen contiguity is the default for the analysis."),
        p("Weights are row-standardised, so each district's neighbours share a total weight of 1. The exception is the Global G test, which needs binary weights. Districts with no neighbours get no spatial lag and are left out of the Moran scatterplot and the LISA map.")),

      accordion_panel("Testing for spatial clustering", value = "clustering",
        p("Moran's I tests whether similar rates sit near each other. The null hypothesis is that the pattern is spatially random; the alternative is positive spatial autocorrelation (clustering). It is checked with 999 random permutations."),
        p("Geary's C and the Global G test (hot and cold spots) give complementary views. Local Moran's I (LISA) then shows where the clusters are: a district is labelled High-High, Low-Low, High-Low or Low-High only if its p-value, adjusted for 1,160 simultaneous tests (false discovery rate), is below 0.05."),
        p("Results were compared across all four neighbour definitions to check that conclusions do not depend on that choice.")),

      accordion_panel("Spatial regression", value = "regression",
                      p("Violent crime was modelled with ordinary least squares (OLS). The outcome is the log of the empirical Bayes smoothed crime rate. The five covariates are informal dwellings, no schooling, urban share, youth aged 15-34 and no piped water, chosen so that no pair is highly correlated."),
                      p("The residuals from the OLS model are still spatially clustered (Moran's I = 0.177, p < 0.001), so OLS is not enough. The Lagrange multiplier tests favour a spatial lag model, because the robust lag test is significant and the robust error test is not."),
                      p("The spatial lag model (queen weights) fits best. Its spatial dependence parameter is 0.40, its AIC falls from 1,994 (OLS) to 1,880, and its residual Moran's I is close to zero (-0.020, p = 0.86), so the clustering has been removed.")),
      accordion_panel("Sampling", value = "sampling",
                      p("Sampling asks how well a sample of districts can estimate the true national mean crime rate (about 1,446 per 100,000), which is known because every district is in the data. Three probability designs were compared: simple random, where every district has an equal chance of selection; stratified, where districts are split into three groups by urbanisation
                        (low, medium, high) and sampled in proportion to group size; and systematic, where districts are ordered from west to east and every k-th one is selected after a random start."),
                      p("Each design was run at sample sizes of 25, 50, 100 and 200 districts and repeated 1,000 times. Bias, standard error and root mean squared error (RMSE) show how far the sample means land from the true value, and lower RMSE is better. All three designs were approximately unbiased, so RMSE is the main comparison.")))
    )
  
}

mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
  home_hero(),
  div(class = "page-wrap",
    div(class = "section-head", h2("Aim")),
    card(card_header("What the app does"),
         card_body(p("The app explores where violent crime is concentrated across South African police districts and tests whether that pattern is spatially clustered. Each tab follows one part of the course: exploratory analysis, spatial weights, spatial autocorrelation, spatial regression and sampling."))),

    methodology_section(),

    div(class = "section-head", h2("Quick Results")),

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

    div(class = "section-head", h2("App Contents")),

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
      p_txt <- if (is.na(m$moran_p)) "" else if (m$moran_p < 0.001) ", p < 0.001" else paste0(", p = ", signif(m$moran_p, 2))
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
