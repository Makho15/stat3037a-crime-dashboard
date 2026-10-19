# =====================================================================
# R/mod_about.R  -  About tab (owner: Phase 7 lead)
# Fill in the [square brackets] before hand-in.
# =====================================================================

mod_about_ui <- function(id) {
  div(class = "page-wrap",
    page_head("About this app",
              "STAT3037A Introduction to Spatial Statistics, University of the Witwatersrand."),

    card(card_header("What the app does"),
         card_body(p("The app explores where violent crime is concentrated across South African police districts and tests whether that pattern is spatially clustered. Each tab follows one part of the course: exploratory analysis, spatial weights, spatial autocorrelation, spatial regression and sampling."))),

    card(card_header("How violent crime is measured"),
         card_body(
           p("Violent crime is the sum of seven SAPS categories, chosen so that no incident is counted twice:"),
           tags$ul(
             tags$li("Murder"), tags$li("Attempted murder"), tags$li("Common assault"),
             tags$li("Assault with intent to inflict grievous bodily harm"),
             tags$li("Sexual offences"), tags$li("Aggravated robbery"), tags$li("Common robbery")),
           p("The crime rate is the violent crime total divided by the district population, multiplied by 100,000."))),

    card(card_header("Data sources"),
         card_body(tags$ul(
           tags$li("SAPS annual crime statistics by police station, 2023/24 (via DataFirst, sapacr-2005-2026-v1.5)."),
           tags$li("Population and area by station (afrith police_stations data)."),
           tags$li("Census 2022 police-district covariates: informal dwellings, schooling, electricity, urban share, household size, piped water, sex and age."),
           tags$li("Police district boundaries (shapefile).")))),

    card(card_header("Building the 1,160-district dataset"),
         card_body(p("Crime records are by police station and the Census covariates are by police district, so names were matched with a crosswalk and joined to the boundaries. Records without a match on both sides were dropped: [number] crime-only stations and [number] covariate-only districts (see the output of scripts/01_build_master.R)."))),

    card(card_header("Limitations"),
         card_body(tags$ul(
           tags$li("Counts are reported crimes, so they reflect reporting as well as occurrence."),
           tags$li("Results describe association, not causation."),
           tags$li("Census 2022 counts are survey-weighted, so some are not whole numbers."),
           tags$li("Polygons are simplified for display; all statistics were computed on the full boundaries."),
           tags$li("[Add any limitation specific to your own tab.]")))),

    card(card_header("Team"),
         card_body(p("[Names and the tab each person built.]")))
  )
}

mod_about_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {})
}
