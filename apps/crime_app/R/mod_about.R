# =====================================================================
# R/mod_about.R  -  About tab (owner: Phase 7 lead)
# Fill in the [square brackets] before hand-in.
# =====================================================================

# A link that opens in a new tab
ext_link <- function(url, text) {
  tags$a(href = url, target = "_blank", rel = "noopener noreferrer", text)
}

# One team member: photo on the left, name and role on the right.
# Photos live in www/team/. If a photo is missing, the initials show instead.
team_member <- function(name, role, file) {
  parts    <- strsplit(name, " ")[[1]]
  initials <- paste(substr(parts[seq_len(min(2, length(parts)))], 1, 1), collapse = "")
  div(class = "team-member",
      div(class = "team-avatar",
          span(initials),
          tags$img(src = paste0("team/", file), alt = paste("Photo of", name),
                   onerror = "this.style.display='none'")),
      div(class = "team-text",
          div(class = "team-name", name),
          div(class = "team-role", role)))
}

mod_about_ui <- function(id) {
  div(class = "page-wrap",
    page_head("About this app",
              "STAT3037A Introduction to Spatial Statistics, University of the Witwatersrand."),

    card(card_header("Data sources"),
         card_body(tags$ul(
           tags$li(ext_link("https://www.saps.gov.za/services/crimestats.php", "SAPS annual crime statistics"),
                   " by police station, 2023/24, obtained through ",
                   ext_link("https://www.datafirst.uct.ac.za/dataportal/index.php/catalog/1012", "DataFirst (sapacr-2005-2026-v1.5)"), "."),
           # TODO: add the link for the afrith police_stations data if you have the URL
           tags$li("Population and area by station (afrith police_stations data)."),
           tags$li("Census 2022 police-district covariates (informal dwellings, schooling, electricity, urban share, household size, piped water, sex and age) from ",
                   ext_link("https://census.statssa.gov.za/", "Statistics South Africa"), ", via the DataFirst ",
                   ext_link("https://datafirst.uct.ac.za/dataportal/index.php/catalog/1131", "Census Administrative and Service Provision District Profiles 2022"), "."),
           tags$li(ext_link("https://open.africa/dataset/police-station-boundaries", "Police station boundaries"),
                   " (shapefile, Code for South Africa / Africa Open Data).")))),

    card(card_header("Building the 1,160-district dataset"),
         card_body(p("Crime records are by police station and the Census covariates are by police district, so names were matched with a crosswalk and joined to the boundaries. Records without a match on both sides were dropped: [number] crime-only stations and [number] covariate-only districts (see the output of scripts/01_build_master.R)."))),

    card(card_header("Limitations"),
         card_body(tags$ul(
           tags$li("Counts are reported crimes, so they reflect reporting as well as occurrence."),
           tags$li("Results describe association, not causation."),
           tags$li("Census 2022 counts are survey-weighted, so some are not whole numbers."),
           tags$li("Polygons are simplified for display; all statistics were computed on the full boundaries."),
           tags$li("Districts excluded because the datasets do not match. SAPS reports crime by police station, while the Census 2022 covariates and the boundary file are by police district, and the three sources do not list exactly the same units")))),

    card(card_header("Team"),
         card_body(div(class = "team-grid",
           team_member("Ayanda Sibanda",    "Autocorrelation tab",         "ayanda.jpeg"),
           team_member("Lethabo Madzibedi", "Explore and Regression tabs", "lethabo.jpeg"),
           team_member("Makhosonke Tugwana", "Weights tab", "makhosonke.jpg"),
           team_member("Musawenkosi Mahlangu",              "Sampling tab",                "musa.jpeg")
         )))
  )
}

mod_about_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {})
}
