# =====================================================================
# apps/crime_app/app.R  -  Violent Crime in South African police districts
# Run with:  shiny::runApp("apps/crime_app")   (from the project root)
# This file only wires things together. Colours and fonts are in R/theme.R,
# shared functions in R/helpers.R, and each tab in its own R/mod_*.R file.
# =====================================================================

library(shiny)
library(bslib)
library(leaflet)
library(sf)
library(dplyr)
library(ggplot2)

# ---- Load code: theme and helpers first, then every module ---------------------
source("R/theme.R")
source("R/helpers.R")
for (f in setdiff(list.files("R", pattern = "\\.R$", full.names = TRUE),
                  c("R/theme.R", "R/helpers.R"))) source(f)

# ---- Load data once at start-up -------------------------------------------------
load_if_exists <- function(f) if (file.exists(f)) readRDS(f) else NULL

app_data <- list(
  districts  = readRDS("data/districts.rds"),
  meta       = readRDS("data/app_meta.rds"),
  links      = readRDS("data/neighbour_links.rds"),
  tables     = readRDS("data/tables.rds"),
  regression = load_if_exists("data/regression_results.rds"),
  sampling   = load_if_exists("data/sampling_results.rds")
)

# ---- Tabs: name, label, owner (owner shows on the placeholder) -----------------
tabs <- list(
  list(name = "home",     label = "Home",            owner = "the lead"),
  list(name = "explore",  label = "Explore",         owner = "the Phase 2 owner"),
  list(name = "weights",  label = "Weights",         owner = "the Phase 3 owner"),
  list(name = "autocorr", label = "Autocorrelation", owner = "the Phase 4 owner"),
  list(name = "regression", label = "Regression",    owner = "the Phase 5 owner"),
  list(name = "sampling", label = "Sampling",        owner = "the Phase 6 owner"),
  list(name = "about",    label = "About",           owner = "the lead")
)

ui <- do.call(
  page_navbar,
  c(
    list(
      id = "nav",
      title = tags$span(tags$span(class = "brand-dot"), "Violent Crime"),
      window_title = "Violent Crime | South African police districts",
      theme = crime_theme,
      fillable = FALSE,
      lang = "en",
      header = tags$head(tags$link(rel = "stylesheet", href = "custom.css"))
    ),
    lapply(tabs, function(t) nav_panel(t$label, tab_ui(t$name, t$label, t$owner)))
  )
)

server <- function(input, output, session) {
  for (t in tabs) tab_server(t$name, app_data)
}

shinyApp(ui, server)
