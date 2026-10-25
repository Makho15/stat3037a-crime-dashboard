# =====================================================================
# R/helpers.R  -  shared functions used by every tab
# Needs theme.R to be sourced first (app.R does this).
# =====================================================================
library(shiny)
library(bslib)
library(leaflet)
library(htmltools)

# ---- Map colours -------------------------------------------------------
# Quintile bins, so the legend shows real rate values
crime_pal <- function(vals) {
  brk <- unique(quantile(vals, probs = seq(0, 1, 0.2), na.rm = TRUE))
  colorBin(crime_ramp, domain = vals, bins = brk, na.color = crime_cols$border)
}

fmt_rate <- function(x) format(round(x, 1), big.mark = ",", nsmall = 1, trim = TRUE)

# ---- Maps ----------------------------------------------------------------
# Choropleth of any numeric column of `data` (an sf object in EPSG:4326).
# Hovering shows the district name and the value.
crime_choropleth <- function(data, var = "crime_rate",
                             legend_title = "Violent crimes per 100,000") {
  vals <- data[[var]]
  pal  <- crime_pal(vals)
  labs <- sprintf("<strong>%s</strong><br/>%s: %s",
                  htmlEscape(data$police_district), htmlEscape(legend_title), fmt_rate(vals))

  leaflet(data, options = leafletOptions(minZoom = 4)) |>
    addPolygons(
      fillColor = pal(vals), fillOpacity = 0.9,
      color = crime_cols$bg, weight = 0.3,
      label = lapply(labs, HTML),
      highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2, bringToFront = TRUE)
    ) |>
    addLegend(position = "bottomright", pal = pal, values = vals,
              title = legend_title, opacity = 0.95)
}

# LISA cluster map. `data` must have a `cluster` column (from lisa_results).
lisa_map <- function(data) {
  cl   <- as.character(data$cluster)
  fill <- unname(lisa_cols[cl])
  labs <- sprintf("<strong>%s</strong><br/>%s", htmlEscape(data$police_district), htmlEscape(cl))
  show <- names(lisa_cols) != "No neighbours"

  leaflet(data, options = leafletOptions(minZoom = 4)) |>
    addPolygons(
      fillColor = fill, fillOpacity = 0.9,
      color = crime_cols$bg, weight = 0.3,
      label = lapply(labs, HTML),
      highlightOptions = highlightOptions(color = "#FFFFFF", weight = 2, bringToFront = TRUE)
    ) |>
    addLegend(position = "bottomright", colors = unname(lisa_cols[show]),
              labels = names(lisa_cols)[show], title = "LISA cluster", opacity = 0.95)
}

# ---- Small UI pieces -------------------------------------------------------
stat_card <- function(label, value, accent = FALSE) {
  div(class = "stat-card",
      div(class = "stat-label", label),
      div(class = paste("stat-value", if (accent) "stat-accent"), value))
}

what_this_shows <- function(...) {
  div(class = "what-card",
      h2("What this shows"),
      p(...))
}

# Page heading with the bullet-and-shield animation (see "Page banner" in custom.css)
page_head <- function(title, subtitle = NULL) {
  div(class = "pbanner",
      div(class = "pb-text",
          div(class = "page-head",
              h1(title),
              if (!is.null(subtitle)) p(class = "lead-muted", subtitle))),
      div(class = "pb-bullet", tags$img(src = "img/bullet.png", alt = "")),
      div(class = "pb-shield",
          tags$img(src = "img/shield.png", alt = "Four people holding a shield")))
}

# ---- Placeholder shown until a tab owner pushes their module ---------------
placeholder_ui <- function(id, title, owner) {
  div(class = "page-wrap",
      page_head(title),
      card(div(class = "placeholder",
               p("This tab is being built by ", strong(owner), "."),
               p(class = "text-muted",
                 "It appears here as soon as its module file is pushed."))))
}

# ---- Tab dispatcher ----------------------------------------------------------
# Uses mod_<name>_ui / mod_<name>_server when that file exists, otherwise the
# placeholder. So tabs switch on automatically as owners push their files.
tab_ui <- function(name, title, owner) {
  fn <- paste0("mod_", name, "_ui")
  if (exists(fn, mode = "function")) get(fn)(name) else placeholder_ui(name, title, owner)
}

tab_server <- function(name, data) {
  fn <- paste0("mod_", name, "_server")
  if (exists(fn, mode = "function")) get(fn)(name, data)
}
