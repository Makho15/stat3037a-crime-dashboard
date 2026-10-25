# =====================================================================
# R/theme.R  -  Theme "Crimson and Slate"
# Every colour and font lives here. Change them here and the whole app,
# including maps and ggplots, follows.
# =====================================================================
library(bslib)
library(ggplot2)

# ---- Colours ---------------------------------------------------------
crime_cols <- list(
  bg           = "#141920",   # page background
  surface      = "#1E2530",   # cards
  border       = "#343E4C",
  text         = "#E8EBEF",
  muted        = "#9AA6B4",
  accent       = "#E0213F",   # crimson: emphasis only
  accent_light = "#FF7A8F"
)

# Sequential ramp for crime rate (low -> high), dark slate to crimson
crime_ramp <- c("#262B35", "#5C2837", "#9E2342", "#E0213F", "#FF7A8F")

# LISA cluster colours: same hues as the Phase 4 figure
lisa_cols <- c(
  "High-High"       = "#D7191C",
  "Low-Low"         = "#2C7BB6",
  "High-Low"        = "#FDAE61",
  "Low-High"        = "#ABD9E9",
  "Not significant" = "#343E4C",
  "No neighbours"   = "#141920"
)

# ---- Bootstrap 5 theme (bslib) -----------------------------------------
# font_google() downloads the fonts once and caches them. If the machine is
# offline, swap both lines for: font_collection("system-ui", "sans-serif")
crime_theme <- bs_theme(
  version      = 5,
  bg           = crime_cols$bg,
  fg           = crime_cols$text,
  primary      = crime_cols$accent,
  secondary    = crime_cols$muted,
  base_font    = font_google("Inter"),
  heading_font = font_google("Inter"),
  code_font    = font_google("IBM Plex Mono")
) |>
  bs_add_variables(
    "card-bg"            = crime_cols$surface,
    "card-border-color"  = crime_cols$border,
    "card-border-radius" = "12px",
    "border-color"       = crime_cols$border
  )

# ---- ggplot theme ------------------------------------------------------
theme_crime <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text             = element_text(colour = crime_cols$text),
      plot.background  = element_rect(fill = crime_cols$surface, colour = NA),
      panel.background = element_rect(fill = crime_cols$surface, colour = NA),
      panel.grid.major = element_line(colour = crime_cols$border, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      axis.text        = element_text(colour = crime_cols$muted),
      axis.title       = element_text(colour = crime_cols$muted),
      plot.title       = element_text(face = "bold", colour = "#FFFFFF"),
      plot.subtitle    = element_text(colour = crime_cols$muted),
      legend.text      = element_text(colour = crime_cols$muted),
      legend.title     = element_text(colour = crime_cols$text),
      strip.text       = element_text(colour = crime_cols$text, face = "bold")
    )
}

scale_fill_crime   <- function(...) scale_fill_gradientn(colours = crime_ramp, ...)
scale_colour_crime <- function(...) scale_colour_gradientn(colours = crime_ramp, ...)
