# =====================================================================
# 07_prepare_app_data.R
# Purpose : Phase 7, Step 2. Turn the finished analysis results into small,
#           app-ready files in apps/crime_app/data/ so the app never has to
#           recompute anything and runs from a fresh clone.
# Input   : data/clean/master_analysis.rds          (Phase 2)
#           outputs/objects/weights.rds             (Phase 3)
#           outputs/objects/lisa_results.rds        (Phase 4)
#           outputs/objects/regression_results.rds  (Phase 5)
#           outputs/objects/sampling_results.rds    (Phase 6)
#           outputs/tables/*.csv                    (Phases 3 and 4)
# Output  : apps/crime_app/data/districts.rds        simplified map + attributes + LISA
#           apps/crime_app/data/neighbour_links.rds  link lines for the Weights tab
#           apps/crime_app/data/tables.rds           summary tables
#           apps/crime_app/data/app_meta.rds         headline numbers
#           apps/crime_app/data/regression_results.rds, sampling_results.rds (copied)
# Run from the project root (open the .Rproj file first).
# Needs: install.packages("rmapshaper")
# =====================================================================

library(sf)
library(dplyr)
library(spdep)
library(rmapshaper)

out_dir <- "apps/crime_app/data"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Load and check alignment --------------------------------------------
master <- readRDS("data/clean/master_analysis.rds")
wts    <- readRDS("outputs/objects/weights.rds")
lisa   <- readRDS("outputs/objects/lisa_results.rds")

stopifnot(
  inherits(master, "sf"),
  nrow(master) == 1160,
  identical(as.character(master$police_district), as.character(wts$id)),
  identical(as.character(master$police_district), as.character(lisa$police_district))
)
cat("Alignment OK:", nrow(master), "districts\n")

# ---- 2. Simplified polygons in WGS84 (what leaflet needs) --------------------
geo <- master |>
  select(police_district) |>
  st_make_valid() |>
  st_transform(4326)

geo <- ms_simplify(geo, keep = 0.05, keep_shapes = TRUE, explode = FALSE) |>
  st_make_valid()

stopifnot(nrow(geo) == nrow(master),
          identical(as.character(geo$police_district), as.character(master$police_district)))

# ---- 3. Attributes + LISA results on the simplified polygons -------------------
lisa_tbl <- lisa |>
  st_drop_geometry() |>
  select(police_district, local_I, p_fdr, cluster)

attrs <- master |>
  st_drop_geometry() |>
  select(any_of(c(
    "police_district", "local_muni", "district_muni",
    "Violent_crime_total", "crime_rate", "log_crime_rate", "crime_rate_eb",
    "murder", "attempted_murder", "common_assault", "assault_gbh",
    "sexual_offences", "aggr_robbery", "common_robbery",
    "Population", "area_km2", "pop_density",
    "pct_informal", "pct_no_schooling", "pct_matric_plus", "pct_no_electricity",
    "pct_urban", "avg_hh_size", "pct_no_piped_water", "pct_female"
  )), matches("^pct_youth")) |>
  left_join(lisa_tbl, by = "police_district")

districts <- st_sf(attrs, geometry = st_geometry(geo))
stopifnot(nrow(districts) == 1160, !anyNA(districts$crime_rate))
saveRDS(districts, file.path(out_dir, "districts.rds"))

# ---- 4. Neighbour link lines for the Weights tab -------------------------------
# Centroids are taken in the projected CRS, then converted to lon/lat.
cent <- master |> st_geometry() |> st_centroid() |> st_transform(4326) |> st_coordinates()

make_links <- function(nb) {
  tryCatch({
    ln <- spdep::nb2lines(nb, coords = cent, as_sf = TRUE)
    st_set_crs(ln, 4326)
  }, error = function(e) { message("Links failed: ", conditionMessage(e)); NULL })
}

# The distance band has far too many links to draw, so it is left out on purpose.
links <- list(queen = make_links(wts$nb$queen),
              rook  = make_links(wts$nb$rook),
              knn   = make_links(wts$nb$knn))
saveRDS(links, file.path(out_dir, "neighbour_links.rds"))

# Number of neighbours per district for every weights structure (Weights tab)
counts <- data.frame(police_district = master$police_district)
for (nm in c("queen", "rook", "knn", "dist")) {
  if (!is.null(wts$nb[[nm]])) counts[[nm]] <- as.integer(spdep::card(wts$nb[[nm]]))
  else message("No nb object named '", nm, "' in weights.rds")
}
saveRDS(counts, file.path(out_dir, "neighbour_counts.rds"))

# ---- 5. Summary tables -------------------------------------------------------
read_tab <- function(f) {
  p <- file.path("outputs/tables", f)
  if (file.exists(p)) read.csv(p, check.names = FALSE) else { message("Missing: ", p); NULL }
}
tables <- list(
  weights_summary       = read_tab("weights_summary.csv"),
  global_autocorrelation = read_tab("global_autocorrelation.csv"),
  moran_sensitivity     = read_tab("moran_sensitivity.csv"),
  joincount             = read_tab("joincount.csv"),
  lisa_cluster_counts   = read_tab("lisa_cluster_counts.csv")
)
saveRDS(tables, file.path(out_dir, "tables.rds"))

# ---- 6. Headline numbers for the Home tab --------------------------------------
ga <- tables$global_autocorrelation
mi <- if (!is.null(ga)) ga[ga$test == "Moran's I" & grepl("Queen", ga$weights), ][1, ] else NULL

meta <- list(
  n_districts = nrow(districts),
  median_rate = median(districts$crime_rate),
  moran_I     = if (!is.null(mi)) mi$statistic else NA_real_,
  moran_p     = if (!is.null(mi)) mi$p_value   else NA_real_,
  n_hh        = sum(districts$cluster == "High-High", na.rm = TRUE),
  n_ll        = sum(districts$cluster == "Low-Low",   na.rm = TRUE),
  period      = "2023/24",
  built       = as.character(Sys.Date())
)
saveRDS(meta, file.path(out_dir, "app_meta.rds"))

# ---- 7. Copy Phase 5 and 6 results as they are ---------------------------------
for (f in c("regression_results.rds", "sampling_results.rds")) {
  src <- file.path("outputs/objects", f)
  if (file.exists(src)) {
    file.copy(src, file.path(out_dir, f), overwrite = TRUE)
    cat("\nCopied", f, "- top-level structure:\n")
    str(readRDS(src), max.level = 1)
  } else message("MISSING: ", src, " (ask the owner to push it)")
}

# ---- 8. Report sizes -----------------------------------------------------------
sizes <- file.info(list.files(out_dir, full.names = TRUE))
print(data.frame(file = basename(rownames(sizes)), MB = round(sizes$size / 1e6, 2)))
cat("\nMoran's I on the Home tab:", round(meta$moran_I, 3),
    "| High-High districts:", meta$n_hh, "\n")
cat("Step 2 finished. Quick look: plot(st_geometry(readRDS('", out_dir, "/districts.rds')))\n", sep = "")
