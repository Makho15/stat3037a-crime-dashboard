library(sf)
library(dplyr)
library(readxl)
library(purrr)
library(stringr)

tidy_name <- function(x) str_to_lower(str_trim(x))

# ---- 1. Boundaries -------------------------------------------------
# TODO: change the file name to your shapefile
boundaries <- st_read("Data/raw/Police District shp/PoliceDistrict.shp") |>
  st_make_valid() |>
  rename(police_district = COMPNT_NM) |>
  mutate(police_district = tidy_name(police_district),
         police_district = recode(police_district, "makhanda" = "grahamstown")) |>
  select(police_district)

st_crs(boundaries)   # check the coordinate system once

# ---- 2. Crime (2023/24 only, population already attached) -----------
crime <- read_excel("Data/clean/crime_2023_2024.xlsx") |>
  rename(any_of(c(area_km2 = "Population_density"))) |>   # that column is really area
  mutate(police_district = tidy_name(police_district)) |>
  select(police_district, local_muni, district_muni, longitude, latitude,
         any_of(c("murder", "attempted_murder", "common_assault", "assault_gbh",
                  "sexual_offences", "aggr_robbery", "common_robbery", "kidnapping")),
         Violent_crime_total, Population, area_km2) |>
  mutate(crime_rate  = Violent_crime_total / Population * 100000,
         pop_density = Population / area_km2)

stopifnot(is.numeric(crime$longitude), all(crime$longitude > 15 & crime$longitude < 34, na.rm = TRUE))

# ---- 3. Covariates (join on police_district only) -------------------
# TODO: edit file names to match yours
cov_spec <- tribble(
  ~file,                                  ~cols,
  "Dwelling_analysis.xlsx",    "pct_informal",
  "Edu_Analysis.xlsx",         c("pct_no_schooling", "pct_matric_plus"),
  "EnergyLighting_Analysis.xlsx", "pct_no_electricity",
  "Geotype-hh_Analysis.xlsx",  "pct_urban",
  "hhsize_Analysis.xlsx",      "avg_hh_size",
  "Pipedwater_Analysis.xlsx",  "pct_no_piped_water",
  "Sex_Analysis.xlsx",         "pct_female",
  "Age_Analysis.xlsx",         "pct_youth_15-34"
)

read_cov <- function(file, cols) {
  read_excel(file.path("Data/clean", file)) |>
    mutate(police_district = tidy_name(police_district)) |>
    select(police_district, all_of(cols))
}

covariates <- pmap(cov_spec, read_cov) |>
  reduce(left_join, by = "police_district")

stopifnot(nrow(covariates) == 1167, !anyDuplicated(covariates$police_district))

# ---- 4. Build the master dataset -----------------------------------
master <- boundaries |>
  inner_join(crime, by = "police_district") |>
  inner_join(covariates, by = "police_district")

# ---- 5. Checks ------------------------------------------------------
nrow(master)                                  # expect 1160
anyDuplicated(master$police_district)         # expect 0
sum(is.na(master$crime_rate))                 # expect 0
anti_join(st_drop_geometry(boundaries), st_drop_geometry(master), by = "police_district")  # polygons dropped
anti_join(crime, st_drop_geometry(master), by = "police_district")$police_district         # crime rows dropped
master |> st_drop_geometry() |> summary()

# ---- 6. Save ---------------------------------------------------------
saveRDS(master, "Data/clean/master_dataset.rds")
