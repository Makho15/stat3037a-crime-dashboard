# =====================================================================
# 02_explore.R
# Purpose : Phase 2 - explore crime_rate and the nine covariates, decide
#           on the transformation, map, check correlations, and save
#           master_analysis.rds for Phases 4 and 5.
# Input   : data/clean/master_dataset.rds
# Output  : data/clean/master_analysis.rds
#           outputs/tables/summary_stats.csv, correlations.csv (+ extras)
#           outputs/figures/*.png
# Author  : <your name>
# Date    : <date>
# Run from the project root (open the .Rproj file first).
# =====================================================================

set.seed(786)

# ---- 0. Packages and folders ---------------------------------------
pkgs <- c("sf", "dplyr", "tidyr", "ggplot2", "e1071", "spdep",
          "corrplot", "readr", "tibble")
missing_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data/clean",      recursive = TRUE, showWarnings = FALSE)

# ---- 1. Load and inspect -------------------------------------------
master <- readRDS("data/clean/master_dataset.rds")
stopifnot(inherits(master, "sf"))

cat("Rows:", nrow(master), "(expected about 1,160)\n")
print(names(master))
str(master, max.level = 1)

covariates <- c("pct_informal", "pct_no_schooling", "pct_matric_plus",
                "pct_no_electricity", "pct_urban", "avg_hh_size",
                "pct_no_piped_water", "pct_female", "pct_youth_15-34")

# Stop early if a column name is spelt differently in your data
needed <- c("crime_rate", "Violent_crime_total", "Population", covariates)
absent <- setdiff(needed, names(master))
if (length(absent) > 0) stop("Columns not found: ", paste(absent, collapse = ", "))

cat("\nTotal missing values per analysis column:\n")
print(colSums(is.na(st_drop_geometry(master)[, needed])))

vars <- c("crime_rate", covariates)

summary_stats <- master |>
  st_drop_geometry() |>
  select(all_of(vars)) |>
  pivot_longer(everything(), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, levels = vars)) |>
  group_by(variable) |>
  summarise(
    n         = sum(!is.na(value)),
    n_missing = sum(is.na(value)),
    mean      = mean(value, na.rm = TRUE),
    sd        = sd(value, na.rm = TRUE),
    min       = min(value, na.rm = TRUE),
    q1        = quantile(value, 0.25, na.rm = TRUE),
    median    = median(value, na.rm = TRUE),
    q3        = quantile(value, 0.75, na.rm = TRUE),
    max       = max(value, na.rm = TRUE),
    skewness  = e1071::skewness(value, na.rm = TRUE),
    .groups   = "drop"
  ) |>
  mutate(across(where(is.numeric), ~ round(.x, 3)))

print(summary_stats, n = Inf)
write_csv(summary_stats, "outputs/tables/summary_stats.csv")

# ---- 2. Distribution of crime_rate ---------------------------------
plot_distribution <- function(data, var, label, stub) {
  df <- st_drop_geometry(data)
  x  <- df[[var]]

  p_hist <- ggplot(df, aes(x = .data[[var]])) +
    geom_histogram(bins = 40, fill = "steelblue", colour = "white") +
    geom_vline(xintercept = mean(x, na.rm = TRUE),
               colour = "red", linewidth = 1) +
    geom_vline(xintercept = median(x, na.rm = TRUE),
               colour = "darkgreen", linewidth = 1, linetype = "dashed") +
    labs(title = paste("Histogram of", label),
         subtitle = "Red = mean, green dashed = median",
         x = label, y = "Number of districts") +
    theme_minimal()

  p_box <- ggplot(df, aes(y = .data[[var]])) +
    geom_boxplot(fill = "steelblue", alpha = 0.7, outlier.alpha = 0.4) +
    labs(title = paste("Boxplot of", label), y = label, x = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  p_qq <- ggplot(df, aes(sample = .data[[var]])) +
    stat_qq(alpha = 0.5) + stat_qq_line(colour = "steelblue", linewidth = 1) +
    labs(title = paste("Normal QQ plot of", label),
         x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_minimal()

  ggsave(paste0("outputs/figures/", stub, "_hist.png"), p_hist,
         width = 7, height = 5, dpi = 300)
  ggsave(paste0("outputs/figures/", stub, "_box.png"),  p_box,
         width = 4, height = 5, dpi = 300)
  ggsave(paste0("outputs/figures/", stub, "_qq.png"),   p_qq,
         width = 6, height = 5, dpi = 300)

  tibble(variable = var,
         mean     = mean(x, na.rm = TRUE),
         median   = median(x, na.rm = TRUE),
         skewness = e1071::skewness(x, na.rm = TRUE),
         excess_kurtosis = e1071::kurtosis(x, na.rm = TRUE))
}

dist_raw <- plot_distribution(master, "crime_rate",
                              "Violent crime rate per 100,000", "crime_rate")
print(dist_raw)
# A mean far above the median and a large positive skewness confirm right skew.

# ---- 3. Unstable rates and empirical Bayes smoothing ---------------
# Rows with zero/missing population cannot be smoothed (they stay NA)
ok <- !is.na(master$Population) & master$Population > 0 &
      !is.na(master$Violent_crime_total)

eb_fit <- spdep::EBest(master$Violent_crime_total[ok], master$Population[ok])

crime_rate_eb <- rep(NA_real_, nrow(master))
crime_rate_eb[ok] <- eb_fit$estmm * 100000     # per 100,000, like crime_rate
master$crime_rate_eb <- crime_rate_eb          # row order is NOT changed

# Sorting happens on a copy, so `master` keeps its original row order
smallest_districts <- master |>
  st_drop_geometry() |>
  select(police_district, local_muni, Population,
         Violent_crime_total, crime_rate, crime_rate_eb) |>
  arrange(Population) |>
  slice_head(n = 20)

print(as_tibble(smallest_districts), n = 20)
write_csv(smallest_districts, "outputs/tables/smallest_population_districts.csv")

# ---- 4. Transformation decision ------------------------------------
has_zero <- any(master$crime_rate == 0, na.rm = TRUE)
if (has_zero) {
  message("Some districts have crime_rate = 0, so log_crime_rate = log(crime_rate + 1)")
  master$log_crime_rate <- log(master$crime_rate + 1)
} else {
  message("No zero rates, so log_crime_rate = log(crime_rate)")
  master$log_crime_rate <- log(master$crime_rate)
}

dist_log <- plot_distribution(master, "log_crime_rate",
                              "Log of violent crime rate", "log_crime_rate")

skew_compare <- bind_rows(dist_raw, dist_log)
print(skew_compare)
write_csv(skew_compare, "outputs/tables/skewness_before_after.csv")

# GROUP DECISION: if skewness is much closer to 0 after logging, the analysis
# variable is log_crime_rate. Write this in the README.
# Note: log_crime_rate can be negative if a rate is below 1 per 100,000.
# Phase 4 therefore runs Global G on the raw crime_rate.

# ---- 5. Maps -------------------------------------------------------
map_quantile <- function(data, var, title, n_class = 5, palette = "YlOrRd") {
  x    <- data[[var]]
  brks <- unique(quantile(x, probs = seq(0, 1, length.out = n_class + 1),
                          na.rm = TRUE))
  data$class <- cut(x, breaks = brks, include.lowest = TRUE, dig.lab = 4)
  ggplot(data) +
    geom_sf(aes(fill = class), colour = NA) +
    scale_fill_brewer(palette = palette, na.value = "grey80", drop = FALSE,
                      name = "Quantile class") +
    labs(title = title) +
    theme_void()
}

ggsave("outputs/figures/map_crime_rate.png",
       map_quantile(master, "crime_rate",
                    "Violent crime rate per 100,000 (quantile classes)"),
       width = 8, height = 7, dpi = 300)

ggsave("outputs/figures/map_log_crime_rate.png",
       map_quantile(master, "log_crime_rate",
                    "Log violent crime rate (quantile classes)"),
       width = 8, height = 7, dpi = 300)

# Small multiples: each covariate shown as a percentile so panels are comparable
cov_long <- master |>
  select(all_of(covariates)) |>
  pivot_longer(all_of(covariates), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, levels = covariates)) |>
  group_by(variable) |>
  mutate(percentile = percent_rank(value)) |>
  ungroup()

p_cov_maps <- ggplot(cov_long) +
  geom_sf(aes(fill = percentile), colour = NA) +
  scale_fill_viridis_c(name = "Percentile\nwithin variable", na.value = "grey80") +
  facet_wrap(~ variable, ncol = 3) +
  labs(title = "Covariates by district (percentile of each variable)") +
  theme_void() +
  theme(strip.text = element_text(face = "bold"))

ggsave("outputs/figures/map_covariates.png", p_cov_maps,
       width = 12, height = 11, dpi = 300)

# ---- 6. Covariate checks -------------------------------------------
cov_df <- st_drop_geometry(master)[, covariates]
cm <- cor(cov_df, use = "pairwise.complete.obs")

idx <- which(abs(cm) > 0.8 & upper.tri(cm), arr.ind = TRUE)
high_pairs <- tibble(var1 = rownames(cm)[idx[, 1]],
                     var2 = colnames(cm)[idx[, 2]],
                     r    = round(cm[idx], 3))

cat("\nPairs with |r| > 0.8 (do not use both in the regression):\n")
print(high_pairs)

write_csv(rownames_to_column(as.data.frame(round(cm, 3)), "variable"),
          "outputs/tables/correlations.csv")
write_csv(high_pairs, "outputs/tables/high_correlation_pairs.csv")

png("outputs/figures/correlation_matrix.png", width = 1000, height = 1000, res = 130)
corrplot::corrplot(cm, method = "color", type = "upper",
                   addCoef.col = "black", number.cex = 0.7,
                   tl.col = "black", tl.cex = 0.8, diag = FALSE)
dev.off()

scatter_long <- st_drop_geometry(master) |>
  select(log_crime_rate, all_of(covariates)) |>
  pivot_longer(all_of(covariates), names_to = "variable", values_to = "value") |>
  mutate(variable = factor(variable, levels = covariates))

p_scatter <- ggplot(scatter_long, aes(x = value, y = log_crime_rate)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "red", linewidth = 0.8) +
  facet_wrap(~ variable, scales = "free_x", ncol = 3) +
  labs(title = "Log crime rate against each covariate",
       x = NULL, y = "Log violent crime rate") +
  theme_minimal()

ggsave("outputs/figures/scatter_covariates.png", p_scatter,
       width = 11, height = 9, dpi = 300)

# ---- 7. Save -------------------------------------------------------
# New file name: never overwrite master_dataset.rds
saveRDS(master, "data/clean/master_analysis.rds")

cat("\nSaved master_analysis.rds with", nrow(master), "rows.\n")
cat("File size (MB):",
    round(file.size("data/clean/master_analysis.rds") / 1e6, 1), "\n")
cat("Analysis variable chosen: see skewness_before_after.csv and tell the group.\n")
