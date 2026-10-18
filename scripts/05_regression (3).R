# =====================================================================
# 05_regression.R
# Purpose : Phase 5 - OLS, residual diagnostics, Moran's I on residuals,
#           Lagrange multiplier tests, spatial lag / error model, impacts.
# Input   : data/clean/master_analysis.rds
#           outputs/objects/weights.rds
# Output  : outputs/objects/regression_results.rds
#           outputs/tables/regression_coefficients.csv
#           outputs/tables/regression_diagnostics.csv (+ extras)
#           outputs/figures/*.png
# Author  : <Lethabo Madzibedi>
# Date    : <date>
# Run from the project root (open the .Rproj file first).
# NEVER sort, filter or reorder master: the weights depend on row order.
# =====================================================================

set.seed(786)

pkgs <- c("sf", "dplyr", "tidyr", "ggplot2", "spdep", "spatialreg",
          "Matrix", "readr", "tibble", "scales")
missing_pkgs <- pkgs[!pkgs %in% rownames(installed.packages())]
if (length(missing_pkgs) > 0) install.packages(missing_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

dir.create("outputs/objects", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

# write_csv refuses matrix/list columns, which spatialreg/spdep sometimes
# return. This flattens every column to a plain vector before saving.
safe_csv <- function(d, path) {
  d <- as.data.frame(d)
  for (nm in names(d)) {
    col <- d[[nm]]
    if (is.list(col)) {
      col <- vapply(col, function(z) as.numeric(unlist(z))[1], numeric(1))
    } else if (is.matrix(col) || is.array(col)) {
      col <- if (length(dim(col)) == 2 && ncol(col) > 1) col[, 1] else as.vector(col)
    }
    if (!is.character(col) && !is.logical(col) && !is.factor(col)) {
      col <- as.numeric(col)
    }
    d[[nm]] <- unname(col)
  }
  readr::write_csv(d, path)
}

# =====================================================================
# CONFIG - the only part you should need to edit
# =====================================================================
# Must be the SAME variable the group agreed in Phase 2 / uses in Phase 4
y_var <- "log_crime_rate_eb"     # or "log_crime_rate"

# 4 to 6 covariates, at most one from any pair with |r| > 0.8.
# Use the spellings that worked in 02_explore.R. Check
# outputs/tables/high_correlation_pairs.csv before choosing.
covariates <- c("pct_informal", "pct_no_schooling", "pct_urban",
                "pct_youth_15-34", "pct_no_piped_water")

model_override  <- NULL          # set to "lag" or "error" to force a model
run_sensitivity <- TRUE          # refit with kNN weights if they exist
# =====================================================================

# ---- 1. Load data and weights --------------------------------------
master <- readRDS("data/clean/master_analysis.rds")
stopifnot(inherits(master, "sf"))
n <- nrow(master)

absent <- setdiff(c(y_var, covariates), names(master))
if (length(absent) > 0) {
  stop("Columns not found: ", paste(absent, collapse = ", "),
       "\nCheck names(master) and edit the CONFIG block.")
}

df <- st_drop_geometry(master)

na_counts <- colSums(is.na(df[, c(y_var, covariates)]))
if (any(na_counts > 0)) {
  print(na_counts[na_counts > 0])
  stop("Missing values found. Do not drop rows (the weights depend on row ",
       "order). Ask the group first.")
}

cm <- cor(df[, covariates]); diag(cm) <- 0
if (any(abs(cm) > 0.8)) {
  print(round(cm, 2))
  stop("Two chosen covariates have |r| > 0.8. Keep only one of each pair.")
}

w_obj <- readRDS("outputs/objects/weights.rds")
# Structure written by 03_weights.R:
#   w_obj$id            police_district names in row order
#   w_obj$W$queen/rook/knn/dist   row-standardised listw objects
#   w_obj$B$queen/dist            binary listw objects
cat("Elements in weights.rds:\n"); print(names(w_obj))

if (is.null(w_obj$W$queen)) {
  stop("weights.rds has no W$queen. Run str(w_obj, max.level = 2) and tell me what it shows.")
}
queen_name <- "W$queen"
lw <- w_obj$W$queen
message("Using weights object: ", queen_name)

if (length(lw$neighbours) != n) stop("Weights length does not match the data.")

# Confirm district order matches the order saved with the weights
if (!identical(as.character(w_obj$id), as.character(master$police_district))) {
  stop("District order differs from the weights (w_obj$id). Rebuild the weights.")
}
message("District order matches w_obj$id.")

# Clean the CONFIG entries (stray spaces, blanks, duplicates) and build the
# formula with backticks so unusual column names cannot break it
y_var      <- trimws(y_var)
covariates <- unique(trimws(covariates))
covariates <- covariates[nzchar(covariates)]
if (length(y_var) != 1 || !nzchar(y_var)) stop("y_var must be ONE column name.")
if (y_var %in% covariates) stop("y_var is also listed in covariates.")
fml <- as.formula(paste0("`", y_var, "` ~ ",
                         paste0("`", covariates, "`", collapse = " + ")))
cat("\nModel formula:\n"); print(fml)

# ---- 2. OLS baseline -----------------------------------------------
ols <- lm(fml, data = df)
ols_sum <- summary(ols)
print(ols_sum)

ols_tab <- tibble(term         = rownames(ols_sum$coefficients),
                  ols_estimate = ols_sum$coefficients[, 1],
                  ols_p        = ols_sum$coefficients[, 4])

cat("\nR-squared:", round(ols_sum$r.squared, 3),
    " Adjusted:", round(ols_sum$adj.r.squared, 3), "\n")

# ---- 3. OLS assumption checks --------------------------------------
diag_df <- tibble(fitted = fitted(ols), resid = resid(ols))

p_rvf <- ggplot(diag_df, aes(fitted, resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, colour = "red") +
  geom_smooth(se = FALSE, colour = "steelblue") +
  labs(title = "OLS residuals vs fitted", x = "Fitted", y = "Residual") +
  theme_minimal()

p_qq <- ggplot(diag_df, aes(sample = resid)) +
  stat_qq(alpha = 0.5) + stat_qq_line(colour = "steelblue") +
  labs(title = "OLS residuals: normal QQ plot",
       x = "Theoretical quantiles", y = "Sample quantiles") +
  theme_minimal()

ggsave("outputs/figures/ols_resid_vs_fitted.png", p_rvf, width = 7, height = 5, dpi = 300)
ggsave("outputs/figures/ols_resid_qq.png", p_qq, width = 6, height = 5, dpi = 300)

# Variance inflation factors (above 5 is a warning)
vif_vals <- vapply(covariates, function(v) {
  f <- as.formula(paste0("`", v, "` ~ ",
                         paste0("`", setdiff(covariates, v), "`", collapse = " + ")))
  1 / (1 - summary(lm(f, data = df))$r.squared)
}, numeric(1))
vif_tab <- tibble(variable = covariates, VIF = round(vif_vals, 2))
print(as.data.frame(vif_tab))

# Cook's distance
cd <- cooks.distance(ols)
influential <- df |>
  mutate(cooks_d = cd) |>
  select(police_district, local_muni, all_of(y_var), cooks_d) |>
  arrange(desc(cooks_d)) |>
  slice_head(n = 10)
cat("\nMost influential districts (Cook's distance; flag if > 4/n =",
    round(4 / n, 4), "):\n")
print(as.data.frame(influential))

p_cd <- ggplot(tibble(i = seq_along(cd), cd = cd), aes(i, cd)) +
  geom_segment(aes(xend = i, yend = 0), colour = "steelblue") +
  geom_hline(yintercept = 4 / n, colour = "red", linetype = "dashed") +
  labs(title = "Cook's distance", x = "District (row)", y = "Cook's D") +
  theme_minimal()
ggsave("outputs/figures/ols_cooks_distance.png", p_cd, width = 8, height = 4.5, dpi = 300)

# ---- 4. Moran's I on OLS residuals ---------------------------------
resid_moran <- spdep::lm.morantest(ols, lw, zero.policy = TRUE)
print(resid_moran)

resid_map <- function(res, title, file) {
  map_df <- master
  map_df$res <- as.numeric(res)
  lim <- quantile(map_df$res, c(0.01, 0.99), na.rm = TRUE)
  p <- ggplot(map_df) +
    geom_sf(aes(fill = res), colour = NA) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                         midpoint = 0, limits = lim, oob = scales::squish,
                         name = "Residual") +
    labs(title = title) + theme_void()
  ggsave(file, p, width = 8, height = 7, dpi = 300)
}
resid_map(resid(ols), "OLS residuals", "outputs/figures/map_ols_residuals.png")

# ---- 5. Lagrange multiplier tests ----------------------------------
use_new <- exists("lm.RStests", envir = asNamespace("spdep"))
if (use_new) {
  lm_fun <- spdep::lm.RStests
  tests  <- c("RSerr", "RSlag", "adjRSerr", "adjRSlag")
  nm <- list(err = "RSerr", lag = "RSlag", rerr = "adjRSerr", rlag = "adjRSlag")
} else {
  lm_fun <- spdep::lm.LMtests
  tests  <- c("LMerr", "LMlag", "RLMerr", "RLMlag")
  nm <- list(err = "LMerr", lag = "LMlag", rerr = "RLMerr", rlag = "RLMlag")
}

lm_res <- unclass(lm_fun(ols, lw, test = tests, zero.policy = TRUE))
lm_tab <- data.frame(test      = names(lm_res),
                     statistic = vapply(lm_res, function(z) as.numeric(unlist(z$statistic))[1], numeric(1)),
                     p_value   = vapply(lm_res, function(z) as.numeric(unlist(z$p.value))[1], numeric(1)),
                     row.names = NULL)
print(lm_tab)
pv <- setNames(lm_tab$p_value, lm_tab$test)

sig_lag <- pv[[nm$lag]] < 0.05
sig_err <- pv[[nm$err]] < 0.05
lm_choice <- if (sig_lag && !sig_err) {
  "lag"
} else if (!sig_lag && sig_err) {
  "error"
} else if (sig_lag && sig_err) {
  if (pv[[nm$rlag]] < pv[[nm$rerr]]) "lag" else "error"
} else {
  NA_character_
}
cat("\nModel suggested by the LM tests:", lm_choice, "\n")

# ---- 6. Fit spatial models (both, so they can be compared) ---------
slm <- spatialreg::lagsarlm(fml, data = df, listw = lw, zero.policy = TRUE)
sem <- spatialreg::errorsarlm(fml, data = df, listw = lw, zero.policy = TRUE)

final_choice <- if (!is.null(model_override)) {
  model_override
} else if (!is.na(lm_choice)) {
  lm_choice
} else {
  message("No LM test significant; choosing the model with the lower AIC.")
  c("lag", "error")[which.min(c(AIC(slm), AIC(sem)))]
}
cat("Model used for the main results:", final_choice, "\n")

chosen <- if (final_choice == "lag") slm else sem
print(summary(chosen))

sp_par_name <- if (final_choice == "lag") "rho" else "lambda"
sp_par <- if (final_choice == "lag") chosen$rho else chosen$lambda
sp_par <- as.numeric(sp_par)
sp_par_p <- tryCatch(as.numeric(summary(chosen)$LR1$p.value[1]),
                     error = function(e) NA_real_)
if (length(sp_par_p) != 1) sp_par_p <- NA_real_

sp_sum <- summary(chosen)
sp_tab <- tibble(term             = rownames(sp_sum$Coef),
                 spatial_estimate = sp_sum$Coef[, 1],
                 spatial_p        = sp_sum$Coef[, 4])

coef_compare <- full_join(ols_tab, sp_tab, by = "term") |>
  bind_rows(tibble(term = paste0(sp_par_name, " (spatial parameter)"),
                   spatial_estimate = sp_par, spatial_p = sp_par_p)) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))
print(as.data.frame(coef_compare))

# ---- 7. Compare models ---------------------------------------------
moran_of <- function(m) spdep::moran.test(as.numeric(residuals(m)), lw,
                                          zero.policy = TRUE)
m_slm <- moran_of(slm); m_sem <- moran_of(sem)

num1 <- function(x) as.numeric(unlist(x))[1]   # force a plain single number
fit_summary <- data.frame(
  model = c("OLS", "Spatial lag", "Spatial error"),
  AIC   = c(num1(AIC(ols)), num1(AIC(slm)), num1(AIC(sem))),
  logLik = c(num1(logLik(ols)), num1(logLik(slm)), num1(logLik(sem))),
  resid_moran_I = c(num1(resid_moran$estimate[1]),
                    num1(m_slm$estimate[1]), num1(m_sem$estimate[1])),
  resid_moran_p = c(num1(resid_moran$p.value), num1(m_slm$p.value),
                    num1(m_sem$p.value))
)
fit_summary[, -1] <- round(fit_summary[, -1], 4)
print(fit_summary)
# Lower AIC = better. A good spatial model has residual Moran's I near 0.

resid_map(residuals(chosen), paste("Residuals of the spatial", final_choice, "model"),
          "outputs/figures/map_spatial_residuals.png")

# ---- 8. Impacts (lag model only) -----------------------------------
impacts_tab <- NULL
if (final_choice == "lag") {
  impacts_tab <- tryCatch({
    W_sp <- as(lw, "CsparseMatrix")
    tr   <- spatialreg::trW(W_sp, type = "mult")
    imp  <- spatialreg::impacts(slm, tr = tr, R = 500)
    s    <- summary(imp, zstats = TRUE, short = TRUE)
    tibble(variable   = names(imp$res$direct),
           direct     = as.numeric(imp$res$direct),
           indirect   = as.numeric(imp$res$indirect),
           total      = as.numeric(imp$res$total),
           direct_p   = as.numeric(s$pzmat[, "Direct"]),
           indirect_p = as.numeric(s$pzmat[, "Indirect"]),
           total_p    = as.numeric(s$pzmat[, "Total"])) |>
      mutate(across(where(is.numeric), ~ round(.x, 4)))
  }, error = function(e) {
    message("Impacts failed: ", conditionMessage(e)); NULL
  })
  print(as.data.frame(impacts_tab))
  if (!is.null(impacts_tab)) safe_csv(impacts_tab, "outputs/tables/regression_impacts.csv")
}

# ---- 9. Sensitivity with kNN weights (optional) --------------------
sens_tab <- NULL
knn_names <- if (!is.null(w_obj$W$knn)) "W$knn" else character(0)

if (run_sensitivity && length(knn_names) > 0) {
  lw_knn <- w_obj$W$knn
  fit_knn <- if (final_choice == "lag") {
    spatialreg::lagsarlm(fml, data = df, listw = lw_knn, zero.policy = TRUE)
  } else {
    spatialreg::errorsarlm(fml, data = df, listw = lw_knn, zero.policy = TRUE)
  }
  k_coef <- summary(fit_knn)$Coef
  sens_tab <- tibble(term = rownames(k_coef),
                     knn_estimate = k_coef[, 1], knn_p = k_coef[, 4]) |>
    left_join(sp_tab, by = "term") |>
    mutate(same_sign = sign(knn_estimate) == sign(spatial_estimate),
           across(where(is.numeric), ~ round(.x, 4)))
  cat("\nSensitivity (", knn_names[1], "):\n", sep = ""); print(as.data.frame(sens_tab))
  safe_csv(sens_tab, "outputs/tables/regression_sensitivity_knn.csv")
}

# ---- 10. Save -------------------------------------------------------
diagnostics <- bind_rows(
  tibble(check = "OLS R-squared", value = round(ols_sum$r.squared, 4)),
  tibble(check = "OLS adjusted R-squared", value = round(ols_sum$adj.r.squared, 4)),
  tibble(check = paste("Max VIF:", vif_tab$variable[which.max(vif_tab$VIF)]),
         value = max(vif_tab$VIF)),
  tibble(check = "Moran's I on OLS residuals", value = round(unname(resid_moran$estimate[1]), 4)),
  tibble(check = "Moran's I p-value (OLS residuals)", value = resid_moran$p.value),
  tibble(check = paste("LM test p:", lm_tab$test), value = lm_tab$p_value)
)

safe_csv(coef_compare, "outputs/tables/regression_coefficients.csv")
safe_csv(diagnostics,  "outputs/tables/regression_diagnostics.csv")
safe_csv(fit_summary,  "outputs/tables/regression_model_comparison.csv")
safe_csv(vif_tab,      "outputs/tables/regression_vif.csv")

saveRDS(list(y_var = y_var, covariates = covariates, formula = fml,
             weights_used = queen_name, final_choice = final_choice,
             ols = ols, lag = slm, error = sem,
             lm_tests = lm_tab, resid_moran = resid_moran,
             coef_compare = coef_compare, fit_summary = fit_summary,
             vif = vif_tab, influential = influential,
             impacts = impacts_tab, sensitivity = sens_tab),
        "outputs/objects/regression_results.rds")

cat("\nSaved regression_results.rds (",
    round(file.size("outputs/objects/regression_results.rds") / 1e6, 1), "MB ).\n")
cat("Tell the group: covariates =", paste(covariates, collapse = ", "),
    "| spatial model =", final_choice, "\n")