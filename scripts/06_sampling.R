# ==========================================
#Step 1: Set up the project
# ==========================================

# Purpose: Sampling study comparing random, stratified and systematic designs
# Input:   data/clean/master_analysis.rds (or master_dataset.rds)
# Output:  outputs/objects/sampling_results.rds, outputs/tables/sampling_summary.csv, outputs/figures/*.png
# Author:  Musawenkosi   Date: 12/10/2026

install.packages("sf")
install.packages("dplyr")
install.packages("ggplot2")
library(sf); library(dplyr); library(ggplot2)

dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/objects", recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)

set.seed(786)

# ==========================================
#Step 2: Load the data and run checks
# ==========================================
master <- readRDS("data/clean/master_dataset.rds")  # fall back to master_dataset.rds if Phase 2 isn't pushed
nrow(master)                    # expect about 1,160
sum(is.na(master$crime_rate))   # must be 0, or decide how to handle
sum(is.na(master$pct_urban))    # must be 0, or the strata will break
sum(is.na(master$longitude))

# ==========================================
#Step 3: Define the population and the target
# ==========================================
d <- st_drop_geometry(master)          # fast copy for the simulation
d$id <- seq_len(nrow(d))
N <- nrow(d)
true_mean <- mean(d$crime_rate)
true_mean

# ==========================================
#Step 4: Build the strata
# ==========================================
d$stratum <- factor(ntile(d$pct_urban, 3), labels = c("low", "medium", "high"))
table(d$stratum)   # should be about 387 each

# ==========================================
#Step 5: Write the three sampling functions
# ==========================================
# 1. Simple random: every district has chance n/N
srs <- function(n) sample(N, n)

# 2. Stratified, proportional allocation
strat <- function(n) {
  sizes <- round(n * table(d$stratum) / N)
  diff  <- n - sum(sizes)                           # fix rounding so total is exactly n
  sizes[which.max(sizes)] <- sizes[which.max(sizes)] + diff
  unlist(lapply(names(sizes), function(s) {
    pool <- d$id[d$stratum == s]
    pool[sample.int(length(pool), sizes[[s]])]
  }))
}

# 3. Systematic: order west to east, random start, every k-th
ord <- order(d$longitude)
syst <- function(n) {
  k <- N / n                                        # often not a whole number
  pos <- ceiling(runif(1, 0, k) + (0:(n - 1)) * k)
  ord[pos]
}

designs <- list(random = srs, stratified = strat, systematic = syst)

sapply(designs, function(f) length(f(50)))   # each should be 50
length(unique(syst(50)))                     # should be 50 (no duplicates)

# ==========================================
#Step 6: Draw and map one example of each design
# ==========================================
set.seed(786)
ex <- lapply(designs, function(f) f(100))

maps <- bind_rows(lapply(names(ex), function(nm) {
  m <- master
  m$selected <- m$id <- seq_len(nrow(m)); m$selected <- m$id %in% ex[[nm]]
  m$design <- nm
  m
}))

ggplot(maps) +
  geom_sf(aes(fill = selected), colour = NA) +
  scale_fill_manual(values = c("grey90", "red"), name = "Sampled") +
  facet_wrap(~design) + theme_void()
ggsave("outputs/figures/sampling_examples.png", width = 10, height = 4)

# ==========================================
#Step 7: Run the simulation
# ==========================================
set.seed(786)
ns <- c(25, 50, 100, 200)
results <- bind_rows(lapply(names(designs), function(nm) {
  bind_rows(lapply(ns, function(n) {
    data.frame(design = nm, n = n, replicate = 1:1000,
               estimate = replicate(1000, mean(d$crime_rate[designs[[nm]](n)])))
  }))
}))
nrow(results)   # 12,000

# ==========================================
#Step 8: Compute the performance measures
# ==========================================
summary_tbl <- results |>
  group_by(design, n) |>
  summarise(mean_estimate = mean(estimate),
            bias = mean(estimate) - true_mean,
            se   = sd(estimate),
            rmse = sqrt(mean((estimate - true_mean)^2)),
            .groups = "drop")
summary_tbl

# ==========================================
#Step 9: Plot the results
# ==========================================
ggplot(filter(results, n == 50), aes(x = design, y = estimate, fill = design)) +
  geom_boxplot(show.legend = FALSE) +
  geom_hline(yintercept = true_mean, colour = "red", linetype = "dashed") +
  labs(title = "Sample means at n = 50",
       subtitle = "Red dashed line = true mean",
       x = NULL, y = "Estimated mean crime rate") +
  theme_minimal()
ggsave("outputs/figures/sampling_boxplot_n50.png", width = 6, height = 4, dpi = 300)

ggplot(summary_tbl, aes(x = n, y = rmse, colour = design)) +
  geom_line(linewidth = 1) + geom_point(size = 2) +
  scale_x_continuous(breaks = c(25, 50, 100, 200)) +
  labs(title = "RMSE by sample size and design",
       x = "Sample size (n)", y = "RMSE", colour = "Design") +
  theme_minimal()
ggsave("outputs/figures/sampling_rmse.png", width = 6, height = 4, dpi = 300)

summary_tbl |> select(design, n, bias, rmse)          # bias near 0?
summary_tbl |> group_by(n) |> slice_min(rmse)         # best design at each n

summary(d$crime_rate)

saveRDS(list(results = results, summary = summary_tbl,
             true_mean = true_mean, example_samples = ex),
        "outputs/objects/sampling_results.rds")
write.csv(summary_tbl, "outputs/tables/sampling_summary.csv", row.names = FALSE)
