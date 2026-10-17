# Phase 4: Spatial autocorrelation
library(sf)
library(spdep)
library(dplyr)
library(ggplot2)

# Do the files from Phases 2 and 3 exist?
file.exists("data/clean/master_analysis.rds")
file.exists("outputs/objects/weights.rds")

# Load them
master  <- readRDS("data/clean/master_analysis.rds")
weights <- readRDS("outputs/objects/weights.rds")

# Have a look
nrow(master)
names(weights)

# Look inside the weights folders
names(weights$W)
names(weights$B)
weights$default

# Is the analysis variable from Phase 2 there?
"log_crime_rate" %in% names(master)

# Do the rows line up with the weights? (must be TRUE)
identical(as.character(master$police_district), as.character(weights$id))

#Global Moran's I
set.seed(786)

w_queen <- weights$W$queen          # the group's default weights
x <- master$log_crime_rate          # the variable we are testing

sum(is.na(x))                       # should be 0 (no missing values)

n <- length(x)
-1 / (n - 1)                        # expected Moran's I if there is NO clustering

# Moran's I test
moran_q <- moran.test(x, w_queen, alternative = "greater", zero.policy = TRUE)
moran_q

# Same test again, using 999 random shuffles as a second check
moran_mc <- moran.mc(x, w_queen, nsim = 999,
                     alternative = "greater", zero.policy = TRUE)
moran_mc

#Geary's C and Global G 

# Geary's C (below 1 = positive clustering)
geary_q <- geary.test(x, w_queen, alternative = "greater", zero.policy = TRUE)
geary_q

# Global G: uses the RAW rate and the BINARY distance weights
min(master$crime_rate)              # must be 0 or more

w_dist_B <- weights$B$dist
globalG <- globalG.test(master$crime_rate, w_dist_B,
                        alternative = "two.sided", zero.policy = TRUE)
globalG

geary_q$estimate; geary_q$p.value


#Moran scatterplot
dir.create("outputs/figures", recursive = TRUE, showWarnings = FALSE)

z     <- as.numeric(scale(x))                          # standardise the variable
lag_z <- lag.listw(w_queen, z, zero.policy = TRUE)     # average of the neighbours

ms_df <- data.frame(z = z, lag_z = lag_z)
ms_df$quadrant <- ifelse(z >= 0 & lag_z >= 0, "High-High",
                         ifelse(z <  0 & lag_z <  0, "Low-Low",
                                ifelse(z >= 0 & lag_z <  0, "High-Low", "Low-High")))

p_ms <- ggplot(ms_df, aes(z, lag_z)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_point(aes(colour = quadrant), alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, colour = "black") +
  scale_colour_manual(values = c("High-High" = "#d7191c", "Low-Low" = "#2c7bb6",
                                 "High-Low" = "#fdae61", "Low-High" = "#abd9e9")) +
  labs(title = "Moran scatterplot of the log violent crime rate",
       subtitle = "Slope = Moran's I = 0.324 (queen weights)",
       x = "Standardised log crime rate",
       y = "Average of neighbours (spatial lag)",
       colour = "Quadrant") +
  theme_minimal()

p_ms                                                    # show the plot
ggsave("outputs/figures/04_moran_scatterplot.png", p_ms,
       width = 7, height = 6, dpi = 300)                # save it for the report

table(ms_df$quadrant)                                   # how many districts in each corner

#Local Moran's I (LISA)
lisa <- localmoran(x, w_queen, zero.policy = TRUE)

p_raw <- lisa[, grep("^Pr\\(", colnames(lisa))[1]]    # p-value for each district
p_fdr <- p.adjust(p_raw, method = "fdr")              # adjusted for 1,160 tests
quad  <- as.character(attr(lisa, "quadr")$mean)       # which corner each district is in
isolated <- card(w_queen$neighbours) == 0             # districts with no neighbours

cluster <- quad
cluster[is.na(p_fdr) | p_fdr >= 0.05] <- "Not significant"
cluster[isolated] <- "No neighbours"
cluster <- factor(cluster, levels = c("High-High", "Low-Low", "High-Low",
                                      "Low-High", "Not significant", "No neighbours"))

# Put the results next to each district
lisa_results <- master %>%
  select(police_district) %>%
  mutate(value   = x,
         local_I = lisa[, "Ii"],
         p_raw   = p_raw,
         p_fdr   = p_fdr,
         cluster = cluster)

table(lisa_results$cluster)                           # how many in each class

# The cluster map
p_lisa <- ggplot(lisa_results) +
  geom_sf(aes(fill = cluster), colour = "grey60", linewidth = 0.05) +
  scale_fill_manual(values = c("High-High" = "#d7191c", "Low-Low" = "#2c7bb6",
                               "High-Low" = "#fdae61", "Low-High" = "#abd9e9",
                               "Not significant" = "grey92", "No neighbours" = "white"),
                    drop = FALSE, name = "LISA cluster") +
  labs(title = "LISA cluster map: log violent crime rate",
       subtitle = "Local Moran's I, queen weights, FDR-adjusted p < 0.05") +
  theme_void()

p_lisa
ggsave("outputs/figures/04_lisa_cluster_map.png", p_lisa,
       width = 9, height = 7.5, dpi = 300)

#Which districts are the hot and cold spots? 
lisa_results$police_district[lisa_results$cluster == "High-High"]
lisa_results$police_district[lisa_results$cluster == "Low-Low"]

#Sensitivity check (does the weight choice matter?)
tidy_test <- function(test, test_name, weights_name) {
  est <- unname(test$estimate)
  data.frame(test = test_name, weights = weights_name,
             statistic = est[1], expected = est[2],
             z = unname(test$statistic), p_value = test$p.value)
}

moran_sens <- rbind(
  tidy_test(moran.test(x, weights$W$queen, zero.policy = TRUE), "Moran's I", "Queen"),
  tidy_test(moran.test(x, weights$W$rook,  zero.policy = TRUE), "Moran's I", "Rook"),
  tidy_test(moran.test(x, weights$W$knn,   zero.policy = TRUE), "Moran's I", "kNN"),
  tidy_test(moran.test(x, weights$W$dist,  zero.policy = TRUE), "Moran's I", "Distance band")
)
moran_sens

# Save everything
dir.create("outputs/tables",  recursive = TRUE, showWarnings = FALSE)
dir.create("outputs/objects", recursive = TRUE, showWarnings = FALSE)

global_results <- rbind(
  tidy_test(moran_q, "Moran's I", "Queen (W)"),
  tidy_test(geary_q, "Geary's C", "Queen (W)"),
  tidy_test(globalG, "Global G",  "Distance band (B)")
)
global_results

write.csv(global_results, "outputs/tables/global_autocorrelation.csv", row.names = FALSE)
write.csv(moran_sens,     "outputs/tables/moran_sensitivity.csv",      row.names = FALSE)
write.csv(as.data.frame(table(cluster = lisa_results$cluster)),
          "outputs/tables/lisa_cluster_counts.csv", row.names = FALSE)
saveRDS(lisa_results, "outputs/objects/lisa_results.rds")

