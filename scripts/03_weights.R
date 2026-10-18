# ============================================================
# 03_weights.R  |  Phase 3: spatial weights
# Input : data/clean/master_dataset.rds
# Output: outputs/objects/weights.rds
#         outputs/tables/weights_summary.csv
#         outputs/figures/*.png
# ============================================================
library(sf)
library(spdep)
library(dplyr)
library(ggplot2)

for (d in c("outputs/objects", "outputs/tables", "outputs/figures"))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ---- 1. Load and prepare geometry ---------------------------
master <- readRDS("data/clean/master_dataset.rds") |> st_make_valid()
nrow(master)                          # about 1160
anyDuplicated(master$police_district) # must be 0
st_crs(master)                        # note the original CRS

# South African Albers (metres), so distances are not in degrees
crs_sa <- "+proj=aea +lat_1=-24 +lat_2=-33 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
m      <- st_transform(master, crs_sa)
coords <- st_coordinates(st_centroid(st_geometry(m)))

# ---- 2. Contiguity neighbours -------------------------------
# snap = 1 metre tolerates tiny slivers between polygons
nb_queen <- poly2nb(m, queen = TRUE,  snap = 1)
nb_rook  <- poly2nb(m, queen = FALSE, snap = 1)
summary(nb_queen)
summary(nb_rook)

isolated <- which(card(nb_queen) == 0)
m$police_district[isolated]           # districts with NO queen neighbours

if (file.exists("data/clean/master_analysis.rds"))
  stopifnot(identical(readRDS("data/clean/master_analysis.rds")$police_district,
                      m$police_district))
# ---- 3. Distance-based neighbours ---------------------------
k <- 5
nb_knn <- knn2nb(knearneigh(coords, k = k))

# smallest distance at which every district has at least one neighbour
nn1    <- knn2nb(knearneigh(coords, k = 1))
d_band <- max(unlist(nbdists(nn1, coords)))
d_band / 1000                         # threshold in km
nb_dist <- dnearneigh(coords, d1 = 0, d2 = d_band)
summary(nb_dist)

# ---- 4. Weights objects -------------------------------------
# W = row-standardised (Moran, Geary, regression); B = binary (Global G, join counts)
w_queen <- nb2listw(nb_queen, style = "W", zero.policy = TRUE)
w_rook  <- nb2listw(nb_rook,  style = "W", zero.policy = TRUE)
w_knn   <- nb2listw(nb_knn,   style = "W", zero.policy = TRUE)
w_dist  <- nb2listw(nb_dist,  style = "W", zero.policy = TRUE)
b_queen <- nb2listw(nb_queen, style = "B", zero.policy = TRUE)
b_dist  <- nb2listw(nb_dist,  style = "B", zero.policy = TRUE)

# ---- 5. Compare structures ----------------------------------
summ <- function(nb, name) {
  n <- card(nb)
  tibble(structure = name, districts = length(nb), links = sum(n),
         mean_neighbours = round(mean(n), 2), min = min(n), max = max(n),
         isolated = sum(n == 0))
}
weights_summary <- bind_rows(
  summ(nb_queen, "queen"), summ(nb_rook, "rook"),
  summ(nb_knn, paste0(k, "-nearest")), summ(nb_dist, "distance band"))
print(weights_summary)

counts <- bind_rows(
  tibble(structure = "queen",         n = card(nb_queen)),
  tibble(structure = "rook",          n = card(nb_rook)),
  tibble(structure = "5-nearest",     n = card(nb_knn)),
  tibble(structure = "distance band", n = card(nb_dist)))
p_hist <- ggplot(counts, aes(n)) +
  geom_histogram(binwidth = 1, fill = "steelblue", colour = "white") +
  facet_wrap(~ structure, scales = "free") +
  labs(x = "Number of neighbours", y = "Districts",
       title = "Neighbour counts by weights structure") +
  theme_minimal()
ggsave("outputs/figures/neighbour_counts.png", p_hist, width = 8, height = 6, dpi = 200)

# Map of queen links between centroids (isolated districts in orange)
edges <- do.call(rbind, lapply(seq_along(nb_queen), function(i) {
  j <- nb_queen[[i]]; j <- j[j > i]
  if (length(j)) cbind(i, j) else NULL
}))
links <- st_sfc(lapply(seq_len(nrow(edges)), function(e)
  st_linestring(rbind(coords[edges[e, 1], ], coords[edges[e, 2], ]))), crs = st_crs(m))
p_map <- ggplot() +
  geom_sf(data = m, fill = "grey95", colour = "grey75", linewidth = 0.1) +
  geom_sf(data = m[isolated, ], fill = "orange", colour = "grey40", linewidth = 0.2) +
  geom_sf(data = links, colour = "red", linewidth = 0.15) +
  labs(title = "Queen neighbour links") + theme_void()
ggsave("outputs/figures/queen_links.png", p_map, width = 7, height = 8, dpi = 200)

# Sanity check: confirm a few districts by eye on a map
show_nb <- function(name) m$police_district[nb_queen[[which(m$police_district == name)]]]
show_nb("aberdeen"); show_nb("acornhoek"); show_nb("akasia")

# ---- 6. Save -------------------------------------------------
weights <- list(
  id = m$police_district,           # row order the weights are tied to
  crs = crs_sa, k = k, dist_band_m = d_band, default = "queen",
  nb = list(queen = nb_queen, rook = nb_rook, knn = nb_knn, dist = nb_dist),
  W  = list(queen = w_queen, rook = w_rook, knn = w_knn, dist = w_dist),
  B  = list(queen = b_queen, dist = b_dist))
saveRDS(weights, "outputs/objects/weights.rds")
write.csv(weights_summary, "outputs/tables/weights_summary.csv", row.names = FALSE)