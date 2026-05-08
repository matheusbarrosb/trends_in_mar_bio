library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

spatial_df = readRDS("data/abundance/spatial_abundance_df.rds") %>%
  mutate(country = recode(country,
                          "Antartica"                  = "Antarctica",
                          "Congo, Dem. Rep. of the"    = "Democratic Republic of the Congo",
                          "Faroe Islands"              = "Faeroe Islands",      # check: world$name for exact spelling
                          "Iran (Islamic Rep. of)"     = "Iran",
                          "USA"     = "United States of America"
  ))

spatial_summary = spatial_df %>%
  group_by(ID,Lon,Lat,Taxa,country) %>%
  mutate(Value_log_diff = log(Value)-log(lag(Value))) %>%
  summarise(mean_change = mean(Value_log_diff, na.rm = TRUE)) %>%
  drop_na(mean_change) 


world <- ne_countries(scale = "medium", returnclass = "sf")

# ── 1. Split into point-based and country-based ──────────────
has_coords  <- spatial_summary %>% filter(!is.na(Lon) & !is.na(Lat))
has_country <- spatial_summary %>% filter(!is.na(country))

# ── 2. Check what your country codes look like ───────────────
# then match the right field in the world shapefile
# world has: iso_a2, iso_a3, iso_n3, name, name_long, admin
# common mismatch: iso_a2 has "-99" for some territories
head(has_country$country)         # e.g. "GBR", "USA", "AUS"?
names(world)                      # check available join keys

# ── 3. Join country rows to world polygons ───────────────────
# Adjust `by` to match your country code format:
#   iso_a3 for 3-letter codes (GBR, USA)
#   iso_a2 for 2-letter codes (GB, US)
country_sf <- world %>%
  left_join(
    has_country %>%
      group_by(country, Taxa) %>%
      summarise(mean_change = mean(mean_change, na.rm = TRUE), .groups = "drop"),
    by = c("name" = "country")   # <- adjust this join key
  ) %>%
  filter(!is.na(mean_change))

# ── 4. Colour scale robust to outliers ───────────────────────
all_vals <- c(has_coords$mean_change, country_sf$mean_change)
limit    <- quantile(abs(all_vals), 0.8, na.rm = TRUE)

col_scale_fill   <- scale_fill_gradient2(
  low = "#d73027", mid = "lightyellow", high = "#1a9850",
  midpoint = 0, limits = c(-limit, limit), oob = squish,
  name = "Mean log\nchange"
)

col_scale_colour <- scale_color_gradient2(
  low = "#d73027", mid = "lightyellow", high = "#1a9850",
  midpoint = 0, limits = c(-limit, limit), oob = squish,
  guide = "none"   # share legend with fill
)

# ── 5. Plot ───────────────────────────────────────────────────
ggplot() +
  # basemap
  geom_sf(data = world, fill = "grey20", color = "grey35", linewidth = 0.15) +
  # country polygons (country-coded data)
  geom_sf(data = country_sf, aes(fill = mean_change), color = NA, alpha = 0.85) +
  # points (lat/lon data) — drawn on top
  geom_point(
    data = has_coords,
    aes(x = Lon, y = Lat, color = mean_change),
    size = 1.5, alpha = 0.5
  ) +
  col_scale_fill +
  col_scale_colour +
  facet_wrap(~Taxa, ncol = 2) +
  coord_sf(expand = FALSE) +
  theme_dark() +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    legend.position = "right",
    panel.grid      = element_blank(),
    plot.title      = element_text(face = "bold")
  ) +
  labs(
    x = NULL, y = NULL
  )


library(tidyverse)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(scales)

spatial_df <- readRDS("data/abundance/spatial_abundance_df.rds") %>%
  mutate(country = recode(country,
                          "Antartica"                  = "Antarctica",
                          "Congo, Dem. Rep. of the"    = "Democratic Republic of the Congo",
                          "Faroe Islands"              = "Faeroe Islands",
                          "Iran (Islamic Rep. of)"     = "Iran",
                          "USA"                        = "United States of America"
  ))

# ── 1. Summarise availability (not trend) ────────────────────
spatial_summary <- spatial_df %>%
  group_by(ID, Lon, Lat, Taxa, country) %>%
  summarise(.groups = "drop")  # just deduplicate to one row per ID

has_coords  <- spatial_summary %>% filter(!is.na(Lon) & !is.na(Lat))
has_country <- spatial_summary %>% filter(!is.na(country))

# ── 2. Country-level: count distinct sites ───────────────────
country_sites <- has_country %>%
  group_by(country, Taxa) %>%
  summarise(n_sites = n_distinct(ID), .groups = "drop")

world <- ne_countries(scale = "medium", returnclass = "sf")

country_sf <- world %>%
  left_join(country_sites, by = c("name" = "country")) %>%
  filter(!is.na(n_sites))

# ── 3. Colour scale (sequential, log for skewed site counts) ─
max_sites <- max(country_sf$n_sites, na.rm = TRUE)

col_scale_fill <- scale_fill_gradientn(
  colours  = c("#f7fbff", "#6baed6", "#08306b"),
  trans    = "log10",
  breaks   = c(1, 10, 100, 1000),
  labels   = comma,
  limits   = c(1, max_sites),
  oob      = squish,
  name     = "No. sites\n(country)"
)

col_scale_colour <- scale_color_gradientn(
  colours  = c("#f7fbff", "#6baed6", "#08306b"),
  trans    = "log10",
  breaks   = c(1, 10, 100, 1000),
  limits   = c(1, max_sites),
  oob      = squish,
  guide    = "none"
)

# ── 4. Plot ───────────────────────────────────────────────────
ggplot() +
  geom_sf(data = world, fill = "grey20", color = "grey35", linewidth = 0.15) +
  geom_sf(data = country_sf, aes(fill = n_sites), color = NA, alpha = 0.85) +
  geom_point(
    data  = has_coords,
    color="#6baed6",
    aes(x = Lon, y = Lat),   # points coloured by time-series length
    size  = 1, alpha = 0.5
  ) +
  col_scale_fill +
  col_scale_colour +
  facet_wrap(~Taxa, ncol = 2) +
  coord_sf(expand = FALSE) +
  theme_dark() +
  theme(
    strip.text      = element_text(face = "bold", size = 11),
    legend.position = "right",
    panel.grid      = element_blank(),
    plot.title      = element_text(face = "bold")
  ) +
  labs(x = NULL, y = NULL)

