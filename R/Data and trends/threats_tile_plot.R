library(tidyverse)
library(readxl)

df_raw <- read_excel("/Users/twangs/Downloads/Threats.xlsx", sheet = "Sheet7")

df <- df_raw %>%
  rename(Category = `Threat Category`) %>%
  pivot_longer(cols = -c(Threat, Category), names_to = "Taxa", values_to = "Impact")

threat_order <- unique(df_raw$Threat)

category_sequence <- df_raw %>%
  distinct(Threat, `Threat Category`) %>%
  mutate(Threat = factor(Threat, levels = threat_order)) %>%
  arrange(Threat)

# inject category subheadings into y-axis
y_levels <- c()
prev_cat  <- ""
for (i in seq_len(nrow(category_sequence))) {
  cat_i <- category_sequence$`Threat Category`[i]
  thr_i <- as.character(category_sequence$Threat[i])
  if (cat_i != prev_cat) { y_levels <- c(y_levels, paste0("__CAT__", cat_i)); prev_cat <- cat_i }
  y_levels <- c(y_levels, thr_i)
}
y_levels_rev <- rev(y_levels)

# heading rows get NA fill tiles
heading_rows <- tibble(
  Threat   = grep("^__CAT__", y_levels, value = TRUE),
  Category = sub("^__CAT__", "", grep("^__CAT__", y_levels, value = TRUE)),
  Taxa     = list(setdiff(names(df_raw), c("Threat", "Threat Category")))
) %>% unnest(Taxa) %>% mutate(Impact = NA_character_)

df_plot <- bind_rows(df, heading_rows) %>%
  mutate(Threat = factor(Threat, levels = y_levels_rev))

axis_labels <- setNames(sub("^__CAT__", "", y_levels_rev), y_levels_rev)
is_heading  <- str_starts(y_levels_rev, "__CAT__")

impact_colours <- c(
  "Direct"            = "#9B2335",
  "Indirect"          = "#D4703A",
  "Minimal"           = "#E8C468",
  "Beneficial"        = "#5A9E72",
  "Mixed" = "#7B6DAA",
  "Uncertain"         = "#A4AFA8",
  "No Data"           = "#F0EDE6"
)

p <- ggplot(df_plot, aes(x = Taxa, y = Threat, fill = Impact)) +
  geom_tile(colour = "white", linewidth = 0.4) +
  scale_fill_manual(values = impact_colours, na.value = NA, name = "Impact Type", drop = FALSE) +
  scale_x_discrete(expand = expansion(add = c(0.5, 0.5))) +
  scale_y_discrete(expand = expansion(add = c(0.5, 0.5)), labels = axis_labels) +
  labs(x = "Taxa", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(
    axis.text.x     = element_text(angle = 45, hjust = 1, size = 9),
    axis.text.y     = element_text(
      size   = ifelse(is_heading, 10, 8.5),
      face   = ifelse(is_heading, "bold", "plain"),
      colour = ifelse(is_heading, "black", "grey30"),
      hjust  = 1),
    axis.title.x    = element_text(margin = margin(t = 8)),
    panel.grid      = element_blank(),
    legend.position = "bottom",
    legend.title    = element_text(face = "bold"),
    plot.margin     = margin(10, 10, 10, 10)
  ) +
  guides(fill = guide_legend(nrow = 1, override.aes = list(colour = "grey60")))
p
