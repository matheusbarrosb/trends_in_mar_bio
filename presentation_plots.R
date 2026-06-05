library(ggplot2)
library(dplyr)
library(stringr)
library(patchwork)

carbon_tbl = read.csv("data/table_richness_carbon.csv")


df_clean = carbon_tbl %>%
  mutate(
    Species.Number = as.numeric(str_remove_all(Species.Number, ",")),
    Biomass = as.numeric(str_remove_all(Biomass.of.carbon..billion.tons., ",")),
    sort_group = ifelse(!is.na(Class) & Class == "habitat", 1, 2)
  ) %>%
  arrange(sort_group, Species.Number) %>%
  mutate(Taxonomic.Group = factor(Taxonomic.Group, levels = Taxonomic.Group))

plot_left = ggplot(df_clean, aes(x = Species.Number, y = Taxonomic.Group)) +
  geom_col(fill = "#2c7fb8") +       
  scale_x_reverse() +                
  labs(x = "Species Richness", y = NULL) +
  geom_hline(yintercept = 5.5, color = "black", linetype = "dashed", linewidth = 0.8) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),  
    axis.text.y = element_text(face = "bold", size = 10, color = "black"), 
    plot.margin = margin(r = 0) 
  )

plot_right = ggplot(df_clean, aes(x = Biomass, y = Taxonomic.Group)) +
  geom_col(fill = "#7fcdbb", na.rm = TRUE) + 
  labs(x = "Biomass of Carbon (Tons)", y = NULL) +
  geom_hline(yintercept = 5.5, color = "black", linetype = "dashed", linewidth = 0.8) +
  theme_minimal() +
  theme(
    axis.text.y = element_blank(), 
    plot.margin = margin(l = 0),    
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank() 
  )

combined_plot = plot_left + plot_right + 
  plot_layout(widths = c(1, 1)) 

print(combined_plot)

# abundance trends
TheD = read.csv('data/abundance/Data Gathering - Data Format (1).csv')%>%
  group_by(ID) %>%
  mutate(Value = ifelse(Value == 0, 0.1*mean(Value[Value > 0], na.rm = TRUE), Value),
         Value_scale = Value/mean(Value),
         Value_log = log(Value_scale)) %>%
  ungroup() %>%
  mutate(ID = droplevels(as.factor(ID)),
         Taxa = droplevels(as.factor(Taxa))) %>%
  filter(Value_log>-10,Year >1950) 

TheD %>%
  filter(ID %in% sample(unique(ID), 100)) %>%
  ggplot(aes(x=Year,y=Value_log, group=ID)) +
  ochRe::scale_colour_ochre(palette="lorikeet")+
  geom_line(aes(color = ID), alpha = 0.8,size=1) + 
  geom_point(aes(color = ID), alpha = 0.8, size = 1) +
  geom_hline(yintercept=0, color = "grey50") +
  labs(y="Mean-scaled index (log)",x="Year")+
  theme_classic()+
  theme(legend.position = "none")


set.seed(420)
year = seq(1950,2020,by=1)
fake_ID = c("A","B","C","D","E")

poop <- expand.grid(Year = year, ID = fake_ID) %>%
  arrange(ID, Year) %>% 
  group_by(ID) %>% 
  mutate(
    log_shock = case_when(
      ID == "A" ~ rnorm(n(), -0.03, 0.1),
      ID == "B" ~ rnorm(n(),  -0.02, 0.1),
      ID == "C" ~ rnorm(n(),  0.0, 0.1),
      ID == "D" ~ rnorm(n(),  0.02, 0.1),
      ID == "E" ~ rnorm(n(),  0.03, 0.1),
    ),
    log_base = case_when(
      ID == "A" ~ log(100000),
      ID == "B" ~ log(10000),
      ID == "C" ~ log(1000),
      ID == "D" ~ log(100),
      ID == "E" ~ log(10),
    ),
    log_shock = if_else(Year == 1950, 0, log_shock),
    log_value = log_base + cumsum(log_shock),
    Value = exp(log_value))   %>% 
  ungroup() %>% 
  mutate(sample = sample(c(TRUE, FALSE), n(), prob=c(0.99,0.01), replace = TRUE))

p1 = poop %>% ggplot(aes(x=Year,y=Value,color=ID)) + geom_line(size=1) +
  facet_wrap(~ID, scales = "free_y",ncol=1) +
  ochRe::scale_colour_ochre(palette="lorikeet")+
  theme_classic()+
  theme(legend.position = "none",
        # Removes the background box entirely
        strip.background = element_blank(), 
        # Configures the text properties
        strip.text = element_text(
          hjust = 0,             # Left-align text (0 = left, 0.5 = center, 1 = right)
          face = "bold",         # Makes text bold
          color = "black",       # Makes text black
          size = 11              # Adjusts font size as needed
        )) +
  expand_limits(y=0) ;p1

p2 = poop %>% 
  # filter(sample==T) %>%
  group_by(ID) %>%
  mutate(Value_scaled = Value/mean(Value),
         Value_log = log(Value_scaled))%>% 
  ungroup() %>%
  group_by(Year) %>%
  mutate(avg = mean(Value_log),sd = sd(Value_log)) %>%
  ggplot() + geom_line(aes(x=Year,y=exp(Value_log),group=ID,color=ID)) +
  ochRe::scale_colour_ochre(palette="lorikeet")+
  geom_ribbon(aes(x=Year,ymin=exp(avg-sd),ymax=exp(avg+sd)), fill = "black",alpha=0.2) +
  geom_line(aes(x=Year,y=exp(avg)), color = "black", size = 2) +
  theme_classic()+
  theme(legend.position = "none",
        # Removes the background box entirely
        strip.background = element_blank(), 
        # Configures the text properties
        strip.text = element_text(
          hjust = 0,             # Left-align text (0 = left, 0.5 = center, 1 = right)
          face = "bold",         # Makes text bold
          color = "black",       # Makes text black
          size = 11              # Adjusts font size as needed
        )) +
  labs(y="Mean-scaled index")+
  expand_limits(y=0) ; p2

library(patchwork)
final_plot <- (p1 | p2) + plot_layout(widths = c(1, 4))
print(final_plot)

df <- read.csv("data/IUCN_tbl.csv", check.names = FALSE)

# 2. Reshape the data from wide to long format
df_long <- df %>%
  # Select only the grouping column and the IUCN category columns
  select(`Number of Species`,`Species Group`, `Data Deficient`, `Not Threatened`, `Endangered`, `Not Evaluated`) %>%
  mutate(`Species Group` = fct_reorder(`Species Group`, `Number of Species`, .desc = FALSE)) %>%
  pivot_longer(
    cols = c(`Data Deficient`, `Not Threatened`, `Endangered`, `Not Evaluated`),
    names_to = "IUCN_Category",
    values_to = "Count"
  ) %>%
  # Make the category a factor to control the stacking order (top to bottom or bottom to top)
  mutate(IUCN_Category = factor(IUCN_Category, 
                                levels = c("Not Evaluated", "Data Deficient", "Not Threatened", "Endangered")))

# 3. Specify your custom colors for each IUCN category
# You can change these to any R color names (e.g., "blue") or hex codes
library(ggpattern)

# 1. Define your colors
# Note: Set "Not Evaluated" to "white" (or another background color) so the crosshatch is visible
# 1. Define your colors (keeping "Not Evaluated" white as the background for the stripes)
my_colors <- c(
  "Not Evaluated"  = "white",   
  "Data Deficient" = "#bdbec0", 
  "Not Threatened" = "#0c6767", 
  "Endangered"     = "#cf6834"  
)

# 2. Define your patterns - changed to "stripe"
my_patterns <- c(
  "Not Evaluated"  = "stripe",
  "Data Deficient" = "none",
  "Not Threatened" = "none",
  "Endangered"     = "none"
)

# 3. Create the stacked barplot
ggplot(df_long, aes(y = `Species Group`, x = Count, fill = IUCN_Category)) +
  geom_col_pattern(
    aes(pattern = IUCN_Category),
    color = "grey50", 
    lwd = 0.15,
    pattern_fill = "black",     # The color of the stripes
    pattern_color = NA,         # Use NA here to prevent drawing lines around the pattern block itself
    pattern_density = 0.1,      # Adjust for thicker/thinner stripes
    pattern_spacing = 0.02,     # Adjust for how close together the stripes are
    pattern_angle = 45          # Angle of the stripes (45 = diagonal, 0 = horizontal, 90 = vertical)
  ) + 
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = my_patterns) + 
  labs(
    title = "IUCN Status by Species Group",
    y = "Species Group",
    x = "Number of Species",
    fill = "IUCN Category",
    pattern = "IUCN Category" 
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = c(0.85, 0.3),
    panel.grid.major.x = element_blank(),
    legend.key.size = unit(1.5, "cm") 
  )


ggplot(df_long %>% filter(`Species Group` %in% c("Elasmobranchs", "Birds","Mammals", "Reptiles")), aes(y = `Species Group`, x = Count, fill = IUCN_Category)) +
  geom_col_pattern(
    aes(pattern = IUCN_Category),
    color = "grey50", 
    lwd = 0.15,
    pattern_fill = "black",     # The color of the stripes
    pattern_color = NA,         # Use NA here to prevent drawing lines around the pattern block itself
    pattern_density = 0.1,      # Adjust for thicker/thinner stripes
    pattern_spacing = 0.02,     # Adjust for how close together the stripes are
    pattern_angle = 45          # Angle of the stripes (45 = diagonal, 0 = horizontal, 90 = vertical)
  ) + 
  scale_fill_manual(values = my_colors) + 
  scale_pattern_manual(values = my_patterns) + 
  labs(
    title = "IUCN Status by Species Group",
    y = "Species Group",
    x = "Number of Species",
    fill = "IUCN Category",
    pattern = "IUCN Category" 
  ) +
  theme_classic(base_size = 16) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_blank(),
    legend.key.size = unit(1.5, "cm") 
  )
