# Install (once) if needed:
install.packages(c("readr","dplyr","sf","ggplot2","viridis","rnaturalearth","rnaturalearthdata"))

library(readr)
library(dplyr)
library(sf)
library(ggplot2)
library(viridis)
library(rnaturalearth)
library(rnaturalearthdata)


# Cargar datos
df <- read.csv("~/Desktop/THESIS_INDEX/H2_Dataframes_and_script_VMLI/VMLI/vMLI_index_results_FINAL.csv")

df <- df %>%
  mutate(ISO3C = toupper(trimws(ISO3C)))
# Cargar mapa mundial
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  # Excluir Antártida para evitar distorsiones de escala
  filter(admin != "Antarctica")

# 3) Unir por ISO3 (iso_a3 del mapa con ISO3C de tu CSV)
merged <- world %>%
  left_join(df, by = c("iso_a3" = "ISO3C"))

# 4) Graficar
ggplot(merged) +
  geom_sf(aes(fill = vMLI), size = 0.1, color = "gray85") +
  scale_fill_viridis(option = "C", na.value = "gray95",
                     name = "vMLI", guide = guide_colorbar(barwidth = 12)) +
  labs(title = "Global Vulnerability Media Literacy Index (vMLI)") +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.2),
    legend.position = "bottom"
)




# 1) Read your computed index
df <- read_csv("~/Desktop/THESIS_INDEX/VMLI/vMLI_index_results_FIXED_recomputed_education.csv", show_col_types = FALSE)

# Ensure ISO codes are character and upper-case (good habit)
df <- df %>%
  mutate(ISO3C.x = toupper(as.character(ISO3C)))

# 2) World boundaries (Natural Earth, as sf)
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  st_as_sf() %>%
  select(iso_a3, name, geometry)

# 3) Some common ISO mismatches (optional; uncomment if you see gaps)
# df <- df %>%
#   mutate(ISO3C = dplyr::case_when(
#     Country == "Kosovo" ~ "XKX",       # Natural Earth uses XKX
#     TRUE ~ ISO3C
#   ))

# 4) Join by ISO3C
map_df <- world %>%
  left_join(df, by = c("iso_a3" = "ISO3C"))

# 5) Quick diagnostics: which countries had no geometry or missing vMLI?
# anti_join(df, world, by = c("ISO3C" = "iso_a3")) %>% select(Country, ISO3C) %>% print(n=50)

# 6) Choropleth
p <- ggplot(map_df) +
  geom_sf(aes(fill = vMLI), color = NA) +
  coord_sf(crs = "+proj=eqearth") +
  scale_fill_viridis(
    option = "C",
    direction = -1,
    na.value = "grey90",
    name = "v-MLI"
  ) +
  labs(
    title = "Vulnerability Media Literacy Index (v-MLI)",
    subtitle = "Weighted composite; proportional reweighting for missing components",
    caption = "(v-MLI), by author"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.major = element_line(color = "grey85", linewidth = 0.2),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

print(p)

# 7) Save to file
ggsave("vMLI_world_map.png", p, width = 12, height = 7, dpi = 300)

# Optional: show top/bottom 10 countries by vMLI (in console)
df %>%
  arrange(desc(vMLI)) %>%
  select(Country, vMLI) %>%
  head(10) %>% print()

df %>%
  arrange(vMLI) %>%
  select(Country, vMLI) %>%
  head(10) %>% print()


library(readr)
library(dplyr)
library(ggplot2)

# Load your datasets
vMLI <- read_csv("~/Desktop/THESIS_INDEX/VMLI/vMLI_index_results_FIXED_recomputed_education.csv", show_col_types = FALSE)
fiw <- read_csv("~/Desktop/THESIS_INDEX/FITW_INDEX/FIW_2024_total_scores.csv", show_col_types = FALSE)

# Clean ISO codes
vMLI <- vMLI %>% mutate(ISO3C = toupper(ISO3C))
fiw <- fiw %>% mutate(ISO3C = toupper(ISO3C))

# Merge by ISO3C
df <- vMLI %>%
  left_join(fiw, by = "ISO3C")

# Automatically detect the Freedom variable
freedom_var <- names(df)[grepl("Total|Freedom|Score", names(df), ignore.case = TRUE)][1]
cat("Detected Freedom variable:", freedom_var, "\n")

# Clean numeric
df[[freedom_var]] <- as.numeric(df[[freedom_var]])

# Drop missing values
df <- df %>% filter(!is.na(vMLI) & !is.na(df[[freedom_var]]))

# Plot
ggplot(df, aes(x = .data[[freedom_var]], y = vMLI)) +
  geom_point(color = "steelblue", size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linewidth = 1) +
  labs(
    title = "Relationship between Freedom in the World and v-MLI",
    subtitle = "Higher Freedom scores associated with lower media vulnerability (if slope negative)",
    x = "Freedom in the World Total Score",
    y = "Vulnerability Media Literacy Index (v-MLI)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

# Save the plot (optional)
ggsave("Freedom_vs_vMLI.png", width = 9, height = 6, dpi = 300)


