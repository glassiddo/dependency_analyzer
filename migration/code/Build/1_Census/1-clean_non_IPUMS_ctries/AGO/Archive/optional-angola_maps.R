source("code/SSA_env_SetUp.R")

ctry_df <- read_dta(
  here(raw.dir, "Countries", "AGO", "Census", "consistent14.dta")
)   
ago_sf1 <- read_sf(here(raw.dir, "Countries", "AGO", "Shapefiles", "level 1", "AGO_adm1.shp"))
ago_sf2 <- read_sf(here(raw.dir, "Countries", "AGO", "Shapefiles", "level 2", "AGO_adm2.shp"))

# mainly used to test everything compared to actual maps

#### pop density ------
# https://www.geo-ref.net/ph/ago.htm
pop2 <- ctry_df %>% 
  mutate(geo2_ao = as.character(geo2_ao)) %>% 
  group_by(geo2_ao) %>% 
  summarise(pop = sum(perwt))

density_breaks <- c(0, 1, 5, 10, 25, 50, 100, 200, 1000, 20000, 500000)

pop_map <- ago_sf2 %>% 
  rename(geo2_ao = GEOLEVEL2) %>% 
  left_join(pop2, by = "geo2_ao") %>%
  mutate(
    area_km2 = as.numeric(st_area(geometry)) / 10^6,
    pop_density = pop / area_km2,
    density_class = cut(
      pop_density,
      breaks = density_breaks,
      include.lowest = TRUE,
      right = TRUE
    )
  )

angola_palette <- c(
  "#E6F5E6",  # 0–1
  "#C9E9C9",  # 1–5
  "#A7DFA7",  # 5–10
  "#7CD47C",  # 10–25
  "#52B852",  # 25–50
  "#3E9E3E",  # 50–100
  "#2C7A2C",  # 100–200
  "#1E531E",  # 200–1000
  "#143B14",  # 1000–20000
  "#0B260B"   # 20000–500000 (new)
)

ggplot(pop_map) +
  geom_sf(aes(fill = density_class), color = "gray40", size = 0.2) +
  scale_fill_manual(
    name = "Population density\n(persons/km²)",
    values = angola_palette,
    drop = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

#### unemployment --------
# p65 in https://www.ine.gov.ao/Arquivos/arquivosCarregados/Carregados/Publicacao_637981512172633350.pdf

uenmp <- ctry_df %>% 
  filter(age > 14) %>% 
  mutate(
    geo1_ao = as.character(geo1_ao),
  ) %>% 
  group_by(geo1_ao) %>% 
  summarise(
    unemp_rate = sum(empstat == 2, na.rm = TRUE) / 
      (sum(empstat == 1, na.rm = TRUE) + sum(empstat == 2, na.rm = TRUE))
  )

density_breaks <- c(0, 0.13, 0.18, 0.24, 0.30, 0.33, 0.45, 1)

unemp_map <- ago_sf1 %>% 
  rename(geo1_ao = GEOLEVEL1) %>% 
  left_join(uenmp, by = "geo1_ao") %>%
  mutate(
    density_class = cut(
      unemp_rate,
      breaks = density_breaks,
      include.lowest = TRUE,
      right = TRUE
    )
  )

angola_palette <- c(
  "white",  # 0–13
  "#faecdc",
  "#edbb88",
  "#eb8c47",
  "#d25823",
  "#98431f",
  "black"
)

ggplot(unemp_map) +
  geom_sf(aes(fill = density_class), color = "gray40", size = 0.2) +
  scale_fill_manual(
    name = "Unemployment rate by region",
    values = angola_palette,
    drop = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )


### agriculture -----
# p76 in https://www.ine.gov.ao/Arquivos/arquivosCarregados/Carregados/Publicacao_637981512172633350.pdf
agriculture <- ctry_df %>% 
  mutate(
    geo1_ao = as.character(geo1_ao),
  ) %>% 
  group_by(geo1_ao) %>% 
  summarise(
    agriculture_rate = sum(indgen == 10, na.rm = TRUE) / 
      sum(empstat == 1, na.rm = TRUE)
  )

density_breaks <- c(0, 0.12, 0.35, 0.47, 0.56, 0.67, 0.73, 1)

agr_map <- ago_sf1 %>% 
  rename(geo1_ao = GEOLEVEL1) %>% 
  left_join(agriculture, by = "geo1_ao") %>%
  mutate(
    density_class = cut(
      agriculture_rate,
      breaks = density_breaks,
      include.lowest = TRUE,
      right = TRUE
    )
  )

angola_palette <- c(
  "white",  # 0–13
  "#ecf7fb",
  "#b9e0e0",
  "#78bea3",
  "#4b9f5e",
  "#2a6434",
  "black"
)

ggplot(agr_map) +
  geom_sf(aes(fill = density_class), color = "gray40", size = 0.2) +
  scale_fill_manual(
    name = "Agirulcutre rate by region",
    values = angola_palette,
    drop = FALSE
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 8)
  )

