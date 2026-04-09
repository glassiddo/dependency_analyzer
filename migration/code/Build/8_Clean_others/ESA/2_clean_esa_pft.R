#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    February 6, 2026
#* Title:   Forest Loss before 2000 
#* Desc:    Get forest loss estimates per unit from ESA CCI
#*******************************************************************************
#* Note:    Have two versions, one from land classes of ESA 50-90, one from Harper et al. 2023
#* https://essd.copernicus.org/articles/15/1465/2023/
# Set Up ----
source("code/SSA_env_SetUp.R")

esa <- readRDS(here(
  raw.dir, "Africa", "Forest", "esa and pft", "sample_units_esa_5090.rds")
  ) %>% 
  select(
    ipums_id, 
    share_fl_esa_00_10 = share_lost_2000_2010,
    share_fl_esa_92_00 = share_lost_1992_2000
  ) 
  
pft <- readRDS(here(
  raw.dir, "Africa", "Forest", "esa and pft", "sample_units_pft.rds")
  ) %>% 
  select(
    ipums_id, 
    share_fl_pft_00_10 = share_lost_2000_2010,
    share_fl_pft_92_00 = share_lost_1992_2000
  ) 

combined_esa <- pft %>%
  left_join(esa, by = "ipums_id") %>% 
  mutate(across(starts_with("share"), ~replace_na(., 0))) %>% 
  set_variable_labels(
    share_fl_pft_00_10  = "Share of forest loss 2000-2010 from Harper et al (2023)",
    share_fl_pft_92_00  = "Share of forest loss 1992-2000 from Harper et al (2023)",
    share_fl_esa_00_10  = "Share of forest loss 2000-2010 from ESA land use classes 50-90",
    share_fl_esa_92_00  = "Share of forest loss 1992-2000 from ESA land use classes 50-90"
  )

write_dta(combined_esa, here(out.dir, "esa_fl_pre2000.dta"))

#### compare to hansen after 2000 ----
# hansen <- read_csv(here(raw.dir,"Africa","Forest", "sampleUnitsTreeCoverLoss.csv"))
# 
# hansen_cover <- read_csv(here(raw.dir,"Africa","Forest", "sampleUnitsTreeCover.csv"))
# 
# ## conver into long
# hansen_loss <- hansen %>%
#   select(ipums_id, starts_with("fcloss30")) %>%
#   pivot_longer(
#     cols = contains("loss"),
#     names_to = "measure",
#     values_to = "loss"
#   ) %>%
#   mutate(
#     year = as.numeric(str_sub(measure, -4)),
#     threshold = str_sub(measure, -6, -5)
#   ) %>%
#   full_join(
#     hansen_cover %>% 
#       select(ipums_id, areaha, treecover = fcarea30),
#     by = "ipums_id"
#   ) %>% 
#   arrange(ipums_id, year) %>%
#   group_by(ipums_id) %>%
#   mutate(
#     cumulative_loss = cumsum(replace_na(loss, 0)),
#     treecover_new = treecover - cumulative_loss
#   ) %>%
#   ungroup() %>% 
#   filter(year == 2010) %>% 
#   transmute(
#     ipums_id,
#     share_lost_hansen = cumulative_loss / treecover
#     ) 
# 
# cor(combined$share_lost_hansen, combined$share_lost_pft)
# 
# simple <- combined %>%
#   group_by(country) %>% 
#   reframe(
#     pft = mean(share_lost_pft),
#     hansen = mean(share_lost_hansen),
#     esa = mean(share_lost_esa),
#     pft9200 = mean(share_lost_pft_1992_2000),
#     esa9200 = mean(share_lost_esa_1992_2000)
#   ) 
# 
# ggplot(simple, aes(y = country)) +
#   geom_point(aes(x = esa, color = "PFT"), size = 2.5) +
#   geom_point(aes(x = pft, color = "ESA"), size = 2.5) +
#   geom_point(aes(x = hansen, color = "Hansen"), size = 2.5) +
#   scale_color_manual(
#     values = c("PFT" = "#E15759", "ESA" = "#4E79A7", "Hansen" = "#59A14F"),
#     name = NULL
#   ) +
#   labs(x = NULL, y = NULL) +
#   scale_y_discrete(expand = expansion(add = c(0.5, 0.5))) +
#   theme_minimal() +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.grid.minor = element_blank(),
#     legend.position = "top",
#     legend.text = element_text(size = 16)
#   )
# 
# summary(combined$share_lost_pft)
# summary(combined$share_lost_esa)
# summary(combined$share_lost_hansen)
