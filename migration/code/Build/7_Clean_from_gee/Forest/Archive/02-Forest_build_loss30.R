# Project: Migration Africa
# Author:  Sam Marshall
# Date:    Jan 19, 2022
# Title:   Build Forest Loss 30 Panels
# Desc:    Create Forest Loss Panels for 30% cutoff
# Output:  lossbyyear30.rds
#*TODO NB:  there are some countries not in the sample that I haven't fixed the
#* ascii coding for. So if any are added/this is expanded, will need to check
#* those
#*******************************************************************************

source("code/SSA_env_SetUp.R")

# function to read and format the region/district names to ascii
read_ascii <- function(fl_name) {
  out_df <- read_dta(paste0("data/Build/Africa/Forest/", fl_name, ".dta"),
                     encoding = "latin1") %>%
    remove_all_labels() %>%
    mutate(admin_name = iconv(admin_name, "latin1", "ASCII", "byte") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a1>", "a") %>%
             str_replace_all(. , "<c3><83><c2><a1>", "a") %>%
             str_replace_all(. , "<c3><83><c2><a3>", "a") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><ad>", "a") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a9>", "e") %>%
             str_replace_all(. , "<c3><83><c2><a9>", "e") %>%
             str_replace_all(. , "<c3><83><c2><a8>", "e") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a8>", "e") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><89>", "e") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><af>", "i") %>%
             str_replace_all(. , "<c3><83><c2><ad>", "i") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><b3>", "o") %>%
             str_replace_all(. , "<c3><83><c2><b3>", "o") %>%
             str_replace_all(. , "<c3><83><c2><85><c3><82><c2><93>", "e") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><ba>", "u") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a7>", "c") %>%
             str_replace_all(. , "<c3><83><c2><83><c3><82><c2><b4>", "o") %>%
             str_to_title(.)) %>%
    #group_by(cntry_name, admin_name, geolevel1) %>%
    drop_na(starts_with('geo')) %>%
    group_by(across(!starts_with('t'))) %>%
    mutate(across(starts_with('t'), ~max(.x, na.rm = TRUE))) %>%
    ungroup() %>%
    distinct() %>%
    # ones still not fixed
    mutate(across(starts_with('t'), ~ifelse(.x == -Inf, NA, .x)))
  
}

loss_long <- function(dat_df, geo_l) {
  if (geo_l == 1) {geo_name <- sym("region_name")}
  if (geo_l == 2) {geo_name <- sym("district_name")}
  
  loss_ann_df <- dat_df %>% 
    pivot_longer(contains("loss"),
                 names_to = "year",
                 values_to = "loss") %>%
    mutate(year = as.numeric(sub("tcloss", "", year))) %>%
    rename(treecover = tc2000, {{geo_name}} := admin_name,
           country = cntry_name) %>%
    filter(str_sub({{geo_name}}, 1, 6) != "Waterb") %>%
    filter(str_sub({{geo_name}}, 1, 3) != "Unk") %>%
    # convert from sq meters to hectares
    mutate(across(c(treecover, loss), ~.x/ 10000))
}

#*******************************************************************************
# IPUMS ----
#*******************************************************************************

## region ----
l1_o_df <- read_ascii("lossbyyear30_IPUMS1") %>%
  loss_long(1) %>%
  drop_na(geolevel1) 

l1_simp_df <- read_ascii("simple_Hansen_IPUMS1") %>%
  loss_long(1) %>%
  drop_na(geolevel1) %>%
  rename(treecover_simple = treecover, loss_simple = loss)

loss_ann_df <- full_join(
  l1_o_df, l1_simp_df,
  by = c("country", "region_name", "cntry_code", "geolevel1", "year")
  ) %>%
  mutate(geolevel1 = as.character(geolevel1),
         geolevel1 = ifelse(str_length(geolevel1) == 5, paste0("0", geolevel1), 
                            geolevel1))

saveRDS(loss_ann_df, file.path(build.dir,"Forest Cover/lossbyyear30_l1.rds"))

## district ----
l2_o_df <- read_ascii("lossbyyear30_IPUMS2") %>%
  loss_long(2) %>%
  drop_na(geolevel2)

l2_simp_df <- read_ascii("simple_Hansen_IPUMS2") %>%
  loss_long(2) %>%
  drop_na(geolevel2) %>%
  rename(treecover_simple = treecover, loss_simple = loss)

loss_ann_df <- full_join(
  l2_o_df, l2_simp_df,
  by = c("country", "district_name", "cntry_code", "geolevel2", "year")
  )

saveRDS(loss_ann_df, file.path(build.dir,"Forest Cover/lossbyyear30_l2.rds"))


#*******************************************************************************
#* Previous version ----
#*******************************************************************************

# l1_df <- read_dta("data/Hansen Forest Loss/Output/lossbyyear30_IPUMS1.dta",
#                   encoding = "latin1") %>%
#   remove_all_labels() %>%
#   mutate(admin_name = iconv(admin_name, "latin1", "ASCII", "byte") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a1>", "a") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><ad>", "a") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a9>", "e") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a8>", "e") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><89>", "e") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><af>", "i") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><b3>", "o") %>%
#            str_replace_all(. , "<c3><83><c2><85><c3><82><c2><93>", "e") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><ba>", "u") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a7>", "c") %>%
#            str_to_title(.))
#   
# 
# loss_ann_df <- l1_df %>% 
#   pivot_longer(contains("loss"),
#                names_to = "year",
#                values_to = "loss") %>%
#   mutate(year = as.numeric(sub("tcloss", "", year))) %>%
#   rename(treecover = tc2000, region_name = admin_name,
#          country = cntry_name) %>%
#   filter(str_sub(region_name, 1, 5) != "Water") %>%
#   filter(str_sub(region_name, 1, 3) != "Unk") %>%
#   # convert from sq meters to hectares
#   mutate(across(c(treecover, loss), ~.x/ 10000))
# 
# saveRDS(loss_ann_df, file.path(build.dir,"Forest Cover/lossbyyear30_l1.rds"))
# 
# l2_df <- read_dta("data/Hansen Forest Loss/Output/lossbyyear30_IPUMS2.dta",
#                   encoding = "latin1") %>%
#   remove_all_labels() %>%
#   mutate(admin_name = iconv(admin_name, "latin1", "ASCII", "byte") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a1>", "a") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><ad>", "a") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a9>", "e") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a8>", "e") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><89>", "e") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><af>", "i") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><b3>", "o") %>%
#            str_replace_all(. , "<c3><83><c2><85><c3><82><c2><93>", "e") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><ba>", "u") %>%
#            str_replace_all(. , "<c3><83><c2><83><c3><82><c2><a7>", "c") %>%
#            str_to_title(.)) 
# 
# loss_ann_df <- l2_df %>% 
#   pivot_longer(contains("loss"),
#                names_to = "year",
#                values_to = "loss") %>%
#   mutate(
#     year = as.numeric(sub("tcloss", "", year))) %>%
#   rename(treecover = tc2000, district_name = admin_name,
#          country = cntry_name) %>%
#   filter(str_sub(district_name, 1, 5) != "Water") %>%
#   filter(str_sub(district_name, 1, 3) != "Unk") %>%
#   # convert from sq meters to hectares
#   mutate(across(c(treecover, loss), ~.x/ 10000))
# 
# saveRDS(loss_ann_df, file.path(build.dir,"Forest Cover/lossbyyear30_l2.rds"))

# input_df <- read_dta("data/Hansen Forest Loss/Output/lossbyyear30_global.dta") %>%
#   remove_all_labels()
# 
# tc2000 <- input_df %>% select(starts_with("adm"), tc2000) %>%
#   rename(treecover = tc2000)
# 
# loss_ann_df <- input_df %>% select(-tc2000) %>%
#   pivot_longer(!starts_with("adm"),
#                names_to = "year",
#                values_to = "loss") %>%
#   mutate(year = as.numeric(sub("tcloss", "", year))) 
# 
# forestloss30_df <- full_join(tc2000, loss_ann_df) %>%
#   rename_with(~sub("adm0", "country", .x), starts_with("adm0")) %>%
#   rename_with(~sub("adm1", "region", .x), starts_with("adm1")) %>%
#   rename_with(~sub("adm2", "district", .x), starts_with("adm2"))
# 
# saveRDS(forestloss30_df, file.path(build.dir,"Forest Cover/lossbyyear30.rds"))

