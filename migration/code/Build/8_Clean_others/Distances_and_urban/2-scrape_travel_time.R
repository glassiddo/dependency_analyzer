#* Author:  Sam Marshall
#* Created: June 9, 2022
#* Title:   Clean census files for each country
#* Output:  
#* Notes:  Need to do Ghana, Mozambique, Sierra Leone one-by-one
#* Can't get travel times to islands from mainland. Problem for 
#* Tanzania. Can check like google maps
#*******************************************************************************
# Set Up ----
source("code/SSA_env_SetUp.R")

# osrm url
osrm.url <- 'http://router.project-osrm.org/table/v1/driving/'
src.url <- paste0('?annotations=duration,distance')

get_dist <- function(dat_df) {
  coor_list <- paste(dat_df$ll, collapse = ';')
  
  maps.url <- paste0(osrm.url,coor_list,src.url)
  
  # read the osrm data
  map_out <- jsonlite::read_json(maps.url)
  
  # frame output for one source and many destinations
  dest <- data.frame()
  for(j in 1: length(map_out$destinations)) {
    dest <- bind_rows(
      dest,
      tibble(d_lng = map_out$destinations[[j]]$location[[1]],
             d_lat = map_out$destinations[[j]]$location[[2]],
             ipums_id_d = dat_df$ipums_id[j])
    )
  }
  
  # merge identifying information
  dest %<>% left_join(d_df, by = join_by(ipums_id_d))
  
  # origin
  orig <- data.frame()
  for(i in 1: length(map_out$sources)) {
    
    iter <- tibble(o_lng = map_out$sources[[i]]$location[[1]],
                   o_lat = map_out$sources[[i]]$location[[2]],
                   ipums_id_o = dat_df$ipums_id[i]) %>%
      merge(dest) %>%
      mutate(dist_m = map_out$distances[[i]],
             dur_s = map_out$durations[[i]]) %>%
      left_join(o_df, by = join_by(ipums_id_o))
    
    orig <- bind_rows(orig, iter)
  }
  
  # clean up and add to main
  travel_rep <- orig %>%
    select(country, admin_name, ipums_id_d, ipums_id_o, prev_admin_name, 
           prev_country, dist = dist_m, dur = dur_s) %>%
    mutate(
      # make distance in kilometers
      dist = as.numeric(dist) / 1000,
      # travel duration in hours
      dur = as.numeric(dur) / (60 * 60)) 
  
  return(travel_rep)
  # wait for a random amount of time
  sleepy_head <- runif(1, min = 1, max = 4)
  Sys.sleep(sleepy_head)
}


#*******************************************************************************
# get used centroid coordinates ----
l2_df <- readRDS(here(build.dir, "Africa", "Maps", paste0("l", 2,"_centriods.rds")))
l1_df <- readRDS(here(build.dir, "Africa", "Maps", paste0("l", 1,"_centriods.rds")))

# list of ctry by admin unit level
id_df <- readRDS(here(out.dir, "R", "id.rds")) 
l2_list <- id_df %>% filter(geo_lvl == 2) %>%
  select(country) %>%
  distinct() %>%
  .$country

l1_list <- id_df %>% filter(geo_lvl == 1) %>%
  select(country) %>%
  distinct() %>%
  .$country

samp <- bind_rows(
  l2_df %>% filter(country %in% l2_list),
  l1_df %>% filter(country %in% l1_list)
  ) %>%  
  mutate(admin_name = ifelse(is.na(district_name) == FALSE,
                             district_name, region_name),
         coor = st_coordinates(geometry),
         lng = str_sub(as.character(coor[,1]),1,8),
         lat = str_sub(as.character(coor[,2]),1,8),
         ll = paste(lng,lat, sep = ',')) %>% 
  select(country, admin_name, ipums_id, geometry, lng, lat,ll)

rm(l1_list, l2_list,l1_df, l2_df)

d_df <- samp %>% as.data.frame() %>%
  select(country, admin_name, ipums_id_d = ipums_id)

o_df <- d_df %>%
  select(prev_country = country, prev_admin_name = admin_name, 
         ipums_id_o = ipums_id_d)
#*******************************************************************************
# fetch distances and travel times ----

od_df <- data.frame()
ctry_list <- samp %>% as.data.frame() %>% select(country) %>% 
  distinct() %>% .$country

for (c in ctry_list) {
  # filter for country
  ctry <- samp %>% filter(country == c)
  
  # Tanzania breaks for the islands
  if (c == 'TZA') {
    ctry %<>% 
      filter(ipums_id %!in% c('834051','834052','834053','834054',
                              '834055'))
  }
  
  ## <100 units ----
  #* countries with less than 100 units can do all at once
  if (nrow(ctry) < 100) {
    
    coor_list <- paste(ctry$ll, collapse = ';')

    maps.url <- paste0(osrm.url,coor_list,src.url)
    
    # read the osrm data
    map_out <- jsonlite::read_json(maps.url)
    
    # frame output for one source and many destinations
    dest <- data.frame()
    for(j in 1: length(map_out$destinations)) {
      dest <- bind_rows(
        dest,
        tibble(d_lng = map_out$destinations[[j]]$location[[1]],
               d_lat = map_out$destinations[[j]]$location[[2]],
               ipums_id_d = ctry$ipums_id[j])
      )
    }
    
    # merge identifying information
    dest %<>% left_join(d_df, by = join_by(ipums_id_d))
    
    # origin
    orig <- data.frame()
    for(i in 1: length(map_out$sources)) {
      
      iter <- tibble(o_lng = map_out$sources[[i]]$location[[1]],
                     o_lat = map_out$sources[[i]]$location[[2]],
                     ipums_id_o = ctry$ipums_id[i]) %>%
        merge(dest) %>%
        mutate(dist_m = map_out$distances[[i]],
               dur_s = map_out$durations[[i]]) %>%
        left_join(o_df, by = join_by(ipums_id_o))
      
      orig <- bind_rows(orig, iter)
    }
    
    # clean up and add to main
    travel_rep <- orig %>%
      select(country, admin_name, ipums_id_d, ipums_id_o, prev_admin_name, 
             prev_country, dist = dist_m, dur = dur_s) %>%
      mutate(
        # make distance in kilometers
        no_dat = is.null(dist),
        dist = ifelse(no_dat == FALSE, as.numeric(dist) / 1000, NA),
        # travel duration in hours
        dur = ifelse(no_dat == FALSE, as.numeric(dur) / (60 * 60), NA)
      ) %>%
      select(-no_dat)
    
    od_df <- bind_rows(od_df, travel_rep)
    
    print(paste0('Country ', c))
    
    # wait for a random amount of time
    sleepy_head <- runif(1, min = 1, max = 4)
    Sys.sleep(sleepy_head)
  }
  
  #****************************************************************************
  ## 100+ units ----
  #* countries with more than 100 units need to be split up
  if (nrow(ctry) >= 100) {
    
    long_df <- data.frame()
    #* break into groups of 50
    r <- 50
    set <- c(r)
    while (r < nrow(ctry)) {
      r <- min(r+ 50, nrow(ctry))
      set <- c(set, r)
    }
    
    # loop over all the pairs of 50 in a medium efficient way
    count <- 0
    for (s1 in 1:(length(set) -1)) {
      for (s2 in 2:length(set)) {
        
        # define the set of rows that are being searched over
        r_list <- c((set[s1]-49):set[s1],(set[s2]-49):set[s2])
        
        coor_list1 <- paste(ctry$ll[(set[s1]-49):set[s1]], collapse = ';')
        coor_list2 <- paste(ctry$ll[(set[s2]-49):set[s2]], collapse = ';')
        coor_list <- paste0(coor_list1, ';', coor_list2)
        
        maps.url <- paste0(osrm.url,coor_list,src.url)
        
        # read the osrm data
        map_out <- jsonlite::read_json(maps.url)
        
        # make the destination data frame
        dest <- data.frame()
        for(j in 1: length(map_out$destinations)) {
          ctry_row <- r_list[j]
          dest <- bind_rows(
            dest,
            tibble(d_lng = map_out$destinations[[j]]$location[[1]],
                   d_lat = map_out$destinations[[j]]$location[[2]],
                   ipums_id_d = ctry$ipums_id[ctry_row])
          )
        }
        
        dest %<>% left_join(d_df, by = join_by(ipums_id_d))
        
        # origin
        orig <- data.frame()
        for(i in 1: length(map_out$sources)) {
          ctry_row <- r_list[i]
          iter <- tibble(o_lng = map_out$sources[[i]]$location[[1]],
                         o_lat = map_out$sources[[i]]$location[[2]],
                         ipums_id_o = ctry$ipums_id[ctry_row]) %>%
            merge(dest) %>%
            mutate(dist_m = map_out$distances[[i]],
                   dur_s = map_out$durations[[i]]) %>%
            left_join(o_df, by = join_by(ipums_id_o))
          
          orig <- bind_rows(orig, iter)
        }
        
        # clean up and add to main
        travel_rep <- orig %>%
          select(country, admin_name, ipums_id_d, ipums_id_o, prev_admin_name, 
                 prev_country, dist = dist_m, dur = dur_s) %>%
          mutate(
            # make distance in kilometers
            no_dat = sapply(dist, is.null),
            dist = ifelse(no_dat == FALSE, as.numeric(dist) / 1000, NA),
            # travel duration in hours
            dur = ifelse(no_dat == FALSE, as.numeric(dur) / (60 * 60), NA)
          ) %>%
          select(-no_dat)
        
        long_df <- bind_rows(long_df, travel_rep)
        
        print(paste0('Country ', c, ' rep ', count))
        
        # wait for a random amount of time
        sleepy_head <- runif(1, min = 1, max = 4)
        Sys.sleep(sleepy_head)
        
        count <- count + 1
      }
    }
    
    # remove duplicate rows and add to main
    uniq_df <- long_df %>% distinct()
    
    # do the number of rows match what is expected?
    print(paste0('Expected rows = ', nrow(ctry)^2, 
                 '; actual rows = ', nrow(uniq_df)))
    
    od_df <- dplyr::bind_rows(od_df, uniq_df)
    
    rm(long_df, r, set, coor_list, coor_list1, coor_list2, count,
       i, j, uniq_df, s1, s2, r_list)
    
    # wait for a random amount of time
    sleepy_head <- runif(1, min = 1, max = 4)
    Sys.sleep(sleepy_head)
  }

}



#*******************************************************************************
# TZA special case ----
# od_df <- readRDS(here(build.dir, "Africa", "Travel Distance", "osm_dist.rds"))

ctry <- samp %>% filter(country == 'TZA')

# Tanzania breaks for the islands
zanzibar <- ctry %>% 
  filter(ipums_id %in% c('834051','834052','834053')) %>%
  get_dist()
#* 2 hours 3 minutes, 80.7km on ferry from Zanzibar to DSM 
#* DSM ferry -6.820419785334469, 39.2883362429173

pemba <- ctry %>% 
filter(ipums_id %in% c('834054','834055')) %>%
  get_dist()

ferry <- ctry %>% 
  filter(ipums_id %!in% c('834051','834052','834053','834054','834055')) %>%
  dplyr::bind_rows(
    tibble(country = 'TZA', admin_name = 'ferry', ipums_id = '834099',
           ll = '39.28834,-6.82042')
  ) %>%
  get_dist()

ferry2 <- ferry %>%
  mutate(prev_admin_name = ifelse(is.na(prev_admin_name), 'ferry', prev_admin_name),
         admin_name = ifelse(is.na(admin_name), 'ferry', admin_name),
         country = 'TZA',
         prev_country = 'TZA',
         #add ferry travel time!
         dist = dist + 80.2,
         dur = dur + 2.05) %>%
  filter(ipums_id_d == '834099' | ipums_id_o == '834099') %>%
  filter(!(ipums_id_d == '834099' & ipums_id_o == '834099'))

# 529 total
#* put all the islands as ferry time plus the additional bit
island_time <- data.frame()

for (id in c('834051','834052','834053','834054','834055')) {
  id_name <- ctry %>% filter(ipums_id == id) %>% .$admin_name
  island_iter <- ferry2 %>%
    mutate(
      across(c(admin_name, prev_admin_name), ~ifelse(.x == 'ferry', id_name, .x)),
      across(c(ipums_id_o, ipums_id_d), ~ifelse(.x == '834099', id, .x))
    )
  island_time <- dplyr::bind_rows(island_time, island_iter)
}

# between islands is just going to be the double of the ferry
inter_island <- dplyr::bind_rows(
  merge(
  ctry %>% 
    filter(ipums_id %in% c('834051','834052','834053')) %>%
    as.data.frame() %>%
    select(country, admin_name, ipums_id_d = ipums_id),
  ctry %>% 
    filter(ipums_id %in% c('834054','834055')) %>%
    as.data.frame() %>%
    select(prev_country = country, prev_admin_name = admin_name, ipums_id_o = ipums_id)
  ),
  merge(
    ctry %>% 
      filter(ipums_id %in% c('834054','834055')) %>%
      as.data.frame() %>%
      select(country, admin_name, ipums_id_d = ipums_id),
    ctry %>% 
      filter(ipums_id %in% c('834051','834052','834053')) %>%
      as.data.frame() %>%
      select(prev_country = country, prev_admin_name = admin_name, ipums_id_o = ipums_id)
  )
) %>%
  mutate(dist = 160.4,
         dur = 4.1)

# magic number 529
tza_df <- dplyr::bind_rows(
  od_df %>% filter(country == 'TZA'),
  zanzibar,
  pemba,
  island_time,
  inter_island
)

od_df <- dplyr::bind_rows(
  od_df,
  zanzibar,
  pemba,
  island_time,
  inter_island
)

saveRDS(od_df, here(build.dir, "Africa", "Travel Distance", "osm_dist.rds"))

#*******************************************************************************
# the one-by-one method ----
#NR ----
# can do later for country pairs...
# # filter for country
# ctry <- samp %>% filter(country == 'Ghana')
# long_df <- data.frame()
# #* break into groups of 50
# r <- 50
# set <- c(r)
# while (r < nrow(ctry)) {
#   r <- min(r+ 50, nrow(ctry))
#   set <- c(set, r)
# }
# 
# count <- 0
# for (s1 in 1:(length(set) -1)) {
#   for (s2 in 2:length(set)) {
#     
#     r_list <- c((set[s1]-49):set[s1],(set[s2]-49):set[s2])
#     
#     coor_list1 <- paste(ctry$ll[(set[s1]-49):set[s1]], collapse = ';')
#     coor_list2 <- paste(ctry$ll[(set[s2]-49):set[s2]], collapse = ';')
#     coor_list <- paste0(coor_list1, ';', coor_list2)
#     
#     maps.url <- paste0(osrm.url,coor_list,src.url)
#     
#     # read the osrm data
#     map_out <- jsonlite::read_json(maps.url)
#     
#     
#     # frame output for one source and many destinations
#     dest <- data.frame()
#     for(j in 1: length(map_out$destinations)) {
#       ctry_row <- r_list[j]
#       dest <- bind_rows(
#         dest,
#         tibble(d_lng = map_out$destinations[[j]]$location[[1]],
#                d_lat = map_out$destinations[[j]]$location[[2]],
#                rn = ctry_row,
#                ipums_id_d = ctry$ipums_id[ctry_row])
#       )
#     }
#     
#     dest %<>% left_join(d_df, by = join_by(ipums_id_d))
#     
#     # origin
#     orig <- data.frame()
#     for(i in 1: length(map_out$sources)) {
#       ctry_row <- r_list[i]
#       iter <- tibble(o_lng = map_out$sources[[i]]$location[[1]],
#                      o_lat = map_out$sources[[i]]$location[[2]],
#                      ipums_id_o = ctry$ipums_id[ctry_row]) %>%
#         merge(dest) %>%
#         mutate(dist_m = map_out$distances[[i]],
#                dur_s = map_out$durations[[i]]) %>%
#         left_join(o_df, by = join_by(ipums_id_o))
#       
#       orig <- bind_rows(orig, iter)
#     }
#     
#     # clean up and add to main
#     travel_rep <- orig %>%
#       select(country, admin_name, ipums_id_d, ipums_id_o, prev_admin_name, 
#              prev_country, dist = dist_m, dur = dur_s) %>%
#       mutate(
#         # make distance in kilometers
#         no_dat = is.null(dist),
#         dist = ifelse(no_dat == FALSE, as.numeric(dist) / 1000, NA),
#         # travel duration in hours
#         dur = ifelse(no_dat == FALSE, as.numeric(dur) / (60 * 60), NA)
#       ) %>%
#       select(-no_dat)
#     
#     long_df <- bind_rows(long_df, travel_rep)
#     
#     print(paste0('rep ', count))
#     
#     # wait for a random amount of time
#     sleepy_head <- runif(1, min = 1, max = 4)
#     Sys.sleep(sleepy_head)
#     
#     count <- count + 1
#   }
# }


