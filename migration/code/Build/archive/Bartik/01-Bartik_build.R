#* Project: Migration Africa
#* Author:  Sam Marshall
#* Date:    June 8, 2022
#* Title:   Build Forest Loss Bartik migration datasets for each country
#* Desc:    1 hectare = 10,000 sq m = 1x10^4 sq m
#*         1 km^2 = 100 hectare = 1x10^6 sq m
#*         New countries: Cameroon, Malawi, Rwanda, South Sudan, Sudan, Togo, Zimbabwe
#*******************************************************************************

# Set Up ----
source("code/SSA_env_SetUp.R")
source("code/Build/Bartik/0-F-Bartik_CI.R")
#source("code/Build/Bartik/0-F-Bartik.R")

#*******************************************************************************
# Benin ----
# Duration use 10 years
#*******************************************************************************
cname <- "Benin"
cens1_df <- read_census(cname, "02")
cens2_df <- read_census(cname, "13")

# geo_l <- 2
for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))

  ann_treeloss <- read_fl(cname, geo_l) 
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  # 1. migration flows in first period
  mig_od <- od_flow(cens1_df, 1992, 2001, lvl = geo_l)
  emig_o <- emig_flow(cens2_df, 2003, 2012, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  # urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2003, 2012) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 2002, 2013, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2011, lvl = geo_l)
  
  benin_df <- f_bartik(ctry3 = "BEN", lvl = geo_l) 
  
  save_ctry(benin_df, cname, lvl = geo_l)

}
rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#temp_df <- read_nl(cname, geo_l)
#temp_df <- benin_df %>% filter(is.na(Bm_ntla) == TRUE)

# birth_od <- od_period_flow(ben02_df, lag = "birth")

#*******************************************************************************
# Botswana ----
# Birth region, region 5 years ago
#*******************************************************************************
cname <- "Botswana"
cens1_df <- read_census(cname, "01")
cens2_df <- read_census(cname, "11")

ann_treeloss <- read_fl(cname, 1)
distance_df <- read_dist(cname, 1)
urb_df <- read_urb(cname, 1)

mig_od <- od_period_flow(cens1_df, lvl = 1)
#birth_od <- od_period_flow(bwa01_df, lag = "birth")
emig_o <- emig_period_flow(cens2_df, 5, lvl = 1)
pop_df <- f_pop(cens1_df, cens2_df, lvl = 1)
#urb_df <- f_urb(lvl = 1)
loss_df <- geo_forestloss(ann_treeloss, 1, 2002, 2010) 
delta_nl <- f_nitelite(read_nl(cname, 1), 2001, 2011, lvl = 1)
delta_crop <- f_croparea(read_crop(cname, 1), 2003, 2011, lvl = 1)

botswana_df <- f_bartik(ctry3 = "BWA", lvl = 1) 

save_ctry(botswana_df, cname, lvl = 1)

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# Burkina Faso ----
# Location 1 year ago
#*******************************************************************************
cname <- "Burkina Faso"
cens1_df <- read_census(cname, "96")
cens2_df <- read_census(cname, "06")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_period_flow(cens1_df, lag = "birth", lvl = geo_l)
  emig_o <- emig_period_flow(cens2_df, 1, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2005) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 1996, 2006, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2007, lvl = geo_l)
  
  burkinafaso_df <- f_bartik(ctry3 = "BFA", lvl = geo_l) 
  
  save_ctry(burkinafaso_df, cname, lvl = geo_l)
}

#mig_od <- od_period_flow(bfa96_df)

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# Cameroon ----
#*******************************************************************************
cname <- "Cameroon"
cens1_df <- read_census(cname, "87")
cens2_df <- read_census(cname, "05")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_flow(cens1_df, 1977, 1986, lvl = geo_l)
  emig_o <- emig_flow(cens2_df, 1995, 2004, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2005) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 1995, 2005, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2007, lvl = geo_l)
  
  cameroon_df <- f_bartik(ctry3 = "CMR", lvl = geo_l) 
  
  save_ctry(cameroon_df, cname, lvl = geo_l)
}

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)


#*******************************************************************************
# Ghana ----
# 5 years (no birth district)
#*******************************************************************************
cname <- "Ghana"
cens1_df <- read_census(cname, "00")
cens2_df <- read_census(cname, "10")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_period_flow(cens1_df, lvl = geo_l)
  emig_o <- emig_period_flow(cens2_df, 5, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2009) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 2000, 2010, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2011, lvl = geo_l)
  
  ghana_df <- f_bartik(ctry3 = "GHA", lvl = geo_l) 
  
  save_ctry(ghana_df, cname, lvl = geo_l)
}

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# Guinea ----
#*******************************************************************************
#* birth location duration (10 years)
#* currently have district only (no region, but could make by aggregating)
cname <- "Guinea"
cens1_df <- read_census(cname, "96")
cens2_df <- read_census(cname, "14")

for(geo_l in c(2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_flow(cens1_df, 1986, 1995, lvl = geo_l)
  emig_o <- emig_flow(cens2_df, 2004, 2013, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2013) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 1996, 2014, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2015, lvl = geo_l)
  
  guinea_df <- f_bartik(ctry3 = "GIN", lvl = geo_l) 
  
  save_ctry(guinea_df, cname, lvl = geo_l)
}

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

# mig_od <- od_flow(gin96_df, 1990, 1995)
# birth_od <- od_period_flow(gin96_df, lag = "birth")
# emig_o <- emig_flow(gin14_df, 2009, 2013)

#*******************************************************************************
# Kenya ----
# previous location round 1, one year ago round 2
#*******************************************************************************
cname <- "Kenya"
cens1_df <- read_census(cname, "99")
cens2_df <- read_census(cname, "09")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_flow(cens1_df, 1989, 1998, lvl = geo_l)
  emig_o <- emig_period_flow(cens2_df, 1, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2008) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 1999, 2009, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2007, lvl = geo_l)
  
  kenya_df <- f_bartik(ctry3 = "KEN", lvl = geo_l) 
  
  save_ctry(kenya_df, cname, lvl = geo_l)
}

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# Lesotho ----
# birth location duration (10 years)
#*******************************************************************************
cname <- "Lesotho"
geo_l <- 1
print(paste0("Running: ",cname, " level ", geo_l))

cens1_df <- read_census(cname, "96")
cens2_df <- read_census(cname, "06")
ann_treeloss <- read_fl(cname, geo_l)
distance_df <- read_dist(cname, geo_l)
urb_df <- read_urb(cname, geo_l)

mig_od <- od_period_flow(cens1_df, lvl = geo_l)
emig_o <- emig_period_flow(cens2_df, period_len = 10, lvl = geo_l)
pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
#urb_df <- f_urb(lvl = geo_l)
loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2005) 
delta_nl <- f_nitelite(read_nl(cname, geo_l), 1996, 2006, lvl = geo_l)
delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2007, lvl = geo_l)

lesotho_df <- f_bartik(ctry3 = "LSO", lvl = geo_l) 

save_ctry(lesotho_df, cname, lvl = geo_l)

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, geo_l, delta_nl, delta_crop)

#*******************************************************************************
# Malawi ----
# duration
#*******************************************************************************
cname <- "Malawi"
cens1_df <- read_census(cname, "08")

geo_l <- 1
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_flow(cens1_df, 1998, 2007, lvl = geo_l)
  pop_df <- f_orig_pop(cens1_df, 10, lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2009, 2013) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 2009, 2013, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2007, 2015, lvl = geo_l)
  
  malawi_df <- f_bartik_nopop(ctry3 = "MWI", lvl = geo_l) 
  
  save_ctry(malawi_df, cname, lvl = geo_l)

rm(cens1_df, distance_df, loss_df, mig_od, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl)

#*******************************************************************************
# Mali ----
# birth location duration (10 years)
#*******************************************************************************
cname <- "Mali"
cens1_df <- read_census(cname, "98")
cens2_df <- read_census(cname, "09")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_flow(cens1_df, 1988, 1997, lvl = geo_l)
  emig_o <- emig_flow(cens2_df, 1999, 2008, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2008) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 1998, 2009, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2007, lvl = geo_l)
  
  mali_df <- f_bartik(ctry3 = "MLI", lvl = geo_l) 
  
  save_ctry(mali_df, cname, lvl = geo_l)
}

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl)

#birth_od <- od_period_flow(mli98_df, lag = "birth")
# mig_od <- od_flow(mli98_df, 1993, 1997)
# emig_o <- emig_flow(mli09_df, 2004, 2008)

#*******************************************************************************
# Mauritius ----
# 5 years region (no birth district)
#*******************************************************************************
cname <- "Mauritius"
cens1_df <- read_census(cname, "00")
cens2_df <- read_census(cname, "11")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_period_flow(cens1_df, lvl = geo_l)
  emig_o <- emig_period_flow(cens2_df, 5, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2010) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 2000, 2011, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2011, lvl = geo_l)
  
  mauritius_df <- f_bartik(ctry3 = "MUS", lvl = geo_l) 
  
  save_ctry(mauritius_df, cname, lvl = geo_l)
}

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# Mozambique ----
# location 5 years ago at end of war, use birth
#*******************************************************************************
cname <- "Mozambique"
cens1_df <- read_census(cname, "97")
cens2_df <- read_census(cname, "07")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_period_flow(cens1_df, lag = "birth", lvl = geo_l)
  emig_o <- emig_period_flow(cens2_df, 5, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2006) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 1997, 2007, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2007, lvl = geo_l)
  
  mozambique_df <- f_bartik(ctry3 = "MOZ", lvl = geo_l) 
  
  save_ctry(mozambique_df, cname, lvl = geo_l)
}

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od,  emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#mig_od <- od_period_flow(moz97_df)

#*******************************************************************************
# Rwanda ----
# duration
#*******************************************************************************
cname <- "Rwanda"
cens1_df <- read_census(cname, "12")

#geo_l <- 2
for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))

  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)

  mig_od <- od_flow(cens1_df, 2002, 2011, lvl = geo_l)
  pop_df <- f_orig_pop(cens1_df, 10, lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2012, 2013) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 2012, 2013, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2011, 2015, lvl = geo_l)

  rwanda_df <- f_bartik_nopop(ctry3 = "RWA", lvl = geo_l) 

  save_ctry(rwanda_df, cname, lvl = geo_l)
}

rm(cens1_df, distance_df, loss_df, mig_od, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl)
#*******************************************************************************
# Senegal ----
# birth location and location 5 years ago
#*******************************************************************************

cname <- "Senegal"
cens1_df <- read_census(cname, "02")
cens2_df <- read_census(cname, "13")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_period_flow(cens1_df, lvl = geo_l)
  emig_o <- emig_period_flow(cens2_df, 5, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2003, 2012) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 2002, 2013, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2011, lvl = geo_l)
  
  senegal_df <- f_bartik(ctry3 = "SEN", lvl = geo_l) 
  
  save_ctry(senegal_df, cname, lvl = geo_l)
}

# birth_od <- od_period_flow(sen02_df, lag = "birth")

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# Sierra Leone ----
# birth location and location 14 years ago in wave 1, 5 years ago wave 2
#*******************************************************************************

cname <- "Sierra Leone"
cens1_df <- read_census(cname, "04")
cens2_df <- read_census(cname, "15")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_period_flow(cens1_df, lag = "birth", lvl = geo_l)
  emig_o <- emig_period_flow(cens2_df, 5, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2005, 2014) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 2004, 2015, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2015, lvl = geo_l)
  
  sierraleone_df <- f_bartik(ctry3 = "SLE", lvl = geo_l) 
  
  save_ctry(sierraleone_df, cname, lvl = geo_l)
}

#mig_od <- od_period_flow(cens1_df)

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# South Africa ----
#*******************************************************************************
cname <- "South Africa"
# cens1_df <- read_census(cname, "01-3")
# cens2_df <- read_census(cname, "11-3")
# 
# ann_treeloss <- read_fl(cname, lvl = 3)
# distance_df <- read_dist(cname, lvl = 3)
# urb_df <- read_urb(cname, lvl = 3)
# 
# mig_od <- od_flow(cens1_df, 1996, 2000, lvl = 2)
# emig_o <- emig_flow(cens2_df, 2002, 2010, lvl = 2)
# pop_df <- f_pop(cens1_df, cens2_df, lvl = 2)
# #urb_df <- f_urb(lvl = 2)
# loss_df <- geo_forestloss(ann_treeloss, lvl = 2, 2002, 2010) 
# 
# southafrica_df <- f_bartik(ctry3 = "SAF", lvl = 2) 
# 
# save_ctry(southafrica_df, cname, lvl = 3)
# 
# rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, 
#    pop_df, ann_treeloss, urb_df)

# cens0_df <- read_census(cname, "07")
# cens1_df <- read_census(cname, "11")
# cens2_df <- read_census(cname, "16")
# 
# for(geo_l in c(1,2)) {
#   print(paste0("Running: ",cname, " level ", geo_l))
#   
#   ann_treeloss <- read_fl(cname, geo_l)
#   distance_df <- read_dist(cname, geo_l)
#   urb_df <- read_urb(cname, geo_l)
#   
#   mig_od <- od_flow(cens1_df, 2001, 2010, lvl = geo_l)
#   emig_o <- emig_flow(cens2_df, 2011, 2015, lvl = geo_l)
#   # get ag weights from 07 because don't have after
#   ag_df <- f_pop(cens0_df, cens1_df, lvl = geo_l) %>% 
#     select(contains("name"), ag_wgt)
#   pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)  %>% select(-ag_wgt) %>%
#     full_join(ag_df)
#   #urb_df <- f_urb(lvl = geo_l)
#   loss_df <- geo_forestloss(ann_treeloss, geo_l, 2011, 2015) 
#   delta_nl <- f_nitelite(read_nl(cname, geo_l), 2011, 2016, lvl = geo_l)
#   delta_crop <- f_croparea(read_crop(cname, geo_l), 2011, 2015, lvl = geo_l)
#   
#   southafrica_df <- f_bartik(ctry3 = "SAF", lvl = geo_l) 
#   
#   save_ctry(southafrica_df, cname, lvl = geo_l)
# }


cens1_df <- read_census(cname, "01")
cens2_df <- read_census(cname, "11")

geo_l <- 2
print(paste0("Running: ",cname, " level ", geo_l))

ann_treeloss <- read_fl(cname, geo_l)
distance_df <- read_dist(cname, geo_l)
urb_df <- read_urb(cname, geo_l)

mig_od <- od_flow(cens1_df, 1996, 2000, lvl = geo_l)
emig_o <- emig_flow(cens2_df, 2002, 2010, lvl = geo_l)

pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l) 
#urb_df <- f_urb(lvl = geo_l)
loss_df <- geo_forestloss(ann_treeloss, geo_l, 2002, 2010) 
delta_nl <- f_nitelite(read_nl(cname, geo_l), 2001, 2011, lvl = geo_l)
delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2011, lvl = geo_l)

southafrica_df <- f_bartik(ctry3 = "SAF", lvl = geo_l) 

save_ctry(southafrica_df, cname, lvl = geo_l)



rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, geo_l,
   pop_df, ann_treeloss, cname, urb_df, ag_df, cens0_df, delta_nl, delta_crop)

#*******************************************************************************
# Sudan, S. Sudan ----
# duration
#*******************************************************************************
cname <- "Sudan"
cens1_df <- read_census(cname, "08")

geo_l <- 1
print(paste0("Running: ",cname, " level ", geo_l))
  
ann_treeloss <- read_fl(cname, geo_l)
distance_df <- read_dist(cname, geo_l)
urb_df <- read_urb(cname, geo_l)
  
mig_od <- od_flow(cens1_df, 1998, 2007, lvl = geo_l)
pop_df <- f_orig_pop(cens1_df, 10, lvl = geo_l)
loss_df <- geo_forestloss(ann_treeloss, geo_l, 2009, 2013) 
delta_nl <- f_nitelite(read_nl(cname, geo_l), 2009, 2013, lvl = geo_l)
delta_crop <- f_croparea(read_crop(cname, geo_l), 2007, 2015, lvl = geo_l)
  
sudan_df <- f_bartik_nopop(ctry3 = "SDN", lvl = geo_l) 
  
save_ctry(sudan_df, cname, lvl = geo_l)


rm(cens1_df, distance_df, loss_df, mig_od, pop_df, delta_crop,
   ann_treeloss, cname, urb_df, delta_nl, geo_l)

#*******************************************************************************
# Tanzania ----
# location 1 year ago
#*******************************************************************************
cname <- "Tanzania"
geo_l <- 1
print(paste0("Running: ",cname, " level ", geo_l))
cens1_df <- read_census(cname, "02")
cens2_df <- read_census(cname, "12")

ann_treeloss <- read_fl(cname, geo_l)
distance_df <- read_dist(cname, geo_l)
urb_df <- read_urb(cname, geo_l)

#mig_od <- od_period_flow(tza02_df, "region", "prev")
mig_od <- od_period_flow(cens1_df, lag = "birth", lvl = geo_l) 
emig_o <- emig_period_flow(cens2_df, 1, lvl = geo_l)
pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
#urb_df <- f_urb(lvl = geo_l)
loss_df <- geo_forestloss(ann_treeloss, geo_l, 2003, 2011)
delta_nl <- f_nitelite(read_nl(cname, geo_l), 2002, 2012, lvl = geo_l)
delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2011, lvl = geo_l)

tanzania_df <- f_bartik(ctry3 = "TZA", lvl = geo_l)

save_ctry(tanzania_df, cname, lvl = geo_l)

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# Togo ----
# duration
#*******************************************************************************
cname <- "Togo"
cens1_df <- read_census(cname, "10")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))

ann_treeloss <- read_fl(cname, geo_l)
distance_df <- read_dist(cname, geo_l)
urb_df <- read_urb(cname, geo_l)

mig_od <- od_flow(cens1_df, 2000, 2009, lvl = geo_l)
pop_df <- f_orig_pop(cens1_df, 10, lvl = geo_l)
loss_df <- geo_forestloss(ann_treeloss, geo_l, 2010, 2013) 
delta_nl <- f_nitelite(read_nl(cname, geo_l), 2010, 2013, lvl = geo_l)
delta_crop <- f_croparea(read_crop(cname, geo_l), 2011, 2015, lvl = geo_l)

togo_df <- f_bartik_nopop(ctry3 = "TGO", lvl = geo_l) 

save_ctry(togo_df, cname, lvl = geo_l)
}

rm(cens1_df, distance_df, loss_df, mig_od, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl)
#*******************************************************************************
# Uganda ----
# duration 10 years
#*******************************************************************************
cname <- "Uganda"
cens1_df <- read_census(cname, "02")
cens2_df <- read_census(cname, "14")

for(geo_l in c(2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_flow(cens1_df, 1992, 2001, lvl = geo_l)
  emig_o <- emig_flow(cens2_df, 2004, 2013, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2003, 2013) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 2002, 2014, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2015, lvl = geo_l)
  
  uganda_df <- f_bartik(ctry3 = "UGA", lvl = geo_l) 
  
  save_ctry(uganda_df, cname, lvl = geo_l)
}

# birth_od <- od_period_flow(uga02_df, lag = "birth")

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# Zambia ----
# birth location (duration 1 year)
#*******************************************************************************
cname <- "Zambia"
cens1_df <- read_census(cname, "00")
cens2_df <- read_census(cname, "10")

for(geo_l in c(1,2)) {
  print(paste0("Running: ",cname, " level ", geo_l))
  
  ann_treeloss <- read_fl(cname, geo_l)
  distance_df <- read_dist(cname, geo_l)
  urb_df <- read_urb(cname, geo_l)
  
  mig_od <- od_period_flow(cens1_df, lag = "birth", lvl = geo_l)
  emig_o <- emig_period_flow(cens2_df, 1, lvl = geo_l)
  pop_df <- f_pop(cens1_df, cens2_df, lvl = geo_l)
  #urb_df <- f_urb(lvl = geo_l)
  loss_df <- geo_forestloss(ann_treeloss, geo_l, 2001, 2009) 
  delta_nl <- f_nitelite(read_nl(cname, geo_l), 2000, 2010, lvl = geo_l)
  delta_crop <- f_croparea(read_crop(cname, geo_l), 2003, 2011, lvl = geo_l)
  
  zambia_df <- f_bartik(ctry3 = "ZMB", lvl = geo_l) 
  
  save_ctry(zambia_df, cname, lvl = geo_l)
}

# mig_od <- od_period_flow(zmb00_df)

rm(cens1_df, cens2_df, distance_df, loss_df, mig_od, emig_o, pop_df, 
   ann_treeloss, cname, urb_df, delta_nl, delta_crop)

#*******************************************************************************
# Zimbabwe ----
# duration
#*******************************************************************************
# cname <- "Zimbabwe"
# cens1_df <- read_census(cname, "12")
# 
# for(geo_l in c(1,2)) {
#   print(paste0("Running: ",cname, " level ", geo_l))
#   
#   ann_treeloss <- read_fl(cname, geo_l)
#   distance_df <- read_dist(cname, geo_l)
#   urb_df <- read_urb(cname, geo_l)
#   
#   mig_od <- od_period_flow(cens1_df, lvl = geo_l)
#   pop_df <- f_orig_pop(cens1_df, 10, lvl = geo_l)
#   loss_df <- geo_forestloss(ann_treeloss, geo_l, 2012, 2013) 
#   delta_nl <- f_nitelite(read_nl(cname, geo_l), 2012, 2013, lvl = geo_l)
#   delta_crop <- f_croparea(read_crop(cname, geo_l), 2011, 2015, lvl = geo_l)
#   
#   zimbabwe_df <- f_bartik_nopop(ctry3 = "ZWE", lvl = geo_l) 
#   
#   save_ctry(zimbabwe_df, cname, lvl = geo_l)
# }
# 
# rm(cens1_df, distance_df, loss_df, mig_od, pop_df, 
#    ann_treeloss, cname, urb_df, delta_nl)

#*******************************************************************************
# All Countries ----
#*******************************************************************************

#benin_df <- read_ctry("Benin")
#botswana_df <- read_ctry("Botswana")
#burkinafaso_df <- read_ctry("Burkina Faso")
#guinea_df <- read_ctry("Guinea")
#ghana_df <- read_ctry("Ghana")
#kenya_df <- read_ctry("Kenya")
#lesotho_df <- read_ctry("Lesotho")
#mali_df <- read_ctry("Mali")
#mauritius_df <- read_ctry("Mauritius")
#mozambique_df <- read_ctry("Mozambique")
#senegal_df <- read_ctry("Senegal")
#sierraleone_df <- read_ctry("Sierra Leone")
#tanzania_df <- read_ctry("Tanzania")
#uganda_df <- read_ctry("Uganda")
#zambia_df <- read_ctry("Zambia")
#southafrica_df <- read_ctry("South Africa")

ben_l1_df <- read_ctry("Benin", lvl = 1)
ben_l2_df <- read_ctry("Benin", lvl = 2)
bwa_l1_df <- read_ctry("Botswana", lvl = 1)
bfa_l1_df <- read_ctry("Burkina Faso", lvl = 1)
bfa_l2_df <- read_ctry("Burkina Faso", lvl = 2)
cmr_l1_df <- read_ctry("Cameroon", lvl = 1)
cmr_l2_df <- read_ctry("Cameroon", lvl = 2)
gin_l2_df <- read_ctry("Guinea", lvl = 2)
gha_l1_df <- read_ctry("Ghana", lvl = 1)
gha_l2_df <- read_ctry("Ghana", lvl = 2)
ken_l1_df <- read_ctry("Kenya", lvl = 1)
ken_l2_df <- read_ctry("Kenya", lvl = 2)
lso_l1_df <- read_ctry("Lesotho", lvl = 1)
mwi_l1_df <- read_ctry("Malawi", lvl = 1)
mli_l1_df <- read_ctry("Mali", lvl = 1)
mli_l2_df <- read_ctry("Mali", lvl = 2)
mus_l1_df <- read_ctry("Mauritius", lvl = 1)
mus_l2_df <- read_ctry("Mauritius", lvl = 2)
moz_l1_df <- read_ctry("Mozambique", lvl = 1)
moz_l2_df <- read_ctry("Mozambique", lvl = 2)
rwa_l1_df <- read_ctry("Rwanda", lvl = 1)
rwa_l2_df <- read_ctry("Rwanda", lvl = 2)
sen_l1_df <- read_ctry("Senegal", lvl = 1)
sen_l2_df <- read_ctry("Senegal", lvl = 2)
sle_l1_df <- read_ctry("Sierra Leone", lvl = 1) 
sle_l2_df <- read_ctry("Sierra Leone", lvl = 2) 
saf_l1_df <- read_ctry("South Africa", lvl = 1)
saf_l2_df <- read_ctry("South Africa", lvl = 2)
saf_l3_df <- read_ctry("South Africa", lvl = 3)
sdn_l1_df <- read_ctry("Sudan", lvl = 1)
tza_l1_df <- read_ctry("Tanzania", lvl = 1)
tgo_l1_df <- read_ctry("Togo", lvl = 1)
tgo_l2_df <- read_ctry("Togo", lvl = 2)
uga_l2_df <- read_ctry("Uganda", lvl = 2)
zmb_l1_df <- read_ctry("Zambia", lvl = 1)
zmb_l2_df <- read_ctry("Zambia", lvl = 2)
#zwe_l1_df <- read_ctry("Zimbabwe", lvl = 1)
#zwe_l2_df <- read_ctry("Zimbabwe", lvl = 2)

xc_df <- bind_rows(
  ben_l2_df,
  bwa_l1_df,
  bfa_l2_df,
  cmr_l2_df,
  gha_l2_df,
  gin_l2_df,
  ken_l2_df,
  lso_l1_df,
  mwi_l1_df,
  mli_l2_df,
  mus_l1_df,
  moz_l2_df,
  rwa_l2_df,
  sen_l2_df,
  sle_l2_df,
  saf_l1_df,
  sdn_l1_df,
  tza_l1_df,
  tgo_l2_df,
  uga_l2_df,
  zmb_l2_df,
  #zwe_l2_df
) %>%
  add_labs()

saveRDS( xc_df, file.path(build.dir, "Cross Country/Bartik_FL.rds"))
write_dta(xc_df, file.path(build.dir, "Cross Country/Bartik_FL.dta"))

## All levels ----
mult_df <- bind_rows(
  ben_l1_df %>% mutate(adm = 1), ben_l2_df %>% mutate(adm = 2),
  bwa_l1_df %>% mutate(adm = 1),
  bfa_l1_df %>% mutate(adm = 1), bfa_l2_df %>% mutate(adm = 2),
  gha_l1_df %>% mutate(adm = 1), gha_l2_df %>% mutate(adm = 2),
  gin_l2_df %>% mutate(adm = 2),
  ken_l1_df %>% mutate(adm = 1), ken_l2_df %>% mutate(adm = 2),
  lso_l1_df %>% mutate(adm = 1),
  mli_l1_df %>% mutate(adm = 1), mli_l2_df %>% mutate(adm = 2),
  mus_l1_df %>% mutate(adm = 1), mus_l2_df %>% mutate(adm = 2),
  moz_l1_df %>% mutate(adm = 1), moz_l2_df %>% mutate(adm = 2),
  sen_l1_df %>% mutate(adm = 1), sen_l2_df %>% mutate(adm = 2),
  sle_l1_df %>% mutate(adm = 1), sle_l2_df %>% mutate(adm = 2),
  saf_l1_df %>% mutate(adm = 1), saf_l2_df %>% mutate(adm = 2),
  saf_l3_df %>% mutate(adm = 3),
  tza_l1_df %>% mutate(adm = 1),
  uga_l2_df %>% mutate(adm = 2),
  zmb_l1_df %>% mutate(adm = 1), zmb_l2_df %>% mutate(adm = 2)) %>%
  mutate(
    year1 = case_when(
      Country == "BEN" ~ 2002,
      Country == "BWA" ~ 2001,
      Country == "BFA" ~ 1996,
      Country == "GHA" ~ 2000,
      Country == "GIN" ~ 1996,
      Country == "KEN" ~ 1999,
      Country == "LSO" ~ 1996,
      Country == "MLI" ~ 1998,
      Country == "MUS" ~ 2000,
      Country == "MOZ" ~ 1997,
      Country == "SEN" ~ 2002,
      Country == "SLE" ~ 2004,
      Country == "SAF" & adm %in% c(1,3) ~ 2001,
      Country == "SAF" & adm == 2 ~ 2011,
      Country == "TZA" ~ 2002,
      Country == "UGA" ~ 2002,
      Country == "ZMB" ~ 2000,
    ),
    year2 = case_when(
      Country == "BEN" ~ 2013,
      Country == "BWA" ~ 2011,
      Country == "BFA" ~ 2006,
      Country == "GHA" ~ 2010,
      Country == "GIN" ~ 2014,
      Country == "KEN" ~ 2009,
      Country == "LSO" ~ 2006,
      Country == "MLI" ~ 2009,
      Country == "MUS" ~ 2011,
      Country == "MOZ" ~ 2007,
      Country == "SEN" ~ 2013,
      Country == "SLE" ~ 2015,
      Country == "SAF" & adm %in% c(1,2) ~ 2016,
      Country == "SAF" & adm == 3 ~ 2011,
      Country == "TZA" ~ 2012,
      Country == "UGA" ~ 2014,
      Country == "ZMB" ~ 2010,
    ),
    period1 = case_when(
      Country == "BEN" ~ "1992 - 2001",
      Country == "BWA" ~ "5 year",
      Country == "BFA" ~ "birth",
      Country == "GHA" ~ "5 year",
      Country == "GIN" ~ "1986-1995",
      Country == "KEN" ~ "1989-1998",
      Country == "LSO" ~ "10 year",
      Country == "MLI" ~ "1988-1997",
      Country == "MUS" ~ "5 year",
      Country == "MOZ" ~ "birth",
      Country == "SEN" ~ "5 year",
      Country == "SLE" ~ "birth",
      Country == "SAF" & adm %in% c(1,2) ~ "2001 - 2010",
      Country == "SAF" & adm == 3 ~ "1996 - 2000",
      Country == "TZA" ~ "birth",
      Country == "UGA" ~ "1992 - 2001",
      Country == "ZMB" ~ "birth",
    ),
    period2 = case_when(
      Country == "BEN" ~ "2003 - 2012",
      Country == "BWA" ~ "5 year",
      Country == "BFA" ~ "1 year",
      Country == "GHA" ~ "5 year",
      Country == "GIN" ~ "2004-2013",
      Country == "KEN" ~ "1 year",
      Country == "LSO" ~ "10 year",
      Country == "MLI" ~ "1999-2008",
      Country == "MUS" ~ "5 year",
      Country == "MOZ" ~ "5 year",
      Country == "SEN" ~ "5 year",
      Country == "SLE" ~ "5 year",
      Country == "SAF" & adm %in% c(1,2) ~ "2011 - 2015",
      Country == "SAF" & adm == 3 ~ "2002 - 2010",
      Country == "TZA" ~ "1 year",
      Country == "UGA" ~ "2004 - 2013",
      Country == "ZMB" ~ "1 year",
    )
  ) %>%
  add_labs()

saveRDS( mult_df, file.path(build.dir, "Cross Country/Bartik_FL_all_lvls.rds"))
write_dta(mult_df, file.path(build.dir, "Cross Country/Bartik_FL_all_lvls.dta"))



# test_df <- mult_df %>%
#   select(region, region_name, district, district_name,
#     country_name, adm, year1, period2, e_o, starts_with("bl"), el_emig_rate)

# c1 <- cens1_df %>% select(district_name) %>% unique() %>% mutate(cens1 = 1)
# c2 <- cens2_df %>% select(district_name) %>% unique() %>% mutate(cens2 = 1)
# t <- ann_treeloss %>% select(district_name) %>% unique()  %>% mutate(tree = 1)
# d <- distance_df %>% select(district_name) %>% unique() %>% mutate(dist = 1)
# 
# m <- full_join(c, d)

#testing for botswana to make sure calculations were correct
# test_df <- cens2_df %>%
#   filter(age >= 15 & age <= 65) %>%
#   mutate(migrant = ifelse(region != prev_region, wgt, 0)) %>%
#   group_by(prev_region_name, prev_region) %>%
#   summarise(migrant = sum(migrant, na.rm = TRUE),
#             .groups = 'drop') %>%
#   rename_with(~str_remove(.x, "prev_"), starts_with("prev")) %>%
#   inner_join(cens1_df %>%
#                mutate(pa = ifelse(age >= 15 & age <= 65, wgt, 0))%>%
#                group_by(region_name, region)%>%
#                summarise(pop = sum(wgt, na.rm = TRUE),
#                          pa = sum(pa, na.rm = TRUE),
#                          .groups = 'drop')) %>%
#   mutate(migrant = migrant / 5,
#          emig_rate = 100 * migrant / pa)
# 
# comp <- botswana_df %>%
#   select(region, region_name, e_o, pop_wgt, starts_with("bl"))


# mult_df <- bind_rows(
#   ben_l1_df %>% mutate(adm = 1, year = 2002, period = "1992 - 2001"),
#   ben_l2_df %>% mutate(adm = 2, year = 2002, period = "1992 - 2001"),
#   bwa_l1_df %>% mutate(adm = 1, year = 2001, period = "5 year"),
#   bfa_l1_df %>% mutate(adm = 1, year = 1996, period = "birth"),
#   bfa_l2_df %>% mutate(adm = 2, year = 1996, period = "birth"),
#   gha_l1_df %>% mutate(adm = 1, year = 2000, period = "5 year"),
#   gha_l2_df %>% mutate(adm = 2, year = 2000, period = "5 year"),
#   gin_l2_df %>% mutate(adm = 2, year = 1996, period = "1986-1995"),
#   ken_l1_df %>% mutate(adm = 1, year = 1999, period = "1989-1998"),
#   ken_l2_df %>% mutate(adm = 2, year = 1999, period = "1989-1998"),
#   lso_l1_df %>% mutate(adm = 1, year = 1996, period = "10 year"),
#   mli_l1_df %>% mutate(adm = 1, year = 1998, period = "1988-1997"),
#   mli_l2_df %>% mutate(adm = 2, year = 1998, period = "1988-1997"),
#   mus_l1_df %>% mutate(adm = 1, year = 2000, period = "5 year"),
#   mus_l2_df %>% mutate(adm = 2, year = 2000, period = "5 year"),
#   moz_l1_df %>% mutate(adm = 1, year = 1997, period = "birth"),
#   moz_l2_df %>% mutate(adm = 2, year = 1997, period = "birth"),
#   sen_l1_df %>% mutate(adm = 1, year = 2002, period = "5 year"),
#   sen_l2_df %>% mutate(adm = 2, year = 2002, period = "5 year"),
#   sle_l1_df %>% mutate(adm = 1, year = 2004, period = "birth"),
#   sle_l2_df %>% mutate(adm = 2, year = 2004, period = "birth"),
#   saf_l1_df %>% mutate(adm = 1, year = 2011, period = "2001 - 2010"),
#   saf_l2_df %>% mutate(adm = 2, year = 2011, period = "2001 - 2010"),
#   saf_l3_df %>% mutate(adm = 3, year = 2001, period = "1996 - 2000"),
#   tza_l1_df %>% mutate(adm = 1, year = 2002, period = "1 year"),
#   uga_l2_df %>% mutate(adm = 2, year = 2002, period = "1992 - 2001"),
#   zmb_l1_df %>% mutate(adm = 1, year = 2000, period = "birth"),
#   zmb_l2_df %>% mutate(adm = 2, year = 2000, period = "birth"),
# )
