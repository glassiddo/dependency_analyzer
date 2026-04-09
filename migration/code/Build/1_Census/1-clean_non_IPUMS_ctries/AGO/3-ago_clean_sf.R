#* Project: Migration Africa
#* Author:  Iddo Glass
#* Date:    December, 2025
#* Title:   Clean shapefile for Angola
#* Desc:    Take the IPUMS shapefile for AGO and clean it slightly + label
#*******************************************************************************

ctry_df <- read_dta(here(raw.dir, "Countries", "AGO", "Census", "consistent14.dta"))

labels_geo1 <- tibble(
  geo1_ao = as.character(attr(ctry_df$geo1_ao, "labels")),
  admin_name = names(attr(ctry_df$geo1_ao, "labels"))
)

labels_geo2 <- tibble(
  geo2_ao = as.character(attr(ctry_df$geo2_ao, "labels")),
  admin_name = names(attr(ctry_df$geo2_ao, "labels"))
) 

ago_sf1 <- read_sf(
  here(raw.dir, "Countries", "AGO", "Shapefiles", "level 1", "AGO_adm1_before_cleaning.shp")) %>% 
  mutate(
    ADM1_EN = case_when(
      ADM1_EN == "Bié" ~ "Bie",
      ADM1_EN == "Huíla" ~ "Huila",
      TRUE ~ ADM1_EN
    )
  ) %>%
  left_join(labels_geo1, by = c("ADM1_EN" = "admin_name")) %>%
  mutate(
    CNTRY_NAME = "Angola",
    CNTRY_CODE = "141",
    PARENT = str_sub(geo1_ao, 1, 3) # get the country code (141)
  ) %>% 
  transmute(
    CNTRY_NAME,
    ADMIN_NAME = ADM1_EN,
    CNTRY_CODE,
    GEOLEVEL1 = as.character(geo1_ao),
    PARENT,
    geometry
  )

ago_sf2 <- read_sf(
  here(raw.dir, "Countries", "AGO", "Shapefiles", "level 2", "AGO_adm2_before_cleaning.shp")) %>%
  mutate(
    ADM2_EN = case_when(
      ADM2_EN == "Amboim (Gabela)" ~ "Amboim (ex. Gabela)",
      ADM2_EN == "Ambuila" ~ "Ambuíla",
      ADM2_EN == "Belas (Samba)" ~ "Belas",
      ADM2_EN == "Buengas" ~ "Buengas (ex Nova Esperança)",
      ADM2_EN == "Bula Atumba" ~ "Bula-Atumba",
      ADM2_EN == "Bundas (Lumbala-Nguimbo)" ~ "Budas-Lumbala-Nguimbo",
      ADM2_EN == "Cacuzo" ~ "Cacuso",
      ADM2_EN == "Caiambambo" ~ "Caimbambo",
      ADM2_EN == "Camacuio" ~ "Camucuio",
      ADM2_EN == "Caombo" ~ "Cahombo",
      ADM2_EN == "Capenda" ~ "Capenda-Camulemba",
      ADM2_EN == "Catabola" ~ "Catabola (ex. Nova Sintra)",
      ADM2_EN == "Cuaba Nzogo" ~ "Quiwaba-N'Zogi",
      ADM2_EN == "Cunda-dia-Baza" ~ "Cunda-Dia-Baze",
      ADM2_EN == "Cunhinga" ~ "Cunhinga (Vouga)",
      ADM2_EN == "Curoca" ~ "Curoca (ex.Oncocua)",
      ADM2_EN == "Dande" ~ "Dande (Caxito)",
      ADM2_EN == "Dembos" ~ "Dembos-Quibaxe",
      ADM2_EN == "Ekunha" ~ "Ecunha",
      ADM2_EN == "Gambos" ~ "Gambos ( ex-Chiange)",
      ADM2_EN == "Icolo e Bengo" ~ "Icolo Bengo",
      ADM2_EN == "Kuito" ~ "Cuito",
      ADM2_EN == "Kuvango" ~ "Cuvango",
      ADM2_EN == "Landana" ~ "Cacongo(ex_Lândana)",
      ADM2_EN == "Libolo" ~ "Libolo (ex. Calulo)",
      ADM2_EN == "Lucano" ~ "Luacano",
      ADM2_EN == "Lumbala-Nguimbo" ~ "Budas-Lumbala-Nguimbo",
      ADM2_EN == "M'Banza Congo" ~ "Mbanza Congo",
      ADM2_EN == "Milunga" ~ "Mulinga(Ex.Santa Cruz)",
      ADM2_EN == "Mucaba" ~ "Mucaba (ex. Quinzala)",
      ADM2_EN == "N'Zeto" ~ "Nzetu",
      ADM2_EN == "Namakunde" ~ "Namacunde",
      ADM2_EN == "Nharea" ~ "N'harea",
      ADM2_EN == "Noqui" ~ "Nóqui",
      ADM2_EN == "Ombadja" ~ "Ombadja (ex. Cuamato)",
      ADM2_EN == "Pango Aluquém" ~ "Pango-Aluquem",
      ADM2_EN == "Seles" ~ "Seles (ex. Uku Seles)",
      ADM2_EN == "Sumbe" ~ "Sumbe (ex. Ngunza)",
      ADM2_EN == "Tchicala-Tcholoanga" ~ "Tchikala-Tcholohanga",
      ADM2_EN == "Tchindjenje" ~ "Tchinjenje",
      ADM2_EN == "Tchipungo" ~ "Quipungo",
      ADM2_EN == "Tombwa" ~ "Tômbwa (ex. Porto Alexandre)",
      ADM2_EN == "Waku Kungo" ~ "Cela ( ex Waku-Kungo)",
      ADM2_EN == "Xá Muteba" ~ "Xá-Muteba",
      ADM2_EN == "Cacongo (Landana)" ~ "Cacongo(ex_Lândana)",
      ADM2_EN == "Cambambe (Kambambe)" ~ "Cambambe",
      ADM2_EN == "Cameia (Lumeje)" ~ "Cameia",
      ADM2_EN == "Cangola (Alto Cauale)" ~ "Cangola",
      ADM2_EN == "Cela (Waku Kungo)" ~ "Cela ( ex Waku-Kungo)",
      ADM2_EN == "Chitato (Lóvua)" ~ "Chitato",
      ADM2_EN == "Cuito (Kuito)" ~ "Cuito",
      ADM2_EN == "Cunda-dia-Baze" ~ "Cunda-Dia-Baze",
      ADM2_EN == "Cuvango (Kuvango)" ~ "Cuvango",
      ADM2_EN == "Ecunha (Ekunha)" ~ "Ecunha",
      ADM2_EN == "Gambos (ex-Chiange)" ~ "Gambos ( ex-Chiange)",
      ADM2_EN == "Kiuaba-N'Zoji (Cuaba Nzogo)" ~ "Quiwaba-N'Zogi",
      ADM2_EN == "Libolo (Calulo)" ~ "Libolo (ex. Calulo)",
      ADM2_EN == "Pango-Aluquém" ~ "Pango-Aluquem",
      ADM2_EN == "Quipungo (Tchipungo)" ~ "Quipungo",
      ADM2_EN == "Quissama (Muxima, Quiçama)" ~ "Quissama",
      ADM2_EN == "Seles (Uku Seles)" ~ "Seles (ex. Uku Seles)",
      ADM2_EN == "Sumbe (Ngangula)" ~ "Sumbe (ex. Ngunza)",
      ADM2_EN == "Tômbwa (Porto Alexandre)" ~ "Tômbwa (ex. Porto Alexandre)",
      TRUE ~ ADM2_EN
    )
  ) %>% 
  left_join(labels_geo2, by = c("ADM2_EN" = "admin_name")) %>% 
  mutate(
    CNTRY_NAME = "Angola",
    CNTRY_CODE = "141",
    PARENT = str_sub(geo2_ao, 4, 6) # get the level 1 parent
  ) %>% 
  transmute(
    CNTRY_NAME,
    ADMIN_NAME = ADM2_EN,
    CNTRY_CODE,
    GEOLEVEL2 = as.character(geo2_ao),
    PARENT,
    geometry
  )

write_sf(ago_sf1, here(raw.dir, "Countries", "AGO", "Shapefiles", "level 1", "AGO_adm1.shp"))
write_sf(ago_sf2, here(raw.dir, "Countries", "AGO", "Shapefiles", "level 2", "AGO_adm2.shp"))
