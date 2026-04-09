source("code/SSA_env_SetUp.R")

wdi_data <- WDI(
  indicator = c(
    "AG.LND.FRST.ZS", "AG.LND.AGRI.ZS", "SP.URB.TOTL.IN.ZS", "DT.ODA.ALLD.KD",
    "SP.POP.TOTL", "NY.GDP.MKTP.CD", "NY.GDP.PCAP.CD", "NY.GDP.MKTP.KD", 
    "NY.GDP.PCAP.KD", "NY.GDP.MKTP.PP.KD", "NV.AGR.TOTL.CD", "NV.IND.TOTL.CD", 
    "NV.SRV.TOTL.CD", "NV.AGR.TOTL.KD", "NV.IND.TOTL.KD", "NV.SRV.TOTL.KD", 
    "SL.EMP.TOTL.SP.ZS", "SL.AGR.EMPL.ZS", "SL.IND.EMPL.ZS", "SL.SRV.EMPL.ZS", 
    "AG.LND.TOTL.K2"
    ),
  start = 1960,
  end = 2024,
  extra = TRUE
)

wdi_data <- wdi_data %>%
  rename(
    ag_lnd_totl_k2 = AG.LND.TOTL.K2,
    country_name = country,
    aid = DT.ODA.ALLD.KD,
    EmpltoPopRatio = SL.EMP.TOTL.SP.ZS,
    EmplShareServ = SL.SRV.EMPL.ZS,
    EmplShareInd = SL.IND.EMPL.ZS,
    EmplShareAg = SL.AGR.EMPL.ZS,
    gdp_serv = NV.SRV.TOTL.KD,
    gdp_ind = NV.IND.TOTL.KD,
    gdp_ag = NV.AGR.TOTL.KD,
    gdp_serv_cd = NV.SRV.TOTL.CD,
    gdp_ind_cd = NV.IND.TOTL.CD,
    gdp_ag_cd = NV.AGR.TOTL.CD,
    pop = SP.POP.TOTL,
    gdp_cd = NY.GDP.MKTP.CD,
    gdp = NY.GDP.MKTP.KD,
    gdp_pc = NY.GDP.PCAP.KD,
    gdp_ppp = NY.GDP.MKTP.PP.KD,
    gdp_pc_cd = NY.GDP.PCAP.CD,
    forest = AG.LND.FRST.ZS,
    agr = AG.LND.AGRI.ZS,
    urb_pop = SP.URB.TOTL.IN.ZS,
  ) %>%
  rename(
    country = iso3c
  ) %>% 
  zap_labels() %>%
  mutate(
    gdp_serv = labelled(gdp_serv, label = "VA in Services (Constant 2015 USD)"),
    gdp_ind = labelled(gdp_ind, label = "VA in Industry (Constant 2015 USD)"),
    gdp_ag = labelled(gdp_ag, label = "VA in Agriculture (Constant 2015 USD)"),
    gdp_serv_cd = labelled(gdp_serv_cd, label = "VA in Services (Current USD)"),
    gdp_ind_cd = labelled(gdp_ind_cd, label = "VA in Industry (Current USD)"),
    gdp_ag_cd = labelled(gdp_ag_cd, label = "VA in Agriculture (Current USD)"),
    gdp_cd = labelled(gdp_cd, label = "GDP (Current USD)"),
    gdp = labelled(gdp, label = "GDP  (Constant 2015 USD)"),
    gdp_pc = labelled(gdp_pc, label = "GDP Per Capita (Constant 2015 USD)"),
    gdp_ppp = labelled(gdp_ppp, label = "GDP  (Constant 2021 USD PPP)"),
    gdp_pc_cd = labelled(gdp_pc_cd, label = "GDP Per Capita (Current USD)"),
    forest = labelled(forest, label = "Forest area (% of land area)"),
    agr = labelled(agr, label = "Agricultural area (% of land area)"),
    urb_pop = labelled(urb_pop, label = "Urban population (% of total population)"),
    pop = labelled(pop, label = "Population")
  )

write_dta(wdi_data, here(build.dir, "World/WDI/WDI_raw.dta"))
