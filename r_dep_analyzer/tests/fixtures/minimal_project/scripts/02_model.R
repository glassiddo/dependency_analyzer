clean <- read.csv("data/derived/clean.csv")
ghost <- read.csv(file.path(dynamic_dir, "missing.csv"))
saveRDS(clean, "data/derived/model.rds")
