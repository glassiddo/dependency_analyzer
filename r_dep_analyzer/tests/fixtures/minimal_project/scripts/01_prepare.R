source("scripts/helper.R")

raw <- read.csv(file.path(raw_dir, "input.csv"))
write.csv(raw, "data/derived/clean.csv", row.names = FALSE)
