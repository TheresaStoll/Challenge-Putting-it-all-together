library(tidyverse)

read_csv("data/BOM_data.csv")

BOM_data <- read_csv("data/BOM_data.csv")

BOM_data_separated <- separate(BOM_data, col = Temp_min_max, into = c("min", "max"), sep = "/")

filter(BOM_data_separated, )
