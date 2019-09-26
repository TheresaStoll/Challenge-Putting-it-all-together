library(tidyverse)

read_csv("data/BOM_data.csv")


#Challenge: Putting it all together
# Question 1: For each station, how many days have a minimum temperature, a maximum
# temperature and a rainfall measurement recorded?

BOM_data <- read_csv("data/BOM_data.csv")

BOM_data_separated <- separate(BOM_data, col = Temp_min_max, into = c("min", "max"), sep = "/")
BOM_data_separated

BOM_data_separated %>%
  filter(min != "-", max != "-", Rainfall != "-") %>% #removes the rows with "-" in the different columns
  group_by(Station_number) %>%  #groups data by station number
  summarise(Day = n()) %>%  #counts the number of days for each station
  write_csv("Results/Question_1_results.csv")


#or do separate filtering ??


#Question 2
#Which month saw the lowest average daily temperature difference?

BOM_data_separated_tidy <- BOM_data_separated %>%
  mutate(min = as.numeric(min)) %>%
  mutate(max = as.numeric(max)) %>%
  mutate(Rainfall = as.numeric(Rainfall)) %>%
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  mutate(Temperature_difference = max-min) 

BOM_data_separated_tidy_2 <- BOM_data_separated_tidy %>%
  filter(Temperature_difference != "NA") %>%
  arrange(Temperature_difference) %>%
  max(Temperature_difference)
  
BOM_data_separated_tidy_2
  

  




