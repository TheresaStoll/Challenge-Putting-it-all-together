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
  mutate(min = as.numeric(min)) %>% #to be able to calculate the temperature difference without an error, you will need to convert them to numeric values with as.numeric() first
  mutate(max = as.numeric(max)) %>%
  mutate(Rainfall = as.numeric(Rainfall)) %>%
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  mutate(Temperature_difference = max-min) #need a mutate() to calculate the temperature difference

#Result 1
BOM_data_Temperature_difference_average <- BOM_data_separated_tidy %>%
  filter(Temperature_difference != "NA") %>% #for rows that are missing a temperature measurement, the temperature difference will be NA - need to get rid of these NA for the analysis by filterin
  group_by(Month) %>% #group by month
  summarise(Temperature_difference_average = mean(Temperature_difference)) %>% #calculate average daily temperature difference for each month
  filter(Temperature_difference_average == min(Temperature_difference_average)) #filter for lowest average temperature difference

BOM_data_Temperature_difference_average

#Result 2
BOM_data_Temperature_difference_average_2 <- BOM_data_separated_tidy %>%
  filter(Temperature_difference != "NA") %>%
  group_by(Month) %>%
  summarise(Temperature_difference_average = mean(Temperature_difference)) %>%
  arrange(Temperature_difference_average)

BOM_data_Temperature_difference_average_2

#Question 3
#Which state saw the lowest average daily temperature difference?

read_csv("data/BOM_stations.csv")

BOM_stations <- read_csv("data/BOM_stations.csv")

#to tidy it before merging, you will need to gather() station data into intermediate form
#that has 3 columns (1 for station ID number, 2 for type of data being recorded, 3 for actual recorded value)

#gather function explained in words:
#first point within bracket: where we got the data from
#2nd point within bracket: this is the name of the new column - the function automatically takes the data for this column from the first row (doesn't matter if it is a header row or not Excel)
#3rd point within bracket: this is the name of the third new column - the function automatically takes the data from the dataset and corresponds its to the related data from the second column
#4th point within bracket: want to do all the above mentioned without changing the Info column - function will automatically add what you don't want to gather into the first column

BOM_stations_tidy <- gather(BOM_stations, Station_number, value, -info)

BOM_stations_tidy

#new dataframe can then be spread() into a shape with onw row for each station
#1st argument (key argument) within bracket (info) identifies the column that will provide the data for the new column names
#2nd argument (value argument) within bracket (value) identifies the column that will provide the data for the new cells
BOM_stations_tidy_2 <- spread(BOM_stations_tidy, info, value) 

BOM_stations_tidy_2 

#need to make sure that the 2 dataframes have a shared column to merge (Station_number - need to have the same names in both datasets)
#need to make sure that data in Station_number columns are the same data type (both e.g. are numeric/dbl columns)
BOM_stations_tidy_2_mutated <- BOM_stations_tidy_2 %>%
  mutate(Station_number = as.numeric(Station_number))

BOM_stations_tidy_2_mutated

#join the 2 datasets together

#clean BOM_data dataset first (as done before)
BOM_data_clean <- BOM_data_separated %>%
  mutate(min = as.numeric(min)) %>% 
  mutate(max = as.numeric(max)) %>%
  mutate(Rainfall = as.numeric(Rainfall)) %>%
  mutate(Solar_exposure = as.numeric(Solar_exposure)) %>%
  mutate(Temperature_difference = max-min) %>%
  filter(Temperature_difference != "NA")

BOM_joined <- full_join(BOM_data_clean, BOM_stations_tidy_2_mutated) 
BOM_joined

#Result 1
Question3_Result1 <- BOM_joined %>%
  group_by(state) %>%
  summarise(Temperature_difference_average = mean(Temperature_difference)) %>%
  filter(Temperature_difference_average == min(Temperature_difference_average))

Question3_Result1

#Result 2 
Question3_Result2 <- BOM_joined %>%
  group_by(state) %>%
  summarise(Temperature_difference_average = mean(Temperature_difference)) %>%
  arrange(Temperature_difference_average)

Question3_Result2
