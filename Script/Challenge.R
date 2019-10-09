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


write_csv(BOM_joined, "Results/BOM_table_visualisation_practice.csv")

BOM_joined


#!!!
#Introduction to Visualisation: Preparing plots for display - p. 7
#Challenge - visualisation practice

#Question 1
#For the Perth station (ID 9225), produce three scatter plots showing the 
#relationship between the maximum temperature and each other measurement recorded 
#(minimum temperature, rainfall and solar exposure).
#Make sure your measurement values are all converted to numeric types with 
#as.numeric(). - in my BOM_joined tibble (that I used), the values were already values (=dbl)  
#To work with just the Perth measurements, make sure to filter() your dataset.
BOM_Perth <- BOM_joined %>%
  filter(Station_number == 9225)

BOM_Perth

#The only aesthetics you will need to map for this question are x and y. 
#Then, a geom_point() layer will produce a scatterplot of the two mapped values.

#scatter plot 1
ggplot(data = BOM_Perth,
       mapping = aes(x = max,
                     y = min)) +
  geom_point()

#scatter plot 2
ggplot(data = BOM_Perth,
       mapping = aes(x = max,
                     y = Rainfall)) +
  geom_point()

#scatter plot 3
ggplot(data = BOM_Perth,
       mapping = aes(x = max,
                     y = Solar_exposure)) +
  geom_point()


#Question 2
#Display these four measurements for the Perth station in a single scatter plot by 
#using additional aesthetic mappings.
#The aesthetics that geom_point() can use are found in the help page (?geom_point) 
#under the Aesthetics heading
#One possible solution is to map the temperature data to the x/y aesthetics, 
#the rainfall to the size aesthetic, and the solar exposure to the colour aesthetic.

#Version 1:
ggplot(data = BOM_Perth,
       mapping = aes(x = max,
                     y = min,
                     size = Rainfall,
                     colour = Solar_exposure)) +
  geom_point()

?geom_point

#Version 2
ggplot(data = BOM_Perth,
       mapping = aes(x = max,
                     y = min,
                     size = Rainfall,
                     alpha = Solar_exposure)) +
  geom_point()


#Question 3
#Take the four plots you have produced in Q1 and Q2 and save them as a multi-panel figure.

install.packages("cowplot")

library(cowplot)

#Saving each individual plot into a variable
plot1 <- ggplot(data = BOM_Perth,
                mapping = aes(x = max,
                              y = min)) +
  geom_point()

plot2 <- ggplot(data = BOM_Perth,
                mapping = aes(x = max,
                              y = Rainfall)) +
  geom_point()

plot3 <- ggplot(data = BOM_Perth,
               mapping = aes(x = max,
                             y = Solar_exposure)) +
  geom_point()

plot4 <- ggplot(data = BOM_Perth,
                mapping = aes(x = max,
                              y = min,
                              size = Rainfall,
                              colour = Solar_exposure)) +
  geom_point()  
  
#providing these variables  as arguments to plot_grid()

Question_3_plot <- plot_grid(plot1, plot2, plot3, plot4)

#playing around / what was explained further above in section
plot_grid(plot1, plot2, plot3, plot4, rel_heights = c(1,4))

plot_grid(plot1, plot2, plot3, plot4, labels = "AUTO")

plot_grid(plot1, plot2, plot3, plot4, labels = "auto")

#The ggsave() function has a width and height argument to control how large a figure 
#to create. This may be useful if your initial attempt at saving a figure looks too 
#squashed.

ggsave("Figures/Question_3_plot.jpg", plot = Question_3_plot, width = 12, height = 10, units = "cm")


#Question 4
#Using the entire BOM dataset, calculate the average monthly rainfall for each station. 
#Produce a lineplot to visualise this data and the state each station is in.

#data frame I will use from Challenge above (from 2 weeks ago)
#BOM_joined <- full_join(BOM_data_clean, BOM_stations_tidy_2_mutated) 
BOM_joined

#For the data manipulation, you will need to use group_by() with multiple variables, 
#and then summarise() to get the averaged rainfall measurements. 
#Then join in a data frame containing the metadata about each station to be able to 
#know which state each station is in.

Question_4_tibble <- BOM_joined %>%
  group_by(Station_number, Month, state) %>%
  drop_na() %>%
  summarise(average_rainfall = mean(Rainfall)) %>%
  ungroup() %>%
  mutate(Station_number = factor(Station_number)) %>%
  mutate(Month = factor(Month))

Question_4_tibble

#If your lines in the plot are looking a bit odd, you may need to use the group 
#aesthetic to make sure there is one line per station
#There are many ways you could use to integrate the state information – the colour 
#and linetype aesthetics, or with by using faceting. 

ggplot(data = Question_4_tibble,
       mapping = aes(x = Month,
                     y = average_rainfall,
                     colour = Station_number,
                     group = Station_number)) +
  facet_wrap(~state) +
  geom_line()




