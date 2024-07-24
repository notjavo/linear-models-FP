#data cleaning for the final project data flights.csv
setwd("/Users/notjavo/Documents/MSDS/STAT6021/linear_models_fp")
library(tidyverse)

#loading in the dataset
flights_data <- read_csv("flights.csv")
#colnames(flights_data)

#Deleting all canceled flights from the dataset
filter(flights_data,CANCELLED == 0)
#Creating the total delay column
flights_data<-flights_data %>%
  mutate(Total_Delay = ELAPSED_TIME - SCHEDULED_TIME)

flights_data<-flights_data%>%
  select(Total_Delay, SCHEDULED_DEPARTURE, SCHEDULED_ARRIVAL, DISTANCE, AIRLINE, MONTH, DAY_OF_WEEK)

#dropping all na values
flights_data <- flights_data %>%
  drop_na(Total_Delay)
flights_data

#checking to see if there are any NA's in the whole set
na_check <- flights_data %>%
  summarise_all(~ any(is.na(.)))
print(na_check)

flights_data <- flights_data %>%
  mutate(
    MONTH = as.numeric(MONTH),
    DAY_OF_WEEK = as.numeric(DAY_OF_WEEK)
  )

#mapping new names to the months and days of the week 
flights_data <- flights_data %>%
  mutate(MONTH = case_when(
    MONTH == 1 ~ "January",
    MONTH == 2 ~ "February",
    MONTH == 3 ~ "March",
    MONTH == 4 ~ "April",
    MONTH == 5 ~ "May",
    MONTH == 6 ~ "June",
    MONTH == 7 ~ "July",
    MONTH == 8 ~ "August",
    MONTH == 9 ~ "September",
    MONTH == 10 ~ "October",
    MONTH == 11 ~ "November",
    MONTH == 12 ~ "December"
  ))


flights_data <- flights_data %>%
  mutate(DAY_OF_WEEK = case_when(
    DAY_OF_WEEK == 1 ~ "Monday",
    DAY_OF_WEEK == 2 ~ "Tuesday",
    DAY_OF_WEEK == 3 ~ "Wednesday",
    DAY_OF_WEEK == 4 ~ "Thursday",
    DAY_OF_WEEK == 5 ~ "Friday",
    DAY_OF_WEEK == 6 ~ "Saturday",
    DAY_OF_WEEK == 7 ~ "Sunday"
  ))
head(flights_data)