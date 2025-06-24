library(tidyverse)
library(dplyr)

#Part 1

#Q1.1
mean(city_budget$total_budget)
median(city_budget$total_budget)

#checken of t numeric is
is.numeric(city_budget$total_budget)

#Q1.2
city_budget <- city_budget %>%
  mutate(education_share = education_budget / total_budget * 100)

city_budget %>%
  group_by(region) %>%
  summarise(mean_education = mean(education_share))

city_budget %>%
  group_by(region) %>%
  summarise(median_education = median(education_share))

is.numeric(city_budget$education_budget)

#Q1.3
city_budget <- city_budget %>%
  mutate(budget_per_capita = total_budget / population) %>%
  arrange(budget_per_capita)

#Q1.4
ggplot(city_budget, aes(x = total_budget, y= service_quality_index)) +
  geom_point() +
  scale_y_continuous(breaks = (3:10)*10, limits = c(30, 100)) +
  scale_x_continuous(breaks = (1:8)*100000 + 50000, limits = c(150000,850000)) +
  geom_smooth(method = "lm")

#Part 2

#Q2.1
public_transport_survey %>%
  count(num_trips_week < 0)

3/147*100

public_transport_survey %>%
  count(satisfaction > 5)

4/150*100

public_transport_survey %>%
  count(punctuality_rating > 5 | punctuality_rating < 1)

#2-4
public_transport_survey[public_transport_survey$num_trips_week < 0, "num_trips_week"] = NA
public_transport_survey[public_transport_survey$satisfaction < 1 | public_transport_survey$satisfaction > 5, "satisfaction"] = NA
public_transport_survey[public_transport_survey$punctuality_rating < 1 | public_transport_survey$punctuality_rating > 5, "punctuality_rating"] = NA

#2.2
public_transport_survey <- public_transport_survey %>%
  mutate(public_transport_survey,
         all_weekday_commuter = ifelse(num_trips_week >= 10, 1, 0),
         high_satisfaction = ifelse(satisfaction >= 4, 1, 0))

100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Bus", "high_satisfaction"], na.rm = TRUE) #46.9%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Metro", "high_satisfaction"], na.rm = TRUE) #88.5%          
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Train", "high_satisfaction"], na.rm = TRUE) #100%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Tram", "high_satisfaction"], na.rm = TRUE) #100%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Bus", "high_satisfaction"], na.rm = TRUE) #59.3%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Metro", "high_satisfaction"], na.rm = TRUE) #80%          
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Train", "high_satisfaction"], na.rm = TRUE) #74.2%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Tram", "high_satisfaction"], na.rm = TRUE) #81.3%

#Q2.3 na.rm = TRUE --> niet meenemen NAs
public_transport_survey %>%
  group_by(transport_mode) %>%
  summarise(mean_punctuality = mean(punctuality_rating, na.rm = TRUE))

#antwoord key
mean(public_transport_survey[public_transport_survey$transport_mode == "Bus", "punctuality_rating"], na.rm = TRUE) #2.9
mean(public_transport_survey[public_transport_survey$transport_mode == "Metro", "punctuality_rating"], na.rm = TRUE) #4
mean(public_transport_survey[public_transport_survey$transport_mode == "Train", "punctuality_rating"], na.rm = TRUE) #3.3
mean(public_transport_survey[public_transport_survey$transport_mode == "Tram", "punctuality_rating"], na.rm = TRUE) #3.7

public_transport_survey %>%
  group_by(transport_mode) %>%
  summarise(median_punctuality = median(punctuality_rating, na.rm = TRUE))

#Q2.4
ggplot(public_transport_survey, aes(x = as.factor(transport_mode), y = num_trips_week)) +
  stat_summary(fun.y = mean, geom = "col") +
  scale_y_continuous(breaks = c(0:6)*2, lim = c(0, 12))

#part 3

#Q3.1
hospital_data %>%
  group_by(region) %>%
  summarise(mean_avg_wait_time = mean(avg_wait_time))

#Q3.2
hospital_data <- mutate(hospital_data, high_satisfaction = ifelse(patient_satisfaction >= 4, 1, 0))

hospital_data %>%
  group_by(high_satisfaction) %>%
  summarise(mean_wait_time2 = mean(avg_wait_time))

hospital_data %>%
  group_by(high_satisfaction) %>%
  summarise(mean_mortality_rate = mean(mortality_rate))

#Q3.3
#merging datasets
merged_hospitals <- 
  merge(hospital_data, readmission_rates, by = c("hospital_id"))

#Sort the data by mortality rate
merged_hospitals = merged_hospitals[order(merged_hospitals$mortality_rate), ]
        
#Create mortality rate terciles
merged_hospitals[1:50, "Tercile"] = "Lowest"
merged_hospitals[51:100, "Tercile"] = "Middle"
merged_hospitals[101:150, "Tercile"] = "Highest"

#Calculating means
merged_hospitals %>%
  group_by(Tercile) %>%
  summarise(mean_total_readmission_rate = mean(thirty_day_readmit_rate))

merged_hospitals %>%
  group_by(Tercile) %>%
  summarise(mean_heart_failure_readmission_rate = mean(heart_failure_readmit))

merged_hospitals %>%
  group_by(Tercile) %>%
  summarise(mean_pneumoni = mean(pneumonia_readmit))

#Q3.4
merged_hospitals <- mutate(merged_hospitals, staff_to_bed = staff_count / bed_count)

ggplot(merged_hospitals, aes(x = Tercile, y = staff_to_bed)) +
  geom_boxplot() +
  scale_y_continuous(breaks = (7:13)*0.1 - 0.05, limits = c(0.65,1.25)) + 
  labs(x = "Terciles of Mortality Rate")

#Part 4

#Q4.1
housing_survey %>% count(monthly_income, is.na = TRUE) %>%
  print(n=200)
30/200 * 100

housing_survey %>% count(satisfaction_score, is.na = TRUE)
55/200 * 100

housing_survey %>% count(utility_costs, is.na = TRUE) %>%
  print(n = 200)
20/200 * 100

#answer key

100*length(which(is.na(housing_survey$monthly_income)))/nrow(housing_survey) #15%
100*length(which(is.na(housing_survey$satisfaction_score)))/nrow(housing_survey) #27.5%
100*length(which(is.na(housing_survey$utility_costs)))/nrow(housing_survey) #10%

length(which(is.na(housing_survey$monthly_income)))

#Q4.2
ggplot(housing_survey, aes(x = household_size)) +
  geom_histogram(bins = 8) +
  scale_x_continuous(breaks = (1:8), limits = c(1, 8)) +
  scale_y_continuous(breaks = (0:7)*10, limits = c(0, 70))

#Q4.3
housing_survey <- mutate(housing_survey, rent_burden = monthly_rent / monthly_income * 100)

housing_survey %>%
  group_by(household_size) %>%
  summarise(mean_rent_burden = mean(rent_burden, na.rm = TRUE))
  
#Q4.4

housing_survey %>%
  group_by(region_type) %>%
  summarise(mean_income = round(mean(monthly_income, na.rm = TRUE), 1))

housing_survey %>%
  group_by(region_type) %>%
  summarise(mean_satisfaction = mean(satisfaction_score, na.rm = TRUE))

housing_survey %>%
  group_by(region_type) %>%
  summarise(mean_utility = round(mean(utility_costs, na.rm = TRUE), 1))
