#Q1.1
mean(city_budget$total_budget)
median(city_budget$total_budget)

#Q1.2
city_budget <- mutate(city_budget, education_share = education_budget/total_budget * 100)

city_budget %>%
  group_by(region) %>%
  summarise(mean_education = mean(education_share))

city_budget %>%
  group_by(region) %>%
  summarise(median_education = median(education_share))

#Q1.3
city_budget <- mutate(city_budget, budget_per_capita = total_budget / population)
arrange(city_budget, desc(budget_per_capita))

#Q1.4
ggplot(city_budget, aes(x = total_budget, y = service_quality_index)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE) +
  scale_y_continuous(breaks = (3:10)*10, limits = c(30, 100)) +
  scale_x_continuous(breaks = (1:8)*100000 + 50000, limits = c(150000, 850000))

#Q2.1
sum(public_transport_survey$num_trips_week < 0)/150*100
sum(public_transport_survey$satisfaction < 1 |public_transport_survey$satisfaction > 5)/150*100
sum(public_transport_survey$punctuality_rating < 1 | public_transport_survey$punctuality_rating > 5)/150*100

which(public_transport_survey$num_trips_week < 0)
length(which(public_transport_survey$num_trips_week < 0))

#Q2.2-2.4
public_transport_survey[public_transport_survey$num_trips_week < 0, "num_trips_week"] = NA
public_transport_survey[public_transport_survey$satisfaction < 1 | public_transport_survey$satisfaction > 5, "satisfaction"] = NA
public_transport_survey[public_transport_survey$punctuality_rating < 1 | public_transport_survey$punctuality_rating > 5, "punctuality_rating"] = NA

#Q2.2
public_transport_survey <- mutate(public_transport_survey, 
                                  all_weekday_commuter = ifelse(num_trips_week >10, 1, 0), 
                                  high_satisfaction = ifelse(satisfaction >= 4, "high-satisfaction", "low-satisfaction"))

100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Bus", "high_satisfaction"], na.rm = TRUE) #46.9%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Metro", "high_satisfaction"], na.rm = TRUE) #88.5%          
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Train", "high_satisfaction"], na.rm = TRUE) #100%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 1 & public_transport_survey$transport_mode == "Tram", "high_satisfaction"], na.rm = TRUE) #100%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Bus", "high_satisfaction"], na.rm = TRUE) #59.3%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Metro", "high_satisfaction"], na.rm = TRUE) #80%          
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Train", "high_satisfaction"], na.rm = TRUE) #74.2%
100*mean(public_transport_survey[public_transport_survey$all_weekday_commuter == 0 & public_transport_survey$transport_mode == "Tram", "high_satisfaction"], na.rm = TRUE) #81.3%

#Q2.3
public_transport_survey %>%
  group_by(transport_mode) %>%
  summarise(mean_punctuality = mean(punctuality_rating, na.rm = TRUE))

#Q2.4
ggplot(public_transport_survey, aes(x = as.factor(transport_mode), y = num_trips_week)) +
  stat_summary(fun.y = mean, geom = "col")
  scale_y_continuous(breaks = (0:6)*2, limits = c(0, 12))

#Q3.1
hospital_data %>%
  group_by(region) %>%
  summarise(mean_time = mean(avg_wait_time))

#Q3.2
hospital_data <- mutate(hospital_data, satisfaction = ifelse(patient_satisfaction >= 4, "High-satisfaction", "Low-satisfaction"))

hospital_data %>%
  group_by(satisfaction) %>%
  summarise(mean_time = mean(avg_wait_time))

hospital_data %>%
  group_by(satisfaction) %>%
  summarise(mean_mortality = mean(mortality_rate))

#Q3.3
hospital_data <- merge(hospital_data, readmission_rates, by = "hospital_id")

hospital_data = hospital_data[order(hospital_data$mortality_rate), ]

hospital_data[1:50, "Tercile"] = "Lowest"
hospital_data[51:100, "Tercile"] = "Middle"
hospital_data[101:150, "Tercile"] = "Highest"

hospital_data %>%
  group_by(Tercile) %>%
  summarise(mean_readmission = mean(thirty_day_readmit_rate))

#Q3.4
hospital_data <- mutate(hospital_data, staff_to_bed = staff_count / bed_count)

ggplot(hospital_data, aes(x = as.factor(Tercile), y = staff_to_bed)) +
  geom_boxplot() +
  labs(x = "Terciles of Mortality rate") +
  scale_y_continuous(breaks = (6:12)*0.1 + 0.05, limits = c(0.65, 1.25))

#Q4.1
length(which(is.na(housing_survey$monthly_income)))

#Q4.2
ggplot(housing_survey, aes(x = household_size)) +
  geom_histogram(bins = 8) +
  scale_x_continuous(breaks = 1:8) +
  scale_y_continuous(breaks = (0:7)*10, limits = c(0, 70))

#Q4.3
housing_survey <- mutate(housing_survey, rent_burden = monthly_rent / monthly_income *100)

housing_survey %>%
  group_by(household_size) %>%
  summarise(mean_rentburden = mean(rent_burden, na.rm = TRUE))

#Q4.4
housing_survey %>%
  group_by(region_type) %>%
  summarise(mean_monthlyincome = mean(monthly_income, na.rm = TRUE))
