#1. What are some trends in smart device usage?
#2. How could these trends apply to Bellabeat customers?
#3. How could these trends help influence Bellabeat marketing strategy?


#   You will produce a repo with the following deliverables:
# 1. A clear summary of the business task
# 2. A description of all data sources used
# 3. Documentation of any cleaning or manipulation of data
# 4. A summary of your analysis
# 5. Supporting visualizations and key findings
# 6. Your top high-level content recommendations based on your analysis

#Packages
library(EnvStats)
library(tidyverse)
library(lubridate)
dailyActivity_merged <- read.csv("dailyActivity_merged.csv")
sleepDay_merged <- read.csv("sleepDay_merged.csv")
dailyIntensities_merged <-read.csv("dailyIntensities_merged.csv")
hourlyIntensities_merged <- read.csv("hourlyIntensities_merged.csv")


#Explore

head(dailyActivity_merged)
colnames(dailyActivity_merged)

head(sleepDay_merged)
colnames(sleepDay_merged)
str(sleepDay_merged)

#Summary Statistics

n_distinct(dailyActivity_merged$Id)
n_distinct(sleepDay_merged$Id)

nrow(dailyActivity_merged)
nrow(sleepDay_merged)

dailyActivity_merged %>% select(TotalSteps,TotalDistance,SedentaryMinutes) %>% summary()
sleepDay_merged %>% select(TotalSleepRecords,TotalMinutesAsleep,TotalTimeInBed) %>%  summary()


#I. EXPLORATION OF DATA


#Exploratory Plots
ggplot(data=sleepDay_merged, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + geom_point() + labs(title= "Total Time in Bed vs. Total Minutes Asleep")
#Data Makes Practical Sense
ggplot(data=dailyActivity_merged, aes(x=TotalSteps,y=SedentaryMinutes)) + geom_point()  + labs(title= "Sedentary Minutes vs. Total Stpes")
 

 #No Apparent Relationship

#Digging Deeper 
new_daily_activities <- dailyActivity_merged %>% mutate(total_minutes_active = LightlyActiveMinutes+FairlyActiveMinutes+VeryActiveMinutes)
activity_means <- new_daily_activities %>% group_by(Id) %>% drop_na() %>% summarise(mean_total_steps=mean(TotalSteps), mean_sedentary=mean(SedentaryMinutes), mean_total_minutes_active=mean(total_minutes_active))

#i.
#Histogram and Summary of Total_Minutes_Active
hist(new_daily_activities$total_minutes_active, probability = TRUE)
summary(new_daily_activities$total_minutes_active)

#i.2
activity_spread_mean <- data.table::copy(combined_data) %>% select('SedentaryMinutes', 'LightlyActiveMinutes', 'FairlyActiveMinutes', 'VeryActiveMinutes','VeryActiveMinutes') %>% summarize(mean_sedentary = mean(SedentaryMinutes), mean_lightly = mean(LightlyActiveMinutes), mean_fairly = mean(FairlyActiveMinutes), mean_very = mean(VeryActiveMinutes))
activity_spread_mean <- pivot_longer(activity_spread_mean, cols=c('mean_sedentary', 'mean_lightly', 'mean_fairly', 'mean_very'), names_to = c('types_of_activity'), values_to = 'time_in_minutes')
ggplot(data=activity_spread_mean, aes(x=types_of_activity, y= time_in_minutes, fill = types_of_activity)) + geom_col()
print(activity_spread_mean)

#ii.
#Histogram and Probability Density Function of Mean Total Steps for Each Participant

hist(activity_means$mean_total_steps,probability = TRUE)
epdfPlot(activity_means$mean_total_steps, epdf.col = "red")
summary(activity_means$mean_total_steps)

#iii.
#Histogram and Probability Density Function of Mean Total Minutes Active for Each Participant
hist(activity_means$mean_total_minutes_active,probability = TRUE)
epdfPlot(activity_means$mean_total_minutes_active, epdf.col = "red")
summary(activity_means$mean_total_minutes_active)

##

#Revisiting Relationship Between Being Active and Being Sedentary Using Means of Data
ggplot(data=activity_means, aes(x=mean_total_steps, y=mean_sedentary)) + geom_point()
ggplot(data=activity_means, aes(x=mean_total_minutes_active, y=mean_sedentary)) + geom_point()
``
ggplot(data=new_daily_activities, aes(x= total_minutes_active, y=SedentaryMinutes)) + geom_point()

#Still not a strong relationship


#Merge Analysis Between Sleep and Activity. Are is any relationship between sleep and activity?
n_distinct(sleepDay_merged)

n_distinct(new_daily_activities)

sleepDay_date <- data.table::copy(sleepDay_merged) %>% mutate(new_date = mdy_hms(SleepDay))

new_daily_activities_date <- data.table::copy(new_daily_activities) %>% mutate(new_date = mdy(ActivityDate))


combined_data <- merge(sleepDay_date, new_daily_activities_date, by= c("Id","new_date")) %>% select(-SleepDay,-ActivityDate)

n_distinct(combined_data)

ggplot(data=combined_data, aes(x=TotalTimeInBed,y=TotalSteps)) + geom_point()
ggplot(data=combined_data, aes(x=TotalTimeInBed,y=total_minutes_active)) + geom_point()
#No clear relationship between Total Time in Bed and Total Steps

ggplot(data=combined_data, aes(x=TotalMinutesAsleep,y=TotalSteps)) + geom_point()
ggplot(data=combined_data, aes(x=TotalMinutesAsleep,y=total_minutes_active)) + geom_point()
ggplot(data=combined_data, aes(x=TotalMinutesAsleep,y=VeryActiveMinutes)) + geom_point()
ggplot(data=combined_data, aes(x=TotalMinutesAsleep,y=FairlyActiveMinutes)) + geom_point()
ggplot(data=combined_data, aes(x=TotalMinutesAsleep,y=LightlyActiveMinutes)) + geom_point()

#No clear relationship between Total Time Asleep and Total Steps

ggplot(data=combined_data, aes(x=TotalMinutesAsleep,y=SedentaryMinutes)) + geom_point(color = "black") + geom_smooth() + labs(title = "Sedentary Minutes Vs. Total Minutes Asleep") 

#Only relationship between TotalMinutesAsleep and Sedentary Minutes.

# II. Trends in Device Usage.
#Questions to perform to answer:
#1 What does the distribution of sleep look like?
#2 What days are the most/least active on average, for most people?
#3.What times of day are most/least active?
#4.What days have the most/least sleep?

#1 

hours_sleep <- select(combined_data, 'Id','TotalMinutesAsleep') %>% mutate(TotalHoursAsleep = TotalMinutesAsleep/60) 
ggplot(data=hours_sleep, aes(x=TotalHoursAsleep)) + geom_histogram(fill = "darkblue")
summary(hours_sleep$TotalHoursAsleep)

#2.

mutated_int_days <- data.table::copy(hourlyIntensities_merged) %>% mutate(Day = mdy_hms(ActivityHour) %>% weekdays()) %>% group_by(Day) %>% drop_na() %>% summarise(mean_total_intensity = mean(TotalIntensity))

ggplot(data = mutated_int_days, aes(x=Day, y=mean_total_intensity)) + geom_histogram(stat = "identity", fill = "pink") + theme(axis.text.x = element_text(angle = 0)) + labs(title = "Average Total Intensity vs. WeekDay" )

summary(mutated_int_days)

#3. 

mutated_int_hours <- data.table::copy(hourlyIntensities_merged) %>% mutate(Time = mdy_hms(ActivityHour) %>% hour()) %>% group_by(Time) %>%  drop_na() %>% summarise(mean_total_intensity = mean(TotalIntensity))

ggplot(data = mutated_int_hours, aes(x=Time, y=mean_total_intensity)) + geom_histogram(stat = "identity", fill = "pink") + theme(axis.text.x = element_text(angle = 0)) + labs(title = "Average Total Intensity vs. Time" )

summary(mutated_int_hours)

#4

mutated_int_sleep <- data.table::copy(combined_data) %>% mutate(Day = weekdays(new_date), TotalHoursAsleep = TotalMinutesAsleep/60) %>% group_by(Day)  %>% drop_na() %>% summarise(mean_hours_sleep = mean(TotalHoursAsleep))

summary(mutated_int_sleep$mean_hours_sleep)

ggplot(data = mutated_int_sleep, aes(x=Day, y=mean_hours_sleep)) + geom_histogram(stat = "identity", fill = "pink") + theme(axis.text.x = element_text(angle = 0)) + labs(title = "Average Total Sleep vs. WeekDay" )
 