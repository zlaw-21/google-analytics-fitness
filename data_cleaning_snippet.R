#### Data Cleaning Overview ####

# To showcase the data cleaning efforts for this project, cleaning scripts are 
# included for the Heart Rate and Calories datasets.

# For all datasets and credits, please see the README for more information and
# downloads.

# Quickly, all data used comes from two datasets for each feature. The early 
# datasets contain information from 3-11-2016 to 4-11-2016 and the late datasets
# contain information from 4-12-2016 to 5-12-2016 (theoretically).

# 6 sets of data were used, with following names and formats:

# Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv
# Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv

# Fitabase Data 3.12.16-4.11.16/minuteCaloriesNarrow_merged.csv
# Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv

# Fitabase Data 3.12.16-4.11.16/minuteIntensitiesNarrow_merged.csv
# Fitabase Data 4.12.16-5.12.16/minuteIntensitiesNarrow_merged.csv

# Fitabase Data 3.12.16-4.11.16/minuteMETsNarrow_merged.csv
# Fitabase Data 4.12.16-5.12.16/minuteMETsNarrow_merged.csv

# Fitabase Data 3.12.16-4.11.16/minuteSleep_merged.csv
# Fitabase Data 4.12.16-5.12.16/minuteSleep_merged.csv

# Fitabase Data 3.12.16-4.11.16/minuteStepsNarrow_merged.csv
# Fitabase Data 4.12.16-5.12.16/minuteStepsNarrow_merged.csv

#### PLEASE NOTE ####

# Heart Rate and Calories datasets are included here. For the merge, all steps
# were completed on each of the four remaining sets, with minor formatting tweaks
# when required. 

# Heart Rate was the only set in seconds format, so it was the only dataset
# which required conversion to minutes format.

#### Data Cleaning Steps ####

# Install and load packages

#install.packages("tidyverse")

library("tidyverse")


#### Cleaning Heartrate Seconds Datasets and converting to minutes ####

# 3.12.16 - 4.11.16
seconds_heartrate_early <- 
  read_csv("C:/xxx/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/heartrate_seconds_merged.csv")

# 4.12.16 - 5.12.16
seconds_heartrate_late <-
  read_csv("C:/xxx/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/heartrate_seconds_merged.csv")

# seconds_heartrate_early and seconds_heartrate_late contain 14 users respectively, 
# with only 13 matching ids between sets. Saving each set for further inspection.

early_heartrate_ids <- unique(seconds_heartrate_early$Id)

late_heartratey_ids <- unique(seconds_heartrate_late$Id)

# Adjust Date/Time to date time format.

seconds_heartrate_early <- seconds_heartrate_early %>% 
  mutate(Time = mdy_hms(Time)) %>% 
  rename(date_time = Time)

seconds_heartrate_late <- seconds_heartrate_late %>% 
  mutate(Time = mdy_hms(Time)) %>% 
  rename(date_time = Time)

# Ensure no duplicates within each respective set and identify overlapping.
# values.

seconds_heartrate_early %>% 
  count(Id, date_time) %>% 
  filter(n > 1)

seconds_heartrate_late %>% 
  count(Id, date_time) %>% 
  filter(n > 1)

seconds_heartrate_early %>% 
  arrange(desc(date_time)) %>% 
  head()

seconds_heartrate_late %>% 
  arrange(date_time) %>% 
  head()

# Overlapping values were found on the last day of seconds_heartrate_early
# and the first day of seconds_heartrate_late respectively.

seconds_heartrate_early <- seconds_heartrate_early %>%
  filter(date_time < as.Date(max(date_time)))

max(seconds_heartrate_early$date_time)

# Combine the data between early and late sets for the full dataframe, arrange
# by id, and clean column names/datatypes.

seconds_heartrate <-
  rbind(seconds_heartrate_early, seconds_heartrate_late) %>% 
  arrange(Id) %>% 
  clean_names() %>% 
  mutate(id = as.factor(id),
         value = as.integer(value)) %>% 
  rename(heartrate = value)

glimpse(seconds_heartrate)

# Convert the data from MDY HMS to MDY HM to combine with other datasets for
# analysis.

minutes_heartrate <- 
  seconds_heartrate %>% 
  mutate(date_time = floor_date(date_time, unit = "minute")) %>% 
  group_by(id, date_time) %>% 
  summarize(heart_rate = as.integer(round(mean(heartrate), 0)))

glimpse(minutes_heartrate)


#### Cleaning Calories Minutes Datasets ####

# 3.12.16 - 4.11.16
minutes_calorie_early <- 
  read_csv("C:/xxx/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/minuteCaloriesNarrow_merged.csv")


# 4.12.16 - 5.12.16
minutes_calorie_late <-
  read_csv("C:/xxx/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/minuteCaloriesNarrow_merged.csv")

# minutes_calorie_early and minutes_calorie_late contain 34 and 33 users respectively, 
# with only 32 matching ids between sets (total of 35 ids). Saving each set for further inspection.

early_calorie_ids <- unique(minutes_calorie_early$Id)

late_calorie_ids <- unique(minutes_calorie_late$Id)

# Adjust Date/Time to the correct format.

minutes_calorie_early <- minutes_calorie_early %>% 
  mutate(ActivityMinute = mdy_hm(sub(":00 ", " ", ActivityMinute))) %>% 
  rename(date_time = ActivityMinute)

minutes_calorie_late <- minutes_calorie_late %>% 
  mutate(ActivityMinute = mdy_hm(sub(":00 ", " ", ActivityMinute))) %>% 
  rename(date_time = ActivityMinute)

# Ensure no duplicates within each respective set and identify overlapping.
# values.

minutes_calorie_early %>% 
  count(Id, date_time) %>% 
  filter(n > 1)

minutes_calorie_late %>% 
  count(Id, date_time) %>% 
  filter(n > 1)

minutes_calorie_early %>% 
  arrange(desc(date_time)) %>% 
  head()

minutes_calorie_late %>% 
  arrange(date_time) %>% 
  head()

# Overlapping values were found on the last day of minutes_calorie_early
# and the first day of minutes_calorie_late respectively.

minutes_calorie_early <- minutes_calorie_early %>%
  filter(date_time < as.Date(max(date_time)))

max(minutes_calorie_early$date_time)

# Bind both minutes_calorie_early and minutes_calorie_late, arrange by id,
# clean names, and convert id to a factor.

minutes_calorie <-
  rbind(minutes_calorie_early, minutes_calorie_late) %>% 
  arrange(Id) %>% 
  clean_names() %>% 
  mutate(id = as.factor(id))

glimpse(minutes_calorie)
