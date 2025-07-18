
#### Develop a Heatmap of device outages over time ####

# join_all_dates is a function that generates complete minute by minute coverage
# between the max and min time stamps of the input dataframe for each ID. It then
# joins to the original dataframe to create NA values for outage measurements.

join_all_dates <- function(df) {
  
  start_time <- min(df$date_time)
  end_time <- max(df$date_time)
  unique_ids <- unique(df$id)
  
  minute_sequence <- seq(from = start_time, to = end_time, by = "min")
  
  unique_dates <- expand.grid(id = unique_ids, date_time = minute_sequence)
  
  join_df <- df %>% 
    right_join(unique_dates, by = c("id", "date_time"))
}

outage_data <- join_all_dates(minutes_data)

# mutate outage data to include weekday, minute of the day, and start of the 
# week information (starting on saturday) for visualization.

outage_data <- outage_data %>% 
  mutate(weekday = wday(date_time, label = TRUE, abbr = TRUE),
         true_min = hour(date_time) * 60 + minute(date_time),
         week_start = floor_date(as.Date(date_time), 
                                 unit = "week", 
                                 week_start = "Sat"))

# Create labels for the y-axis of the heatmap.

time_labels <- format(seq(
  from = as.POSIXct("00:00", format = "%H:%M"),
  by = "120 min",
  length.out = 12
), format = "%H:%M")

time_labels <- c(time_labels, "23:59")

# create breaks to align with the labels on the heatmap.

breaks <- seq(0, 1440, by = 120)

# Filter the dataframe to normalize the number of days in each week and remove
# non NA values (only outages), then visualize the plot.

outage_data %>% 
  filter(week_start < max(week_start),
         if_all(3:9, is.na)) %>%
  ggplot(aes(x = weekday, y = true_min)) +
  geom_bin2d(bins = c(7, 48),
             color = "white") +
  scale_y_continuous(
    breaks = breaks,
    labels = time_labels) +
  ggtitle("Total Fitbit Inactivity by Weekday and Time") +
  xlab("Weekday") +
  ylab("Time of Day") +
  scale_fill_gradient(low = "gray90", 
                       high = "red") +
  theme_minimal()

#### Compare Feature Usage ####

# Visualize all minutes (represented in hours) in a bar graph visual to determine
# the most used feature across all users.

minutes_data %>% 
  select(-id, -date_time, -log_id) %>% 
  summarize(across(everything(), ~ sum(. > 0, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "count") %>% 
  mutate(count = count / 60,
         feature = fct_reorder(feature, count, .desc = TRUE)) %>% 
  ggplot(aes(x = feature, y = count, fill = feature)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::label_comma(),
                     breaks = scales::breaks_extended(n = 10)) +
  scale_fill_viridis_d() +
  ggtitle("Total Feature Usage in Hours",
          subtitle = "From 3-12-2016 to 5-11-2016") +
  xlab("Feature") +
  ylab("Total Hours") +
  theme_minimal() +
  theme(legend.position = "none")

# Recreate the same visualization but remove redundant feature usage. In this
# case, Calories and METs have twice the usage of the third most used feature,
# and METs are used to calculate Calories, so their values are identical.

minutes_data %>% 
  select(-id, -date_time, -log_id, -calories, -mets) %>% 
  summarize(across(everything(), ~ sum(. > 0, na.rm = TRUE))) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "count") %>% 
  mutate(count = count / 60,
         feature = fct_reorder(feature, count, .desc = TRUE)) %>% 
  ggplot(aes(x = feature, y = count, fill = feature)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::label_comma(),
                     breaks = scales::breaks_extended(n = 10)) +
  scale_fill_viridis_d() +
  ggtitle("Total Feature Usage in Hours",
          subtitle = "From 3-12-2016 to 5-11-2016") +
  xlab("Feature") +
  ylab("Total Hours") +
  theme_minimal() +
  theme(legend.position = "none")

#### Determine the Consistency of Feature Usage ####

# Develop a line graph to usage for all features throughout the tracking period.
# Create floor date for grouping, remove redundant fields, sum all values
# greater than 0 and convert to hours, pivot from wide to long format to pipe
# into ggplot/geom_line.

minutes_data %>%
  mutate(day_group = floor_date(date_time, unit = "day")) %>%
  select(-id, -date_time, -log_id) %>% 
  group_by(day_group) %>% 
  summarize(across(everything(), ~ sum(. > 0, na.rm = TRUE) / 60)) %>% 
  pivot_longer(!day_group, names_to = "feature", values_to = "count") %>% 
  ggplot(aes(x = day_group, y = count, color = feature)) +
  geom_line(linewidth = 1.2) +
  scale_x_datetime(date_breaks = "1 week") +
  labs(title = "Daily Feature Usage",
       subtitle = "From 3-12-2016 to 5-11-2016",
       x = "Date",
       y = "Total Hours",
       color = "Feature") +
  theme_minimal() +
  theme(legend.text = element_text(size = 10),
        legend.title = element_text(size = 12))

#### Determine post-workout times throughout the week ####

# Create a df for heat map visualizations. Mutate minutes data to include weekday, 
# minute of the day, and start of the week information (starting on saturday).

heat_minutes <-
  minutes_data %>% mutate(weekday = wday(date_time, label = TRUE, abbr = TRUE),
                          true_min = hour(date_time) * 60 + minute(date_time),
                          week_start = floor_date(as.Date(date_time), 
                                                  unit = "week", 
                                                  week_start = "Sat"))
# Heat Map for intensity

# Convert datapoints to 30 min bins, remove the last partial week to normalize
# data length, group by weekly bins, and sum intensity to visualize in ggplot.

heat_minutes %>%
  mutate(min = floor(true_min / 30)) %>%
  filter(week_start < max(week_start),
          !is.na(intensity)) %>% 
  group_by(min, weekday) %>% 
  summarize(intensity = sum(intensity)) %>%
  ggplot(aes(x = weekday, y = min, fill = intensity)) +
  geom_tile(color = "white") +
  scale_y_continuous(breaks = c(seq(0, 44, by = 4), 47),
                     labels = time_labels) +
  scale_fill_gradient(low = "gray90", 
                      high = "red") +
  labs(title = "Total Intensity by Weekday and Time",
       subtitle = "30 min intervals, From 3-12-2016 to 5-11-2016",
       x = "Weekday",
       y = "Time",
       fill = "Total Intensity") +
  geom_hline(yintercept = 39, color = "black", linetype = "dashed", size = 1.06) +
  annotate("text",
           x = 4,
           y = 39,
           label = "End of peak activity at 7:30 PM",
           vjust = -1.09,
           size = 4,
           color = "black",
           fontface = "bold.italic") +
  theme_minimal()

# Map for METs

# Convert datapoints to 30 min bins, remove the last partial week to normalize
# data length, group by weekly bins, and sum METs to visualize in ggplot.

heat_minutes %>%
  mutate(min = floor(true_min / 30)) %>%
  filter(week_start < max(week_start),
         !is.na(mets)) %>% 
  group_by(min, weekday) %>% 
  summarize(mets = sum(mets)) %>%
  ggplot(aes(x = weekday, y = min, fill = mets)) +
  geom_tile(color = "white") +
  scale_y_continuous(breaks = c(seq(0, 44, by = 4), 47),
                     labels = time_labels) +
  scale_fill_gradient(low = "gray90", 
                      high = "red") +
  labs(title = "Total METs by Weekday and Time",
       subtitle = "30 min intervals, From 3-12-2016 to 5-11-2016",
       x = "Weekday",
       y = "Time",
       fill = "Total METs") +
  geom_hline(yintercept = 39, color = "black", linetype = "dashed", size = 1.06) +
  annotate("text",
           x = 4,
           y = 39,
           label = "End of peak activity at 7:30 PM",
           vjust = -1.09,
           size = 4,
           color = "black",
           fontface = "bold.italic") +
  theme_minimal()

# Map for heartrate

# Convert datapoints to 30 min bins, remove the last partial week to normalize
# data length, group by weekly bins, and sum heart rate to visualize in ggplot.

heat_minutes %>%
  mutate(min = floor(true_min / 30)) %>%
  filter(week_start < max(week_start),
         !is.na(heart_rate)) %>% 
  group_by(min, weekday) %>% 
  summarize(heart_rate = sum(heart_rate)) %>%
  ggplot(aes(x = weekday, y = min, fill = heart_rate)) +
  geom_tile(color = "white") +
  scale_y_continuous(breaks = c(seq(0, 44, by = 4), 47),
                     labels = time_labels) +
  scale_fill_gradient(low = "gray90", 
                      high = "red") +
  labs(title = "Total Heart Rate by Weekday and Time",
       subtitle = "30 min intervals, From 3-12-2016 to 5-11-2016",
       x = "Weekday",
       y = "Time",
       fill = "Total Heart Rate") +
  geom_hline(yintercept = 42, color = "black", linetype = "dashed", size = 1.06) +
  annotate("text",
           x = 4,
           y = 42,
           label = "End of peak activity at 9:00 PM",
           vjust = -1.09,
           size = 4,
           color = "black",
           fontface = "bold.italic") +
  theme_minimal()













