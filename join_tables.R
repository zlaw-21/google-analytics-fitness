#### Join all Dataframes ####

library("tidyverse")

# List of minute level fitness tracker dataframes created from cleaning.

df_list <- list(minutes_calorie,
                minutes_mets, 
                minutes_heartrate, 
                minutes_intensities, 
                minutes_sleep, 
                minutes_steps)

# Function to fully join a list of dataframes by shared keys

join_sets <- function(dfs, keys) {
  
  data <- dfs[[1]]
  for (i in 2:length(dfs)) {
    data <- full_join(data, dfs[[i]], by = keys)
    }
  return(data)
  
}

# Combine all datasets by "id" / "date_time" and filter to normalize data length
# across days.

minutes_data <- 
  join_sets(df_list, c("id", "date_time")) %>% 
  filter(date_time > as.Date("2016-03-12") &
           date_time < as.Date("2016-05-12"))

glimpse(minutes_data)


