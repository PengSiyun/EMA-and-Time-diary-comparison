library(haven)
library(dplyr)

# Load activity-level file
df <- read_dta("C:/Users/siyunpeng/OneDrive - University of South Florida/ATUS/atusact_2023.dta")

# Recode 'tewhere' as 'home', 'work', or 'other'
df <- df %>%
  mutate(where = case_when(
    tewhere == -1 ~ NA_integer_,  # -1 becomes missing
    tewhere == -3 ~ NA_integer_,  # -3 becomes missing
    tewhere == 1 ~ 1,
    tewhere == 2 ~ 2,
    TRUE ~ 3
  )) %>%
  mutate(where = factor(where, levels = c(1, 2, 3), labels = c("Home", "Work", "Other")))

# Recode 'tutier1code' as 'household chores', 'eating and drinking', or 'other'
df <- df %>%
  mutate(what = case_when(
    tutier1code == 50 ~ NA_integer_,  # 50 becomes missing
    tutier1code == 2 ~ 1,
    tutier1code == 11 ~ 2,
    TRUE ~ 3
  )) %>%
  mutate(what = factor(what, levels = c(1, 2, 3), labels = c("Household chores", "Eating/drinking", "Other")))

# Recode 'tuwho' as alone
who <- read_dta("C:/Users/siyunpeng/OneDrive - University of South Florida/ATUS/atuswho_2023.dta")
who <- who %>%
  mutate(alone = case_when(
    tuwho_code %in% c(-1, -2, -3) ~ NA_integer_,  # Recoding -1, -2, -3 as NA (missing)
    tuwho_code %in% c(18, 19) ~ 1,                 # Recoding 18 and 19 as 1 (alone)
    TRUE ~ 0                                      # All other values as 0
  )) %>%
  mutate(alone = factor(alone, levels = c(0, 1), labels = c("Not alone", "Alone")))

# only count one activity regardless of multiple people (Remove duplicates based on tucaseid and tuactivity_n)
who <- who %>%
  distinct(tucaseid, tuactivity_n, .keep_all = TRUE)

# Merge the WHO data
df <- df %>%
  left_join(who, by = c("tucaseid", "tuactivity_n"))

# Convert time strings to minutes since 4 AM (ATUS diary day start)
time_to_minutes <- function(time_str) {
  # Parse time string
  time_parts <- as.numeric(strsplit(time_str, ":")[[1]])
  hours <- time_parts[1]  
  minutes <- time_parts[2]
  
  # Convert to minutes since 4 AM
  total_minutes <- hours * 60 + minutes
  
  # Adjust for diary day (4 AM = 0 minutes, times before 4 AM are next day)
  if (total_minutes >= 240) {  # 4 AM or later
    return(total_minutes - 240)
  } else {  # Before 4 AM (next day)
    return(total_minutes + 1200)  # Add 20 hours (24 - 4)
  }
}

# Apply time conversion
df <- df %>%
  rowwise() %>%
  mutate(
    start_minutes = time_to_minutes(tustarttim),
    end_minutes = start_minutes + tuactdur24  # Add duration to get end time
  ) %>%
  ungroup()

# Adjust the end time of the last activity to not exceed 24 hours (1440 minutes)
df <- df %>%
  group_by(tucaseid) %>%
  mutate(
    max_activity = max(tuactivity_n, na.rm = TRUE),
    end_minutes = ifelse(
      tuactivity_n == max_activity & end_minutes > 1440,
      1440,  # Cap at 24 hours (4 AM next day)
      end_minutes
    )
  ) %>%
  ungroup() %>%
  select(-max_activity)

# Define time windows in minutes since 4 AM
# 8 AM to 10 AM: 4 hours to 6 hours after 4 AM = 240 to 360 minutes
# 12 PM to 8 PM: 8 hours to 16 hours after 4 AM = 480 to 960 minutes
window_1_start <- 240   # 8 AM
window_1_end <- 360     # 10 AM
window_2_start <- 480   # 12 PM
window_2_end <- 960     # 8 PM

# Function to calculate overlap between activity and time window
calculate_overlap <- function(activity_start, activity_end, window_start, window_end) {
  overlap_start <- max(activity_start, window_start)
  overlap_end <- min(activity_end, window_end)
  overlap_duration <- overlap_end - overlap_start
  return(max(0, overlap_duration))  # Ensure non-negative overlap
}

# Calculate total time for where, what, and alone per tucaseid
time_home <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(where == "Home", calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(where == "Home", calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_home = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_home = sum(time_home, na.rm = TRUE))

time_work <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(where == "Work", calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(where == "Work", calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_work = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_work = sum(time_work, na.rm = TRUE))

time_whe_other <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(where == "Other", calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(where == "Other", calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_whe_other = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_whe_other = sum(time_whe_other, na.rm = TRUE))

time_whe_miss <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(is.na(where), calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(is.na(where), calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_whe_miss = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_whe_miss = sum(time_whe_miss, na.rm = TRUE))

time_alone <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(alone == "Alone", calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(alone == "Alone", calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_alone = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_alone = sum(time_alone, na.rm = TRUE))

time_nalone <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(alone == "Not alone", calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(alone == "Not alone", calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_nalone = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_nalone = sum(time_nalone, na.rm = TRUE))

time_alone_miss <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(is.na(alone), calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(is.na(alone), calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_alone_miss = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_alone_miss = sum(time_alone_miss, na.rm = TRUE))

time_eat <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(what == "Eating/drinking", calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(what == "Eating/drinking", calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_eat = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_eat = sum(time_eat, na.rm = TRUE))

time_chore <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(what == "Household chores", calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(what == "Household chores", calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_chore = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_chore = sum(time_chore, na.rm = TRUE))

time_wha_other <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(what == "Other", calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(what == "Other", calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_wha_other = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_wha_other = sum(time_wha_other, na.rm = TRUE))

time_wha_miss <- df %>%
  rowwise() %>%
  mutate(
    overlap_where_1 = ifelse(is.na(what), calculate_overlap(start_minutes, end_minutes, window_1_start, window_1_end), 0),
    overlap_where_2 = ifelse(is.na(what), calculate_overlap(start_minutes, end_minutes, window_2_start, window_2_end), 0),
    time_wha_miss = overlap_where_1 + overlap_where_2
  ) %>%
  ungroup() %>%
  group_by(tucaseid) %>%
  summarise(time_wha_miss = sum(time_wha_miss, na.rm = TRUE))

# Merge where, what,and alone time back into the main dataframe
match_df <- df %>%
  left_join(time_home, by = "tucaseid") %>%
  left_join(time_work, by = "tucaseid") %>%
  left_join(time_whe_other, by = "tucaseid") %>%
  left_join(time_alone, by = "tucaseid") %>%
  left_join(time_nalone, by = "tucaseid") %>%
  left_join(time_eat, by = "tucaseid") %>%
  left_join(time_chore, by = "tucaseid") %>%
  left_join(time_wha_other, by = "tucaseid") %>%
  left_join(time_whe_miss, by = "tucaseid") %>%
  left_join(time_alone_miss, by = "tucaseid") %>%
  left_join(time_wha_miss, by = "tucaseid")

# Drop duplicates by tucaseid
match_df <- match_df %>%
  distinct(tucaseid, .keep_all = TRUE)

# keep necessary variables
match_df <- match_df %>%
  select(starts_with("time_"), tucaseid)

# Check if match_df was created successfully
print(paste("match_df created with", nrow(match_df), "rows"))
print("match_df columns:")
print(names(match_df))




##########################################################
# restrict to workdays, Indiana, and age 53 and above
##########################################################




# load summary file
work_data <- read_dta("C:/Users/siyunpeng/OneDrive - University of South Florida/ATUS/atussum_2023.dta")
weight_data <- read_dta("C:/Users/siyunpeng/OneDrive - University of South Florida/ATUS/atusrepwgt_2023.dta")
roster_data <- read_dta("C:/Users/siyunpeng/OneDrive - University of South Florida/ATUS/atusrost_2023.dta")
date_data <- read_dta("C:/Users/siyunpeng/OneDrive - University of South Florida/ATUS/atusresp_2023.dta") %>% 
  select(tucaseid, tuyear, tumonth)
state_data <- read_dta("C:/Users/siyunpeng/OneDrive - University of South Florida/ATUS/atuscps_2023.dta") %>% 
  select(tucaseid, gestfips, ptdtrace, peeduca, pemaritl, hefaminc)

# get cohabiting partner from roster
roster_data <- roster_data %>%
  group_by(tucaseid) %>%
  mutate(cohabit = ifelse(any(terrp == 21), 1, 0)) 

roster_data <- roster_data %>%
  distinct(tucaseid, .keep_all = TRUE) # Drop duplicates by tucaseid

roster_data <- roster_data %>%
  select(cohabit, tucaseid)

# Merge the datasets 
filter_data <- work_data %>%
  left_join(weight_data, by = "tucaseid") %>%
  left_join(roster_data, by = "tucaseid") %>%
  left_join(date_data, by = "tucaseid") %>%
  left_join(state_data, by = "tucaseid") 

# Check if filter_data was created successfully  
print(paste("filter_data created with", nrow(filter_data), "rows"))

# Include midwest states
filter_data <- filter_data %>%
  filter(gestfips %in% c(17, 18, 19, 20, 26, 27, 29, 31, 38, 39, 46, 55))

# Include age 53 and above
filter_data <- filter_data %>%
  filter(teage >= 55)

# keep necessary variables
filter_data <- filter_data %>%
  select(tucaseid, gestfips, tuyear, tumonth, trholiday, tudiaryday, tesex, 
         teage, ptdtrace.y, pemaritl, cohabit, trchildnum, peeduca.y, trdpftpt, hefaminc,
         tufinlwgt, starts_with("finlwgt"))

# Check if filter_data still has data after filtering
print(paste("filter_data after filtering has", nrow(filter_data), "rows"))

# Merge the filter data with  match_df data
final_data <- match_df %>%
  inner_join(filter_data, by = "tucaseid")

# Check if final_data was created successfully
print(paste("final_data created with", nrow(final_data), "rows"))

# Rename columns to replace dots with underscores
final_data <- final_data %>%
  rename_with(~ gsub("\\.", "_", .))

# Save the filtered data to a new Stata file (optional)
write_dta(final_data, "C:/Users/siyunpeng/OneDrive - University of South Florida/SECHURA/work/EMA and ATUS/ATUS_cleaned.dta")