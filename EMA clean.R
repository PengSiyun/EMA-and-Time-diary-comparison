library(haven)
library(dplyr)

# Load activity-level file
ema <- read_dta("C:/Users/peng_admin/OneDrive - Indiana University/SECHURA/SECHURA data/SECHURA-EMA data-Qu-10-13-24.dta")

# Define holidays for the given period (November 2023 - March 2024)

holidays <- c(
  '01-01',  # New Year’s Day (January 1, 2024)
  '11-23',  # Thanksgiving Day (November 23, 2023)
  '12-25',  # Christmas Day (December 25, 2023)
  '03-31'   # Easter Sunday (March 31, 2024)
)

# Create the holiday variable
ema <- ema %>% mutate(holiday = ifelse(format(as.Date(date_only), '%m-%d') %in% holidays, 1, 0))

# Create a weekday (Monday to Friday)
ema <- ema %>% mutate(weekday = ifelse(weekdays(as.Date(date_only)) %in% c('Saturday', 'Sunday'), 0, 1))

# load non-EMA data
sechura <- read_dta("C:/Users/peng_admin/OneDrive - Indiana University/SECHURA/SECHURA data/SECHURA-analysis-ROTH-10-15-24.dta")

# Merge datasets: left_joun: drop sechura cases not matched in ema
ema_cleaned <- ema %>%
  left_join(sechura, by = "surveyid")

# Rename columns to replace dots with underscores
ema_cleaned <- ema_cleaned %>%
  rename_with(~ gsub("\\.", "_", .))

# Save as new Stata file 
write_dta(ema_cleaned, "C:/Users/peng_admin/OneDrive - Indiana University/SECHURA/work/EMA and ATUS/EMA_cleaned.dta")
