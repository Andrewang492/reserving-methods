library(lubridate)
library(dplyr)

policies <- read.csv("ACTL31425110AssignmentData2022.csv")
View(policies)
policies$claim_loss_date <- ifelse(policies$claim_loss_date == "", NA, policies$claim_loss_date)

#strptime(policies[142,]$claim_loss_date, format="%Y-%m-%d %H:%M:%S+%z", tz="GMT")
#policies %>% select(claim_loss_date) %>% filter(!is.na(claim_loss_date))
policies$claim_loss_date <- ymd_hms(policies$claim_loss_date)
policies$term_start_date <- ymd_hms(policies$term_start_date)
policies$term_expiry_date <- ymd_hms(policies$term_expiry_date)
policies$accident_month <- ymd(policies$accident_month)

policies <- policies %>% mutate(exposure_days = exposure * 365)
policies %>% 
  select(accident_month, claim_loss_date) %>% 
  filter(!is.na(claim_loss_date)) %>%
  filter(month(claim_loss_date) != month(accident_month) |
  (year(claim_loss_date) != year(accident_month)))

