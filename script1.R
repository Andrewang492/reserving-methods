library(lubridate)
library(dplyr)

policies <- read.csv("ACTL31425110AssignmentData2022.csv")
policies$claim_loss_date <- ifelse(policies$claim_loss_date == "", NA, policies$claim_loss_date)

#strptime(policies[142,]$claim_loss_date, format="%Y-%m-%d %H:%M:%S+%z", tz="GMT")
#policies %>% select(claim_loss_date) %>% filter(!is.na(claim_loss_date))
policies$claim_loss_date <- ymd_hms(policies$claim_loss_date)
policies$term_start_date <- ymd_hms(policies$term_start_date)
policies$term_expiry_date <- ymd_hms(policies$term_expiry_date)
policies$accident_month <- ymd(policies$accident_month)

policies <- policies %>% mutate(exposure_days = exposure * 365)
#policies %>% 
#  select(accident_month, claim_loss_date) %>% 
#  filter(!is.na(claim_loss_date)) %>%
#  filter(month(claim_loss_date) != month(accident_month) |
#  (year(claim_loss_date) != year(accident_month)))

claims  <- policies %>% filter(!is.na(total_claims_cost) & !is.na(claim_loss_date))

#----  show that sum insured is always more than the total claims costs

policies %>% 
  mutate(total_claims_payment = ifelse(is.na(policies$total_claims_cost), 0, policies$total_claims_cost)) %>%
  select(policy_id, sum_insured, total_claims_payment) %>% 
  filter(total_claims_payment != 0) %>%
  group_by(policy_id, sum_insured) %>%
  summarise(total_claims = sum(total_claims_payment)) %>%
  filter(total_claims > sum_insured) # this code wrongly groups things.
  
# apparently not:
policies %>% 
  select(policy_id, sum_insured, total_claims_cost) %>% 
  filter(!is.na(total_claims_cost)) %>%
  filter(total_claims_cost > sum_insured) 

policies %>% 
  select(policy_id, sum_insured, total_claims_cost) %>% 
  filter(!is.na(total_claims_cost)) %>%
  filter(total_claims_cost<sum_insured) 


# policies exposure 0 (another claim in a month)
View(policies %>% filter(exposure == 0) %>% group_by(policy_id) %>% select(policy_id) %>% summarise(n= n()))

# policy each record data
# how much each policy paid and for what exposure length.
policies %>%
  mutate(total_claims_payment = ifelse(is.na(policies$total_claims_cost), 0, policies$total_claims_cost)) %>%
  group_by(policy_id) %>%
  summarise(total_payment = sum(total_claims_payment), total_exposure_days = sum(exposure_days), 
            start_date = min(term_start_date)) #assume policies are continuous (they are currently split up into years.)

# analysis on tenure
year.tenure.costs <- claims %>% 
  mutate(year = year(term_start_date)) %>%
  group_by(policy_tenure, year) %>%
  summarise(total_cost = sum(total_claims_cost))
library(tidyr)
runoff <- data.frame(pivot_wider(year.tenure.costs, names_from = policy_tenure, values_from = total_cost ))
rownames(runoff) <- runoff[,1]
runoff <- runoff[,-1]
