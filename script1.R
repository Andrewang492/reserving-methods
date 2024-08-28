library(lubridate)
library(dplyr)

policies <- read.csv("ACTL31425110AssignmentData2022.csv")
# Insert NA's for claim_loss_date
policies$claim_loss_date <- ifelse(policies$claim_loss_date == "", NA, policies$claim_loss_date)

#strptime(policies[142,]$claim_loss_date, format="%Y-%m-%d %H:%M:%S+%z", tz="GMT")
#policies %>% select(claim_loss_date) %>% filter(!is.na(claim_loss_date))
# Change variables to date objects.
policies$claim_loss_date <- ymd_hms(policies$claim_loss_date)
policies$term_start_date <- ymd_hms(policies$term_start_date)
policies$term_expiry_date <- ymd_hms(policies$term_expiry_date)
policies$accident_month <- ymd(policies$accident_month)
# Add Exposure_days field.
policies <- policies %>% mutate(exposure_days = exposure * 365)
#policies %>% 
#  select(accident_month, claim_loss_date) %>% 
#  filter(!is.na(claim_loss_date)) %>%
#  filter(month(claim_loss_date) != month(accident_month) |
#  (year(claim_loss_date) != year(accident_month)))

# Claims is wherever a claim is made. Assume 0 value claims are not claims.
claims  <- policies %>% filter(!is.na(total_claims_cost) & !is.na(claim_loss_date) & total_claims_cost != 0)

# ---- some observations:
# show that sum insured is always more than the total claims costs
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


# policies exposure 0 (another claim in a single month)
View(policies %>% filter(exposure == 0) %>% group_by(policy_id) %>% select(policy_id) %>% summarise(n= n()))

# policy each record data
# how much each policy paid and for what exposure length.
policies %>%
  mutate(total_claims_payment = ifelse(is.na(policies$total_claims_cost), 0, policies$total_claims_cost)) %>%
  group_by(policy_id) %>%
  summarise(total_payment = sum(total_claims_payment), total_exposure_days = sum(exposure_days), 
            start_date = min(term_start_date)) #assume policies are continuous (they are currently split up into years.)

# analysis on Total costs for a given tenure for a policy origination year.
year.tenure.costs <- claims %>% 
  mutate(year = year(term_start_date)) %>%
  group_by(policy_tenure, year) %>%
  summarise(total_cost = sum(total_claims_cost))
# then try making a runoff on this data.
library(tidyr)
runoff <- data.frame(pivot_wider(year.tenure.costs, names_from = policy_tenure, values_from = total_cost ))
rownames(runoff) <- runoff[,1]
runoff <- runoff[,-1]

# ---- Claim Loss forecasting attempt !!!!!!!!!!!!!!!!!
# For a given policy, assume that all 'claims' are actually just losses, and that there is only one accident.
# Find the first accident year and use that as the 'first' of the 'tenure' (development).

origination_dates <- claims %>%
  group_by(policy_id) %>%
  mutate(origination_date = min(claim_loss_date)) %>%
  mutate(development_year = year(claim_loss_date) - year(origination_date)+1) %>%
  select(policy_id, origination_date, development_year, total_claims_cost)
origination_dates %>% filter(development_year > 5) # maximum is only 6 years development.
origination_dates  %>% filter( development_year == 4) %>% filter(origination_date > dmy("01-01-2019")) # as expected, no 4 year developments go from 2019 to 2023 (since the data is taken at 2022)

runoff.table <- origination_dates %>% group_by(year(origination_date), development_year) %>% summarise(total_losses = sum(total_claims_cost))
runoff.triangle <- data.frame(pivot_wider(runoff.table, names_from = development_year, values_from = total_losses ))


# ---- Chain ladder method
# cumulative triangle
years <- 6
runoff.triangle.cumul <- runoff.triangle
for (i in seq(1,years)) { #i represents row
  for (j in seq(1,years)) {
    print(runoff.triangle[i,2:(j+1)])
    runoff.triangle.cumul[i,(j+1)] <- sum(runoff.triangle[i,2:(j+1)])
  }
}

#factors
factors <- rep(1, years-1)
for(i in seq(1,years-1)) {
  factor <- sum(runoff.triangle.cumul[1:(years-i), i+2]) / sum(runoff.triangle.cumul[1:(years-i), i+1]) #i+1 because first column is years.
  factors[i] <- factor
}
factors

# now forecast
reserves <- matrix(rep(0,years*years), nrow=years, ncol=years)
for (i in seq(1,years)) { #i represents row
  for (j in seq(2,years)) { # start considering from column 2; obviously the first column is entered and does need projection.
    if (is.na(runoff.triangle[i,(j+1)])) { # then we need to calculate reserves
      # get a 'previous' observation, then multiply
      if (is.na(runoff.triangle[i,j])) {
        # must get from the recently calculatedreserves
        reserves[i,j] =  reserves[i, (j-1)] * factors[j-1]
      } else {
        # otherwise multiply to the previous development year's observation
        reserves[i,j] = runoff.triangle.cumul[i,j] * factors[j-1]
      }
    }
  }
}

# The answer:
sum(reserves)

