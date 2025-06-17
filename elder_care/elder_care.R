library(tidyverse)

setwd("C:/Users/mohammadif/Documents/DSPG - VOL - summer - 2025/elder_care") 

median_income <- read_csv("Median_Income.csv")

# Median income for Virginia
va_median_income <- 85200 

# Base monthly elder care cost for Virginia
base_elder_care_cost <- 6512

elder_care_cost <- median_income %>%
  mutate(
    Elder_Care_1_Adult_65 = round(base_elder_care_cost * (Median_Household_Income / va_median_income), 2),
    Elder_Care_2_Adults_65 = round(Elder_Care_1_Adult_65 * 1.8, 2)
  ) %>%
  select(County, Elder_Care_1_Adult_65, Elder_Care_2_Adults_65)

print(base_elder_care_cost)

write_csv(elder_care_cost, "Elder_Care_Cost.csv")


# Let's do the same for the minimum cost

min_elder_care_cost <- 1766  # Adult Day Health Care (monthly)

elder_care_min_cost <- median_income %>%
  mutate(
    Min_Elder_Care_1_Adult_65 = round(min_elder_care_cost * (Median_Household_Income / va_median_income), 2),
    Min_Elder_Care_2_Adults_65 = round(Min_Elder_Care_1_Adult_65 * 1.8, 2)
  ) %>%
  select(County, Min_Elder_Care_1_Adult_65, Min_Elder_Care_2_Adults_65)


write_csv(elder_care_min_cost, "County_Minimum_Elder_Care_Cost.csv")



setwd("C:/Users/mohammadif/Documents/DSPG - VOL - summer - 2025/cost_of_living_dspg")


