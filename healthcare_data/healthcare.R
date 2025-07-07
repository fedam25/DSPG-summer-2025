library(tidyverse)
library(readxl)

# Load the data
healthcare <- read_excel("VA Healthcare Costs.xlsx")

# Define base annual costs
avg_healthcare_base <- 9943.68  # From Medicare data
min_healthcare_base <- 6000     # Assumed survival-level baseline

# Convert to monthly
avg_healthcare_month <- avg_healthcare_base / 12
min_healthcare_month <- min_healthcare_base / 12

# Define scaling function
scale_healthcare <- function(base_cost, income, state_income, multiplier) {
  return(round(base_cost * (income / state_income) * multiplier, 2))
}

# Apply the scaling for each family structure
average_healthcare <- healthcare %>%
  mutate(
    `1 Adult: 19-50 Years` = scale_healthcare(avg_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.0),
    `2 Adults: 19-50 Years` = scale_healthcare(avg_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.8),
    `1 Adult, 1 Child` = scale_healthcare(avg_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.5),
    `2 Adults, 2 Children` = scale_healthcare(avg_healthcare_month, County_Median_Income, Virginia_Median_Income, 2.6),
    `1 Adult: 65+` = scale_healthcare(avg_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.1),
    `2 Adults: 65+` = scale_healthcare(avg_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.9)
  ) %>%
  select(County, `1 Adult: 19-50 Years`, `2 Adults: 19-50 Years`,
         `1 Adult, 1 Child`, `2 Adults, 2 Children`, 
         `1 Adult: 65+`, `2 Adults: 65+`)

minimum_healthcare <- healthcare %>%
  mutate(
    `1 Adult: 19-50 Years` = scale_healthcare(min_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.0),
    `2 Adults: 19-50 Years` = scale_healthcare(min_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.8),
    `1 Adult, 1 Child` = scale_healthcare(min_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.5),
    `2 Adults, 2 Children` = scale_healthcare(min_healthcare_month, County_Median_Income, Virginia_Median_Income, 2.6),
    `1 Adult: 65+` = scale_healthcare(min_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.1),
    `2 Adults: 65+` = scale_healthcare(min_healthcare_month, County_Median_Income, Virginia_Median_Income, 1.9)
  ) %>%
  select(County, `1 Adult: 19-50 Years`, `2 Adults: 19-50 Years`,
         `1 Adult, 1 Child`, `2 Adults, 2 Children`, 
         `1 Adult: 65+`, `2 Adults: 65+`)


write_csv(average_healthcare, "average_healthcare_cost.csv")
write_csv(minimum_healthcare, "minimum_healthcare_cost.csv")






