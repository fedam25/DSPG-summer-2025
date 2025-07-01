library(readr)
library(dplyr)


transport_data <- read_csv("final_va_transportation_cost.csv")

# More realistic commuter multipliers for each family structure
multipliers <- c(
  "1 Adult (19-50)" = 1.0,
  "2 Adults (19-50)" = 2.0,
  "1 Adult, 1 Child" = 1.0,
  "2 Adults, 2 Children" = 2.0,
  "1 Adult 65+" = 0.3,    # less travel
  "2 Adults 65+" = 0.6    # both travel less
)

# Function to scale and get monthly costs
generate_costs <- function(df, multiplier_factor) {
  df %>%
    select(County, Transportation_Cost) %>%
    mutate(
      `1 Adult (19-50)` = round((Transportation_Cost * multiplier_factor * multipliers["1 Adult (19-50)"]) / 12, 2),
      `2 Adults (19-50)` = round((Transportation_Cost * multiplier_factor * multipliers["2 Adults (19-50)"]) / 12, 2),
      `1 Adult, 1 Child` = round((Transportation_Cost * multiplier_factor * multipliers["1 Adult, 1 Child"]) / 12, 2),
      `2 Adults, 2 Children` = round((Transportation_Cost * multiplier_factor * multipliers["2 Adults, 2 Children"]) / 12, 2),
      `1 Adult 65+` = round((Transportation_Cost * multiplier_factor * multipliers["1 Adult 65+"]) / 12, 2),
      `2 Adults 65+` = round((Transportation_Cost * multiplier_factor * multipliers["2 Adults 65+"]) / 12, 2)
    ) %>%
    select(-Transportation_Cost)
}

# Use more realistic scaling factors
min_costs <- generate_costs(transport_data, multiplier_factor = 0.85)  # MINIMUM
avg_costs <- generate_costs(transport_data, multiplier_factor = 1.00)  # AVERAGE

# Save to CSV
write_csv(min_costs, "final_minimum_transportation_cost1.csv")
write_csv(avg_costs, "final_average_transportation_cost1.csv")
