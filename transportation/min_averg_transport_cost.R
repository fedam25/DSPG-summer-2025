library(tidyverse)

county_costs <- read_csv("county_monthly_transportation_cost_by_family.csv")

# Calculate min and average across family structures for each county
county_summary <- county_costs %>%
  rowwise() %>%
  mutate(
    `Minimum Monthly Cost` = min(c_across(-County), na.rm = TRUE),
    `Average Monthly Cost` = round(mean(c_across(-County), na.rm = TRUE), 2)
  ) %>%
  ungroup() %>%
  select(County, `Minimum Monthly Cost`, `Average Monthly Cost`)

print(head(county_summary))
write_csv(county_summary, "county_transportation_cost_summary.csv")
