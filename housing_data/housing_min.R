# Load required library
library(dplyr)
library(readr)

# Read your housing cost file
housing <- read.csv("Virginia_FMR_110.csv")

# Rename columns (adjust if necessary)
housing <- housing %>%
  rename(
    County = County,
    `1-Bed` = X1.Bed_110pct,
    `2-Bed` = X2.Bed_110pct,
    `3-Bed` = X3.Bed_110pct
  )

# Assign housing costs by family structure (same logic for both min and avg)
housing_summary <- housing %>%
  mutate(
    `1 Adult: 19–50 Years` = `1-Bed`,
    `2 Adults: 19–50 Years` = `1-Bed`,
    `1 Adult + 1 Child` = `2-Bed`,
    `2 Adults + 2 Children` = `3-Bed`,
    `1 Adult: 65+` = `1-Bed`,
    `2 Adults: 65+` = `1-Bed`
  ) %>%
  select(
    County,
    `1 Adult: 19–50 Years`,
    `2 Adults: 19–50 Years`,
    `1 Adult + 1 Child`,
    `2 Adults + 2 Children`,
    `1 Adult: 65+`,
    `2 Adults: 65+`
  ) %>%
  arrange(County)

# Save both as-is (they reflect different costs across counties)
write_csv(housing_summary, "minimum_housing_cost.csv")
write_csv(housing_summary, "average_housing_cost.csv")
