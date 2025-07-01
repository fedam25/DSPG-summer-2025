library(readr)
library(dplyr)

# Step 1: Load the raw CSV
fmr <- read_csv("Virginia Fair Market Rents.csv")

# Step 2: Convert rent columns safely to numbers
fmr_avg <- fmr %>%
  rename(
    County = County,
    `1-Bed` = `1-Bed`,
    `2-Bed` = `2-Bed`,
    `3-Bed` = `3-Bed`
  ) %>%
  mutate(
    `1-Bed` = round(parse_number(`1-Bed`) * 1.25, 2),
    `2-Bed` = round(parse_number(`2-Bed`) * 1.25, 2),
    `3-Bed` = round(parse_number(`3-Bed`) * 1.25, 2)
  )

# Step 3: Map to family structures
average_housing_cost <- fmr_avg %>%
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

# Step 4: Save to CSV
write_csv(average_housing_cost, "average_housing_cost.csv")
