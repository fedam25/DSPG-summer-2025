library(readxl)
library(dplyr)
library(writexl)

# setwd("C:/Users/mohammadif/Documents/DSPG - VOL - summer - 2025/cost_of_living_dspg/cost_of_living_dspg/food-data")

# Load the data
# food_data <- read_excel("food_data_all_county.xlsx")

# Define multipliers for each family structure
multipliers <- c(
  "1 Adult: 19-50 Years" = 1.00,
  "2 Adults: 19-50 Years" = 1.60,
  "1 Adult 1 Child" = 1.45,
  "2 Adults 2 Children" = 2.10,
  "1 Adults 65+" = 0.85,
  "2 Adults 65+" = 1.40
)

# Apply multipliers to calculate costs per family structure
expanded_food_data <- food_data %>%
  rowwise() %>%
  mutate(
    `1 Adult: 19-50 Years (Min)` = `Minimum food cost` * multipliers["1 Adult: 19-50 Years"],
    `2 Adults: 19-50 Years (Min)` = `Minimum food cost` * multipliers["2 Adults: 19-50 Years"],
    `1 Adult 1 Child (Min)` = `Minimum food cost` * multipliers["1 Adult 1 Child"],
    `2 Adults 2 Children (Min)` = `Minimum food cost` * multipliers["2 Adults 2 Children"],
    `1 Adults 65+ (Min)` = `Minimum food cost` * multipliers["1 Adults 65+"],
    `2 Adults 65+ (Min)` = `Minimum food cost` * multipliers["2 Adults 65+"],
    
    `1 Adult: 19-50 Years (Avg)` = `Average food cost` * multipliers["1 Adult: 19-50 Years"],
    `2 Adults: 19-50 Years (Avg)` = `Average food cost` * multipliers["2 Adults: 19-50 Years"],
    `1 Adult 1 Child (Avg)` = `Average food cost` * multipliers["1 Adult 1 Child"],
    `2 Adults 2 Children (Avg)` = `Average food cost` * multipliers["2 Adults 2 Children"],
    `1 Adults 65+ (Avg)` = `Average food cost` * multipliers["1 Adults 65+"],
    `2 Adults 65+ (Avg)` = `Average food cost` * multipliers["2 Adults 65+"]
  ) %>%
  ungroup()

# Optional: Write to Excel
write.csv(expanded_food_data, "expanded_food_data.csv", row.names = FALSE)

