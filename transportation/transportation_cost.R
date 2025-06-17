library(tidyverse)

transport_data <- read_csv("transportation-Data.csv")

head(transport_data)

# Select and rename relevant columns
transport_cleaned <- transport_data %>%
  select(
    County = NAME,
    Total_Workers = B08101_001E,
    Car_Alone = B08101_009E,
    Carpool = B08101_010E,
    Public_Transit = B08101_017E
  )

# Calculate shares
transport_cleaned <- transport_cleaned %>%
  mutate(
    Car_Alone = as.numeric(Car_Alone),
    Carpool = as.numeric(Carpool),
    Public_Transit = as.numeric(Public_Transit),
    Total_Workers = as.numeric(Total_Workers),
    Car_Total = Car_Alone + Carpool,
    Car_Share = round(Car_Total / Total_Workers, 4),
    Transit_Share = round(Public_Transit / Total_Workers, 4)
  )


# Adding cost assumptions (can be adjusted later)
transport_cleaned <- transport_cleaned %>%
  mutate(
    Car_Cost = 9000,
    Transit_Cost = 900,
    Weighted_Transportation_Cost = round(Car_Share * Car_Cost + Transit_Share * Transit_Cost, 2)
  )

# Compute average vehicles per household
va_vehicles <- va_vehicles %>%
  mutate(
    total_vehicles = (owner_1E + renter_1E) * 1 +
      (owner_2E + renter_2E) * 2 +
      (owner_3plusE + renter_3plusE) * 3, 
    avg_vehicles_per_household = round(total_vehicles / totalE, 2)
  )

colnames(va_vehicles)



# Filter Out or Flag Counties with Low Car Access
va_vehicles <- va_vehicles %>%
  mutate(low_car_access = avg_vehicles_per_household < 1)




# Preview
head(transport_cleaned)

write_csv(transport_cleaned, "final_transportation_costs.csv")


# Create a new final file which contains the final cost

transport_va <- transport_cleaned %>%
  filter(str_detect(County, ", Virginia")) %>%
  select(
    County,
    Car_Share,
    Transit_Share
  )



# Add scaled_car_cost to your va_vehicles dataset
va_vehicles <- va_vehicles %>%
  mutate(
    scaled_car_cost = round(avg_vehicles_per_household * 9000, 2)
  )


va_vehicles_small <- va_vehicles %>%
  select(
    County = NAME,
    avg_vehicles_per_household,
    scaled_car_cost
  )


final_transport_table <- transport_va %>%
  left_join(va_vehicles_small, by = "County")

final_transport_table <- final_transport_table %>%
  mutate(
    Transit_Cost = 900,  # default transit cost — adjust later if needed
    Transportation_Cost = round(Car_Share * scaled_car_cost + Transit_Share * Transit_Cost, 2)
  )


head(final_transport_table)

# Save to CSV
write_csv(final_transport_table, "final_va_transportation_cost.csv")


# Transit costs
transit_costs <- tibble(
  County = c(
    "Arlington County, Virginia",
    "Richmond city, Virginia",
    "Norfolk city, Virginia",
    "Charlottesville city, Virginia",
    "Roanoke city, Virginia",
    "Lynchburg city, Virginia",
    "Danville city, Virginia",
    "Staunton city, Virginia",
    "Harrisonburg city, Virginia"
  ),
  Assigned_Transit_Cost = c(
    972,  # Arlington (WMATA/ART)
    0,    # Richmond (GRTC Zero Fare)
    600,  # Norfolk (HRT)
    0,    # Charlottesville (Zero Fare)
    360,  # Roanoke (Valley Metro)
    480,  # Lynchburg (GLTC)
    240,  # Danville (DMTS)
    120,  # Staunton (BRITE)
    0     # Harrisonburg (HPT - Free)
  )
)



final_transport_table <- final_transport_table %>%
  left_join(transit_costs, by = "County") %>%
  mutate(
    Transit_Cost = coalesce(Transit_Cost, 0),  # fallback to 0 if not in table
    Transportation_Cost = round(Car_Share * scaled_car_cost + Transit_Share * Transit_Cost, 2)
  )

write_csv(final_transport_table, "final_va_transportation_cost.csv")



# Add family structure multipliers
# Assumptions based on commuting patterns:
# - 1 adult = 1 commuter
# - 2 adults = 2 commuters
# - Adults 65+ = reduced travel (0.5 per person)
# - Children = no commute
# Based on AAA average cost of $9000/year per commuter

family_structure_weights <- tibble(
  `Family Structure` = c(
    "1 Adult (19–50)",
    "2 Adults (19–50)",
    "1 Adult, 1 Child",
    "2 Adults, 2 Children",
    "1 Adult 65+",
    "2 Adults 65+"
  ),
  Multiplier = c(1.0, 2.0, 1.0, 2.0, 0.5, 1.0)
)

# Calculate true monthly costs correctly
summary_table <- family_structure_weights %>%
  rowwise() %>%
  mutate(
    Average_Monthly_Cost = round(mean((final_transport_table$Transportation_Cost * Multiplier) / 12, na.rm = TRUE), 2),
    Minimum_Monthly_Cost = round(min((final_transport_table$Transportation_Cost * Multiplier) / 12, na.rm = TRUE), 2)
  )


print(summary_table)
write_csv(summary_table, "monthly_transportation_summary_by_family_structure.csv")




family_structure_weights <- tibble(
  Structure = c(
    "1 Adult (19–50)",
    "2 Adults (19–50)",
    "1 Adult, 1 Child",
    "2 Adults, 2 Children",
    "1 Adult 65+",
    "2 Adults 65+"
  ),
  Multiplier = c(1.0, 2.0, 1.0, 2.0, 0.5, 1.0)
)

# Apply these multipliers to each county's transportation cost
county_summary <- final_transport_table %>%
  select(County, Transportation_Cost) %>%
  mutate(
    `1 Adult (19–50)` = round((Transportation_Cost * 1.0) / 12, 2),
    `2 Adults (19–50)` = round((Transportation_Cost * 2.0) / 12, 2),
    `1 Adult, 1 Child` = round((Transportation_Cost * 1.0) / 12, 2),
    `2 Adults, 2 Children` = round((Transportation_Cost * 2.0) / 12, 2),
    `1 Adult 65+` = round((Transportation_Cost * 0.5) / 12, 2),
    `2 Adults 65+` = round((Transportation_Cost * 1.0) / 12, 2)
  )

print(head(county_summary))
write_csv(county_summary, "county_monthly_transportation_cost_by_family.csv")













