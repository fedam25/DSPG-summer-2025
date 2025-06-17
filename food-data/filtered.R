library(readxl)
library(dplyr)
library(writexl)

# --- Re-run the initial data loading and processing steps to get all_family_costs_filtered ---
# Set your working directory to where your files are located
setwd("C:/Users/mohammadif/Documents/DSPG - VOL - summer - 2025/food-data")

# Load the datasets (ensure these file names match your actual files and extensions)
average_food_costs <- read_excel("average_food_costs.xlsx - Sheet1.xlsx")
minimum_food_costs <- read_excel("minimum_food_costs.xlsx - Sheet1.xlsx")

# Clean and prepare dataframes for minimum food costs
minimum_food_costs_cleaned <- minimum_food_costs %>%
  filter(!grepl("Individuals3 Child|Reference Family", `Age-sex group`)) %>%
  filter(!is.na(`Age-sex group`) & `Age-sex group` != "Male:" & `Age-sex group` != "Female:")

# Rename and select columns for average food costs
average_food_costs_cleaned <- average_food_costs %>%
  rename(
    age_group = `Age-sex group`,
    monthly_low = `Monthly cost (Low-cost)`,
    monthly_moderate = `Monthly cost (Moderate-cost)`,
    monthly_liberal = `Monthly cost (Liberal)`
  ) %>%
  select(age_group, monthly_low, monthly_moderate, monthly_liberal)

# Rename and select columns for minimum food costs
minimum_food_costs_cleaned <- minimum_food_costs_cleaned %>%
  rename(
    age_group = `Age-sex group`,
    monthly_minimum = `Monthly cost`
  ) %>%
  select(age_group, monthly_minimum)

# Standardize age_group names for average food costs
average_food_costs_cleaned$age_group <- case_when(
  grepl("Child: 1 year", average_food_costs_cleaned$age_group) ~ "Child: 1 year",
  grepl("Child: 2-3 years", average_food_costs_cleaned$age_group) ~ "Child: 2-3 years",
  grepl("Child: 4-5 years", average_food_costs_cleaned$age_group) ~ "Child: 4-5 years",
  grepl("Child: 6-8 years", average_food_costs_cleaned$age_group) ~ "Child: 6-8 years",
  grepl("Child: 9-11 years", average_food_costs_cleaned$age_group) ~ "Child: 9-11 years",
  grepl("Male: 12-13 years", average_food_costs_cleaned$age_group) ~ "Male: 12-13 years",
  grepl("Male: 14-18 years", average_food_costs_cleaned$age_group) ~ "Male: 14-18 years",
  grepl("Male: 19-50 years", average_food_costs_cleaned$age_group) ~ "Male: 19-50 years",
  grepl("Male: 51-70 years", average_food_costs_cleaned$age_group) ~ "Male: 51-70 years",
  grepl("Male: 71+ years", average_food_costs_cleaned$age_group) ~ "Male: 71+ years",
  grepl("Female: 12-13 years", average_food_costs_cleaned$age_group) ~ "Female: 12-13 years",
  grepl("Female: 14-18 years", average_food_costs_cleaned$age_group) ~ "Female: 14-18 years",
  grepl("Female: 19-50 years", average_food_costs_cleaned$age_group) ~ "Female: 19-50 years",
  grepl("Female: 51-70 years", average_food_costs_cleaned$age_group) ~ "Female: 51-70 years",
  grepl("Female: 71+ years", average_food_costs_cleaned$age_group) ~ "Female: 71+ years",
  TRUE ~ average_food_costs_cleaned$age_group
)

# Standardize age_group names for minimum food costs
minimum_food_costs_cleaned$age_group <- case_when(
  grepl("1 year", minimum_food_costs_cleaned$age_group) ~ "Child: 1 year",
  grepl("2-3 years", minimum_food_costs_cleaned$age_group) ~ "Child: 2-3 years",
  grepl("4-5 years", minimum_food_costs_cleaned$age_group) ~ "Child: 4-5 years",
  grepl("6-8 years", minimum_food_costs_cleaned$age_group) ~ "Child: 6-8 years",
  grepl("9-11 years", minimum_food_costs_cleaned$age_group) ~ "Child: 9-11 years",
  grepl("Male: 12-13 years", minimum_food_costs_cleaned$age_group) ~ "Male: 12-13 years",
  grepl("Male: 14-19 years", minimum_food_costs_cleaned$age_group) ~ "Male: 14-18 years",
  grepl("Male: 20-50 years", minimum_food_costs_cleaned$age_group) ~ "Male: 19-50 years",
  grepl("Male: 51 - 70 years", minimum_food_costs_cleaned$age_group) ~ "Male: 51-70 years",
  grepl("Male: 71+ years", minimum_food_costs_cleaned$age_group) ~ "Male: 71+ years",
  grepl("Female: 12-13 years", minimum_food_costs_cleaned$age_group) ~ "Female: 12-13 years",
  grepl("Female: 14-19 years", minimum_food_costs_cleaned$age_group) ~ "Female: 14-18 years",
  grepl("Female: 20-50 years", minimum_food_costs_cleaned$age_group) ~ "Female: 19-50 years",
  grepl("Female: 51-70 years", minimum_food_costs_cleaned$age_group) ~ "Female: 51-70 years",
  grepl("Female: 71+ years", minimum_food_costs_cleaned$age_group) ~ "Female: 71+ years",
  TRUE ~ minimum_food_costs_cleaned$age_group
)

# Merge the cleaned food cost dataframes
food_costs <- left_join(average_food_costs_cleaned, minimum_food_costs_cleaned, by = "age_group")

# Function to calculate family costs based on individual age/sex group costs
calculate_family_cost <- function(family_structure, data) {
  total_low <- 0
  total_moderate <- 0
  total_liberal <- 0
  total_minimum <- 0
  
  for (member in names(family_structure)) {
    num_individuals <- family_structure[[member]]
    if (num_individuals > 0) {
      cost_data <- data %>% filter(age_group == member)
      if (nrow(cost_data) > 0) {
        total_low <- total_low + (cost_data$monthly_low * num_individuals)
        total_moderate <- total_moderate + (cost_data$monthly_moderate * num_individuals)
        total_liberal <- total_liberal + (cost_data$monthly_liberal * num_individuals)
        total_minimum <- total_minimum + (cost_data$monthly_minimum * num_individuals)
      } else {
        warning(paste("No cost data found for:", member))
      }
    }
  }
  return(data.frame(
    Low_Cost = total_low,
    Moderate_Cost = total_moderate,
    Liberal_Cost = total_liberal,
    Minimum_Cost = total_minimum
  ))
}

# Define the family structures for calculation
family_structures <- list(
  "1 Adult: 19-50 years" = list("Male: 19-50 years" = 1),
  "2 Adults: 19-50 years" = list("Male: 19-50 years" = 1, "Female: 19-50 years" = 1),
  "1 Child: 6-10" = list("Child: 6-8 years" = 1),
  "1 Adult 1 Child" = list("Male: 19-50 years" = 1, "Child: 6-8 years" = 1),
  "2 Adults 2 Children" = list("Male: 19-50 years" = 1, "Female: 19-50 years" = 1, "Child: 6-8 years" = 1, "Child: 9-11 years" = 1),
  "1 Adult: 65+" = list("Male: 71+ years" = 1),
  "2 Adults: 65+" = list("Male: 71+ years" = 1, "Female: 71+ years" = 1)
)

# Initialize dataframe to store all family costs
all_family_costs <- data.frame(
  Family_Structure = character(),
  Low_Cost = numeric(),
  Moderate_Cost = numeric(),
  Liberal_Cost = numeric(),
  Minimum_Cost = numeric(),
  stringsAsFactors = FALSE
)

# Calculate costs for each defined family structure
for (family_name in names(family_structures)) {
  cost <- calculate_family_cost(family_structures[[family_name]], food_costs)
  all_family_costs <- rbind(all_family_costs, data.frame(Family_Structure = family_name, cost))
}

# Filter out the 'Liberal_Cost' and 'Minimum_Cost' columns
all_family_costs_filtered <- all_family_costs %>%
  select(-Liberal_Cost, -Minimum_Cost)


# --- New Step: Load and Prepare County-Specific Food Cost Data ---
# Load the county-specific food data (assuming it's a CSV file)
county_food_data <- read_csv("food_data_all_county.csv")

# Clean Locality names by removing ", Virginia" and rename to 'County'
county_food_data <- county_food_data %>%
  mutate(Locality = gsub(", Virginia", "", Locality)) %>%
  rename(County = Locality)

# --- Determine a "National Baseline" from your filtered family costs ---
# Using the "2 Adults 2 Children" family as the baseline for scaling
national_baseline_costs <- all_family_costs_filtered %>%
  filter(Family_Structure == "2 Adults 2 Children")

national_base_low_cost <- national_baseline_costs$Low_Cost
national_base_moderate_cost <- national_baseline_costs$Moderate_Cost

# Check if baseline costs are valid to prevent division by zero or empty results
if (length(national_base_low_cost) == 0 || length(national_base_moderate_cost) == 0 ||
    national_base_low_cost == 0 || national_base_moderate_cost == 0) {
  stop("Could not find a valid baseline for '2 Adults 2 Children' family structure or costs are zero. Please check 'all_family_costs_filtered' data.")
}


# --- Calculate County-Specific Scaling Factors and Apply to all Family Structures ---
# This will create a row for each County and each Family_Structure combination.
final_county_family_costs <- county_food_data %>%
  rowwise() %>% # Process data row by row for each county
  mutate(
    # Calculate county-specific scaling factors based on national baseline
    min_scaling_factor = minimum_food_cost / national_base_low_cost,
    mod_scaling_factor = moderate_food_cost / national_base_moderate_cost
  ) %>%
  ungroup() %>% # Remove rowwise grouping for the next operation
  # Perform a cross-join with the filtered family costs to apply scaling
  cross_join(all_family_costs_filtered) %>%
  # Apply the calculated scaling factors to the family costs
  mutate(
    Low_Cost_Adjusted = Low_Cost * min_scaling_factor,
    Moderate_Cost_Adjusted = Moderate_Cost * mod_scaling_factor
  ) %>%
  # Select and reorder columns for clarity in the final output
  select(
    County,
    Family_Structure,
    Low_Cost_Adjusted,
    Moderate_Cost_Adjusted,
    minimum_food_cost_County_Ref = minimum_food_cost, # Rename for clarity (county's original min cost)
    moderate_food_cost_County_Ref = moderate_food_cost, # Rename for clarity (county's original moderate cost)
    national_base_low_cost_Used = national_base_low_cost, # Show the national baseline low cost used for scaling
    national_base_moderate_cost_Used = national_base_moderate_cost # Show the national baseline moderate cost used for scaling
  )

# Display the first few rows of the final result
print(head(final_county_family_costs))