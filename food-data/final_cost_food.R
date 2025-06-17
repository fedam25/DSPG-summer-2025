library(readxl)
library(writexl)

average_food_costs <- read_excel("average_food_costs.xlsx") 
minimum_food_costs <- read_excel("minimum_food_costs.xlsx") 


minimum_food_costs_cleaned <- minimum_food_costs %>%
  filter(!grepl("Individuals3 Child|Reference Family", `Age-sex group`)) %>%
  filter(!is.na(`Age-sex group`) & `Age-sex group` != "Male:" & `Age-sex group` != "Female:")

average_food_costs_cleaned <- average_food_costs %>%
  rename(
    age_group = `Age-sex group`,
    monthly_low = `Monthly cost (Low-cost)`,
    monthly_moderate = `Monthly cost (Moderate-cost)`,
    monthly_liberal = `Monthly cost (Liberal)`
  ) %>%
  select(age_group, monthly_low, monthly_moderate, monthly_liberal)

minimum_food_costs_cleaned <- minimum_food_costs_cleaned %>%
  rename(
    age_group = `Age-sex group`,
    monthly_minimum = `Monthly cost`
  ) %>%
  select(age_group, monthly_minimum)

# Standardize age_group names
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

food_costs <- left_join(average_food_costs_cleaned, minimum_food_costs_cleaned, by = "age_group")

# Function to calculate family costs
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

# Define family structures
family_structures <- list(
  "1 Adult: 19-50 years" = list("Male: 19-50 years" = 1),
  "2 Adults: 19-50 years" = list("Male: 19-50 years" = 1, "Female: 19-50 years" = 1),
  "1 Child: 6-10" = list("Child: 6-8 years" = 1),
  "1 Adult 1 Child" = list("Male: 19-50 years" = 1, "Child: 6-8 years" = 1),
  "2 Adults 2 Children" = list("Male: 19-50 years" = 1, "Female: 19-50 years" = 1, "Child: 6-8 years" = 1, "Child: 9-11 years" = 1),
  "1 Adult: 65+" = list("Male: 71+ years" = 1),
  "2 Adults: 65+" = list("Male: 71+ years" = 1, "Female: 71+ years" = 1)
)

# Initialize results dataframe
all_family_costs <- data.frame(
  Family_Structure = character(),
  Low_Cost = numeric(),
  Moderate_Cost = numeric(),
  Liberal_Cost = numeric(),
  Minimum_Cost = numeric(),
  stringsAsFactors = FALSE
)

all_family_costs_filtered <- all_family_costs %>%
  select(-Liberal_Cost, -Minimum_Cost)


# Calculate costs for each family structure and combine results
for (family_name in names(family_structures)) {
  cost <- calculate_family_cost(family_structures[[family_name]], food_costs)
  all_family_costs <- rbind(all_family_costs, data.frame(Family_Structure = family_name, cost))
}

print(all_family_costs)

write_xlsx(all_family_costs_filtered, path = "family_food_costs_summary.xlsx")





