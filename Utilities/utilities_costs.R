# Load necessary libraries
library(readxl)
library(dplyr)
library(tidyr)


# Step 1: Read Excel file
utilities <- read_excel("utilities_costs.xlsx")

# Step 2: Clean column names
colnames(utilities) <- gsub("\t", "", colnames(utilities))
utilities <- utilities %>% filter(!is.na(County))

# Step 3: Filter only relevant family structures
relevant_rows <- utilities %>%
  filter(`Family Structure` %in% c(
    "1 Adult: 15-50 Years",
    "2 Adults",
    "1 Adult, 1 Child",
    "2 Adults, 2 Children",
    "Single Adult 65+",
    "2 Adults 65+"
  ))

# Step 4: Rename structures, including mapping 15–50 to 19–50
relevant_rows <- relevant_rows %>%
  mutate(Family = case_when(
    `Family Structure` == "1 Adult: 15-50 Years" ~ "1 Adult: 19-50 Years",
    `Family Structure` == "2 Adults" ~ "2 Adults: 19-50 Years",
    `Family Structure` == "1 Adult, 1 Child" ~ "1 Adult 1 Child",
    `Family Structure` == "2 Adults, 2 Children" ~ "2 Adults 2 Children",
    `Family Structure` == "Single Adult 65+" ~ "1 Adult 65+",
    `Family Structure` == "2 Adults 65+" ~ "2 Adults 65+"
  ))

# Step 5: Calculate total utility cost
relevant_rows <- relevant_rows %>%
  mutate(Total_Utility = Water + Electricity + `Trash / Garbage Collection`)

# Step 6: Reshape — each family type as a column
final_utilities <- relevant_rows %>%
  select(County, Family, Total_Utility) %>%
  pivot_wider(names_from = Family, values_from = Total_Utility) %>%
  mutate(Total = rowSums(across(where(is.numeric)), na.rm = TRUE)) %>%
  arrange(County)

# Step 7: View result
print(final_utilities)



library(writexl)

# Save the file to the specified directory
write_xlsx(
  final_utilities,
  path = "C:/Users/mohammadif/Documents/DSPG - VOL - summer - 2025/cost_of_living_dspg/cost_of_living_dspg/Utilities/final_utilities.xlsx"
)






