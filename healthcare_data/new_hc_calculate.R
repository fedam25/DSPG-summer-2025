install.packages("readxl")
library(readxl)
library(dplyr)
# Replace with your actual paths

meps <- read_excel("C:\\Users\\mohammadif\\Documents\\DSPG - VOL - summer - 2025\\cost_of_living_dspg\\cost_of_living_dspg\\healthcare_data\\508CompliantExport-for-MEPSICDashoardData_Civilian-20250422.xlsx")
meps <- read_excel("C:\\Users\\mohammadif\\Documents\\DSPG - VOL - summer - 2025\\cost_of_living_dspg\\cost_of_living_dspg\\healthcare_data\\508CompliantExport-for-MEPSICDashoardData_PrivateNatl-20250422.xlsx")
meps <- read_excel("C:\\Users\\mohammadif\\Documents\\DSPG - VOL - summer - 2025\\cost_of_living_dspg\\cost_of_living_dspg\\healthcare_data\\508CompliantExport-for-MEPSICDashoardData_Public-20250422.xlsx")

bls <- read_excel("C:\\Users\\mohammadif\\Documents\\DSPG - VOL - summer - 2025\\cost_of_living_dspg\\cost_of_living_dspg\\healthcare_data\\cu-all-multi-year-2021-2023.xlsx", sheet = 1)  # change sheet if needed
# Filter for Virginia
meps_va <- meps %>%
  filter(`State Name` == "Virginia")

# Extract 2023 single coverage (minimum)
min_premium <- meps_va %>%
  filter(grepl("Single", `Coverage Type`, ignore.case = TRUE),
         !is.na(`Average Premium`)) %>%
  summarise(avg_premium = mean(`Average Premium`, na.rm = TRUE)) %>%
  pull(avg_premium)

# Extract 2023 family coverage (average)
avg_premium <- meps_va %>%
  filter(grepl("Family", `Coverage Type`, ignore.case = TRUE),
         !is.na(`Average Premium`)) %>%
  summarise(avg_premium = mean(`Average Premium`, na.rm = TRUE)) %>%
  pull(avg_premium)
# Clean item names to lower case
bls_clean <- bls %>%
  mutate(Item = tolower(Item))

# Select relevant rows
oop_items <- c("medical supplies", "drugs", "medical services")
bls_oop <- bls_clean %>%
  filter(Item %in% oop_items) %>%
  select(Item, `All Consumer Units`)

# Sum total out-of-pocket cost (annual)
oop_total <- sum(as.numeric(bls_oop$`All Consumer Units`), na.rm = TRUE)

# Annual cost = premium + out-of-pocket
min_healthcare_annual <- min_premium + oop_total
avg_healthcare_annual <- avg_premium + oop_total

# Monthly cost
min_healthcare_monthly <- min_healthcare_annual / 12
avg_healthcare_monthly <- avg_healthcare_annual / 12

# Print results
cat("✅ Minimum Healthcare Cost (monthly): $", round(min_healthcare_monthly, 2), "\n")
cat("✅ Average Healthcare Cost (monthly): $", round(avg_healthcare_monthly, 2), "\n")

