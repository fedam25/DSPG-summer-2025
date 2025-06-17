install.packages("tidycensus")
library(tidycensus)


census_api_key("5b6127ec792e1f49b49be6490650ce95a611b022", install = TRUE)
Sys.getenv("CENSUS_API_KEY")
readRenviron("~/.Renviron")

# poverty 

poverty <- get_acs(geography = "county", state = "VA", 
                   year = 2023, survey = "acs5", variables = "DP03_0119PE")

# Health insurance

health_insurance <- get_acs(geography = "county", state = "VA", 
                            year = 2023, survey = "acs5", variables = "DP03_0099PE")

# Population

total_population <- get_acs(geography = "county", state = "VA", 
                            year = 2023, survey = "acs5", variables = "DP02_0087PE")


# Gross rent

gross_rent <- get_acs(geography = "county", state = "VA", 
                            year = 2023, survey = "acs5", variables = "DP04_0126PE")

# Average H-size

average_household <- get_acs(geography = "county", state = "VA", 
                      year = 2023, survey = "acs5", variables = "DP02_0016")



# saving the data to an excel sheet

install.packages("openxlsx")
library(openxlsx)
setwd("C:/Users/mohammadif/Documents/DSPG - VOL - summer - 2025/dspg-data")

getwd()

# Save it to an excel

 write.xlsx(poverty, "poverty_data")
 write.xlsx(gross_rent, "rent_data")
 write.xlsx(average_household, "household_data")

 
 # Load vehicle ownership data for all VA counties
 va_vehicles <- get_acs(
   geography = "county",
   state = "VA",
   variables = c(
     total = "B25044_001",
     owner_0 = "B25044_003",
     owner_1 = "B25044_004",
     owner_2 = "B25044_005",
     owner_3plus = "B25044_006",
     renter_0 = "B25044_008",
     renter_1 = "B25044_009",
     renter_2 = "B25044_010",
     renter_3plus = "B25044_011"
   ),
   year = 2022,
   survey = "acs5",
   output = "wide"
 )
 
 
 
 
 
 
 
 
 
 


