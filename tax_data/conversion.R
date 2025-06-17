library(readr)

final_annual_tax_cost <- read_csv("final_annual_tax_cost.csv")

monthly_tax_cost <- final_annual_tax_cost

# Converting annual tax to monthly (divide columns 2 to end by 12)
monthly_tax_cost[, 2:ncol(monthly_tax_cost)] <- round(monthly_tax_cost[, 2:ncol(monthly_tax_cost)] / 12, 2)

# Renaming the columns to indicate they are monthly values
colnames(monthly_tax_cost)[-1] <- paste0("Monthly – ", colnames(final_annual_tax_cost)[-1])


head(monthly_tax_cost)

write_csv(monthly_tax_cost, "monthly_tax_cost_by_family_type.csv")
