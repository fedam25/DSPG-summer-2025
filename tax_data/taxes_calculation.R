library(readxl)

hourly_data <- read_excel("hourly_wage.xlsx")

head(hourly_data)

# Create annual income columns
hourly_data$Annual_Single_Adult <- hourly_data$`Single Adult` * 2080
hourly_data$Annual_1Adult1Child <- hourly_data$`1 Adult, 1 Child` * 2080
hourly_data$Annual_1Adult1ChildCare <- hourly_data$`1 Adult, 1 in Child Care` * 2080
hourly_data$Annual_2Adults <- hourly_data$`2 Adults` * 2 * 2080
hourly_data$Annual_2Adults2Children <- hourly_data$`2 Adults, 2 Children` * 2 * 2080
hourly_data$Annual_2Adults2ChildCare <- hourly_data$`2 Adults, 2 in Child Care` * 2 * 2080
hourly_data$Annual_1Adult65 <- hourly_data$`Single Adult 65+` * 2080
hourly_data$Annual_2Adults65 <- hourly_data$`2 Adults 65+` * 2 * 2080


# Payroll Tax (Social Security + Medicare)
payroll_tax <- function(income) {
  ss_tax <- min(income, 160200) * 0.062  # Social Security cap for 2023
  medicare_tax <- income * 0.0145
  total_payroll <- ss_tax + medicare_tax
  return(total_payroll)
}

# Federal Income Tax
fed_tax <- function(income, filing_status = "single") {
  brackets <- c(0, 11000, 44725, 95375, 182100, 231250, 578125)
  rates <- c(0.10, 0.12, 0.22, 0.24, 0.32, 0.35, 0.37)
  deduction <- switch(filing_status,
                      "single" = 13850,
                      "married" = 27700,
                      "head" = 20800)
  taxable_income <- max(0, income - deduction)
  tax <- 0
  for (i in 1:(length(brackets) - 1)) {
    if (taxable_income > brackets[i]) {
      tax <- tax + (min(taxable_income, brackets[i + 1]) - brackets[i]) * rates[i]
    }
  }
  if (taxable_income > brackets[length(brackets)]) {
    tax <- tax + (taxable_income - brackets[length(brackets)]) * rates[length(rates)]
  }
  return(tax)
}

# Virginia State Income Tax
va_tax <- function(income, filing_status = "single") {
  deduction <- ifelse(filing_status == "married", 16000, 8000)
  taxable <- max(0, income - deduction)
  if (taxable <= 3000) {
    tax <- taxable * 0.02
  } else if (taxable <= 5000) {
    tax <- 60 + (taxable - 3000) * 0.03
  } else if (taxable <= 17000) {
    tax <- 120 + (taxable - 5000) * 0.05
  } else {
    tax <- 720 + (taxable - 17000) * 0.0575
  }
  return(tax)
}

# Child Tax Credit
child_tax_credit <- function(income, num_children) {
  total_ctc <- num_children * 2000
  refundable <- min(num_children * 1600, max(0, (income - 2500) * 0.15))
  non_refundable <- total_ctc - refundable
  return(list(total_ctc = total_ctc, refundable = refundable, non_refundable = non_refundable))
}



# Function for Single Adult
calculate_total_tax_single_adult <- function(income) {
  ptax <- payroll_tax(income)
  ftax <- fed_tax(income, "single")
  ctc <- child_tax_credit(income, 0)$non_refundable
  stax <- va_tax(income, "single")
  
  total <- ptax + ftax - ctc + stax
  return(total)
}

# Apply to dataset
hourly_data$Tax_Single_Adult <- sapply(hourly_data$Annual_Single_Adult, 
                                       calculate_total_tax_single_adult)

# Just to see if it works
head(hourly_data[, c("County", "Annual_Single_Adult", "Tax_Single_Adult")])


# Dependent Care Credit Function
dep_care_credit <- function(income, expenses, num_children) {
  # Step 1: Set maximum eligible expenses
  max_expense <- ifelse(num_children >= 2, 6000, 3000)
  eligible_expense <- min(expenses, max_expense)
  
  # Step 2: Calculate credit rate (20–35% based on income)
  rate <- if (income < 15000) {
    0.35
  } else if (income > 43000) {
    0.20
  } else {
    0.35 - 0.01 * floor((income - 15000) / 2000)
  }
  rate <- max(rate, 0.20)  # Floor the minimum to 20%
  
  # Step 3: Return credit
  credit <- eligible_expense * rate
  return(credit)
}

hourly_data$Tax_1Adult_1Child <- sapply(hourly_data$Annual_1Adult1Child, 
                                        calculate_total_tax_1adult_1child)

# 2 Adults
calculate_total_tax_1adult_1childcare <- function(income) {
  ptax <- payroll_tax(income)
  ftax <- fed_tax(income, "head")
  ctc <- child_tax_credit(income, 1)$non_refundable
  dcc <- dep_care_credit(income, 6000, 1)  # $6,000 expenses
  stax <- va_tax(income, "head")
  
  total <- ptax + ftax - ctc - dcc + stax
  return(total)
}

# Apply
hourly_data$Tax_1Adult_1ChildCare <- sapply(hourly_data$Annual_1Adult1ChildCare, 
                                            calculate_total_tax_1adult_1childcare)


# 2 Adults, 2 Children

calculate_total_tax_2adults_2children <- function(income) {
  ptax <- payroll_tax(income)
  ftax <- fed_tax(income, "married")
  ctc <- child_tax_credit(income, 2)$non_refundable
  dcc <- dep_care_credit(income, 6000, 2)
  stax <- va_tax(income, "married")
  
  total <- ptax + ftax - ctc - dcc + stax
  return(total)
}

# Apply
hourly_data$Tax_2Adults_2Children <- sapply(hourly_data$Annual_2Adults2Children, 
                                            calculate_total_tax_2adults_2children)


# 2 Adults, 2 in Child Care
calculate_total_tax_2adults_2childcare <- function(income) {
  ptax <- payroll_tax(income)
  ftax <- fed_tax(income, "married")
  ctc <- child_tax_credit(income, 2)$non_refundable
  dcc <- dep_care_credit(income, 7000, 2)
  stax <- va_tax(income, "married")
  
  total <- ptax + ftax - ctc - dcc + stax
  return(total)
}

# Apply
hourly_data$Tax_2Adults_2ChildCare <- sapply(hourly_data$Annual_2Adults2ChildCare, 
                                             calculate_total_tax_2adults_2childcare)


# 1 Adult (65+)

calculate_total_tax_1adult_65 <- function(income) {
  ptax <- payroll_tax(income)
  ftax <- fed_tax(income, "single")
  ctc <- child_tax_credit(income, 0)$non_refundable
  stax <- va_tax(income, "single")
  
  total <- ptax + ftax - ctc + stax
  return(total)
}

# Apply
hourly_data$Tax_1Adult_65 <- sapply(hourly_data$Annual_1Adult65, 
                                    calculate_total_tax_1adult_65)


# 2 Adults (65+)

calculate_total_tax_2adults_65 <- function(income) {
  ptax <- payroll_tax(income)
  ftax <- fed_tax(income, "married")
  ctc <- child_tax_credit(income, 0)$non_refundable
  stax <- va_tax(income, "married")
  
  total <- ptax + ftax - ctc + stax
  return(total)
}

# Apply
hourly_data$Tax_2Adults_65 <- sapply(hourly_data$Annual_2Adults65, 
                                     calculate_total_tax_2adults_65)

calculate_total_tax_2adults <- function(income) {
  ptax <- payroll_tax(income)
  ftax <- fed_tax(income, "married")
  ctc <- child_tax_credit(income, 0)$non_refundable
  stax <- va_tax(income, "married")
  
  total <- ptax + ftax - ctc + stax
  return(total)
}

# Apply the function to create the missing column
hourly_data$Tax_2Adults <- sapply(hourly_data$Annual_2Adults, calculate_total_tax_2adults)


write.csv(hourly_data, "final_virginia_tax_burden_by_county.csv", row.names = FALSE)

write.csv(hourly_data, "~/Desktop/tax_data/final_virginia_tax_burden_by_county.csv", row.names = FALSE)





# To clean up the document and finalize it

# Only the relevant columns I chose
tax_only <- hourly_data[, c(
  "County",
  "Tax_Single_Adult",
  "Tax_2Adults",
  "Tax_1Adult_1ChildCare",
  "Tax_2Adults_2Children",
  "Tax_1Adult_65",
  "Tax_2Adults_65"
)]

# Renamed the columns
colnames(tax_only) <- c(
  "County",
  "1 Adult (19–50)",
  "2 Adults (19–50)",
  "1 Adult + 1 Child (6–10)",
  "2 Adults + 2 Children (6–10)",
  "1 Adult (65+)",
  "2 Adults (65+)"
)

write.csv(tax_only, "cleaned_tax_burden_by_family_type.csv", row.names = FALSE)





