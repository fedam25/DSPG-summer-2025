library(shiny)
library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(readxl)

options(tigris_use_cache = TRUE)

# --- Core App Setup and Data Loading ---
va_counties <- counties(state = "VA", cb = TRUE, class = "sf")
virginia_county_names <- sort(unique(va_counties$NAME))

# Define the lists that structure the dashboard's table and data processing.
family_structures_list <- c(
  "1 Adult: 19–50 Years",
  "2 Adults: 19–50 Years",
  "1 Adult + 1 Child",
  "2 Adults + 2 Children",
  "1 Adult: 65+",
  "2 Adults: 65+"
)

cost_variables_list <- c(
  "Housing", "Food", "Transportation", "Taxes", "Healthcare",
  "Childcare", "Technology", "Elder Care", "Utilities",
  "Miscellaneous"
)

# --- Load and Process REAL Data from Cleaned CSV files ---

# Utilities data
min_utilities_raw <- read_csv("minimum_final_utilities_cleaned.csv")
avg_utilities_raw <- read_csv("average_final_utilities_cleaned.csv")

# Elder Care data
min_elder_care_raw <- read_csv("minimum_elder_care_cost.csv")
avg_elder_care_raw <- read_csv("average_elder_care_cost.csv")

# Transportation data
min_transportation_raw <- read_csv("minimum_transportation_data.csv")
avg_transportation_raw <- read_csv("average_transportation_data.csv")

# Technology data
min_technology_raw <- read_csv("minimum_technology_costs.csv")
avg_technology_raw <- read_csv("average_technology_costs.csv")

# Food data
min_food_raw <- read_csv("final_minimum_food_data.csv")
avg_food_raw <- read_csv("final_average_food_data.csv")

# Tax data
min_tax_raw <- read_csv("minimum_tax_cost.csv")
avg_tax_raw <- read_csv("average_tax_cost.csv")

# Childcare data
min_childcare_raw <- read_csv("childcare_minimum_cost.csv")
avg_childcare_raw <- read_csv("childcare_average_cost.csv")

# Housing data
min_housing_raw <- read_csv("minimum_housing_cost.csv")
avg_housing_raw <- read_csv("average_housing_cost.csv")


# Function to standardize column names, ensuring they match the app's internal lists.
standardize_cols <- function(df) {
  df %>%
    rename_with(~"1 Adult: 19–50 Years", .cols = matches("1 Adult.*19-50.*")) %>%
    rename_with(~"2 Adults: 19–50 Years", .cols = matches("2 Adults.*19-50.*")) %>%
    rename_with(~"1 Adult + 1 Child", .cols = matches("1 Adult.*1 Child")) %>%
    rename_with(~"2 Adults + 2 Children", .cols = matches("2 Adults.*2 Children")) %>%
    rename_with(~"1 Adult: 65+", .cols = matches("1 Adult.*65+")) %>%
    rename_with(~"2 Adults: 65+", .cols = matches("2 Adults.*65+")) %>%
    # Clean the 'County' column for reliable matching.
    mutate(County = as.character(trimws(County)))
}

# Function to handle missing 'Total Monthly Cost' column
process_data <- function(df) {
  df_std <- df %>%
    standardize_cols()
  
  # Make sure the column "2 Adults + 2 Children" is numeric before we use it
  if ("2 Adults + 2 Children" %in% names(df_std)) {
    df_std <- df_std %>% mutate(`2 Adults + 2 Children` = as.numeric(`2 Adults + 2 Children`))
  }
  
  # Check if 'Total Monthly Cost' column exists.
  if (!"Total Monthly Cost" %in% names(df_std)) {
    # If not, create it. We'll assume it should be the value for a representative family.
    if ("2 Adults + 2 Children" %in% names(df_std)) {
      df_processed <- df_std %>%
        mutate(`Total Monthly Cost` = `2 Adults + 2 Children`)
    } else {
      
      df_processed <- df_std %>%
        mutate(`Total Monthly Cost` = 0)
    }
  } else {
    df_processed <- df_std
  }
  
  # Make sure all cost-related columns that are present are numeric
  all_cols_to_check <- c(family_structures_list, "Total Monthly Cost")
  present_cols <- intersect(all_cols_to_check, names(df_processed))
  
  df_final <- df_processed %>%
    mutate(across(all_of(present_cols), as.numeric))
  
  return(df_final)
}

# Process all data files
min_utilities_data <- process_data(min_utilities_raw)
avg_utilities_data <- process_data(avg_utilities_raw)
min_elder_care_data <- process_data(min_elder_care_raw)
avg_elder_care_data <- process_data(avg_elder_care_raw)
min_transportation_data <- process_data(min_transportation_raw)
avg_transportation_data <- process_data(avg_transportation_raw)
min_technology_data <- process_data(min_technology_raw)
avg_technology_data <- process_data(avg_technology_raw)
min_food_data <- process_data(min_food_raw)
avg_food_data <- process_data(avg_food_raw)
min_tax_data <- process_data(min_tax_raw)
avg_tax_data <- process_data(avg_tax_raw)
min_childcare_data <- process_data(min_childcare_raw)
avg_childcare_data <- process_data(avg_childcare_raw)
# *** NEW: Process Housing data ***
min_housing_data <- process_data(min_housing_raw)
avg_housing_data <- process_data(avg_housing_raw)


# --- Create Unified Data Sources ---

# 1. Tidy data source for the detailed TABLE and BAR GRAPH
all_costs_long_for_table_raw <- bind_rows(
  min_utilities_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Utilities", Type = "min"),
  avg_utilities_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Utilities", Type = "avg"),
  min_elder_care_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Elder Care", Type = "min"),
  avg_elder_care_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Elder Care", Type = "avg"),
  min_transportation_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Transportation", Type = "min"),
  avg_transportation_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Transportation", Type = "avg"),
  min_technology_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Technology", Type = "min"),
  avg_technology_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Technology", Type = "avg"),
  min_food_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Food", Type = "min"),
  avg_food_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Food", Type = "avg"),
  min_tax_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Taxes", Type = "min"),
  avg_tax_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Taxes", Type = "avg"),
  min_childcare_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Childcare", Type = "min"),
  avg_childcare_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Childcare", Type = "avg"),
  min_housing_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Housing", Type = "min"),
  avg_housing_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Housing", Type = "avg")
)

all_costs_long_for_table <- all_costs_long_for_table_raw %>%
  group_by(County, FamilyStructure, CostVariable, Type) %>%
  summarise(Cost = mean(Cost, na.rm = TRUE), .groups = 'drop')


# 2. Prepare Data for the MAP (Summing Total Costs from all files)
min_cost_dfs <- list(
  min_utilities_data %>% select(County, Cost_Utilities = `Total Monthly Cost`),
  min_elder_care_data %>% select(County, Cost_ElderCare = `Total Monthly Cost`),
  min_transportation_data %>% select(County, Cost_Transportation = `Total Monthly Cost`),
  min_technology_data %>% select(County, Cost_Technology = `Total Monthly Cost`),
  min_food_data %>% select(County, Cost_Food = `Total Monthly Cost`),
  min_tax_data %>% select(County, Cost_Taxes = `Total Monthly Cost`),
  min_childcare_data %>% select(County, Cost_Childcare = `Total Monthly Cost`),
  min_housing_data %>% select(County, Cost_Housing = `Total Monthly Cost`)
)

avg_cost_dfs <- list(
  avg_utilities_data %>% select(County, Cost_Utilities = `Total Monthly Cost`),
  avg_elder_care_data %>% select(County, Cost_ElderCare = `Total Monthly Cost`),
  avg_transportation_data %>% select(County, Cost_Transportation = `Total Monthly Cost`),
  avg_technology_data %>% select(County, Cost_Technology = `Total Monthly Cost`),
  avg_food_data %>% select(County, Cost_Food = `Total Monthly Cost`),
  avg_tax_data %>% select(County, Cost_Taxes = `Total Monthly Cost`),
  avg_childcare_data %>% select(County, Cost_Childcare = `Total Monthly Cost`),
  avg_housing_data %>% select(County, Cost_Housing = `Total Monthly Cost`)
)

total_min_costs <- min_cost_dfs %>%
  reduce(full_join, by = "County") %>%
  pivot_longer(-County, names_to = "Cost_Category", values_to = "Cost_Value") %>%
  group_by(NAME = County) %>%
  summarise(Cost = sum(Cost_Value, na.rm = TRUE), .groups = 'drop')

total_avg_costs <- avg_cost_dfs %>%
  reduce(full_join, by = "County") %>%
  pivot_longer(-County, names_to = "Cost_Category", values_to = "Cost_Value") %>%
  group_by(NAME = County) %>%
  summarise(Cost = sum(Cost_Value, na.rm = TRUE), .groups = 'drop')

va_map_data_min <- left_join(va_counties, total_min_costs, by = "NAME")
va_map_data_avg <- left_join(va_counties, total_avg_costs, by = "NAME")


# --- UI Definition ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Basic page styling */
      .custom-header { background-color: #001f3f; padding: 30px 20px; margin-bottom: 20px; text-align: center; border-radius: 10px; }
      .custom-header h1 { color: white; font-size: 38px; font-weight: bold; margin: 0; }
      .intro-text, .project-intro, .about-section, .future-text-section { font-size: 17px; margin-bottom: 20px; padding: 15px; background-color: #f8f8f8; border-radius: 10px; border: 1px solid #e0e0e0; }
      .future-text-section { margin-top: 30px; }
      .section-title { font-size: 24px; font-weight: bold; margin-top: 30px; margin-bottom: 10px; color: #001f3f; }
      .section-desc { font-size: 16px; margin-bottom: 20px; color: #555; }
      .about-variable-item { margin-bottom: 15px; padding-left: 20px; border-left: 3px solid #001f3f; }
      .about-variable-item h4 { margin-top: 0; margin-bottom: 5px; color: #333; }
      .about-variable-item p { font-size: 16px; line-height: 1.5; }
      .nav-tabs > li > a { background-color: #e9ecef; color: #001f3f; font-weight: bold; border-top-left-radius: 8px; border-top-right-radius: 8px; margin-right: 5px; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover { color: white; background-color: #007bff; border-color: #007bff; }
      .content-container { max-width: 95%; margin: 0 auto; padding: 0 15px; }
      .shiny-plot-output, .leaflet-container, .plotly { border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); width: 100% !important; }

      /* Table Container Styling */
      .table-container { margin: 20px 0; border-radius: 10px; overflow: hidden; box-shadow: 0 4px 8px rgba(0,0,0,0.1); background-color: white; }
      table.data { width: 100%; border-collapse: collapse; margin: 0; font-size: 15px; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      table.data td { padding: 12px; border-bottom: 1px solid #e2e8f0; transition: background-color 0.2s ease; }
      table.data tbody tr:nth-child(even) { background-color: #f8fafc; }
      table.data tbody tr:hover { background-color: #f1f5f9; transform: translateY(-1px); box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      
      /* Table Header (First Row) Styling */
      table.data th { background-color: #2b6cb0; color: white; font-weight: bold; padding: 15px 12px; text-align: left; border: none; font-size: 16px; }
      table.data th:first-child { border-top-left-radius: 10px; }
      table.data th:last-child { border-top-right-radius: 10px; }
      
      /* Table First Column Styling */
      table.data td:first-child { background-color: #EBF8FF; font-weight: bold; color: #1a365d; border-right: 2px solid #3182ce; position: relative; }
      table.data td:first-child::before { content: ''; position: absolute; left: 0; top: 0; bottom: 0; width: 4px; background: linear-gradient(to bottom, #3182ce, #2c5282); }
      table.data tbody tr:hover td:first-child { background-color: #BEE3F8; }
      
      /* Special styling for the 'Total Cost' row */
      table.data tbody tr:last-child td { border-bottom: none; }
      table.data tbody tr:last-child { background-color: #f0fff4; font-weight: bold; border-top: 2px solid #38a169; }
      table.data tbody tr:last-child td { color: #22543d; font-size: 16px; font-weight: bold; }
      table.data tbody tr:last-child td:first-child { background-color: #2f855a; color: white; }
      
      /* Map Popup Styling */
      .leaflet-popup-content-wrapper { border-radius: 8px; padding: 10px; }
      .leaflet-popup-content { margin: 8px 12px; line-height: 1.5; }
      .map-popup-title { font-weight: bold; font-size: 16px; margin-bottom: 5px; color: #001f3f; }
      .map-popup-value { font-size: 14px; color: #333; }
    "))
  ),
  
  div(class = "custom-header",
      h1("Virginia Cost of Living")
  ),
  
  mainPanel(
    width = 12,
    tabsetPanel(
      id = "main_tabs",
      selected = "Introduction", 
      
      tabPanel("Introduction",
               div(class = "content-container",
                   div(class = "about-section",
                       h2("About Our Project"),
                       p("This dashboard was developed as part of the Virginia Tech Data Science for the Public Good (DSPG) Summer Research Program."),
                       p("Its primary purpose is to provide data-driven insights into the cost of living across all counties and cities in Virginia. Our goal is to empower citizens, policymakers, and researchers with accessible information to better understand financial landscapes and make informed decisions."),
                       p("This tool includes both minimum and average cost estimates, breaking down expenses into key categories for various common family types."),
                       div(class = "section-title", "Why is This Important?"),
                       tags$ul(
                         tags$li("For individuals and families, it helps in financial planning, budgeting, and making decisions about where to live."),
                         tags$li("For policymakers, it provides data to develop effective social programs, minimum wage policies, and affordable housing initiatives."),
                         tags$li("For businesses, it can inform decisions about employee compensation and location planning."),
                         tags$li("For researchers, it offers a robust dataset for studying economic disparities and well-being across the state.")
                       ),
                       div(class = "section-title", "Minimum vs Average Cost"),
                       p("In this dashboard, you’ll see both 'Minimum Cost' and 'Average Cost' estimates for each location in Virginia. But what do they really mean?"),
                       p("The Minimum Cost is based on a survival budget, it reflects the lowest possible expenses needed to cover basic needs like housing, food, healthcare, and transportation. This is often used to understand what it takes to just get by, without any extras."),
                       p("The Average Cost, on the other hand, reflects a more typical lifestyle. It includes the average amount people actually spend on the same categories, which can vary depending on where they live and how much they earn."),
                       p("We separated these two to help users compare different standards of living. You can switch between the 'Minimum Cost' and 'Average Cost' tabs to see how costs change, and what a basic vs. average lifestyle might look like in different parts of Virginia."),
                       
                       div(class = "section-title", "How to Use This Dashboard"),
                       tags$ol(
                         tags$li("Start on the 'Introduction' page for an overview of our project, why it's important, and how to use this tool."),
                         tags$li("Navigate to the 'Methodology' tab to understand how we calculate costs and the sources we use."),
                         tags$li("Click the 'Minimum Cost' or 'Average Cost' tab to explore the data."),
                         tags$li("On these tabs, you will find separate controls for the table and the bar chart, allowing you to compare different scenarios."),
                         tags$li("The Map displays total costs across Virginia and operates independently.")
                       ),
                       div(class = "section-title", "Acknowledgement"),
                       tags$ul(
                         tags$li("This dashboard was developed by Feda Mohammadi and Julia Vecharello as part of the Virginia Tech Data Science for the Public Good (DSPG) Summer Research Program in Summer 2025. We extend our sincere gratitude to the DSPG program for providing this valuable opportunity to contribute to public understanding through data science:)")
                       )
                   )
               )
      ),
      tabPanel("Minimum Cost",
               div(class = "content-container",
                   div(class = "intro-text", h4("What is Minimum Cost?"), p("The Minimum Cost represents a survival budget. It covers only the most essential expenses required to maintain a basic standard of living in a given county or city. This estimate does not include discretionary spending or savings.")),
                   div(class = "section-title", "Minimum Cost Table"),
                   selectInput("county_min_table", "Select County or City for Table:", choices = virginia_county_names, selected = virginia_county_names[1]),
                   div(class = "section-desc", "This table shows the monthly minimum cost by category for all family types in the selected county."),
                   div(class = "table-container", tableOutput("min_table")),
                   div(class = "section-title", "Interactive County Map"),
                   div(class = "section-desc", "This map displays the total minimum monthly cost for a representative family."),
                   leafletOutput("min_map", height = 450),
                   div(class = "section-title", "Cost Breakdown Bar Chart"),
                   p("This graph shows the monthly minimum cost breakdown for a selected family structure and location. Use the dropdown menus below to customize the view."),
                   fluidRow(
                     column(6, selectInput("county_min_plot", "Select County or City for Graph:", choices = virginia_county_names, selected = virginia_county_names[1])),
                     column(6, selectInput("family_structure_min", "Select Family Structure:", choices = family_structures_list, selected = family_structures_list[4]))
                   ),
                   plotlyOutput("min_plot", height = 350),
                   div(class = "future-text-section", h4("Additional"), p("This section is reserved for future analysis..."))
               )
      ),
      tabPanel("Average Cost",
               div(class = "content-container",
                   div(class = "intro-text", h4("What is Average Cost?"), p("The Average Cost estimate reflects typical expenses of average households, going beyond just survival needs. It includes a more comfortable standard of living, allowing for some discretionary spending, savings, and a wider range of goods and services.")),
                   div(class = "section-title", "Average Cost Table"),
                   selectInput("county_avg_table", "Select County or City for Table:", choices = virginia_county_names, selected = virginia_county_names[1]),
                   div(class = "section-desc", "This table shows the monthly average cost by category for all family types in the selected county."),
                   div(class = "table-container", tableOutput("avg_table")),
                   div(class = "section-title", "Interactive County Map"),
                   div(class = "section-desc", "This map displays the total average monthly cost for a representative family."),
                   leafletOutput("avg_map", height = 450),
                   div(class = "section-title", "Cost Breakdown Bar Chart"),
                   p("This graph shows the monthly average cost breakdown for a selected family structure and location. Use the dropdown menus below to customize the view."),
                   fluidRow(
                     column(6, selectInput("county_avg_plot", "Select County or City for Graph:", choices = virginia_county_names, selected = virginia_county_names[1])),
                     column(6, selectInput("family_structure_avg", "Select Family Structure:", choices = family_structures_list, selected = family_structures_list[4]))
                   ),
                   plotlyOutput("avg_plot", height = 350),
                   div(class = "future-text-section", h4("Additional"), p("This section is reserved for future analysis..."))
               )
      ),
      tabPanel("Methodology",
               div(class = "content-container",
                   div(class = "about-section",
                       div(class = "section-title", "Our Methodology"),
                       p("Our methodology involves compiling data from various sources to estimate the costs associated with essential goods and services. We categorize expenses to provide a comprehensive view of living costs. We differentiate between 'Minimum Cost' and 'Average Cost' to reflect different standards of living."),
                       p("The data is collected for each county and independent city in Virginia and processed to align with the family structures defined in the next section."),
                       
                       div(class = "section-title", "Our Variables"),
                       tags$ol(
                         tags$li(div(class = "about-variable-item", h4("Housing"), p("This variable represents the monthly cost associated with housing, including rent or mortgage payments, and basic maintenance. It is calculated based on median rental costs and homeownership expenses specific to each county/city, adjusted for family size and type of dwelling."))),
                         tags$li(div(class = "about-variable-item", h4("Food"), p("Food costs cover the typical monthly expenses for groceries and meals. This is calculated using average food prices for common items, considering the nutritional needs and dietary patterns for different age groups and family structures. It accounts for both at-home consumption and a small allowance for eating out."))),
                         tags$li(div(class = "about-variable-item", h4("Transportation"), p("Transportation expenses include costs related to commuting, personal vehicle maintenance (gas, insurance, repairs), and public transit fares where applicable. We factor in the average commute distances in each county and availability of public transportation options."))),
                         tags$li(div(class = "about-variable-item", h4("Taxes"), p("This category includes estimated state and local income taxes, sales taxes on goods and services, and property taxes (for homeowners or indirectly through rent). Federal taxes are also considered to provide a holistic view of the tax burden."))),
                         tags$li(div(class = "about-variable-item", h4("Healthcare"), p("Healthcare costs cover monthly premiums for health insurance, out-of-pocket expenses for doctor visits, prescriptions, and other medical services. These estimates are based on typical health plan costs and average healthcare utilization rates."))),
                         tags$li(div(class = "about-variable-item", h4("Childcare"), p("For families with children, childcare costs include expenses for daycare, preschool, or after-school programs. These costs are highly variable and are estimated based on the average rates for licensed childcare facilities in each geographic area."))),
                         tags$li(div(class = "about-variable-item", h4("Technology"), p("Technology costs encompass essential communication and digital access, such as internet service, cell phone plans, and a portion for device depreciation or replacement. This reflects the modern necessity of digital connectivity."))),
                         tags$li(div(class = "about-variable-item", h4("Elder Care"), p("This variable accounts for potential costs associated with elder care, which might include in-home care services, assisted living facilities, or medical supplies for seniors. This is primarily relevant for households with elderly dependents (65+)."))),
                         tags$li(div(class = "about-variable-item", h4("Utilities"), p("Utilities cover basic household services such as electricity, water, natural gas, heating oil, and waste removal. These are estimated based on average consumption rates for typical households."))),
                         tags$li(div(class = "about-variable-item", h4("Miscellaneous"), p("The miscellaneous category covers a range of other essential expenses not covered elsewhere, such as personal care products, clothing, household supplies, and a small allowance for entertainment or emergencies. It represents a buffer for unforeseen costs."))),
                         tags$li(div(class = "about-variable-item", h4("Hourly Wage"), p("The hourly wage represents the estimated pre-tax hourly income required for a single adult to cover the minimum or average cost of living in a given area. It is calculated by dividing the total annual cost by the standard working hours in a year.")))
                       ),
                       
                       div(class = "section-title", "Sources"),
                       p("Data is compiled from a variety of public and private sources to ensure accuracy and relevance. Key sources include:"),
                       tags$ul(
                         tags$li("U.S. Census Bureau (population demographics, income data)"),
                         tags$li("Bureau of Labor Statistics (consumer price index, employment costs)"),
                         tags$li("Local government data (property tax rates, utility costs)"),
                         tags$li("Other relevant research and surveys.")
                       )
                   )
               )
      ),
      tabPanel("Results",
               div(class = "content-container",
                   div(class = "about-section",
                       h2("Project Results"),
                       p("This section will present the key findings and results from our cost of living analysis."),
                       p("Detailed comparisons between minimum and average costs across different regions of Virginia will be provided here, along with visualizations highlighting significant disparities and trends."),
                       div(class="future-text-section",
                           h4("Analysis Coming Soon"),
                           p("This area is reserved for future text, charts, and data tables that summarize our findings.")
                       )
                   )
               )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # --- Reactive data filtering for Table ---
  min_cost_data_for_table_filtered <- reactive({
    req(input$county_min_table)
    all_costs_long_for_table %>% filter(County == input$county_min_table, Type == "min")
  })
  
  avg_cost_data_for_table_filtered <- reactive({
    req(input$county_avg_table)
    all_costs_long_for_table %>% filter(County == input$county_avg_table, Type == "avg")
  })
  
  
  # --- Reusable Functions for Rendering ---
  generate_table_display <- function(filtered_data) {
    base_table <- tibble(`Cost Variable` = cost_variables_list)
    
    if (nrow(filtered_data) > 0) {
      wide_data <- filtered_data %>%
        pivot_wider(
          id_cols = CostVariable,
          names_from = FamilyStructure,
          values_from = Cost
        ) %>%
        rename(`Cost Variable` = CostVariable)
      
      final_table <- base_table %>%
        left_join(wide_data, by = "Cost Variable")
    } else {
      final_table <- base_table
      for(fam_struct in family_structures_list) {
        final_table[[fam_struct]] <- NA_real_
      }
    }
    
    final_table %>%
      mutate(`Cost Variable` = factor(`Cost Variable`, levels = cost_variables_list)) %>%
      arrange(`Cost Variable`)
  }
  
  format_cost_table <- function(df) {
    df$`Cost Variable` <- as.character(df$`Cost Variable`)
    
    total_row <- df %>%
      summarise(across(where(is.numeric), ~sum(., na.rm = TRUE))) %>%
      mutate(`Cost Variable` = "Total Cost")
    
    df_with_total <- bind_rows(df, total_row)
    
    df_with_total %>%
      mutate(across(where(is.numeric), ~case_when(
        is.na(.) ~ "N/A",
        . == 0 ~ "$0.00",
        TRUE ~ paste0("$", trimws(format(round(., 2), nsmall = 2, big.mark = ",")))
      )))
  }
  
  # --- Minimum Cost Tab Outputs ---
  output$min_table <- renderTable({
    table_data <- generate_table_display(min_cost_data_for_table_filtered())
    format_cost_table(table_data)
  }, striped = FALSE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE)
  
  output$min_plot <- renderPlotly({
    
    req(input$county_min_plot, input$family_structure_min)
    plot_data <- all_costs_long_for_table %>%
      filter(
        County == input$county_min_plot,
        Type == "min",
        FamilyStructure == input$family_structure_min
      )
    
    validate(need(nrow(plot_data) > 0, "No cost data available for this selection to plot."))
    
    p <- ggplot(plot_data, aes(
      x = CostVariable, 
      y = Cost,
      text = paste0(CostVariable, ": $", format(round(Cost, 2), nsmall = 2, big.mark = ","))
    )) +
      geom_col(fill = "steelblue", width = 0.8) + 
      scale_x_discrete(limits = cost_variables_list) + 
      labs(
        title = paste("Minimum Monthly Cost Breakdown for", input$family_structure_min, "in", input$county_min_plot),
        y = "Monthly Cost ($)", 
        x = "Cost Category"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$min_map <- renderLeaflet({
    pal <- colorQuantile(palette = c("green", "yellow", "red"), domain = va_map_data_min$Cost, n = 5, na.color = "#bdbdbd")
    leaflet(va_map_data_min) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Cost), weight = 1, color = "white", fillOpacity = 0.7,
        popup = ~paste0("<div class='map-popup-title'>", NAME, "</div><div class='map-popup-value'><strong>Total Cost:</strong><br>$", format(round(Cost, 0), big.mark=",")),
        label = ~NAME,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Cost, title = "Total Monthly Cost", na.label = "No Data")
  })
  
  # --- Average Cost Tab Outputs ---
  output$avg_table <- renderTable({
    table_data <- generate_table_display(avg_cost_data_for_table_filtered())
    format_cost_table(table_data)
  }, striped = FALSE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE)
  
  output$avg_plot <- renderPlotly({
    
    req(input$county_avg_plot, input$family_structure_avg)
    plot_data <- all_costs_long_for_table %>%
      filter(
        County == input$county_avg_plot,
        Type == "avg",
        FamilyStructure == input$family_structure_avg
      )
    
    validate(need(nrow(plot_data) > 0, "No cost data available for this selection to plot."))
    
    p <- ggplot(plot_data, aes(
      x = CostVariable, 
      y = Cost,
      text = paste0(CostVariable, ": $", format(round(Cost, 2), nsmall = 2, big.mark = ","))
    )) +
      geom_col(fill = "darkorange", width = 0.8) +
      scale_x_discrete(limits = cost_variables_list) +
      labs(
        title = paste("Average Monthly Cost Breakdown for", input$family_structure_avg, "in", input$county_avg_plot),
        y = "Monthly Cost ($)", 
        x = "Cost Category"
      ) +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$avg_map <- renderLeaflet({
    pal <- colorQuantile(palette = c("lightgreen", "gold", "darkred"), domain = va_map_data_avg$Cost, n=5, na.color = "#bdbdbd")
    leaflet(va_map_data_avg) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Cost), weight = 1, color = "white", fillOpacity = 0.7,
        popup = ~paste0("<div class='map-popup-title'>", NAME, "</div><div class='map-popup-value'><strong>Total Cost:</strong><br>$", format(round(Cost, 0), big.mark=",")),
        label = ~NAME,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Cost, title = "Total Monthly Cost", na.label = "No Data")
  })
  
}

shinyApp(ui = ui, server = server)