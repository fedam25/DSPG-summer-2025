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

# Load Virginia county data
va_counties <- counties(state = "VA", cb = TRUE, class = "sf")
virginia_county_names <- sort(unique(va_counties$NAME))

# --- Pre-process Utility Data --------------------------------------

# Load the minimum and average utility data from CSV files.
min_utilities_raw <- read_csv("minimum_final_utilities.csv")
avg_utilities_raw <- read_csv("average_final_utilities.csv")

# Function to standardize column names in utility dataframes and ensure correct types.
standardize_utility_cols <- function(df, type_name) {
  df_renamed <- df %>%
    rename(
      # Standardize '19-50 Years' to use en-dash '–' as used in your app
      `1 Adult: 19–50 Years` = `1 Adult: 19-50 Years`,
      `2 Adults: 19–50 Years` = `2 Adults: 19-50 Years`,
      
      # Standardize '1 Child' and '2 Children' to include '+' as used in your app
      `1 Adult + 1 Child` = `1 Adult 1 Child`,
      `2 Adults + 2 Children` = `2 Adults 2 Children`,
      
      # Ensure 65+ columns are consistent (these should already match based on common naming)
      `1 Adult: 65+` = `1 Adult 65+`,
      `2 Adults: 65+` = `2 Adults 65+`
    ) %>%
    # Ensure 'County' column is character and TRIM WHITESPACE!
    # This is crucial for accurate matching with `input$county_min` and `input$county_avg`.
    mutate(County = as.character(trimws(County))) %>%
    # Convert all relevant utility columns to numeric, coercing any non-numeric values to NA.
    # This ensures proper calculations later.
    mutate(across(starts_with("1 Adult") | starts_with("2 Adults"), as.numeric)) %>%
    mutate(`Total Monthly Cost` = as.numeric(`Total Monthly Cost`))
  
  # Diagnostic prints to help with debugging the data loading and standardization.
  message(paste0("--- Processed ", type_name, " Utilities Data (first 5 rows): ---"))
  print(head(df_renamed, 5))
  message(paste0("--- Glimpse of Processed ", type_name, " Utilities Data: ---"))
  glimpse(df_renamed)
  
  return(df_renamed)
}

# Apply standardization to both minimum and average utility dataframes.
min_utilities_data <- standardize_utility_cols(min_utilities_raw, "Minimum")
avg_utilities_data <- standardize_utility_cols(avg_utilities_raw, "Average")

# --- NEW DIAGNOSTIC MESSAGES FOR COUNTY NAMES ---
message("\n--- Unique County Names Loaded from minimum_final_utilities.csv (after standardization) ---")
print(sort(unique(min_utilities_data$County)))
message("\n--- Unique County Names Loaded from average_final_utilities.csv (after standardization) ---")
print(sort(unique(avg_utilities_data$County)))
message("\n--- Unique County Names from Tigris (used for Shiny Dropdown Choices) ---")
print(sort(unique(virginia_county_names)))
# --- END NEW DIAGNOSTIC MESSAGES ---


# Define family structures and cost variables, consistent with the UI.
# These lists are critical for defining the structure of the cost tables.
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

# --- Function to generate data specifically for the cost tables -----------------

generate_table_display_data <- function(county_name, type) {
  
  # Create a base dataframe for the table, filled with NA_real_ (R's numeric NA).
  # This ensures all non-utility cells are initially NA.
  df_table <- as.data.frame(matrix(NA_real_,
                                   nrow = length(cost_variables_list),
                                   ncol = length(family_structures_list),
                                   dimnames = list(cost_variables_list, family_structures_list)))
  
  # Add the 'Cost Variable' column and reorder it to be the first column.
  df_table$`Cost Variable` <- rownames(df_table)
  df_table <- df_table %>%
    select(`Cost Variable`, everything())
  
  # Select the correct utility dataset (minimum or average) based on the 'type' argument.
  utility_source_data <- if (type == "min") min_utilities_data else avg_utilities_data
  
  # Trim whitespace from the input county name for robust comparison.
  # This handles potential leading/trailing spaces in user input or CSV.
  county_name_trimmed <- trimws(county_name) 
  
  # Find the row in the utility data corresponding to the selected county.
  # The 'County' column in utility_source_data is already trimmed by standardize_utility_cols.
  utility_row <- utility_source_data %>% filter(County == county_name_trimmed)
  
  # Check if the county was found in the utility data for debugging.
  message(paste0("--- Searching for ", type, " utility data for: '", county_name_trimmed, "' ---"))
  if (nrow(utility_row) == 0) {
    message(paste0("WARNING: No ", type, " utility data found for county: '", county_name_trimmed, "'. Please check county name exact match in CSV."))
    # Diagnostic: Print all unique county names in the utility data to help user debug
    # This was already here and helps debug during runtime.
    message(paste0("Available counties in utility data for type ", type, " (from active data): ", paste(sort(unique(utility_source_data$County)), collapse = ", ")))
  } else {
    message(paste0("INFO: Found ", type, " utility data for county: '", county_name_trimmed, "'"))
    # Diagnostic: Print the specific utility row found.
    print(utility_row) 
  }
  
  # Populate the "Utilities" row in the table dataframe with actual data.
  # This is where the utilities data is merged into the dashboard's table structure.
  if (nrow(utility_row) > 0) { # Proceed only if data for the county was found.
    for (col_name in family_structures_list) {
      if (col_name %in% colnames(utility_row)) { # Ensure the column exists in the utility data.
        value_to_assign <- utility_row[[col_name]]
        # Only assign if the value is not NA in the source utility data.
        # If it's NA in source, it will remain NA_real_ from the initial matrix creation.
        if (!is.na(value_to_assign)) {
          df_table[df_table$`Cost Variable` == "Utilities", col_name] <- value_to_assign
        }
      }
    }
    # Populate the 'Total Monthly Cost' column for the Utilities row if available.
    if ("Total Monthly Cost" %in% colnames(utility_row)) {
      df_table[df_table$`Cost Variable` == "Utilities", "Total Monthly Cost"] <- utility_row$`Total Monthly Cost`
    }
  }
  
  return(df_table)
}

# --- Sample Data Generation (RETAINED for Maps and Plots) ------------------------
# These functions are intentionally kept as they currently provide the only data
# for the interactive maps and cost breakdown bar charts. Removing them would disable
# those visualizations entirely, as real data for all cost categories is not yet
# provided for these components.
generate_county_cost_data <- function(county_name, base_cost_multiplier = 1) {
  set.seed(nchar(county_name) * 100 + which(virginia_county_names == county_name))
  
  cost_variables <- c("Housing", "Food", "Transportation", "Taxes", "Healthcare",
                      "Childcare", "Technology", "Elder Care", "Utilities",
                      "Miscellaneous") 
  
  family_structures <- c(
    "1 Adult: 19–50 Years",
    "2 Adults: 19–50 Years",
    "1 Adult + 1 Child",
    "2 Adults + 2 Children",
    "1 Adult: 65+",
    "2 Adults: 65+"
  )
  
  data <- matrix(round(runif(length(family_structures) * length(cost_variables),
                             base_cost_multiplier * 50, base_cost_multiplier * 300), 2),
                 nrow = length(cost_variables),
                 ncol = length(family_structures),
                 dimnames = list(cost_variables, family_structures))
  
  df <- as.data.frame(data)
  df$`Total Monthly Cost` <- round(rowSums(df), 2)
  df <- tibble::rownames_to_column(df, var = "Cost Variable")
  return(df)
}

# This function sums costs for the maps. It uses the original sample data logic.
generate_county_total_cost <- function(county_name, base_cost_multiplier = 1) {
  cost_data <- generate_county_cost_data(county_name, base_cost_multiplier)
  total_cost <- cost_data %>%
    # Filter out 'Hourly Wage' as it's not a cost component for map totals
    filter(`Cost Variable` != "Hourly Wage") %>% 
    summarise(total = sum(`1 Adult: 19–50 Years`)) %>% # Sums for a specific family type as used for map colors
    pull(total)
  return(total_cost)
}

# Generate map data with total costs for all counties (UNCHANGED)
# These use the original generate_county_total_cost logic for sample map data.
sample_map_costs_min <- data.frame(
  NAME = virginia_county_names,
  Cost = sapply(virginia_county_names, function(x) generate_county_total_cost(x, 1))
)

sample_map_costs_avg <- data.frame(
  NAME = virginia_county_names,
  Cost = sapply(virginia_county_names, function(x) generate_county_total_cost(x, 1.5))
)

va_map_data_min <- left_join(va_counties, sample_map_costs_min, by = "NAME")
va_map_data_avg <- left_join(va_counties, sample_map_costs_avg, by = "NAME")

# UI Definition (UNCHANGED)
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* Custom header styling */
      .custom-header {
        background-color: #001f3f;
        padding: 30px 20px;
        margin-bottom: 20px;
        text-align: center;
        border-radius: 10px;
      }
      .custom-header h1 {
        color: white;
        font-size: 38px;
        font-weight: bold;
        margin: 0;
      }
      /* General text styling for sections */
      .intro-text, .project-intro, .about-section, .future-text-section {
        font-size: 17px;
        margin-bottom: 20px;
        padding: 15px;
        background-color: #f8f8f8;
        border-radius: 10px;
        border: 1px solid #e0e0e0;
      }
      .future-text-section {
        margin-top: 30px;
      }
      .section-title {
        font-size: 24px;
        font-weight: bold;
        margin-top: 30px;
        margin-bottom: 10px;
        color: #001f3f;
      }
      .section-desc {
        font-size: 16px;
        margin-bottom: 20px;
        color: #555;
      }
      .about-variable-item {
        margin-bottom: 15px;
        padding-left: 20px;
        border-left: 3px solid #001f3f;
      }
      .about-variable-item h4 {
        margin-top: 0;
        margin-bottom: 5px;
        color: #333;
      }
      .about-variable-item p {
        font-size: 16px;
        line-height: 1.5;
      }
      /* Styling for the tabset panel buttons */
      .nav-tabs > li > a {
        background-color: #e9ecef;
        color: #001f3f;
        font-weight: bold;
        border-top-left-radius: 8px;
        border-top-right-radius: 8px;
        margin-right: 5px;
      }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover {
        color: white;
        background-color: #007bff;
        border-color: #007bff;
      }
      /* Adjust output sizes and centering */
      .content-container {
        max-width: 95%;
        margin: 0 auto;
        padding: 0 15px;
      }
      .shiny-plot-output, .leaflet-container, .plotly {
        border-radius: 10px;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        width: 100% !important;
      }
      
      /* Enhanced Table styling */
      .table-container {
        margin: 20px 0;
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        background-color: white;
      }
      
      table.data {
        width: 100%;
        border-collapse: collapse;
        margin: 0;
        font-size: 15px;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
      }
      
      table.data th {
        background: linear-gradient(135deg, #001f3f 0%, #004080 100%);
        color: white;
        font-weight: bold;
        padding: 15px 12px;
        text-align: left;
        border: none;
        font-size: 16px;
        text-shadow: 0 1px 2px rgba(0,0,0,0.3);
      }
      
      table.data th:first-child {
        background: linear-gradient(135deg, #2c5282 0%, #3182ce 100%);
        border-top-left-radius: 10px;
      }
      
      table.data th:last-child {
        border-top-right-radius: 10px;
      }
      
      table.data td {
        padding: 12px;
        border-bottom: 1px solid #e2e8f0;
        transition: background-color 0.2s ease;
      }
      
      table.data td:first-child {
        background: linear-gradient(135deg, #e6f3ff 0%, #cce7ff 100%);
        font-weight: bold;
        color: #1a365d;
        border-right: 2px solid #3182ce;
        position: relative;
      }
      
      table.data td:first-child::before {
        content: '';
        position: absolute;
        left: 0;
        top: 0;
        bottom: 0;
        width: 4px;
        background: linear-gradient(to bottom, #3182ce, #2c5282);
      }
      
      table.data tbody tr:hover {
        background-color: #f7fafc;
        transform: translateY(-1px);
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      
      table.data tbody tr:hover td:first-child {
        background: linear-gradient(135deg, #d6f0ff 0%, #b3d9ff 100%);
      }
      
      table.data tbody tr:last-child td {
        border-bottom: none;
      }
      
      /* Zebra striping */
      table.data tbody tr:nth-child(even) {
        background-color: #f8fafc;
      }
      
      /* Total Monthly Cost row highlighting */
      table.data tbody tr:last-child {
        background: linear-gradient(135deg, #e6fffa 0%, #b3f5e6 100%);
        font-weight: bold;
        border-top: 2px solid #38a169;
      }
      
      table.data tbody tr:last-child td {
        color: #22543d;
        font-size: 16px;
        font-weight: bold;
      }
      
      table.data tbody tr:last-child td:first-child {
        background: linear-gradient(135deg, #38a169 0%, #2f855a 100%);
        color: white;
        text-shadow: 0 1px 2px rgba(0,0,0,0.3);
      }
      
      /* Enhanced first column styling */
      .highlight-first-col td:first-child {
        font-weight: bold;
        background-color: #f0f8ff;
        color: #003366;
        border-right: 2px solid #007bff;
      }
      
      /* Improved table header */
      .table-header {
        background: linear-gradient(to right, #001f3f, #003366);
        color: white;
        font-size: 16px;
      }
      
      /* Map popup styling */
      .leaflet-popup-content-wrapper {
        border-radius: 8px;
        padding: 10px;
      }
      
      .leaflet-popup-content {
        margin: 8px 12px;
        line-height: 1.5;
      }
      
      .map-popup-title {
        font-weight: bold;
        font-size: 16px;
        margin-bottom: 5px;
        color: #001f3f;
      }
      
      .map-popup-value {
        font-size: 14px;
        color: #333;
      }
    "))
  ),
  
  div(class = "custom-header",
      h1("Virginia Cost of Living")
  ),
  
  # Main content area with tabset panel
  mainPanel(
    width = 12,
    tabsetPanel(
      id = "main_tabs",
      selected = "About",
      
      # --- About Page ----------------------------------------------------------------
      tabPanel("About",
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
                       
                       div(class = "section-title", "Our Methodology"),
                       p("Our methodology involves compiling data from various sources to estimate the costs associated with essential goods and services. We categorize expenses to provide a comprehensive view of living costs. We differentiate between 'Minimum Cost' and 'Average Cost' to reflect different standards of living."),
                       p("The data presented here is a sample and will be replaced with real datasets in future iterations."),
                       
                       div(class = "section-title", "Our Variables"),
                       tags$ol(
                         tags$li(div(class = "about-variable-item",
                                     h4("Housing"),
                                     p("This variable represents the monthly cost associated with housing, including rent or mortgage payments, and basic maintenance. It is calculated based on median rental costs and homeownership expenses specific to each county/city, adjusted for family size and type of dwelling.")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Food"),
                                     p("Food costs cover the typical monthly expenses for groceries and meals. This is calculated using average food prices for common items, considering the nutritional needs and dietary patterns for different age groups and family structures. It accounts for both at-home consumption and a small allowance for eating out.")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Transportation"),
                                     p("Transportation expenses include costs related to commuting, personal vehicle maintenance (gas, insurance, repairs), and public transit fares where applicable. We factor in the average commute distances in each county and availability of public transportation options.")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Taxes"),
                                     p("This category includes estimated state and local income taxes, sales taxes on goods and services, and property taxes (for homeowners or indirectly through rent). Federal taxes are also considered to provide a holistic view of the tax burden.")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Healthcare"),
                                     p("Healthcare costs cover monthly premiums for health insurance, out-of-pocket expenses for doctor visits, prescriptions, and other medical services. These estimates are based on typical health plan costs and average health care utilization rates.")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Childcare"),
                                     p("For families with children, childcare costs include expenses for daycare, preschool, or after-school programs. These costs are highly variable and are estimated based on the average rates for licensed childcare facilities in each geographic area.")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Technology"),
                                     p("Technology costs encompass essential communication and digital access, such as internet service, cell phone plans, and a portion for device depreciation or replacement. This reflects the modern necessity of digital connectivity.")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Elder Care"),
                                     p("This variable accounts for potential costs associated with elder care, which might include in-home care services, assisted living facilities, or medical supplies for seniors. This is primarily relevant for households with elderly dependents (65+).")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Utilities"),
                                     p("Utilities cover basic household services such as electricity, water, natural gas, heating oil, and waste removal. These are estimated based on average consumption rates for typical households.")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Miscellaneous"),
                                     p("The miscellaneous category covers a range of other essential expenses not covered elsewhere, such as personal care products, clothing, household supplies, and a small allowance for entertainment or emergencies. It represents a buffer for unforeseen costs.")
                         )),
                         tags$li(div(class = "about-variable-item",
                                     h4("Hourly Wage"),
                                     p("The hourly wage represents the estimated pre-tax hourly income required for a single adult to cover the minimum or average cost of living in a given area. It is calculated by dividing the total annual cost by the standard working hours in a year.")
                         ))
                       ),
                       
                       div(class = "section-title", "How to Use This Dashboard"),
                       p("Navigating this dashboard is straightforward! Follow these simple steps to explore the cost of living data across Virginia:"),
                       tags$ol(
                         tags$li("To begin, the 'About' page provides a comprehensive introduction to our project, its importance, our methodology, and a detailed explanation of all the variables used in our calculations."),
                         tags$li("Click on either the 'Minimum Cost' or 'Average Cost' tab at the top of the page. These tabs will take you to the core data visualizations."),
                         tags$li("On either the 'Minimum Cost' or 'Average Cost' page, you will see a dropdown menu labeled 'Select County or City'. Click on this menu and choose any county or city in Virginia."),
                         tags$li("When you select a county or city, the 'Cost Table', 'Interactive County Map', and 'Cost Breakdown Bar Chart' below will automatically update to display the data specific to your chosen location."),
                         tags$li("The 'Cost Table' provides a detailed numerical breakdown of expenses for various family types. The 'Interactive County Map' shows the relative cost level across all counties, with your selected county highlighted."),
                         tags$li("The 'Cost Breakdown Bar Chart' visualizes how different expense categories contribute to the total cost. You can hover over any bar on the graph to see precise values and details for that specific cost variable."),
                         tags$li("You can switch between the 'Minimum Cost' and 'Average Cost' tabs to compare different standards of living across Virginia. You can explore different counties and family structures to gain deeper insights!"),
                         tags$li("More instructions here...")
                       ),
                       
                       div(class = "section-title", "Sources"),
                       p("Our data is compiled from a variety of reputable sources, including government agencies and economic research institutions. Examples include:"),
                       tags$ul(
                         tags$li("U.S. Census Bureau (population demographics, income data)"),
                         tags$li("Bureau of Labor Statistics (consumer price index, employment costs)"),
                         tags$li("Local government data (property tax rates, utility costs)"),
                         tags$li("Other relevant research and surveys."),
                         tags$li("More data sources here...")
                       ),
                       
                       div(class = "section-title", "Acknowledgement"),
                       tags$ul(
                         tags$li("This dashboard was developed by Feda Mohammadi and Julia Vecharello as part of the Virginia Tech Data Science for the Public Good (DSPG) Summer Research Program in Summer 2025. We extend our sincere gratitude to the DSPG program for providing this valuable opportunity to contribute to public understanding through data science.")
                       )
                   )
               )
      ),
      
      # --- Minimum Cost Page --------------------------------------------------------
      tabPanel("Minimum Cost",
               div(class = "content-container",
                   div(class = "intro-text",
                       h4("What is Minimum Cost?"),
                       p("The Minimum Cost represents a survival budget. It covers only the most essential expenses required to maintain a basic standard of living in a given county or city. This estimate does not include discretionary spending or savings.")
                   ),
                   selectInput("county_min", "Select County or City:", choices = virginia_county_names, selected = virginia_county_names[1]),
                   div(class = "section-title", "Minimum Cost Table"),
                   div(class = "section-desc", "Monthly minimum cost by category for different family types in the selected area."),
                   div(class = "table-container",
                       tableOutput("min_table") # This output will use the new data logic
                   ),
                   div(class = "section-title", "Interactive County Map"),
                   div(class = "section-desc", "This map displays the total minimum monthly cost across all Virginia counties. The selected county is highlighted."),
                   leafletOutput("min_map", height = 450),
                   div(class = "section-title", "Cost Breakdown Bar Chart"),
                   div(class = "section-desc", "A visualization of the minimum cost components for the selected county/city. Hover over bars for details!"),
                   plotlyOutput("min_plot", height = 350),
                   div(class = "future-text-section",
                       h4("Additional"),
                       p("This section is reserved for future analysis and detailed explanations related to minimum cost data. We plan to include more in-depth breakdowns, comparisons, and policy implications here as more datasets are integrated.")
                   )
               )
      ),
      
      # --- Average Cost Page ---------------------------------------------------------
      tabPanel("Average Cost",
               div(class = "content-container",
                   div(class = "intro-text",
                       h4("What is Average Cost?"),
                       p("The Average Cost estimate reflects typical expenses of average households, going beyond just survival needs. It includes a more comfortable standard of living, allowing for some discretionary spending, savings, and a wider range of goods and services.")
                   ),
                   selectInput("county_avg", "Select County or City:", choices = virginia_county_names, selected = virginia_county_names[1]),
                   div(class = "section-title", "Average Cost Table"),
                   div(class = "section-desc", "Monthly average cost by category for different family types in the selected area."),
                   div(class = "table-container",
                       tableOutput("avg_table") # This output will use the new data logic
                   ),
                   div(class = "section-title", "Interactive County Map"),
                   div(class = "section-desc", "This map displays the total average monthly cost across all Virginia counties. The selected county is highlighted."),
                   leafletOutput("avg_map", height = 450),
                   div(class = "section-title", "Cost Breakdown Bar Chart"),
                   div(class = "section-desc", "A visualization of the average cost components for the selected county/city. Hover over bars for details!"),
                   plotlyOutput("avg_plot", height = 350),
                   div(class = "future-text-section",
                       h4("Additional"),
                       p("This section is reserved for future analysis and detailed explanations related to average cost data. We plan to include more in-depth breakdowns, comparisons, and policy implications here as more datasets are integrated.")
                   )
               )
      )
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  
  # Reactive expression for Minimum Cost data for tables.
  # This uses 'generate_table_display_data' which populates only 'Utilities'
  # and sets others to NA.
  min_cost_data_for_table <- reactive({
    req(input$county_min)
    generate_table_display_data(input$county_min, "min")
  })
  
  # Reactive expression for Minimum Cost data for plots (uses sample data).
  # This remains unchanged as per your instruction to keep sample data for plots/maps.
  min_cost_data_plot <- reactive({
    generate_county_cost_data(input$county_min, base_cost_multiplier = 1) %>%
      filter(`Cost Variable` != "Hourly Wage")
  })
  
  # Reactive expression for total minimum cost for selected county (for maps, uses sample data).
  # This remains unchanged as per your instruction to keep sample data for plots/maps.
  min_total_cost <- reactive({
    req(input$county_min)
    generate_county_total_cost(input$county_min, 1)
  })
  
  # Reactive expression for Average Cost data for tables.
  # This uses 'generate_table_display_data' which populates only 'Utilities'
  # and sets others to NA.
  avg_cost_data_for_table <- reactive({
    req(input$county_avg)
    generate_table_display_data(input$county_avg, "avg")
  })
  
  # Reactive expression for Average Cost data for plots (uses sample data).
  # This remains unchanged as per your instruction to keep sample data for plots/maps.
  avg_cost_data_plot <- reactive({
    generate_county_cost_data(input$county_avg, base_cost_multiplier = 1.5) %>%
      filter(`Cost Variable` != "Hourly Wage")
  })
  
  # Reactive expression for total average cost for selected county (for maps, uses sample data).
  # This remains unchanged as per your instruction to keep sample data for plots/maps.
  avg_total_cost <- reactive({
    req(input$county_avg)
    generate_county_total_cost(input$county_avg, 1.5)
  })
  
  # Function to format table with Total Cost row.
  # This function is crucial for displaying NA values as "N/A" and correctly
  # calculating the total row, considering that most data points might be NA.
  format_cost_table <- function(df) {
    # Isolate numeric columns for summation.
    df_numeric_only <- df %>% select(-`Cost Variable`)
    
    # Calculate column sums for numeric columns, ignoring NAs.
    # sum(., na.rm = TRUE) will sum only the non-NA values (i.e., utilities costs).
    # If all values in a column are NA, the sum will be 0.
    total_row_values <- df_numeric_only %>%
      summarise(across(everything(), ~sum(., na.rm = TRUE)))
    
    # Create the 'Total Cost' row with its label.
    total_row <- total_row_values %>%
      mutate(`Cost Variable` = "Total Cost") %>%
      select(`Cost Variable`, everything())
    
    # Convert all numeric columns in the main data frame to character for formatting.
    # Replace NA values with "N/A" string and round numeric values to 2 decimal places.
    df_formatted <- df %>%
      mutate(across(where(is.numeric), ~ifelse(is.na(.), "N/A", as.character(round(., 2)))))
    
    # Convert numeric values in the total row to character.
    # If the sum is NA or 0 (meaning no data, or only NAs for that family type), display "N/A".
    total_row_formatted <- total_row %>%
      mutate(across(where(is.numeric), ~ifelse(is.na(.) | . == 0, "N/A", as.character(round(., 2)))))
    
    # Combine the formatted main data frame with the formatted total row.
    bind_rows(df_formatted, total_row_formatted)
  }
  
  # --- Minimum Cost Tab Outputs ---------------------------------------------------
  
  output$min_table <- renderTable({
    # Use the 'min_cost_data_for_table' reactive, which provides the utilities data
    # and NAs for other categories, and then format it.
    format_cost_table(min_cost_data_for_table())
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE,
  sanitize.text.function = function(x) x # Keeps HTML/special characters as is
  )
  
  # Plot output (uses sample data) - UNCHANGED
  output$min_plot <- renderPlotly({
    plot_data <- min_cost_data_plot() %>%
      mutate(`Cost Variable` = factor(`Cost Variable`, levels = unique(`Cost Variable`)))
    
    p <- ggplot(plot_data, aes(x = `Cost Variable`, y = `1 Adult: 19–50 Years`,
                               text = paste("Variable:", `Cost Variable`, "<br>Cost: $", `1 Adult: 19–50 Years`))) +
      geom_bar(stat = "identity", fill = "steelblue") +
      labs(title = paste("Minimum Cost Breakdown -", input$county_min),
           y = "Monthly Cost ($)",
           x = "Cost Variable") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, margin = margin(t = 10)),
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
  
  # Map output (uses sample data) - UNCHANGED
  output$min_map <- renderLeaflet({
    pal_min <- colorNumeric(palette = c("green", "yellow", "red"), domain = va_map_data_min$Cost)
    
    selected_county_polygon <- va_counties %>% filter(NAME == input$county_min)
    
    # Custom popup content with vertical layout
    popup_content <- paste0(
      "<div class='map-popup-title'>", input$county_min, "</div>",
      "<div class='map-popup-value'><strong>Total Minimum Monthly Cost:</strong><br>$", 
      round(min_total_cost(), 0), "</div>"
    )
    
    map_obj <- leaflet(va_map_data_min) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal_min(Cost),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        popup = ~paste0("<div class='map-popup-title'>", NAME, "</div>",
                        "<div class='map-popup-value'><strong>Total Monthly Cost:</strong><br>$", 
                        round(Cost, 0), "</div>"),
        label = ~paste0("County: ", NAME, "<br>Total Monthly Cost: $", round(Cost, 0)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal_min, values = ~Cost, title = "Total Monthly Cost")
    
    center_coords <- sf::st_coordinates(sf::st_centroid(selected_county_polygon))
    
    if (!is.null(center_coords) && !any(is.na(center_coords))) {
      map_obj <- map_obj %>%
        setView(lng = center_coords[1], lat = center_coords[2], zoom = 8) %>%
        addPolygons(
          data = selected_county_polygon,
          fillColor = "#007bff",
          weight = 3,
          opacity = 1,
          color = "darkblue",
          fillOpacity = 0.8,
          popup = popup_content,
          label = ~paste0("County: ", NAME, "<br>Total Monthly Cost: $", round(min_total_cost(), 0), "<br>(Selected County)"),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
    }
    return(map_obj)
  })
  
  # --- Average Cost Tab Outputs ----------------------------------------------------
  
  output$avg_table <- renderTable({
    # Use the 'avg_cost_data_for_table' reactive, which provides the utilities data
    # and NAs for other categories, and then format it.
    format_cost_table(avg_cost_data_for_table())
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE,
  sanitize.text.function = function(x) x
  )
  
  # Plot output (uses sample data) - UNCHANGED
  output$avg_plot <- renderPlotly({
    plot_data <- avg_cost_data_plot() %>%
      mutate(`Cost Variable` = factor(`Cost Variable`, levels = unique(`Cost Variable`)))
    
    p <- ggplot(plot_data, aes(x = `Cost Variable`, y = `1 Adult: 19–50 Years`,
                               text = paste("Variable:", `Cost Variable`, "<br>Cost: $", `1 Adult: 19–50 Years`))) +
      geom_bar(stat = "identity", fill = "darkorange") +
      labs(title = paste("Average Cost Breakdown -", input$county_avg),
           y = "Monthly Cost ($)",
           x = "Cost Variable") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 9, margin = margin(t = 10)),
            plot.title = element_text(hjust = 0.5))
    
    ggplotly(p, tooltip = "text") %>%
      layout(hovermode = "x unified")
  })
  
  # Map output (uses sample data) - UNCHANGED
  output$avg_map <- renderLeaflet({
    pal_avg <- colorNumeric(palette = c("lightgreen", "gold", "darkred"), domain = va_map_data_avg$Cost)
    
    selected_county_polygon <- va_counties %>% filter(NAME == input$county_avg)
    
    # Custom popup content with vertical layout
    popup_content <- paste0(
      "<div class='map-popup-title'>", input$county_avg, "</div>",
      "<div class='map-popup-value'><strong>Total Monthly Cost:</strong><br>$", 
      round(avg_total_cost(), 0), "</div>"
    )
    
    map_obj <- leaflet(va_map_data_avg) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal_avg(Cost),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        popup = ~paste0("<div class='map-popup-title'>", NAME, "</div>",
                        "<div class='map-popup-value'><strong>Total Monthly Cost:</strong><br>$", 
                        round(Cost, 0), "</div>"),
        label = ~paste0("County: ", NAME, "<br>Total Monthly Cost: $", round(Cost, 0)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(pal = pal_avg, values = ~Cost, title = "Total Average<br>Monthly Cost")
    
    center_coords <- sf::st_coordinates(sf::st_centroid(selected_county_polygon))
    
    if (!is.null(center_coords) && !any(is.na(center_coords))) {
      map_obj <- map_obj %>%
        setView(lng = center_coords[1], lat = center_coords[2], zoom = 8) %>%
        addPolygons(
          data = selected_county_polygon,
          fillColor = "#dc3545",
          weight = 3,
          opacity = 1,
          color = "darkred",
          fillOpacity = 0.8,
          popup = popup_content,
          label = ~paste0("County: ", NAME, "<br>Total Average Monthly Cost: $", round(avg_total_cost(), 0), "<br>(Selected County)"),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        )
    }
    return(map_obj)
  })
}

shinyApp(ui = ui, server = server)