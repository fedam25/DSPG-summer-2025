library(shiny)
library(tidyverse)
library(tigris)
library(sf)
library(leaflet)
library(RColorBrewer)
library(ggplot2)
library(plotly)
library(readxl)
library(viridis)

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
  "Childcare", "Technology", "Elder Care",
  "Miscellaneous"
)

# --- Load and Process REAL Data from Cleaned CSV files ---
min_elder_care_raw <- read_csv("minimum_elder_care_cost.csv")
avg_elder_care_raw <- read_csv("average_elder_care_cost.csv")
min_transportation_raw <- read_csv("minimum_transportation_data.csv")
avg_transportation_raw <- read_csv("average_transportation_data.csv")
min_technology_raw <- read_csv("minimum_technology_costs.csv")
avg_technology_raw <- read_csv("average_technology_costs.csv")
min_food_raw <- read_csv("final_minimum_food_data.csv")
avg_food_raw <- read_csv("final_average_food_data.csv")
min_tax_raw <- read_csv("minimum_tax_cost.csv")
avg_tax_raw <- read_csv("average_tax_cost.csv")
min_childcare_raw <- read_csv("childcare_minimum_cost.csv")
avg_childcare_raw <- read_csv("childcare_average_cost.csv")
min_housing_raw <- read_csv("minimum_housing_cost.csv")
avg_housing_raw <- read_csv("average_housing_cost.csv")
min_healthcare_raw <- read_csv("minimum_healthcare_cost.csv")
avg_healthcare_raw <- read_csv("average_healthcare_cost.csv")


# Function to standardize column names
standardize_cols <- function(df) {
  df %>%
    rename_with(~"1 Adult: 19–50 Years", .cols = matches("1 Adult.*19-50.*")) %>%
    rename_with(~"2 Adults: 19–50 Years", .cols = matches("2 Adults.*19-50.*")) %>%
    rename_with(~"1 Adult + 1 Child", .cols = matches("1 Adult.*1 Child")) %>%
    rename_with(~"2 Adults + 2 Children", .cols = matches("2 Adults.*2 Children")) %>%
    rename_with(~"1 Adult: 65+", .cols = matches("1 Adult.*65+")) %>%
    rename_with(~"2 Adults: 65+", .cols = matches("2 Adults.*65+")) %>%
    mutate(County = as.character(trimws(County)))
}

# Function to handle missing 'Total Monthly Cost' column
process_data <- function(df) {
  df_std <- df %>% standardize_cols()
  if ("2 Adults + 2 Children" %in% names(df_std)) {
    df_std <- df_std %>% mutate(`2 Adults + 2 Children` = as.numeric(`2 Adults + 2 Children`))
  }
  if (!"Total Monthly Cost" %in% names(df_std)) {
    if ("2 Adults + 2 Children" %in% names(df_std)) {
      df_processed <- df_std %>% mutate(`Total Monthly Cost` = `2 Adults + 2 Children`)
    } else {
      df_processed <- df_std %>% mutate(`Total Monthly Cost` = 0)
    }
  } else {
    df_processed <- df_std
  }
  all_cols_to_check <- c(family_structures_list, "Total Monthly Cost")
  present_cols <- intersect(all_cols_to_check, names(df_processed))
  df_final <- df_processed %>% mutate(across(all_of(present_cols), as.numeric))
  return(df_final)
}

# Process all data files
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
min_housing_data <- process_data(min_housing_raw)
avg_housing_data <- process_data(avg_housing_raw)
min_healthcare_data <- process_data(min_healthcare_raw)
avg_healthcare_data <- process_data(avg_healthcare_raw)


# --- Create Unified Data Sources ---
all_costs_long_for_table_raw <- bind_rows(
  min_elder_care_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Elder Care", Type = "min"),
  avg_elder_care_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Elder Care", Type = "avg"),
  min_transportation_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Transportation", Type = "min"),
  avg_transportation_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Transportation", Type = "avg"),
  min_technology_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Technology", Type = "min"),
  avg_technology_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Technology", Type = "avg"),
  min_food_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Food", Type = "min"),
  avg_food_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Food", Type = "avg"),
  min_tax_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Taxes", Type = "min"),
  avg_tax_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Taxes", Type = "avg"),
  min_childcare_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Childcare", Type = "min"),
  avg_childcare_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Childcare", Type = "avg"),
  min_housing_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Housing", Type = "min"),
  avg_housing_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Housing", Type = "avg"),
  min_healthcare_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Healthcare", Type = "min"),
  avg_healthcare_data %>% pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>% mutate(CostVariable = "Healthcare", Type = "avg")
)

all_costs_long_for_table <- all_costs_long_for_table_raw %>%
  group_by(County, FamilyStructure, CostVariable, Type) %>%
  summarise(Cost = mean(Cost, na.rm = TRUE), .groups = 'drop') %>%
  group_by(County, FamilyStructure, Type) %>%
  # Calculate subtotal for Miscellaneous EXCLUDING taxes.
  mutate(subtotal_for_misc = sum(Cost[CostVariable != 'Taxes'], na.rm = TRUE)) %>%
  ungroup() %>%
  bind_rows(
    distinct(., County, FamilyStructure, Type, subtotal_for_misc) %>%
      mutate(CostVariable = "Miscellaneous", Cost = subtotal_for_misc * 0.10) %>%
      select(-subtotal_for_misc)
  )

# Prepare comprehensive data for the MAPS
total_costs_for_map <- all_costs_long_for_table %>%
  group_by(NAME = County, Type, FamilyStructure) %>%
  summarise(TotalCost = sum(Cost, na.rm = TRUE), .groups = 'drop')

# Join the aggregated cost data with the spatial data for plotting
va_map_data_full <- left_join(va_counties, total_costs_for_map, by = "NAME")


# --- UI Definition ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      :root {
        --header-bg: #2D3A52; --active-tab-bg: #35B779; --title-color: #2D3A52;
        --table-header-bg: #404788; --table-total-bg: #21908C; --table-total-text: #FFFFFF;
      }
      .dashboard-wrapper { max-width: 1300px; margin: 0 auto; }
      .custom-header { background-color: var(--header-bg); padding: 30px 20px; margin-bottom: 20px; text-align: center; border-radius: 10px; }
      .custom-header h1 { color: white; font-size: 38px; font-weight: bold; margin: 0; }
      .intro-text, .project-intro, .about-section, .future-text-section { font-size: 17px; margin-bottom: 20px; padding: 15px; background-color: #f8f8f8; border-radius: 10px; border: 1px solid #e0e0e0; }
      .future-text-section { margin-top: 30px; }
      .section-title { font-size: 24px; font-weight: bold; margin-top: 40px; margin-bottom: 10px; color: var(--title-color); border-bottom: 2px solid var(--active-tab-bg); padding-bottom: 5px;}
      .section-desc { font-size: 16px; margin-bottom: 20px; color: #555; }
      .about-variable-item { margin-bottom: 15px; padding-left: 20px; border-left: 3px solid var(--title-color); }
      .about-variable-item h4 { margin-top: 0; margin-bottom: 5px; color: #333; }
      .about-variable-item p { font-size: 16px; line-height: 1.5; }
      .nav-tabs > li > a { background-color: #e9ecef; color: var(--title-color); font-weight: bold; border-top-left-radius: 8px; border-top-right-radius: 8px; margin-right: 5px; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:focus, .nav-tabs > li.active > a:hover { color: white; background-color: var(--active-tab-bg); border-color: var(--active-tab-bg); }
      .content-container { max-width: 95%; margin: 0 auto; padding: 0 15px; }
      .shiny-plot-output, .leaflet-container, .plotly { border-radius: 10px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); width: 100% !important; }
      
      /* --- Table Styling --- */
      .table-container { margin: 0; border-radius: 10px; overflow-y: auto; max-height: 420px; box-shadow: 0 4px 8px rgba(0,0,0,0.1); background-color: white; }
      .table-container table { width: 100%; border-collapse: collapse; margin: 0; font-size: 15px; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      .table-container table td { padding: 12px; border-bottom: 1px solid #e2e8f0; transition: background-color 0.2s ease, transform 0.2s ease; }
      .table-container table tbody tr:nth-child(even) { background-color: #f8fafc; }
      .table-container table th { background-color: var(--table-header-bg); color: white; font-weight: bold; padding: 15px 12px; text-align: left; border: none; font-size: 16px; }
      
      .table-container table tbody tr:hover { background-color: #f1f5f9; transform: translateY(-2px); box-shadow: 0 4px 8px rgba(0,0,0,0.15); }
      
      #min_table table td:first-child, #avg_table table td:first-child { background-color: var(--table-header-bg); font-weight: bold; color: white; border-right: 2px solid var(--table-header-bg); position: relative; }
      #min_table table td:first-child::before, #avg_table table td:first-child::before { content: ''; position: absolute; left: 0; top: 0; bottom: 0; width: 4px; background: linear-gradient(to bottom, var(--table-header-bg), var(--title-color)); }
      #min_table table tbody tr:hover td:first-child, #avg_table table tbody tr:hover td:first-child { background-color: #5C67A1; }
      
      .table-container table tbody tr:last-child td { border-bottom: none; }

      /* CSS rules to color the entire 'Total' rows green */
      .table-container table tbody tr:nth-last-child(-n+2) {
          border-top: 2px solid var(--table-header-bg);
      }
      .table-container table tbody tr:nth-last-child(-n+2) td {
          background-color: var(--table-total-bg);
          color: var(--table-total-text);
          font-size: 16px;
          font-weight: bold;
      }

      .leaflet-popup-content-wrapper { border-radius: 8px; padding: 10px; }
      .selectize-dropdown { z-index: 1001; }
      
      .controls-info-box {
        font-size: 12px;
        background-color: #f8f9fa;
        border: 1px solid #dee2e6;
        border-radius: 5px;
        padding: 10px;
        margin-top: 15px;
        line-height: 1.4;
      }
      .controls-info-box p { margin-bottom: 10px; }
      .controls-info-box p:last-child { margin-bottom: 0; }
      
      .final-note-text {
        font-size: 16px;
        color: #555;
        line-height: 1.6;
      }
      
    "))
  ),
  
  div(class = "dashboard-wrapper",
      div(class = "custom-header", h1("Virginia Cost of Living")),
      
      mainPanel(
        width = 12,
        tabsetPanel(
          id = "main_tabs",
          
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
                             tags$li("The Map displays total costs across Virginia. Use the new dropdown menu above the map to see costs for different family types.")
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
                       div(class = "section-desc", "This table shows the monthly minimum cost by category for all family types in the selected county."),
                       fluidRow(
                         column(3,
                                selectInput("county_min_table", "Select County or City for Table:", choices = virginia_county_names, selected = virginia_county_names[1], width = "100%"),
                                div(class = "controls-info-box",
                                    p("Use the dropdown menu above to select a county or city in Virginia. Once selected, the table will show a full breakdown of the minimum monthly costs for that area based on six different common family structures (like a single adult, a family with two children, or elderly adults)."),
                                    p("This table reflects a 'survival budget' - the absolute minimum needed to cover essential living expenses such as housing, food, transportation, and healthcare. These estimates do not include extra spending, savings, or luxuries - only what's needed to get by."),
                                    p(" You can use this to understand how much it might cost just to meet basic needs in different counties, depending on your household setup.")
                                )
                         ),
                         column(9,
                                div(class = "table-container", tableOutput("min_table"))
                         )
                       ),
                       
                       div(class = "section-title", "Interactive County Map"),
                       div(class = "section-desc", "This map displays the total minimum monthly cost across Virginia. Use the dropdown to select a family structure to see how costs vary."),
                       fluidRow(
                         column(3,
                                selectInput("family_structure_map_min", "Select Family Structure for Map:", choices = family_structures_list, selected = family_structures_list[4], width = "100%"),
                                div(class = "controls-info-box",
                                    p("This interactive map helps you visualize the total minimum monthly cost of living across all counties and independent cities in Virginia."),
                                    p("First, use the dropdown menu to select a family type. Then, the map will automatically update to show how much it costs for that family to cover essential needs in each area. Darker colors usually mean higher costs. You can hover over any county to see its name and cost, or click on it for more information."),
                                    p("This map gives a quick and powerful overview of how the basic cost of living changes across the state, which can help with comparing locations or planning where to live.")
                                )
                         ),
                         column(9,
                                leafletOutput("min_map", height = 420)
                         )
                       ),
                       
                       div(class = "section-title", "Cost Breakdown Bar Chart"),
                       p("This graph shows the monthly minimum cost breakdown for a selected family structure and location. Use the dropdown menus below to customize the view."),
                       fluidRow(
                         column(3,
                                selectInput("county_min_plot", "Select County or City for Graph:", choices = virginia_county_names, selected = virginia_county_names[1], width = "100%"),
                                selectInput("family_structure_min", "Select Family Structure:", choices = family_structures_list, selected = family_structures_list[4], width = "100%"),
                                div(class = "controls-info-box",
                                    p("This chart shows the breakdown of monthly minimum costs by category - like housing, food, taxes, and healthcare - for one specific family type in one location. To use this graph, choose a county or city and a family structure using the dropdown menus above. The chart will update instantly to show you how much each category contributes to the total cost."),
                                    p("This is useful if you want to understand which expenses are the biggest drivers of basic living costs in a specific area. For example, in some places, housing may be the biggest cost, while in others, it could be healthcare or transportation. Use this chart to better understand how your basic monthly budget might look, and to spot which categories need the most attention when planning expenses.")
                                )
                         ),
                         column(9,
                                plotlyOutput("min_plot", height = 380)
                         )
                       ),
                       
                       div(class = "section-title", "Custom Family Structure Comparison (Minimum Cost)"),
                       p(class = "section-desc", "Build a custom family profile and select up to 3 locations to compare estimated costs. Note: This is an estimate based on the closest available data profile."),
                       fluidRow(
                         column(3,
                                numericInput("num_adults_min", "Number of Adults (19-50):", 0, min = 0, max = 3, width = "100%"),
                                numericInput("num_children_min", "Number of Children:", 0, min = 0, max = 4, width = "100%"),
                                numericInput("num_childcare_min", "Children in Childcare:", 0, min = 0, max = 4, width = "100%"),
                                numericInput("num_elders_min", "Number of Elders (65+):", 0, min = 0, max = 2, width = "100%"),
                                selectInput("compare_counties_min", "Select up to 3 Counties/Cities:", choices = virginia_county_names, multiple = TRUE, selected = "Fairfax County", width = "100%"),
                                div(class = "controls-info-box",
                                    p("You can use the controls above to build your own custom family profile - choose how many adults, children (including those in childcare), and elders are in your household. Then, you can select up to three counties or cities to compare."),
                                    p("The table shows a side-by-side comparison of the estimated minimum monthly costs for each location. This is especially helpful if you're planning to move or want to explore how costs vary across different parts of Virginia for your specific family situation.")
                                )
                         ),
                         column(9,
                                div(class = "table-container", tableOutput("custom_table_min"))
                         )
                       ),
                       
                       div(class = "section-title", "Understanding the Minimum Cost Estimates"),
                       p(class = "final-note-text", "The minimum cost estimates presented on this page are designed to represent a 'survival' budget, which covers the most basic needs for a household to live and work in Virginia. This budget does not account for savings, emergencies, or non-essential spending that contributes to quality of life. As you explore this data, consider that households earning below this minimum threshold are likely facing significant financial hardship. This information is crucial for policymakers, non-profits, and community leaders when discussing topics like living wages, affordable housing, and the effectiveness of social support systems.")
                   )
          ),
          
          tabPanel("Average Cost",
                   div(class = "content-container",
                       div(class = "intro-text", h4("What is Average Cost?"), p("The Average Cost estimate reflects typical expenses of average households, going beyond just survival needs. It includes a more comfortable standard of living, allowing for some discretionary spending, savings, and a wider range of goods and services.")),
                       
                       div(class = "section-title", "Average Cost Table"),
                       div(class = "section-desc", "This table shows the monthly average cost by category for all family types in the selected county."),
                       fluidRow(
                         column(3,
                                selectInput("county_avg_table", "Select County or City for Table:", choices = virginia_county_names, selected = virginia_county_names[1], width = "100%"),
                                div(class = "controls-info-box",
                                    p("Use the dropdown menu above to choose a county or city in Virginia. The table will then show you a detailed monthly cost breakdown for six common family types - like single adults, families with children, or elder couples."),
                                    p("These numbers reflect an average lifestyle; it means they include more than just the bare minimum. In addition to basic needs like housing and food, the estimates also account for some discretionary spending, savings, and comfort, such as better healthcare plans, more flexible transportation options, and modest entertainment or technology costs."),
                                    p("This table is helpful for anyone who wants to understand what it might realistically cost to live in a certain area with a decent standard of living, not just scraping by.")
                                )
                         ),
                         column(9,
                                div(class = "table-container", tableOutput("avg_table"))
                         )
                       ),
                       
                       div(class = "section-title", "Interactive County Map"),
                       div(class = "section-desc", "This map displays the total average monthly cost across Virginia. Use the dropdown to select a family structure to see how costs vary."),
                       fluidRow(
                         column(3,
                                selectInput("family_structure_map_avg", "Select Family Structure for Map:", choices = family_structures_list, selected = family_structures_list[4], width = "100%"),
                                div(class = "controls-info-box",
                                    p("This interactive map lets you see how the average monthly cost of living changes from one Virginia county or city to another. To use this map, select a family structure from the dropdown menu. The map will then update to show how much, on average, it costs families of that type to live in each area."),
                                    p("The color shading helps you quickly see which areas are more or less expensive. Darker areas usually mean higher costs. You can also hover over or click on a county to see its name and total average monthly cost. This map is useful for comparing lifestyles across the state. It can help you decide where to live, plan budgets, or better understand regional cost differences in day-to-day living.")
                                )
                         ),
                         column(9,
                                leafletOutput("avg_map", height = 420)
                         )
                       ),
                       
                       div(class = "section-title", "Cost Breakdown Bar Chart"),
                       p("This graph shows the monthly average cost breakdown for a selected family structure and location. Use the dropdown menus below to customize the view."),
                       fluidRow(
                         column(3,
                                selectInput("county_avg_plot", "Select County or City for Graph:", choices = virginia_county_names, selected = virginia_county_names[1], width = "100%"),
                                selectInput("family_structure_avg", "Select Family Structure:", choices = family_structures_list, selected = family_structures_list[4], width = "100%"),
                                div(class = "controls-info-box",
                                    p("This bar chart gives a clear picture of how much each major cost category contributes to the total average monthly expenses for a selected family structure in a specific location. To use it, just choose a county or city and a family type from the menus above."),
                                    p("The chart will update to show the average monthly costs for housing, food, healthcare, transportation, taxes, and more. Unlike the minimum cost version, this chart reflects a more comfortable and realistic lifestyle, not luxury, but not bare-bones either. It includes typical spending habits of families who can afford more flexibility in their choices. This is a great way to see which costs take up the most space in a real-world budget, and where people might be spending more than just the essentials.")
                                )
                         ),
                         column(9,
                                plotlyOutput("avg_plot", height = 380)
                         )
                       ),
                       
                       div(class = "section-title", "Custom Family Structure Comparison (Average Cost)"),
                       p(class = "section-desc", "Build a custom family profile and select up to 3 locations to compare estimated costs. Note: This is an estimate based on the closest available data profile."),
                       fluidRow(
                         column(3,
                                numericInput("num_adults_avg", "Number of Adults (19-50):", 0, min = 0, max = 3, width = "100%"),
                                numericInput("num_children_avg", "Number of Children:", 0, min = 0, max = 4, width = "100%"),
                                numericInput("num_childcare_avg", "Children in Childcare:", 0, min = 0, max = 4, width = "100%"),
                                numericInput("num_elders_avg", "Number of Elders (65+):", 0, min = 0, max = 2, width = "100%"),
                                selectInput("compare_counties_avg", "Select up to 3 Counties/Cities:", choices = virginia_county_names, multiple = TRUE, selected = "Fairfax County", width = "100%"),
                                div(class = "controls-info-box",
                                    p("Want to explore cost estimates for your exact family setup? Use the input boxes above to create a custom family profile, just select how many adults, children, elders, and kids in childcare are in your household. Then you can pick up to three Virginia counties or cities to compare. The table will update to show average monthly costs side by side for each place based on your custom family structure."),
                                    p("These estimates reflect what a typical, modest lifestyle might cost in different regions. That means a bit more comfort and flexibility, such as better food choices, safer transportation, and some money set aside for entertainment or unexpected needs.")
                                )
                         ),
                         column(9,
                                div(class = "table-container", tableOutput("custom_table_avg"))
                         )
                       ),
                       
                       div(class = "section-title", "Understanding the Average Cost Estimates"),
                       p(class = "final-note-text", "The average cost estimates on this page reflect a more typical, modestly comfortable standard of living. This budget goes beyond basic survival to include items that allow families to participate more fully in their communities, such as modest entertainment, occasional meals out, and adequate savings for emergencies or future goals. While not a measure of wealth, this data provides a more realistic picture of what a financially stable household might spend to maintain a decent quality of life in Virginia. It's a valuable benchmark for individuals, employers, and policymakers aiming to understand the costs associated with economic security.")
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
                             tags$li(div(class = "about-variable-item", h4("Miscellaneous"), 
                                         p("The miscellaneous category covers a range of other essential expenses not covered elsewhere, such as personal care products, clothing, household supplies, and a small allowance for entertainment or emergencies."),
                                         p(strong("Note:"), "Miscellaneous costs are estimated at 10% of the total budget (excluding taxes) to cover unexpected or one-time expenses (like new shoes or household repairs). It's just a standard estimate (10%) based on their calculation method (or methodology).")
                             )),
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
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  min_cost_data_for_table_filtered <- reactive({ req(input$county_min_table); all_costs_long_for_table %>% filter(County == input$county_min_table, Type == "min") })
  avg_cost_data_for_table_filtered <- reactive({ req(input$county_avg_table); all_costs_long_for_table %>% filter(County == input$county_avg_table, Type == "avg") })
  min_map_data_filtered <- reactive({ req(input$family_structure_map_min); va_map_data_full %>% filter(Type == "min", FamilyStructure == input$family_structure_map_min) })
  avg_map_data_filtered <- reactive({ req(input$family_structure_map_avg); va_map_data_full %>% filter(Type == "avg", FamilyStructure == input$family_structure_map_avg) })
  
  generate_table_display <- function(filtered_data) {
    base_table <- tibble(`Cost Variable` = cost_variables_list)
    if (nrow(filtered_data) > 0) {
      wide_data <- filtered_data %>%
        pivot_wider(id_cols = CostVariable, names_from = FamilyStructure, values_from = Cost) %>%
        rename(`Cost Variable` = CostVariable)
      final_table <- base_table %>% left_join(wide_data, by = "Cost Variable")
    } else {
      final_table <- base_table
      for(fam_struct in family_structures_list) { final_table[[fam_struct]] <- NA_real_ }
    }
    final_table %>% mutate(`Cost Variable` = factor(`Cost Variable`, levels = cost_variables_list)) %>% arrange(`Cost Variable`)
  }
  
  format_cost_table <- function(df) {
    df$`Cost Variable` <- as.character(df$`Cost Variable`)
    monthly_total_row <- df %>% summarise(across(where(is.numeric), ~sum(., na.rm = TRUE))) %>% mutate(`Cost Variable` = "Monthly Total")
    annual_total_row <- monthly_total_row %>% mutate(across(where(is.numeric), ~ . * 12)) %>% mutate(`Cost Variable` = "Annual Total")
    df_with_totals <- bind_rows(df, monthly_total_row, annual_total_row)
    df_with_totals %>% mutate(across(where(is.numeric), ~case_when(is.na(.) ~ "N/A", . == 0 ~ "$0", TRUE ~ paste0("$", trimws(format(round(., 0), nsmall = 0, big.mark = ","))))))
  }
  
  output$min_table <- renderTable({ generate_table_display(min_cost_data_for_table_filtered()) %>% format_cost_table() }, striped = FALSE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE)
  output$avg_table <- renderTable({ generate_table_display(avg_cost_data_for_table_filtered()) %>% format_cost_table() }, striped = FALSE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE)
  
  output$min_plot <- renderPlotly({
    req(input$county_min_plot, input$family_structure_min)
    plot_data <- all_costs_long_for_table %>% filter(County == input$county_min_plot, Type == "min", FamilyStructure == input$family_structure_min)
    validate(need(nrow(plot_data) > 0, "No cost data available for this selection to plot."))
    p <- ggplot(plot_data, aes(x = CostVariable, y = Cost, text = paste0(CostVariable, ": $", format(round(Cost, 0), nsmall = 0, big.mark = ",")))) +
      geom_col(aes(fill = CostVariable), width = 0.9) +
      scale_fill_viridis_d(option = "cividis", guide = "none") +
      scale_x_discrete(limits = cost_variables_list) +
      labs(title = paste("Minimum Monthly Cost Breakdown for", input$family_structure_min, "in", input$county_min_plot), y = "Monthly Cost ($)", x = "Cost Category") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
    
  })
  
  output$min_map <- renderLeaflet({
    map_data <- min_map_data_filtered()
    validate(need(nrow(map_data) > 0 && any(!is.na(map_data$TotalCost) & is.finite(map_data$TotalCost)), "No data available for this selection."))
    
    costs <- map_data$TotalCost[is.finite(map_data$TotalCost)]
    bins <- unique(quantile(costs, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
    
    # Round bins to the nearest thousand for a cleaner legend
    bins <- unique(round(bins, -3))
    
    if (length(bins) < 2) {
      bins <- c(min(costs, na.rm = TRUE), max(costs, na.rm = TRUE))
    }
    
    pal <- colorBin(
      palette = "plasma",
      domain = map_data$TotalCost,
      bins = bins,
      na.color = "#bdbdbd"
    )
    
    leaflet(map_data) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(fillColor = ~pal(TotalCost), weight = 1, color = "white", fillOpacity = 0.8,
                  popup = ~paste0("<div class='map-popup-title'>", NAME, "</div><div class='map-popup-value'><strong>Total Minimum Cost:</strong><br>$", format(round(TotalCost, 0), big.mark=",")),
                  label = ~NAME,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
                  highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = ~TotalCost, title = "Total Monthly Cost", na.label = "No Data", opacity = 1)
  })
  
  output$avg_plot <- renderPlotly({
    req(input$county_avg_plot, input$family_structure_avg)
    plot_data <- all_costs_long_for_table %>% filter(County == input$county_avg_plot, Type == "avg", FamilyStructure == input$family_structure_avg)
    validate(need(nrow(plot_data) > 0, "No cost data available for this selection to plot."))
    p <- ggplot(plot_data, aes(x = CostVariable, y = Cost, text = paste0(CostVariable, ": $", format(round(Cost, 0), nsmall = 0, big.mark = ",")))) +
      geom_col(aes(fill = CostVariable), width = 0.9) +
      scale_fill_viridis_d(option = "plasma", guide = "none") +
      scale_x_discrete(limits = cost_variables_list) +
      labs(title = paste("Average Monthly Cost Breakdown for", input$family_structure_avg, "in", input$county_avg_plot), y = "Monthly Cost ($)", x = "Cost Category") +
      theme_minimal() + theme(plot.title = element_text(hjust = 0.5), axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = "text") %>% layout(showlegend = FALSE)
    
  })
  
  output$avg_map <- renderLeaflet({
    map_data <- avg_map_data_filtered()
    validate(need(nrow(map_data) > 0 && any(!is.na(map_data$TotalCost) & is.finite(map_data$TotalCost)), "No data available for this selection."))
    
    costs <- map_data$TotalCost[is.finite(map_data$TotalCost)]
    bins <- unique(quantile(costs, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
    
    # Round bins to the nearest thousand for a cleaner legend
    bins <- unique(round(bins, -3))
    
    if (length(bins) < 2) {
      bins <- c(min(costs, na.rm = TRUE), max(costs, na.rm = TRUE))
    }
    
    pal <- colorBin(
      palette = "viridis", # Use a different viridis palette for the average map
      domain = map_data$TotalCost,
      bins = bins,
      na.color = "#bdbdbd"
    )
    
    leaflet(map_data) %>% 
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(fillColor = ~pal(TotalCost), weight = 1, color = "white", fillOpacity = 0.8,
                  popup = ~paste0("<div class='map-popup-title'>", NAME, "</div><div class='map-popup-value'><strong>Total Average Cost:</strong><br>$", format(round(TotalCost, 0), big.mark=",")),
                  label = ~NAME,
                  labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
                  highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)) %>%
      addLegend(pal = pal, values = ~TotalCost, title = "Total Monthly Cost", na.label = "No Data", opacity = 1)
  })
  
  
  # --- New Server Logic for Custom Comparison ---
  
  map_family_structure <- function(adults, children, elders) {
    if (elders > 0) {
      if (children > 0) { if (adults >= 2) "2 Adults + 2 Children" else "1 Adult + 1 Child" } 
      else { if (elders >= 2) "2 Adults: 65+" else "1 Adult: 65+" }
    } else {
      if (children > 0) { if (adults >= 2) "2 Adults + 2 Children" else "1 Adult + 1 Child" } 
      else { if (adults >= 2) "2 Adults: 19–50 Years" else "1 Adult: 19–50 Years" }
    }
  }
  
  calculate_custom_cost <- function(cost_type, counties, adults, children, childcare_n, elders) {
    validate(
      need(length(counties) > 0, "Please select a location to see results."),
      need(length(counties) <= 3, "Please select no more than 3 locations."),
      need(childcare_n <= children, "Number of children in childcare cannot exceed the total number of children."),
      need(adults + children + elders > 0, "Please select at least one person for the family profile.")
    )
    
    base_structure <- map_family_structure(adults, children, elders)
    childcare_structure <- if (childcare_n == 1) "1 Adult + 1 Child" else if (childcare_n >= 2) "2 Adults + 2 Children" else NA
    elder_structure <- if (elders == 1) "1 Adult: 65+" else if (elders >= 2) "2 Adults: 65+" else NA
    
    base_costs <- all_costs_long_for_table %>%
      filter(Type == cost_type, County %in% counties, FamilyStructure == base_structure, 
             !CostVariable %in% c("Childcare", "Elder Care", "Miscellaneous"))
    
    get_costs_for <- function(variable, structure) {
      if (!is.na(structure)) {
        all_costs_long_for_table %>% filter(Type == cost_type, County %in% counties, FamilyStructure == structure, CostVariable == variable)
      } else {
        tibble(County = counties, CostVariable = variable, Cost = 0, Type = cost_type, FamilyStructure = "N/A") %>% distinct()
      }
    }
    childcare_costs <- get_costs_for("Childcare", childcare_structure)
    elder_care_costs <- get_costs_for("Elder Care", elder_structure)
    
    primary_costs <- bind_rows(base_costs, childcare_costs, elder_care_costs)
    
    misc_costs <- primary_costs %>%
      filter(CostVariable != "Taxes") %>%
      group_by(County) %>%
      summarise(Cost = sum(Cost, na.rm = TRUE) * 0.10, .groups = 'drop') %>%
      mutate(CostVariable = "Miscellaneous")
    
    final_costs_long <- bind_rows(primary_costs, misc_costs) %>% select(County, CostVariable, Cost)
    
    template <- expand.grid(CostVariable = cost_variables_list, County = counties, stringsAsFactors = FALSE)
    
    complete_costs <- left_join(template, final_costs_long, by = c("CostVariable", "County"))
    
    pivoted_costs <- complete_costs %>%
      pivot_wider(names_from = County, values_from = Cost, values_fill = 0)
    
    pivoted_costs %>%
      mutate(CostVariable = factor(CostVariable, levels = cost_variables_list)) %>%
      arrange(CostVariable) %>%
      rename(` ` = CostVariable)
  }
  
  format_comparison_table <- function(df) {
    df_char <- df %>% mutate(` ` = as.character(` `))
    
    misc_row <- df_char %>% filter(` ` == "Miscellaneous")
    df_ordered <- df_char %>% filter(` ` != "Miscellaneous") %>% bind_rows(misc_row)
    
    monthly_total_row <- df_ordered %>% summarise(across(where(is.numeric), sum, na.rm = TRUE)) %>% mutate(` ` = "Monthly Total")
    annual_total_row <- monthly_total_row %>% mutate(across(where(is.numeric), ~ . * 12), ` ` = "Annual Total")
    
    df_with_totals <- bind_rows(df_ordered, monthly_total_row, annual_total_row)
    df_with_totals %>% mutate(across(where(is.numeric), ~case_when(is.na(.) ~ "N/A", . == 0 ~ "$0", TRUE ~ paste0("$", trimws(format(round(., 0), nsmall = 0, big.mark = ","))))))
  }
  
  output$custom_table_min <- renderTable({
    req(input$compare_counties_min)
    result_df <- calculate_custom_cost("min", input$compare_counties_min, input$num_adults_min, input$num_children_min, input$num_childcare_min, input$num_elders_min)
    format_comparison_table(result_df)
  }, striped = FALSE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE)
  
  output$custom_table_avg <- renderTable({
    req(input$compare_counties_avg)
    result_df <- calculate_custom_cost("avg", input$compare_counties_avg, input$num_adults_avg, input$num_children_avg, input$num_childcare_avg, input$num_elders_avg)
    format_comparison_table(result_df)
  }, striped = FALSE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE)
  
}

shinyApp(ui = ui, server = server)