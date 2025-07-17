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
                           h2("About This Dashboard"),
                           p("The Virginia Cost of Living Dashboard was created as part of the Virginia Tech Data Science for the Public Good (DSPG) Summer Research Program in 2025. This tool helps individuals, policymakers, and researchers explore how the cost of living varies across Virginia’s 133 counties and independent cities."),
                           
                           p("It includes two views: the minimum cost of living (a basic survival budget) and the average cost of living (a more typical monthly budget). Users can view breakdowns by family structure and customize scenarios to see how costs differ from one place to another."),
                           
                           p(strong("Important Note:"), " All values in this dashboard are ", strong("nominal"), " and represent the cost of living in ", strong("2023"), ". The estimates are ", strong("not seasonally adjusted"), ", meaning they reflect the average for the year rather than short-term fluctuations."),
                           
                           div(class = "section-title", "Why This Matters"),
                           tags$ul(
                             tags$li("Families can use this data to plan where to live based on affordability."),
                             tags$li("Policymakers can make better decisions on living wages, housing policies, and public assistance programs."),
                             tags$li("Businesses can plan compensation and understand workforce needs."),
                             tags$li("Researchers can study regional economic disparities across the state.")
                           ),
                           
                           div(class = "section-title", "Minimum vs. Average Costs"),
                           p("We report both 'Minimum Cost' and 'Average Cost' estimates. The Minimum Cost reflects a no-frills survival budget — the lowest amount needed to cover basic needs. The Average Cost includes a more typical level of spending based on how people actually live, allowing for some discretionary items and comfort."),
                           
                           p("Both perspectives are important. The minimum cost can highlight where people are most economically vulnerable, while the average cost shows what’s needed for a modest, stable life."),
                           
                           div(class = "section-title", "What’s Included"),
                           p("This dashboard breaks down the total monthly cost of living into categories like housing, food, transportation, healthcare, taxes, childcare, elder care, technology, and miscellaneous expenses. These estimates are calculated separately for six common family structures: 1 Adult: 19-50 Years, 2 Adults: 19-50 Years, 1 Adult 1 Child, 2 Adults 2 Children, 1 Adults 65+, and 2 Adults 65+."),
                           
                           div(class = "section-title", "How to Use This Tool"),
                           tags$ol(
                             tags$li("Start with this page to understand the goals and features of the dashboard."),
                             tags$li("Go to the 'Minimum Cost' or 'Average Cost' tabs to explore data for different counties and family types. (More instructions are provided on each page.) "),
                             tags$li("Use the dropdown menus and maps to see how costs vary by location and household structure."),
                             tags$li("Compare multiple counties side-by-side or build custom family profiles to see tailored results.")
                           ),
                           
                           div(class = "section-title", "Who Made This"),
                           p("This dashboard was developed by Feda Mohammadi and Julia Vecharello through the Virginia Tech DSPG Summer Research Program. We hope this tool helps make local cost data more accessible and useful to everyone.")
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
                                    p("This interactive map lets you see how the average monthly cost of living changes from one Virginia county or city to another. To use this map, select a family structure from the dropdown menu. The map will then update to show how much, on average, it costs for families of that type to live in each area."),
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
                       div(class = "methodology-section",
                           div(class = "section-title", "Methodology Overview"),
                           p("Our methodology estimates the monthly cost of living across Virginia's 133 counties and independent cities using the data sources from 2023. We compiled data from authoritative national and state-level sources for nine essential cost categories. For each category, we developed both a 'Minimum Cost' and an 'Average Cost' estimate to represent a basic survival budget versus a more typical, stable living standard. Costs were then tailored to six common family structures to reflect how expenses change with household size and composition. Methodologies that rely on proxies (The Healthcare and Elder Care data), such as scaling by local income, do so because direct, county-level data for that specific cost is unavailable. This approach allows for consistent and reasonable estimates across all localities."),
                           
                           div(class = "section-title", "Our Variables"),
                           
                           div(class = "about-variable-item",
                               h4(strong("Housing & Utilities")),
                               p("Housing costs were estimated using the 2023 Fair Market Rent (FMR) data from the U.S. Department of Housing and Urban Development (HUD). FMRs represent the 40th percentile of gross rents for a modest housing unit in a specific locality and importantly, they already include the cost of basic utilities like electricity, water, and heat. We assigned apartment sizes based on family type (e.g., 1-bedroom for single adults, 3-bedroom for a family of four)."),
                               p("Minimum Cost was set at 110% of the FMR. The 10% buffer accounts for potential utility costs that may exceed HUD's standard allowance, ensuring a realistic survival budget. And the Average Cost was set at 125% of the FMR. This multiplier provides a more typical market-rate rent, reflecting what households not receiving subsidies might pay for slightly better housing."),
                               
                               withMathJax(),
                               helpText("$$\\text{Minimum Housing Cost} = \\text{FMR} \\times 1.10$$"),
                               helpText("$$\\text{Average Housing Cost} = \\text{FMR} \\times 1.25$$")
                               
                           ),
                           
                           div(class = "about-variable-item",
                               h4(strong("Food")),
                               p("Food costs are based on the U.S. Department of Agriculture's (USDA) official Food Plans, which provide cost estimates for a healthy diet at different budget levels. We calculated the per-person monthly cost and multiplied it by the number of individuals in each family structure."),
                               p("Minimum Cost uses the USDA's 'Thrifty Food Plan.' This plan represents the bare minimum cost for a nutritionally adequate diet and is the basis for the federal SNAP (food stamp) program. And the Average Cost uses the USDA's 'Moderate-Cost Food Plan,' which allows for a wider variety of foods and represents a more realistic budget for a typical middle-class household."),
                               
                               withMathJax(),
                               helpText("$$\\text{Total Food Cost} = \\text{Per Person Cost} \\times \\text{Household Size}$$")
                           ),
                           
                           div(class = "about-variable-item",
                               h4(strong("Transportation")),
                               p("Transportation costs were estimated using a weighted model that combines vehicle and public transit expenses at the county level. We used data from the U.S. Census Bureau (ACS) to determine the share of workers who drive versus use public transit in each county. A base cost was calculated using AAA's national average for vehicle ownership, scaled by local vehicle availability, and combined with local transit pass prices."),
                               p("This county-level base cost was then adjusted using multipliers to reflect the different travel needs of each family structure (e.g., two-adult households have double the cost of one-adult households, while seniors have reduced costs). The Minimum Cost is set at 85% of this final value to reflect more conservative travel behavior, while the Average Cost is 100%."),
                               
                               withMathJax(),
                               helpText("$$\\text{Minimum Transportation Cost} = \\text{Average Cost} \\times 0.85$$")
                           ),
                           
                           
                           div(class = "about-variable-item",
                               h4(strong("Taxes")),
                               p("Tax estimates include federal and state income taxes, federal payroll taxes (Social Security and Medicare), and considerations for tax credits. The calculation begins by estimating the annual income for each family structure based on county-level wage data from the U.S. Census Bureau (ACS)."),
                               p("Average Cost is calculated directly from this average income using 2023 tax brackets, standard deductions, and applicable credits like the Child Tax Credit and Dependent Care Credit."),
                               p("Minimum Cost is an approximation derived by scaling down the average tax burden. We calculated a ratio of the minimum survival income to the average income for a given family and applied this ratio to the average tax amount, providing a reasonable estimate for taxes at a lower income level."), 
                               withMathJax(),
                               helpText("$$\\text{Minimum Tax} = \\text{Average Tax} \\times \\left( \\frac{\\text{Minimum Income}}{\\text{Average Income}} \\right)$$")
                           ),
                           
                           div(class = "about-variable-item",
                               h4(strong("Healthcare")),
                               p("Since direct county-level healthcare cost data is unavailable, we used an indirect estimation method. We started with state-level baseline annual costs for a household, derived from data published by KFF and the Centers for Medicare & Medicaid Services (CMS). These base costs were then scaled for each county using a ratio of the county's median household income to Virginia's statewide median income (both from the ACS). This common economic practice assumes that healthcare costs tend to be higher in areas with higher incomes."),
                               p("Finally, these county-scaled costs were adjusted using multipliers for each family structure to account for different healthcare needs based on age and household size (e.g., families with children have higher costs). These multipliers were adapted from models like the MIT Living Wage Calculator. The Minimum Cost and Average Cost scenarios are based on different starting baseline figures to reflect lower- and higher-cost health plans, respectively."), 
                               withMathJax(),
                               helpText("$$\\text{County Healthcare Cost} = \\text{Base Cost} \\times \\left( \\frac{\\text{County Median Income}}{\\text{State Median Income}} \\right)$$"),
                               helpText("$$\\text{Final Healthcare Cost} = \\text{County Cost} \\times \\text{Multiplier}_{\\text{family}}$$")
                           ),
                           
                           div(class = "about-variable-item",
                               h4(strong("Childcare")),
                               p("Childcare costs were calculated using county-level 2024 price data from the National Database of Childcare Prices (NDCP). This dataset provides annual minimum and general (average) prices for both home-based and center-based care across different child age groups (infant, toddler, etc.). For each county, we first calculated an average cost across all age groups for both home-based and center-based care, and then averaged these two values together to get a single 'combined' cost for the county."),
                               p("This annual figure was converted to a monthly cost. For families with children, this per-child cost was multiplied by the number of children in the household (e.g., twice the cost for a two-child family). For households with no children, the childcare cost is zero."), 
                               withMathJax(),
                               helpText("$$\\text{Monthly Childcare Cost} = \\left( \\frac{\\text{Annual Cost}}{12} \\right) \\times \\text{Number of Children}$$")
                           ),
                           
                           div(class = "about-variable-item",
                               h4(strong("Technology")),
                               p("Technology costs are defined as one broadband internet connection per household plus one smartphone plan per adult. Costs are based on national benchmarks and are assumed to be uniform across all Virginia counties, as local data is not available."),
                               p("Minimum Cost uses baseline costs of $77/month for internet and $15/month per adult for a smartphone plan. And the Average Cost uses higher benchmarks of $81/month for internet and $64.50/month per adult for a smartphone plan. Households with no adults have a technology cost of zero."), 
                               withMathJax(),
                               helpText("$$\\text{Minimum Technology Cost} = 77 + (15 \\times \\text{Number of Adults})$$"),
                               helpText("$$\\text{Average Technology Cost} = 81 + (64.5 \\times \\text{Number of Adults})$$")
                           ),
                           
                           div(class = "about-variable-item",
                               h4(strong("Elder Care")),
                               p("Elder care costs are estimated for the '1 Adult: 65+' and '2 Adults: 65+' family structures using an income-scaling method similar to healthcare. We used Virginia-specific monthly median costs from the Genworth 2024 Cost of Care Survey as our baseline."),
                               p("Minimum Cost is based on the median cost of Adult Day Health Care ($1,766/month). And Average Cost is based on the median cost of an Assisted Living Facility ($6,512/month)."),
                               p("These state-level base costs were then scaled for each county using the ratio of local to state median household income. The cost for a two-elder household was set at 1.8 times the single-elder cost to account for shared expenses."), 
                               withMathJax(),
                               helpText("$$\\text{County Elder Care Cost} = \\text{Base Cost} \\times \\left( \\frac{\\text{County Median Income}}{\\text{State Median Income}} \\right)$$"),
                               helpText("$$\\text{Two-Elder Cost} = \\text{Single Elder Cost} \\times 1.8$$")
                           ),
                           
                           div(class = "about-variable-item",
                               h4(strong("Miscellaneous")),
                               p("The miscellaneous category covers a range of other essential but non-itemized expenses, such as clothing, personal care products, and household supplies. Following a common budgeting principle, this cost is estimated as 10% of the total monthly budget, excluding taxes. It provides a buffer for minor, unpredictable expenses that are a normal part of life."), 
                               withMathJax(),
                               helpText("$$\\text{Miscellaneous Cost} = 0.10 \\times (\\text{Total Monthly Cost} - \\text{Taxes})$$")
                           ),
                           
                           div(class = "about-variable-item",
                               h4(strong("Hourly Wage")),
                               p("The hourly wage is not shown in the table, but we used it for the taxes calculation. It represents the pre-tax hourly wage a household's earner(s) must make to cover the total monthly cost of living. It is calculated by taking the 'Annual Total' cost for a given family structure and dividing it by 2,080 (the standard number of work hours in a year, based on a 40-hour work week)."), 
                               withMathJax(),
                               helpText("$$\\text{Hourly Wage} = \\frac{\\text{Annual Total Cost}}{2,080}$$")
                           ),
                           
                           
                           div(class = "section-title", "Sources"),
                           p("Data was compiled and processed from a variety of public and private sources to ensure accuracy and relevance. The following key sources were used in this project:"),
                           tags$ul(class = "sources-list",
                                   tags$li(strong("AAA. (2023)."), em("Your Driving Costs."), "Retrieved from the AAA Exchange website."),
                                   tags$li(strong("Dunn, A., Grosse, S. D., & Zuvekas, S. H. (2018)."), "Adjusting Health Expenditures for Inflation: A Review of Measures for Health Services Research in the United States.", em("Health Services Research, 53"),"(1), 175–196. ", tags$a(href="https://doi.org/10.1111/1475-6773.12618", target="_blank", "https://doi.org/10.1111/1475-6773.12618")),
                                   tags$li(strong("Genworth. (2024)."), em("Cost of Care Survey."), "Retrieved from ", tags$a(href="https://www.genworth.com/aging-and-you/finances/cost-of-care.html", target="_blank", "https://www.genworth.com/aging-and-you/finances/cost-of-care.html")),
                                   tags$li(strong("Internal Revenue Service. (2022)."), em("IRS provides tax inflation adjustments for tax year 2023."), "Retrieved from ", tags$a(href="https://www.irs.gov/newsroom/irs-provides-tax-inflation-adjustments-for-tax-year-2023", target="_blank", "https://www.irs.gov/newsroom/irs-provides-tax-inflation-adjustments-for-tax-year-2023")),
                                   tags$li(strong("KFF. (2022)."), em("Health Care Expenditures per Capita by Service by State of Residence."), "Retrieved July 14, 2025, from ", tags$a(href="https://www.kff.org/other/state-indicator/health-spending-per-capita-by-service/", target="_blank", "www.kff.org")),
                                   tags$li(strong("MIT. (n.d.)."), em("Living Wage Calculator Methodology."), "Retrieved from ", tags$a(href="https://livingwage.mit.edu/pages/methodology", target="_blank", "https://livingwage.mit.edu/pages/methodology")),
                                   tags$li(strong("Pebesma, E. (2018)."), "Simple Features for R: Standardized Support for Spatial Vector Data.", em("The R Journal, 10"),"(1), 439-446. ", tags$a(href="https://doi.org/10.32614/RJ-2018-009", target="_blank", "https://doi.org/10.32614/RJ-2018-009")),
                                   tags$li(strong("Posit Team. (2023)."), em("RStudio: Integrated Development Environment for R."), "Posit Software, PBC. ", tags$a(href="http://www.posit.co/", target="_blank", "http://www.posit.co/")),
                                   tags$li(strong("R Core Team. (2023)."), em("R: A Language and Environment for Statistical Computing."), "R Foundation for Statistical Computing. ", tags$a(href="https://www.R-project.org/", target="_blank", "https://www.R-project.org/")),
                                   tags$li(strong("United for ALICE. (n.d.)."), em("The Cost of Basics."), "Retrieved from ", tags$a(href="https://www.unitedforalice.org/the-cost-of-basics/virginia", target="_blank", "https://www.unitedforalice.org/the-cost-of-basics/virginia")),
                                   tags$li(strong("U.S. Census Bureau. (2023)."), em("American Community Survey 5-Year Estimates (Table S1901)."), "Retrieved from ", tags$a(href="https://data.census.gov", target="_blank", "https://data.census.gov")),
                                   tags$li(strong("U.S. Census Bureau. (2023)."), em("American Community Survey 5-Year Estimates (Table B08101)."), "Retrieved from ", tags$a(href="https://data.census.gov", target="_blank", "https://data.census.gov")),
                                   tags$li(strong("U.S. Census Bureau. (2023)."), em("American Community Survey 5-Year Estimates (Table B25044)."), "Retrieved from ", tags$a(href="https://data.census.gov", target="_blank", "https://data.census.gov")),
                                   tags$li(strong("U.S. Department of Agriculture. (2023)."), em("Cost of Food Reports."), "Retrieved from ", tags$a(href="https://www.fns.usda.gov/cnpp/usda-food-plans-cost-food-monthly-reports", target="_blank", "https://www.fns.usda.gov")),
                                   tags$li(strong("U.S. Department of Housing and Urban Development. (2023)."), em("Fair Market Rents."), "Retrieved from ", tags$a(href="https://www.huduser.gov/portal/datasets/fmr.html", target="_blank", "https://www.huduser.gov/portal/datasets/fmr.html")),
                                   tags$li(strong("U.S. Department of Labor, Women's Bureau. (2024)."), em("National Database of Childcare Prices."), "DataLumos. Retrieved July 14, 2025, from ", tags$a(href="https://www.dol.gov/agencies/wb/topics/featured-childcare", target="_blank", "www.dol.gov/agencies/wb/topics/featured-childcare")),
                                   tags$li(strong("Virginia Department of Taxation. (n.d.)."), em("Individual Income Tax."), "Retrieved from ", tags$a(href="https://www.tax.virginia.gov/individual-income-tax", target="_blank", "https://www.tax.virginia.gov/individual-income-tax")),
                                   tags$li(strong("Walker, K. (2023)."), em("tigris: Load Census TIGER/Line Shapefiles (R package version 2.2.1)."), tags$a(href="https://cran.r-project.org/package=tigris", target="_blank", "https://cran.r-project.org/package=tigris")),
                                   tags$li(strong("Wickham, H., et al. (2023)."), em("dplyr: A Grammar of Data Manipulation (R package version 1.1.4)."), tags$a(href="https://dplyr.tidyverse.org", target="_blank", "https://dplyr.tidyverse.org")),
                                   tags$li(strong("Wickham, H., et al. (2023)."), em("tidyr: Tidy Messy Data (R package version 1.3.1)."), tags$a(href="https://tidyr.tidyverse.org", target="_blank", "https://tidyr.tidyverse.org"))
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
    
    popup_content <- sapply(map_data$NAME, function(county_name) {
      req(input$family_structure_map_min)
      
      # Filter data for the specific county, family structure, and type.
      county_data <- all_costs_long_for_table %>%
        filter(
          County == county_name,
          FamilyStructure == input$family_structure_map_min,
          Type == "min"
        )
      
      # Create a complete, ordered data frame for all cost variables.
      # By adding this we make sure that all categories are present, NAs are handled, and order is maintained.
      popup_df <- tibble(CostVariable = cost_variables_list) %>%
        left_join(county_data %>% select(CostVariable, Cost), by = "CostVariable") %>%
        mutate(Cost = replace_na(Cost, 0))
      
      # Calculate total cost and percentage contribution for each category.
      total_cost <- sum(popup_df$Cost)
      popup_df <- popup_df %>%
        mutate(Percentage = if (total_cost > 0) (Cost / total_cost) * 100 else 0)
      
      # Format each line for the popup
      breakdown_lines <- sprintf(
        "%s: $%s (%s%%)",
        popup_df$CostVariable,
        format(round(popup_df$Cost), nsmall = 0, big.mark = ","),
        round(popup_df$Percentage)
      )
      
      # Assemble the final HTML content for the popup.
      breakdown_html <- paste0(
        "<strong>County: </strong>", htmltools::htmlEscape(county_name), "<br><br>",
        paste(breakdown_lines, collapse = "<br>"),
        "<br><br><strong>Total Monthly Cost (minimum):</strong> $",
        format(round(total_cost), nsmall = 0, big.mark = ",")
      )
      
      return(breakdown_html)
    }, USE.NAMES = FALSE)
    
    costs <- map_data$TotalCost[is.finite(map_data$TotalCost)]
    bins <- unique(quantile(costs, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
    bins <- unique(round(bins, -3))
    if (length(bins) < 2) {
      bins <- c(min(costs, na.rm = TRUE), max(costs, na.rm = TRUE))
    }
    pal <- colorBin(palette = "plasma", domain = map_data$TotalCost, bins = bins, na.color = "#bdbdbd", reverse = TRUE)
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal(TotalCost), weight = 1, color = "white", fillOpacity = 0.8,
        popup = popup_content,
        label = ~NAME,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
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
    
    popup_content <- sapply(map_data$NAME, function(county_name) {
      req(input$family_structure_map_avg)
      
      # Filter data for the specific county, family structure, and type.
      county_data <- all_costs_long_for_table %>%
        filter(
          County == county_name,
          FamilyStructure == input$family_structure_map_avg,
          Type == "avg"
        )
      
      # Create a complete, ordered data frame for all cost variables.
      
      popup_df <- tibble(CostVariable = cost_variables_list) %>%
        left_join(county_data %>% select(CostVariable, Cost), by = "CostVariable") %>%
        mutate(Cost = replace_na(Cost, 0))
      
      # Calculate total cost and percentage contribution for each category.
      total_cost <- sum(popup_df$Cost)
      popup_df <- popup_df %>%
        mutate(Percentage = if (total_cost > 0) (Cost / total_cost) * 100 else 0)
      
      # Format each line for the popup
      breakdown_lines <- sprintf(
        "%s: $%s (%s%%)",
        popup_df$CostVariable,
        format(round(popup_df$Cost), nsmall = 0, big.mark = ","),
        round(popup_df$Percentage)
      )
      
      # Assemble the final HTML content for the popup.
      breakdown_html <- paste0(
        "<strong>County: </strong>", htmltools::htmlEscape(county_name), "<br><br>",
        paste(breakdown_lines, collapse = "<br>"),
        "<br><br><strong>Total Monthly Cost (average):</strong> $",
        format(round(total_cost), nsmall = 0, big.mark = ",")
      )
      
      return(breakdown_html)
    }, USE.NAMES = FALSE)
    
    costs <- map_data$TotalCost[is.finite(map_data$TotalCost)]
    bins <- unique(quantile(costs, probs = seq(0, 1, length.out = 6), na.rm = TRUE))
    bins <- unique(round(bins, -3))
    if (length(bins) < 2) {
      bins <- c(min(costs, na.rm = TRUE), max(costs, na.rm = TRUE))
    }
    pal <- colorBin(palette = "viridis", domain = map_data$TotalCost, bins = bins, na.color = "#bdbdbd", reverse = TRUE)
    
    leaflet(map_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
      addPolygons(
        fillColor = ~pal(TotalCost), weight = 1, color = "white", fillOpacity = 0.8,
        popup = popup_content,
        label = ~NAME,
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto"),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~TotalCost, title = "Total Monthly Cost", na.label = "No Data", opacity = 1)
  })
  
  
  # --- The Server Logic for Custom Comparison ---
  
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



