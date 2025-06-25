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
# Load Virginia county boundaries data once
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

# --- Load and Process REAL Data from Cleaned CSVs ---

# Load Utilities data
min_utilities_raw <- read_csv("minimum_final_utilities_cleaned.csv")
avg_utilities_raw <- read_csv("average_final_utilities_cleaned.csv")

# Load Elder Care data
min_elder_care_raw <- read_csv("minimum_elder_care_cost.csv")
avg_elder_care_raw <- read_csv("average_elder_care_cost.csv")

# *** NEW: Load Transportation data ***
min_transportation_raw <- read_csv("minimum_transportation_data.csv")
avg_transportation_raw <- read_csv("average_transportation_data.csv")

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

# Function to apply standardization and convert cost columns to numeric
process_data <- function(df) {
  df %>%
    standardize_cols() %>%
    # Ensure all cost columns are numeric for calculations
    mutate(across(all_of(c(family_structures_list, "Total Monthly Cost")), as.numeric))
}

# Process all data files
min_utilities_data <- process_data(min_utilities_raw)
avg_utilities_data <- process_data(avg_utilities_raw)
min_elder_care_data <- process_data(min_elder_care_raw)
avg_elder_care_data <- process_data(avg_elder_care_raw)
# *** NEW: Process Transportation data ***
min_transportation_data <- process_data(min_transportation_raw)
avg_transportation_data <- process_data(avg_transportation_raw)


# --- Create Unified Data Sources ---

# 1. Tidy data source for the detailed TABLE
all_costs_long_for_table <- bind_rows(
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
  # *** NEW: Add Transportation data for the table ***
  min_transportation_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Transportation", Type = "min"),
  avg_transportation_data %>%
    pivot_longer(cols = all_of(family_structures_list), names_to = "FamilyStructure", values_to = "Cost") %>%
    mutate(CostVariable = "Transportation", Type = "avg")
)

# 2. Tidy data source for the total cost BAR GRAPH
all_costs_for_plot <- bind_rows(
  min_utilities_data %>% select(County, Cost = `Total Monthly Cost`) %>% mutate(CostVariable = "Utilities", Type = "min"),
  avg_utilities_data %>% select(County, Cost = `Total Monthly Cost`) %>% mutate(CostVariable = "Utilities", Type = "avg"),
  min_elder_care_data %>% select(County, Cost = `Total Monthly Cost`) %>% mutate(CostVariable = "Elder Care", Type = "min"),
  avg_elder_care_data %>% select(County, Cost = `Total Monthly Cost`) %>% mutate(CostVariable = "Elder Care", Type = "avg"),
  # *** NEW: Add Transportation data for the bar graph ***
  min_transportation_data %>% select(County, Cost = `Total Monthly Cost`) %>% mutate(CostVariable = "Transportation", Type = "min"),
  avg_transportation_data %>% select(County, Cost = `Total Monthly Cost`) %>% mutate(CostVariable = "Transportation", Type = "avg")
)


# 3. Prepare Data for the MAP (Summing Total Costs from all files)
total_min_costs <- min_utilities_data %>%
  select(County, Cost_Utilities = `Total Monthly Cost`) %>%
  full_join(
    min_elder_care_data %>% select(County, Cost_ElderCare = `Total Monthly Cost`),
    by = "County"
  ) %>%
  # *** NEW: Join Transportation costs for the map ***
  full_join(
    min_transportation_data %>% select(County, Cost_Transportation = `Total Monthly Cost`),
    by = "County"
  ) %>%
  rowwise() %>%
  mutate(Cost = sum(c_across(starts_with("Cost_")), na.rm = TRUE)) %>%
  select(NAME = County, Cost)

total_avg_costs <- avg_utilities_data %>%
  select(County, Cost_Utilities = `Total Monthly Cost`) %>%
  full_join(
    avg_elder_care_data %>% select(County, Cost_ElderCare = `Total Monthly Cost`),
    by = "County"
  ) %>%
  # *** NEW: Join Transportation costs for the map ***
  full_join(
    avg_transportation_data %>% select(County, Cost_Transportation = `Total Monthly Cost`),
    by = "County"
  ) %>%
  rowwise() %>%
  mutate(Cost = sum(c_across(starts_with("Cost_")), na.rm = TRUE)) %>%
  select(NAME = County, Cost)

va_map_data_min <- left_join(va_counties, total_min_costs, by = "NAME")
va_map_data_avg <- left_join(va_counties, total_avg_costs, by = "NAME")


# --- UI Definition (Content Restored and Unchanged) ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      /* All CSS styles are preserved from your original code */
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
      .table-container { margin: 20px 0; border-radius: 10px; overflow: hidden; box-shadow: 0 4px 8px rgba(0,0,0,0.1); background-color: white; }
      table.data { width: 100%; border-collapse: collapse; margin: 0; font-size: 15px; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif; }
      table.data th { background: linear-gradient(135deg, #001f3f 0%, #004080 100%); color: white; font-weight: bold; padding: 15px 12px; text-align: left; border: none; font-size: 16px; text-shadow: 0 1px 2px rgba(0,0,0,0.3); }
      table.data th:first-child { background: linear-gradient(135deg, #2c5282 0%, #3182ce 100%); border-top-left-radius: 10px; }
      table.data th:last-child { border-top-right-radius: 10px; }
      table.data td { padding: 12px; border-bottom: 1px solid #e2e8f0; transition: background-color 0.2s ease; }
      table.data td:first-child { background: linear-gradient(135deg, #e6f3ff 0%, #cce7ff 100%); font-weight: bold; color: #1a365d; border-right: 2px solid #3182ce; position: relative; }
      table.data td:first-child::before { content: ''; position: absolute; left: 0; top: 0; bottom: 0; width: 4px; background: linear-gradient(to bottom, #3182ce, #2c5282); }
      table.data tbody tr:hover { background-color: #f7fafc; transform: translateY(-1px); box-shadow: 0 2px 4px rgba(0,0,0,0.1); }
      table.data tbody tr:hover td:first-child { background: linear-gradient(135deg, #d6f0ff 0%, #b3d9ff 100%); }
      table.data tbody tr:last-child td { border-bottom: none; }
      table.data tbody tr:nth-child(even) { background-color: #f8fafc; }
      table.data tbody tr:last-child { background: linear-gradient(135deg, #e6fffa 0%, #b3f5e6 100%); font-weight: bold; border-top: 2px solid #38a169; }
      table.data tbody tr:last-child td { color: #22543d; font-size: 16px; font-weight: bold; }
      table.data tbody tr:last-child td:first-child { background: linear-gradient(135deg, #38a169 0%, #2f855a 100%); color: white; text-shadow: 0 1px 2px rgba(0,0,0,0.3); }
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
      selected = "About",
      
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
                         tags$li(div(class = "about-variable-item", h4("Housing"), p("This variable represents the monthly cost associated with housing..."))),
                         tags$li(div(class = "about-variable-item", h4("Food"), p("Food costs cover the typical monthly expenses for groceries..."))),
                         tags$li(div(class = "about-variable-item", h4("Transportation"), p("Transportation expenses include costs related to commuting..."))),
                         tags$li(div(class = "about-variable-item", h4("Taxes"), p("This category includes estimated state and local income taxes..."))),
                         tags$li(div(class = "about-variable-item", h4("Healthcare"), p("Healthcare costs cover monthly premiums for health insurance..."))),
                         tags$li(div(class = "about-variable-item", h4("Childcare"), p("For families with children, childcare costs include expenses..."))),
                         tags$li(div(class = "about-variable-item", h4("Technology"), p("Technology costs encompass essential communication and digital access..."))),
                         tags$li(div(class = "about-variable-item", h4("Elder Care"), p("This variable accounts for potential costs associated with elder care..."))),
                         tags$li(div(class = "about-variable-item", h4("Utilities"), p("Utilities cover basic household services such as electricity..."))),
                         tags$li(div(class = "about-variable-item", h4("Miscellaneous"), p("The miscellaneous category covers a range of other essential expenses..."))),
                         tags$li(div(class = "about-variable-item", h4("Hourly Wage"), p("The hourly wage represents the estimated pre-tax hourly income...")))
                       ),
                       div(class = "section-title", "How to Use This Dashboard"),
                       tags$ol(
                         tags$li("To begin, the 'About' page provides a comprehensive introduction..."),
                         tags$li("Click on either the 'Minimum Cost' or 'Average Cost' tab..."),
                         tags$li("On either the 'Minimum Cost' or 'Average Cost' page, you will see a dropdown menu..."),
                         tags$li("When you select a county or city, the 'Cost Table', 'Interactive County Map', and 'Cost Breakdown Bar Chart' below will automatically update..."),
                         tags$li("The 'Cost Table' provides a detailed numerical breakdown of expenses..."),
                         tags$li("The 'Cost Breakdown Bar Chart' visualizes how different expense categories contribute..."),
                         tags$li("You can switch between the 'Minimum Cost' and 'Average Cost' tabs to compare..."),
                         tags$li("More instructions here...")
                       ),
                       div(class = "section-title", "Sources"),
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
      tabPanel("Minimum Cost",
               div(class = "content-container",
                   div(class = "intro-text", h4("What is Minimum Cost?"), p("The Minimum Cost represents a survival budget...")),
                   selectInput("county_min", "Select County or City:", choices = virginia_county_names, selected = virginia_county_names[1]),
                   div(class = "section-title", "Minimum Cost Table"),
                   div(class = "section-desc", "Monthly minimum cost by category..."),
                   div(class = "table-container", tableOutput("min_table")),
                   div(class = "section-title", "Interactive County Map"),
                   div(class = "section-desc", "This map displays the total minimum monthly cost..."),
                   leafletOutput("min_map", height = 450),
                   div(class = "section-title", "Cost Breakdown Bar Chart"),
                   div(class = "section-desc", "A visualization of the minimum cost components..."),
                   plotlyOutput("min_plot", height = 350),
                   div(class = "future-text-section", h4("Additional"), p("This section is reserved for future analysis..."))
               )
      ),
      tabPanel("Average Cost",
               div(class = "content-container",
                   div(class = "intro-text", h4("What is Average Cost?"), p("The Average Cost estimate reflects typical expenses...")),
                   selectInput("county_avg", "Select County or City:", choices = virginia_county_names, selected = virginia_county_names[1]),
                   div(class = "section-title", "Average Cost Table"),
                   div(class = "section-desc", "Monthly average cost by category..."),
                   div(class = "table-container", tableOutput("avg_table")),
                   div(class = "section-title", "Interactive County Map"),
                   div(class = "section-desc", "This map displays the total average monthly cost..."),
                   leafletOutput("avg_map", height = 450),
                   div(class = "section-title", "Cost Breakdown Bar Chart"),
                   div(class = "section-desc", "A visualization of the average cost components..."),
                   plotlyOutput("avg_plot", height = 350),
                   div(class = "future-text-section", h4("Additional"), p("This section is reserved for future analysis..."))
               )
      )
    )
  )
)

# --- Server Logic ---
server <- function(input, output, session) {
  
  # --- Reactive data filtering ---
  min_cost_data_for_table_filtered <- reactive({
    req(input$county_min)
    all_costs_long_for_table %>% filter(County == input$county_min, Type == "min")
  })
  
  avg_cost_data_for_table_filtered <- reactive({
    req(input$county_avg)
    all_costs_long_for_table %>% filter(County == input$county_avg, Type == "avg")
  })
  
  min_plot_data_filtered <- reactive({
    req(input$county_min)
    all_costs_for_plot %>% filter(County == input$county_min, Type == "min")
  })
  
  avg_plot_data_filtered <- reactive({
    req(input$county_avg)
    all_costs_for_plot %>% filter(County == input$county_avg, Type == "avg")
  })
  
  # --- Reusable Functions for Rendering ---
  # FIX: Rewritten function to prevent duplicate rows in the table
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
  
  # Function to format the final table for rendering
  format_cost_table <- function(df) {
    df$`Cost Variable` <- as.character(df$`Cost Variable`)
    
    total_row <- df %>%
      summarise(across(where(is.numeric), ~sum(., na.rm = TRUE))) %>%
      mutate(`Cost Variable` = "Total Cost")
    
    df_with_total <- bind_rows(df, total_row)
    
    # FIX: Modified case_when to show "$0.00" for zero values correctly
    df_with_total %>%
      mutate(across(where(is.numeric), ~case_when(
        is.na(.) ~ "N/A",
        . == 0 ~ "$0.00",
        TRUE ~ paste0("$", format(round(., 2), nsmall = 2, big.mark = ","))
      )))
  }
  
  # --- Minimum Cost Tab Outputs ---
  output$min_table <- renderTable({
    table_data <- generate_table_display(min_cost_data_for_table_filtered())
    format_cost_table(table_data)
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE)
  
  output$min_plot <- renderPlotly({
    plot_data <- min_plot_data_filtered()
    
    validate(need(nrow(plot_data) > 0, "No cost data available for this selection to plot."))
    
    p <- ggplot(plot_data, aes(
      x = CostVariable, 
      y = Cost,
      text = paste("Total Cost: $", format(round(Cost, 2), nsmall = 2, big.mark = ","))
    )) +
      geom_col(fill = "steelblue", width = 0.5) + 
      scale_x_discrete(limits = cost_variables_list) + 
      coord_cartesian(ylim = c(0, 20000)) + # Use coord_cartesian for zoom without data clipping
      labs(title = paste("Total Minimum Monthly Costs by Category -", input$county_min), y = "Total Monthly Cost ($)", x = "Cost Category") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$min_map <- renderLeaflet({
    pal <- colorNumeric(palette = c("green", "yellow", "red"), domain = va_map_data_min$Cost, na.color = "#bdbdbd")
    leaflet(va_map_data_min) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Cost), weight = 1, color = "white", fillOpacity = 0.7,
        popup = ~paste0("<div class='map-popup-title'>", NAME, "</div><div class='map-popup-value'><strong>Total Cost:</strong><br>$", format(round(Cost, 0), big.mark=",")),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Cost, title = "Total Monthly Cost", na.label = "No Data")
  })
  
  # --- Average Cost Tab Outputs ---
  output$avg_table <- renderTable({
    table_data <- generate_table_display(avg_cost_data_for_table_filtered())
    format_cost_table(table_data)
  }, striped = TRUE, hover = TRUE, spacing = "xs", width = "100%", rownames = FALSE)
  
  output$avg_plot <- renderPlotly({
    plot_data <- avg_plot_data_filtered()
    
    validate(need(nrow(plot_data) > 0, "No cost data available for this selection to plot."))
    
    p <- ggplot(plot_data, aes(
      x = CostVariable, 
      y = Cost,
      text = paste("Total Cost: $", format(round(Cost, 2), nsmall = 2, big.mark = ","))
    )) +
      geom_col(fill = "darkorange", width = 0.5) +
      scale_x_discrete(limits = cost_variables_list) +
      coord_cartesian(ylim = c(0, 20000)) + # Use coord_cartesian for zoom without data clipping
      labs(title = paste("Total Average Monthly Costs by Category -", input$county_avg), y = "Total Monthly Cost ($)", x = "Cost Category") +
      theme_minimal() +
      theme(
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1)
      )
    
    ggplotly(p, tooltip = "text")
  })
  
  output$avg_map <- renderLeaflet({
    pal <- colorNumeric(palette = c("lightgreen", "gold", "darkred"), domain = va_map_data_avg$Cost, na.color = "#bdbdbd")
    leaflet(va_map_data_avg) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~pal(Cost), weight = 1, color = "white", fillOpacity = 0.7,
        popup = ~paste0("<div class='map-popup-title'>", NAME, "</div><div class='map-popup-value'><strong>Total Cost:</strong><br>$", format(round(Cost, 0), big.mark=",")),
        highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
      ) %>%
      addLegend(pal = pal, values = ~Cost, title = "Total Monthly Cost", na.label = "No Data")
  })
}

shinyApp(ui = ui, server = server)