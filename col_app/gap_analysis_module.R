library(shiny)
library(leaflet)
library(plotly)
library(tidyverse)
library(scales) 

# --- UI Function for the Gap Analysis Module ---
gap_analysis_ui <- function(id, family_structures_choices) {
  ns <- NS(id) # Create a namespace
  
  # This tagList contains all the UI elements for the tab
  tagList(
    
    # --- CSS for the legend ---
    tags$style(HTML(paste0(
      "#", ns("gap_map"), " .leaflet-control.legend { font-size: 10px; padding: 4px; }",
      "#", ns("gap_map"), " .legend i { width: 12px; height: 12px; margin-top: 2px; }",
      "#", ns("gap_map"), " .info.legend.leaflet-control { line-height: 1.2; }"
    ))),
    
    div(class = "content-container",
        div(class = "intro-text", h4(strong("What is the Income-Cost Gap?")), p("This section compares the cost of living with local income levels to reveal the financial well-being of households across Virginia. The 'gap' is the difference between monthly income and the monthly cost of living. A positive gap (surplus) means income exceeds costs, while a negative gap (deficit) means costs are higher than income, indicating potential financial strain.")),
        
        div(class = "section-title", "Gap Analysis Map"),
        p(class = "section-desc", "This map shows the monthly financial gap for a selected family type. Green areas have an income surplus, while red areas have a deficit. Use the controls to switch between minimum and average scenarios."),
        
        fluidRow(
          column(3,
                 selectInput(ns("gap_type"), "Select Cost/Income Level:", 
                             choices = c("Minimum" = "min", "Average" = "avg"), 
                             selected = "avg"),
                 selectInput(ns("gap_family_structure"), "Select Family Structure:", 
                             choices = family_structures_choices, # This now uses the argument
                             selected = family_structures_choices[4]),
                 div(class = "controls-info-box",
                     p("Use the dropdown menus to explore the financial gap across Virginia."),
                     p(strong("Cost/Income Level:"), " Choose 'Minimum' to compare survival costs with minimum wage data, or 'Average' to compare typical costs with average wage data."),
                     p(strong("Family Structure:"), " Select a household type to see how the gap changes for different families."),
                     p("The map, charts, and analysis below will update to show where households may be financially secure (green) or struggling (red).")
                 )
          ),
          column(9,
                 leafletOutput(ns("gap_map"), height = 450)
          )
        ),
        
        div(class = "section-title", "Counties with the Largest Gaps"),
        p(class = "section-desc", "These charts highlight the counties with the largest financial surpluses (where income most exceeds costs) and the counties with the largest deficits (where costs most exceed income)."),
        fluidRow(
          column(6, plotlyOutput(ns("surplus_plot"), height = 400)),
          column(6, plotlyOutput(ns("deficit_plot"), height = 400))
        ),
        
        # This section replaces the old data table with dynamic analysis
        div(class = "section-title", "Interpreting the Results"),
        div(class = "about-section",
            uiOutput(ns("gap_analysis_text")) # Dynamic text will be rendered here
        )
    )
  )
}

# --- Server Function for the Gap Analysis Module ---
gap_analysis_server <- function(id, gap_data_full) {
  moduleServer(id, function(input, output, session) {
    
    # 1. Reactive data frame filtered by user inputs
    filtered_gap_data <- reactive({
      req(input$gap_type, input$gap_family_structure)
      gap_data_full() %>%
        filter(Type == input$gap_type, FamilyStructure == input$gap_family_structure)
    })
    
    # 2. Render the Gap Map
    output$gap_map <- renderLeaflet({
      map_data <- filtered_gap_data()
      
      validate(need(nrow(map_data) > 0 && any(!is.na(map_data$MonthlyGap)), "No gap data to display for this selection.")) 
      
      max_abs_gap <- max(abs(map_data$MonthlyGap), na.rm = TRUE)
      pal <- colorNumeric(palette = "RdYlGn", domain = c(-max_abs_gap, max_abs_gap))
      
      popup_content <- paste0(
        "<strong>County: </strong>", map_data$NAME, "<br/>",
        "<strong>Monthly Cost: </strong>", dollar(map_data$TotalCost, accuracy = 1), "<br/>",
        "<strong>Monthly Income: </strong>", dollar(map_data$MonthlyIncome, accuracy = 1), "<br/>",
        "<strong>Monthly Gap: </strong>", dollar(map_data$MonthlyGap, accuracy = 1)
      )
      
      leaflet(map_data) %>%
        addProviderTiles("CartoDB.Positron") %>%
        setView(lng = -79.0, lat = 37.5, zoom = 7) %>%
        addPolygons(
          fillColor = ~pal(MonthlyGap), weight = 1, color = "white", fillOpacity = 0.8,
          popup = popup_content, label = ~paste0(NAME, ": ", dollar(MonthlyGap, accuracy = 1)),
          highlightOptions = highlightOptions(weight = 2, color = "#666", bringToFront = TRUE)
        ) %>%
        addLegend(pal = pal, values = ~MonthlyGap, title = "Monthly Gap (Surplus/Deficit)",
                  labFormat = labelFormat(prefix = "$"), opacity = 1, na.label = "")
    })
    
    # 3. Render Bar Charts for Surplus and Deficit
    output$surplus_plot <- renderPlotly({
      plot_data <- filtered_gap_data() %>%
        filter(!is.na(MonthlyGap) & MonthlyGap > 0) %>%
        # 1. Add distinct() for consistency and to prevent segmented bars.
        distinct(NAME, .keep_all = TRUE) %>%
        slice_max(order_by = MonthlyGap, n = 10)
      
      validate(need(nrow(plot_data) > 0, "No counties with a surplus for this selection."))
      
      p <- ggplot(plot_data, aes(x = MonthlyGap, y = reorder(NAME, MonthlyGap), text = paste0(NAME, ": ", dollar(MonthlyGap, accuracy = 1)))) +
        # 2. Add width to control bar thickness so it looks good with few counties.
        geom_col(fill = "#21908C", width = 0.8) + 
        labs(title = "Top Largest Surpluses", x = "Monthly Surplus ($)", y = "") + 
        theme_minimal() + 
        # 3. Add limits to ensure the x-axis always starts at 0.
        scale_x_continuous(labels = dollar, limits = c(0, max(plot_data$MonthlyGap) * 1.05))
      ggplotly(p, tooltip = "text")
    })
    
    output$deficit_plot <- renderPlotly({
      plot_data <- filtered_gap_data() %>%
        filter(!is.na(MonthlyGap) & MonthlyGap < 0) %>%
     
        distinct(NAME, .keep_all = TRUE) %>%
        slice_min(order_by = MonthlyGap, n = 10)
      
      validate(need(nrow(plot_data) > 0, "No counties with a deficit for this selection."))
      
      p <- ggplot(plot_data, aes(x = MonthlyGap, y = reorder(NAME, MonthlyGap, decreasing = FALSE), text = paste0(NAME, ": ", dollar(MonthlyGap, accuracy = 1)))) +
        geom_col(fill = "#D9534F") + 
        labs(title = "Top Largest Deficits", x = "Monthly Deficit ($)", y = "") +
        theme_minimal() + 
        # FIX 2: Set explicit limits on the x-axis to make bars stretch.
        scale_x_continuous(labels = dollar, limits = c(min(plot_data$MonthlyGap) * 1.05, 0))
      ggplotly(p, tooltip = "text")
    })
    
    # 4. Render the Detailed Analysis Text
    output$gap_analysis_text <- renderUI({
      data <- filtered_gap_data()
      validate(need(nrow(data) > 0 && any(!is.na(data$MonthlyGap)), "Select a scenario to see the analysis."))
      
      top_deficit <- data %>% filter(!is.na(MonthlyGap)) %>% slice_min(order_by = MonthlyGap, n = 1)
      scenario_type <- if(input$gap_type == "min") "minimum survival" else "average"
      income_type <- if(input$gap_type == "min") "minimum wage" else "average wage"
      
      tags$div(
        h4(strong("Understanding the Financial Gap")),
        p("The visualizations on this page reveal a critical story about economic well-being across Virginia. The monthly gap, the difference between income and the cost of living, shows whether a typical household in a county can make ends meet. A ", strong("green county (surplus)"), " suggests that the ", income_type, " is enough to cover the estimated ", scenario_type, " costs. In contrast, a ", strong("red county (deficit)"), " indicates that the income is not sufficient, forcing households to face difficult financial choices."),
        h4("What Drives the Deficit in Red Counties?"),
        p("Counties appear red for different reasons, highlighting diverse economic challenges across the state:"),
        tags$ul(
          tags$li(strong("High Cost of Living:"), " In regions like Northern Virginia (like Arlington, Alexandria), costs for housing and childcare are exceptionally high. Even with higher-than-average incomes, these costs can create a significant deficit, making it difficult for even middle-income families to afford a stable lifestyle."),
          tags$li(strong("Low Local Wages:"), " In some rural and southern parts of Virginia, the cost of living may be lower, but wages have not kept pace. In these areas, a deficit occurs because incomes are insufficient to cover even a modest budget, leaving households economically vulnerable."),
          tags$li(strong("The 'Average' vs. 'Minimum' Scenarios:"), " When viewing the ", strong("Minimum"), " scenario, red counties represent areas where even a survival budget may be out of reach for those earning minimum wage. When switching to the ", strong("Average"), " scenario, you can see how many more counties turn red, illustrating the widespread challenge of achieving a modest, stable lifestyle on an average income.")
        ),
        h4("Key Takeaways from the Charts"),
        p("The bar charts of the top 10 surpluses and deficits provide a clear ranking of the most and least affordable places for the selected family type. The county with the largest deficit is currently ", strong(top_deficit$NAME), ", where the monthly costs exceed income by ", strong(dollar(abs(top_deficit$MonthlyGap), accuracy = 1)), ". This amounts to an annual shortfall of over ", strong(dollar(abs(top_deficit$MonthlyGap * 12), accuracy = 1)), ", a gap that can lead to debt, housing instability, and food insecurity."),
        p("This analysis is crucial for policymakers, community leaders, and residents to understand the true cost of living and advocate for solutions like affordable housing, childcare support, and policies that promote wage growth.")
      )
    })
  })
}



