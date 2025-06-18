library(shiny)
library(tidyverse)
library(tigris)

va_counties <- counties(state = "VA", cb = TRUE)
virginia_county_names <- sort(unique(va_counties$NAME))

cost_variables <- c("Housing", "Food", "Transportation", "Taxes", "Healthcare",
                    "Childcare", "Technology", "Elder Care", "Utilities",
                    "Miscellaneous", "Hourly Wage")

family_structures <- c(
  "1 Adult: 19–50 Years",
  "2 Adults: 19–50 Years",
  "1 Child: 6–10 Years",
  "1 Adult + 1 Child",
  "2 Adults + 2 Children",
  "1 Adult: 65+",
  "2 Adults: 65+"
)

# Variables as rows, Structures as columns
generate_sample_cost_data <- function(base_cost) {
  data <- matrix(round(runif(length(family_structures) * length(cost_variables),
                             base_cost, base_cost * 2), 2),
                 nrow = length(cost_variables),
                 ncol = length(family_structures),
                 dimnames = list(cost_variables, family_structures))
  
  df <- as.data.frame(data)
  df$`Total Monthly Cost` <- round(rowSums(df), 2)
  df <- tibble::rownames_to_column(df, var = "Cost Variable")
  return(df)
}

# UI
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .custom-header {
        background-color: #001f3f;
        padding: 30px 20px;
        margin-bottom: 20px;
        text-align: center;
      }
      .custom-header h1 {
        color: white;
        font-size: 38px;
        font-weight: bold;
        margin: 0;
      }
      .intro-text, .project-intro {
        font-size: 17px;
        margin-bottom: 20px;
        padding: 15px;
        background-color: #f2f2f2;
        border-radius: 10px;
      }
      .county-dropdown {
        margin-bottom: 25px;
      }
      table {
        font-size: 14px;
      }
    "))
  ),
  
  div(class = "custom-header",
      h1("Virginia Cost of Living")
  ),
  
  div(class = "project-intro",
      h4("About This Dashboard"),
      p("This interactive dashboard was developed as part of the Virginia Tech Data Science for the Public Good (DSPG) Summer Research Program."),
      p("Its goal is to help citizens, policymakers, and researchers better understand the cost of living across all counties and independent cities in Virginia."),
      p("The dashboard presents two types of data: Minimum Cost (basic survival budget) and Average Cost (typical expense levels), tailored to different family structures."),
      p("The data used here includes variables such as housing, food, transportation, taxes, healthcare, childcare, and more. While the numbers shown are simulated for demonstration, they reflect the structure and categories our research focuses on."),
      p("Select a county or city using the dropdown menu on each page to explore localized cost-of-living estimates.")
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Minimum Cost",
               div(class = "intro-text",
                   h4("What is Minimum Cost?"),
                   p("This page shows the lowest estimated cost of living in each Virginia county or city. It includes essential needs like housing, food, transportation, and more for different family types.")
               ),
               div(class = "county-dropdown",
                   selectInput("county_min", "Select County or City:", choices = virginia_county_names)
               ),
               h4(textOutput("min_title")),
               tableOutput("min_table"),
               plotOutput("min_plot")
      ),
      
      tabPanel("Average Cost",
               div(class = "intro-text",
                   h4("What is Average Cost?"),
                   p("This page displays the average cost of living, based on commonly observed rates for services and expenses in each Virginia locality. It reflects a more typical spending pattern than the minimum estimate.")
               ),
               div(class = "county-dropdown",
                   selectInput("county_avg", "Select County or City:", choices = virginia_county_names)
               ),
               h4(textOutput("avg_title")),
               tableOutput("avg_table"),
               plotOutput("avg_plot")
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  
  output$min_title <- renderText({
    paste("Minimum Cost of Living in", input$county_min)
  })
  
  output$avg_title <- renderText({
    paste("Average Cost of Living in", input$county_avg)
  })
  
  output$min_table <- renderTable({
    generate_sample_cost_data(base_cost = 400)
  }, striped = TRUE, hover = TRUE, spacing = "xs")
  
  output$avg_table <- renderTable({
    generate_sample_cost_data(base_cost = 600)
  }, striped = TRUE, hover = TRUE, spacing = "xs")
  
  output$min_plot <- renderPlot({
    barplot(c(800, 300, 150, 400, 250), 
            names.arg = c("Housing", "Food", "Transport", "Healthcare", "Taxes"),
            main = paste("Min Cost Breakdown -", input$county_min),
            col = "steelblue")
  })
  
  output$avg_plot <- renderPlot({
    barplot(c(1000, 400, 250, 500, 350), 
            names.arg = c("Housing", "Food", "Transport", "Healthcare", "Taxes"),
            main = paste("Avg Cost Breakdown -", input$county_avg),
            col = "darkorange")
  })
}

shinyApp(ui = ui, server = server)
