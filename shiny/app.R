library(shiny)
library(data.table)
library(DT)
library(arulesViz)
library(ggplot2)
library(jsonlite)

# --- Data Loading ---

# Define relative paths from shiny directory (app.R location)
path_retail_clean <- "../data/processed/retail_clean.csv"
path_rfm_segmented <- "../data/processed/rfm_segmented.csv"
path_rules <- "../data/processed/association_rules.rds"
path_nbo_engine <- "../src/05_nbo_engine.R"

# Load the data directly without checking if paths exist
retail <- fread(path_retail_clean)
rfm <- fread(path_rfm_segmented)
rules <- readRDS(path_rules)

cat("Loading data complete👍\n")

# Source the engine script (with working directory changing)
original_wd <- getwd()
source(path_nbo_engine, chdir = TRUE)
setwd(original_wd)

# --- Define UI ---
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {background-color: #f7f9fc; font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;}
      h4 {color: #2c3e50; font-weight: 600;}
      .shiny-input-container {margin-bottom: 15px;}
      .sidebar .well {background-color: #ecf0f1; border-radius: 10px; padding: 20px;}
      .tab-content {background-color: #ffffff; padding: 20px; border-radius: 10px; box-shadow: 0 4px 15px rgba(0,0,0,0.1);}
      .datatable caption {font-weight: 700; font-size: 1.1em; color: #34495e; margin-bottom: 10px;}
      .btn {background-color: #3498db; border-color: #2980b9; color: white;}
      .btn:hover {background-color: #2980b9; color: white;}
    "))
  ),
  
  titlePanel(
    div(style = "font-weight: bold; font-size: 30px; color: #2980b9;",
        "Next Best Offer (NBO) Engine Dashboard")
  ),
  
  sidebarLayout(
    sidebarPanel(
      h4("🔍 Customer Selection"),
      selectInput("customer_id", "Select Customer ID:",
                  choices = sort(unique(rfm$CustomerID)),
                  selected = sample(rfm$CustomerID, 1)),
      numericInput("top_k", "Top K Recommendations:", value = 5, min = 1, max = 10),
      actionButton("recommend", "Generate Recommendations", icon = icon("play"), class = "btn btn-primary btn-lg"),
      hr(),
      h4("📊 View Options"),
      checkboxInput("show_rules_table", "Show Association Rules Table", TRUE),
      checkboxInput("show_rules_plot", "Show Association Rules Plot", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard Overview",
                 h4("Customer Segment Distribution"),
                 plotOutput("segmentPlot", height = "350px"),
                 hr(),
                 h4("Top 15 Most Popular Products"),
                 plotOutput("topItemsPlot", height = "350px")
        ),
        tabPanel("Recommendations",
                 h4("Recommended Offers for Selected Customer"),
                 DTOutput("recommendTable"),
                 hr(),
                 h4("JSON Output"),
                 verbatimTextOutput("jsonPreview")
        ),
        tabPanel("Association Rules Explorer",
                 uiOutput("rulesUI")
        )
      )
    )
  )
)

# --- Define Server logic ---
server <- function(input, output, session) {
  
  recommendation_data <- eventReactive(input$recommend, {
    customer_id <- input$customer_id
    k <- input$top_k
    
    if (exists("compute_nbo_score")) {
      recs <- compute_nbo_score(customer_id, top_k = k)
      return(list(recommendations = recs, customer = customer_id))
    } else {
      return(list(recommendations = data.table(Error = "Recommendation function not found."), customer = customer_id))
    }
  })
  
  output$segmentPlot <- renderPlot({
    seg_data <- rfm[, .N, by = Segment]
    ggplot(seg_data, aes(x = factor(Segment), y = N, fill = factor(Segment))) +
      geom_bar(stat = "identity", show.legend = FALSE) +
      labs(title = "Customer Segment Distribution", x = NULL, y = "Number of Customers") +
      theme_minimal(base_size = 15) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$topItemsPlot <- renderPlot({
    top_items <- retail[, .N, by = Description][order(-N)][1:15]
    ggplot(top_items, aes(x = reorder(Description, N), y = N)) +
      geom_bar(stat = "identity", fill = "#3498db") +
      coord_flip() +
      labs(title = "Top 15 Most Purchased Products", x = NULL, y = "Total Purchase Count") +
      theme_minimal(base_size = 15)
  })
  
  output$recommendTable <- renderDT({
    recs_list <- recommendation_data()
    if (nrow(recs_list$recommendations) == 0) {
      datatable(data.table(Message = "No specific recommendations found for this customer based on their history."),
                options = list(dom = 't'), rownames = FALSE)
    } else {
      datatable(recs_list$recommendations,
                options = list(pageLength = 5, scrollX = TRUE),
                rownames = FALSE,
                caption = htmltools::tags$caption(
                  style = 'caption-side: top; text-align: left; color: #2980b9; font-weight: bold;',
                  paste("Top recommendations for Customer ID:", recs_list$customer)
                ))
    }
  })
  
  output$jsonPreview <- renderText({
    recs_list <- recommendation_data()
    jsonlite::toJSON(recs_list, pretty = TRUE, auto_unbox = TRUE)
  })
  
  output$rulesUI <- renderUI({
    tagList(
      if (input$show_rules_table) {
        div(
          h4("Top 20 Association Rules (by Lift)"),
          DTOutput("ruleTable")
        )
      },
      if (input$show_rules_plot) {
        div(
          hr(),
          h4("Graph of Top 20 Association Rules"),
          plotOutput("rulePlot", height = "500px")
        )
      }
    )
  })
  
  output$ruleTable <- renderDT({
    if (input$show_rules_table) {
      top_rules <- sort(rules, by = "lift", decreasing = TRUE)[1:20]
      datatable(as(top_rules, "data.frame")[, c("lhs", "rhs", "support", "confidence", "lift")],
                options = list(pageLength = 10, scrollX = TRUE),
                caption = "Showing rules with the highest lift, indicating a strong association.")
    }
  })
  
  output$rulePlot <- renderPlot({
    if (input$show_rules_plot) {
      top_rules <- sort(rules, by = "lift", decreasing = TRUE)[1:20]
      plot(top_rules, method = "graph", control = list(type = "items"))
    }
  })
}

# Run the application
shinyApp(ui, server)
