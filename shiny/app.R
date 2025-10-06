# app.R
# Interactive Shiny Dashboard for Next Best Offer (NBO) Engine

library(shiny)
library(data.table)
library(DT)
library(arulesViz)
library(ggplot2)
library(jsonlite)

# Load precomputed data
retail <- fread("../data/processed/retail_clean.csv")
rfm <- fread("../data/processed/rfm_segmented.csv")
rules <- readRDS("../data/processed/association_rules.rds")
source("../src/05_nbo_engine.R", chdir = TRUE)

setwd('C:/Users/BMC/Desktop/NBORecommendationEngine/')


# Define UI
ui <- fluidPage(
  titlePanel("Next Best Offer (NBO) Engine Dashboard"),
  
  sidebarLayout(
    sidebarPanel(
      h4("🔍 Customer Selection"),
      selectInput("customer_id", "Select Customer ID:",
                  choices = sort(unique(rfm$CustomerID)),
                  selected = sample(rfm$CustomerID, 1)),
      numericInput("top_k", "Top K Recommendations:", value = 5, min = 1, max = 10),
      actionButton("recommend", "Generate Recommendations", icon = icon("play")),
      hr(),
      h4("📊 Filters"),
      checkboxInput("show_rules", "Show Association Rules", TRUE),
      checkboxInput("show_segments", "Show Customer Segments", TRUE)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard Overview",
                 h4("Customer Segment Overview"),
                 plotOutput("segmentPlot", height = "350px"),
                 h4("Top Products by Popularity"),
                 plotOutput("topItemsPlot", height = "350px")
        ),
        tabPanel("Recommendations",
                 h4("Recommended Offers"),
                 DTOutput("recommendTable"),
                 verbatimTextOutput("jsonPreview")
        ),
        tabPanel("Association Rules",
                 h4("Top Association Rules (by Lift)"),
                 DTOutput("ruleTable"),
                 plotOutput("rulePlot", height = "500px")
        )
      )
    )
  )
)

# Define Server logic
server <- function(input, output, session) {
  
  # --- Segment distribution plot
  output$segmentPlot <- renderPlot({
    seg_data <- rfm[, .N, by = Segment]
    ggplot(seg_data, aes(x = factor(Segment), y = N, fill = factor(Segment))) +
      geom_bar(stat = "identity") +
      labs(title = "Customer Segment Distribution", x = "Segment", y = "Count") +
      theme_minimal()
  })
  
  # --- Top items by popularity
  output$topItemsPlot <- renderPlot({
    top_items <- retail[, .N, by = Description][order(-N)][1:15]
    ggplot(top_items, aes(x = reorder(Description, N), y = N)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Top 15 Most Purchased Products", x = "Product", y = "Purchase Count") +
      theme_minimal()
  })
  
  # --- Generate recommendations
  observeEvent(input$recommend, {
    customer_id <- input$customer_id
    k <- input$top_k
    
    recs <- compute_nbo_score(customer_id, top_k = k)
    
    if (nrow(recs) == 0) {
      output$recommendTable <- renderDT({
        datatable(data.table(Message = "No recommendations found"))
      })
      output$jsonPreview <- renderText("No recommendations available.")
      return()
    }
    
    output$recommendTable <- renderDT({
      datatable(recs, options = list(pageLength = 5), rownames = FALSE)
    })
    
    output$jsonPreview <- renderText({
      jsonlite::toJSON(list(
        customer = customer_id,
        recommendations = recs
      ), pretty = TRUE, auto_unbox = TRUE)
    })
  })
  
  # --- Association Rules Table
  output$ruleTable <- renderDT({
    if (input$show_rules) {
      top_rules <- sort(rules, by = "lift", decreasing = TRUE)[1:20]
      datatable(as(top_rules, "data.frame")[, c("lhs", "rhs", "support", "confidence", "lift")],
                options = list(pageLength = 10))
    }
  })
  
  # --- Association Rules Graph
  output$rulePlot <- renderPlot({
    if (input$show_rules) {
      top_rules <- sort(rules, by = "lift", decreasing = TRUE)[1:20]
      plot(top_rules, method = "graph", control = list(type = "items"))
    }
  })
}

# Run app
shinyApp(ui, server)
