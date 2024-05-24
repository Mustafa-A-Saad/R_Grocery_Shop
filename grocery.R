#---------------------------------------------------------Data Exploration----------------------------------------------------------------------------#
df <- read.csv("grc.csv",TRUE,",")
head(df)

# how many duplicated rows are in your data
sum(duplicated(df))

# To remove duplicate rows in a data frame.
df_without= unique(df)
head(df_without)
sum(duplicated(df_without))

sum(is.na(df))

outlier <- boxplot(df[, 3])$out 
outlier 

#----------------------------------------------------------------------GUI-----------------------------------------------------------------------------#
library(shiny)
library(dplyr)
library(arules)

ui <- fluidPage(
  titlePanel("Grocery Data Analysis"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose CSV File"),
      numericInput("clusters", "Number of Clusters:", value = 2, min = 2, max = 4),
      numericInput("support", "Minimum Support", value = 0.01, min = 0.001, max = 1, step = 0.001),
      numericInput("confidence", "Minimum Confidence", value = 0.4, min = 0.001, max = 1, step = 0.001),
      actionButton("submit_button", "Submit")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Dashboard",
                 plotOutput("dashboard_plot")
        ),
        tabPanel("Clustering Results",
                 tableOutput("cluster_table")
        ),
        tabPanel("Association Rules",
                 verbatimTextOutput("rulesOutput")
        )
      )
    )
  )
)


server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    read.csv(input$file$datapath)
  })
  
  # Generate dashboard plots
  output$dashboard_plot <- renderPlot({
    req(input$submit_button, data())
    data <- data()
    par(mfrow=c(2, 2))
    
    # Cash vs Credit pie chart
    payment_type_table <- table(data$paymentType)
    percentage <- round(100 * payment_type_table / sum(payment_type_table))
    pie(payment_type_table, labels = paste0(percentage, "%"), 
        main = "Comparison of Cash and Credit Totals", col = c("green", "blue"))
    legend("bottomright", legend = c("cash", "credit"), fill = c("green", "blue"))
    
    # Age vs Total Spending bar plot
    age_vs_spending <- aggregate(total ~ age, data = data, FUN = sum)
    barplot(age_vs_spending$total, names.arg = age_vs_spending$age, 
            main = "Total Spending by Age", xlab = "Age", ylab = "Total Spending", col = "yellow")
    
    # City Total Spending bar plot
    city_total_spending <- aggregate(total ~ city, data = data, FUN = sum)
    city_total_spending_sorted <- city_total_spending[order(-city_total_spending$total), ]
    barplot(city_total_spending_sorted$total, names.arg = city_total_spending_sorted$city, 
            main = "Total Spending by City", ylab = "Total Spending", las = 3, col = "orange")
    
    # Total Spending Distribution histogram
    hist(data$total, main = "Distribution of Total Spending", xlab = "Total Spending", col = "skyblue")
  }, width = 870, height = 560)
  
  
  # K-Means Clustering Algorthim
  output$cluster_table <- renderTable({
    req(input$submit_button, data())
    data <- data() %>%
      group_by(customer, age) %>%
      summarise(total_spent = sum(total))
    
    agg_data <- data %>%
      group_by(customer, age) %>%
      summarise(total_spent = sum(total_spent))
    
    Kmean_clustering <- kmeans(agg_data$total_spent, centers = input$clusters)
    result <- data.frame(
      customer = agg_data$customer,
      age = agg_data$age, 
      total_spent = agg_data$total_spent,
      Kmean_clustering = Kmean_clustering$cluster
    )
    result
  })
  
  
  # Apriori Association Rule 
  output$rulesOutput <- renderPrint({
    req(input$submit_button, data())
    items <- as(strsplit(data()$items, ","), "transactions")
    apriori <- apriori(items, parameter = list(supp = input$support, conf = input$confidence), minlen = 2)
    inspect(apriori)
  })
  
}


shinyApp(ui = ui, server=server)