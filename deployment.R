library(shiny)
library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)
library(lubridate)
library(shinythemes)
library(rsconnect)

data <- read.csv("https://raw.githubusercontent.com/Aaron00Yu94/Milestone2/refs/heads/main/Electronic_sales_Sep2023-Sep2024.csv")
data <- data %>%
  mutate(Purchase.Date = as.Date(Purchase.Date, format = "%Y-%m-%d")) %>%
  filter(year(Purchase.Date) == 2024) %>%
  mutate(
    Month = format(Purchase.Date, "%Y-%m"),
    Age_Group = cut(Age, breaks = seq(0, 100, by = 10), 
                    labels = paste(seq(10, 100, by = 10)-10, seq(10, 100, by = 10)-1, sep = "-")),
    Add_On_Purchase = ifelse(!is.na(Add.ons.Purchased) & Add.ons.Purchased != "", "Yes", "No")
  )

cleaned_data <- data %>%
  separate_rows(Add.ons.Purchased, sep = ",") %>%
  mutate(Add_On = trimws(Add.ons.Purchased)) %>%
  filter(!is.na(Add_On) & Add_On != "")

summary_data <- data %>%
  group_by(Month, Product.Type, Order.Status) %>%
  summarise(Sales = n(), .groups = "drop")

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  
  titlePanel("Combined Sales Analysis Dashboard"),
  p("This dashboard aims to help sales managers and marketing teams identify patterns and trends in electronic sales. 
    We focus on understanding which products and SKUs perform best in terms of revenue and quantity, 
    how different age groups and genders influence purchase behavior, 
    and how seasonal trends might affect order completions and cancellations."),
  p("Use the controls to select specific date ranges, product types, order statuses, and demographic groups. 
    The visualizations provide insights to support data-driven decisions on stocking, marketing strategies, and personalized offers."),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("visualization", "Select Visualization:", 
                  choices = c("Heatmap of Sales", 
                              "Sales Analysis by Product Type", 
                              "Customer Behavior Analysis",
                              "Graphical Query",
                              "Detailed Customer Behavior")),
      
      # Heatmap inputs
      conditionalPanel(
        condition = "input.visualization == 'Heatmap of Sales'",
        dateRangeInput("dateRange1", "Select Date Range:", 
                       start = min(data$Purchase.Date), 
                       end = max(data$Purchase.Date),
                       max = max(data$Purchase.Date)),
        selectInput("productType1", "Select Product Type:", choices = c("All", unique(data$Product.Type)), selected = "All"),
        selectInput("orderStatus1", "Select Order Status:", choices = c("All", unique(data$Order.Status)), selected = "All")
      ),
      
      # Sales Analysis by Product Type inputs
      conditionalPanel(
        condition = "input.visualization == 'Sales Analysis by Product Type'",
        selectInput("product_type2", "Select Product Type(s):", 
                    choices = unique(data$Product.Type), multiple = TRUE, 
                    selected = unique(data$Product.Type)[1]),
        textOutput("maxRevenueMonth")
      ),
      
      # Customer Behavior Analysis inputs
      conditionalPanel(
        condition = "input.visualization == 'Customer Behavior Analysis'",
        selectInput("age_group3", "Select Age Group:", choices = unique(data$Age_Group)),
        dateRangeInput("date_range3", "Select Date Range:", 
                       start = min(data$Purchase.Date, na.rm = TRUE), 
                       end = max(data$Purchase.Date, na.rm = TRUE),
                       max = max(data$Purchase.Date)),
        selectInput("order_status3", "Select Order Status:", choices = c("Completed", "Cancelled"), selected = "Completed"),
        selectInput("relationship3", "Select Relationship to View:", 
                    choices = c("Product Type vs Add-On Purchase", "Product Type vs Shipping Type")),
        helpText("‘Add-On Purchase’ indicates whether the customer bought additional accessories or services."),
        helpText("‘Rating’ represents customer satisfaction score. Exact scale depends on data collection method.")
      ),
      
      # Graphical Query inputs
      conditionalPanel(
        condition = "input.visualization == 'Graphical Query'",
        checkboxGroupInput("age_group4", "Select Age Group:", choices = unique(data$Age_Group), selected = unique(data$Age_Group)[1]),
        dateRangeInput("date_range4", "Select Date Range:", start = min(data$Purchase.Date, na.rm = TRUE), 
                       end = max(data$Purchase.Date, na.rm = TRUE), max = max(data$Purchase.Date)),
        checkboxGroupInput("order_status4", "Select Order Status:", choices = c("Completed", "Cancelled"), selected = "Completed"),
        checkboxGroupInput("rating4", "Select Rating:", choices = unique(data$Rating), selected = unique(data$Rating)[1]),
        selectInput("relationship4", "Select Relationship to View:", 
                    choices = c("Product Type vs Add-On Purchase", "Product Type vs Shipping Type")),
        helpText("Use these filters to explore relationships between product types, add-ons, shipping types, and customer segments.")
      ),
      
      conditionalPanel(
        condition = "input.visualization == 'Detailed Customer Behavior'",
        checkboxGroupInput("age_group5", "Select Age Group:", 
                           choices = unique(data$Age_Group), 
                           selected = unique(data$Age_Group)[1]),
        dateRangeInput("date_range5", "Select Date Range:",
                       start = min(data$Purchase.Date, na.rm = TRUE),
                       end = max(data$Purchase.Date, na.rm = TRUE),
                       max = max(data$Purchase.Date)),
        checkboxGroupInput("order_status5", "Select Order Status:",
                           choices = c("Completed", "Cancelled"),
                           selected = "Completed"),
        checkboxGroupInput("rating5", "Select Rating:", 
                           choices = unique(data$Rating), 
                           selected = unique(data$Rating)[1]),
        selectInput("relationship5", "Select Relationship to View:",
                    choices = c("Product Type vs Add-On Purchase", 
                                "Product Type vs Shipping Type", 
                                "Total Price by SKU", 
                                "Quantity by SKU"),
                    selected = "Product Type vs Add-On Purchase")
      )
    ),
    
    mainPanel(
      conditionalPanel(
        condition = "input.visualization == 'Heatmap of Sales'",
        plotlyOutput("heatmap")
      ),
      conditionalPanel(
        condition = "input.visualization == 'Sales Analysis by Product Type'",
        tabsetPanel(
          tabPanel("Sales Quantity", plotlyOutput("quantityPlot")),
          tabPanel("Sales Revenue", plotlyOutput("revenuePlot"))
        )
      ),
      conditionalPanel(
        condition = "input.visualization == 'Customer Behavior Analysis'",
        plotlyOutput("relationship_plot3"),
        h4("Filtered Data"),
        tableOutput("filtered_data3")
      ),
      conditionalPanel(
        condition = "input.visualization == 'Graphical Query'",
        plotlyOutput("relationship_plot4"),
        h4("Filtered Data"),
        tableOutput("filtered_data4")
      ),
      conditionalPanel(
        condition = "input.visualization == 'Detailed Customer Behavior'",
        plotlyOutput("relationship_plot5"),
        h4("Filtered Data"),
        tableOutput("filtered_data5")
      )
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  # Heatmap reactive filtering
  filtered_summary <- reactive({
    req(input$dateRange1, input$productType1, input$orderStatus1)
    fd <- summary_data %>%
      filter(Month >= format(input$dateRange1[1], "%Y-%m"),
             Month <= format(input$dateRange1[2], "%Y-%m"))
    if (input$productType1 != "All") {
      fd <- fd %>% filter(Product.Type == input$productType1)
    }
    if (input$orderStatus1 != "All") {
      fd <- fd %>% filter(Order.Status == input$orderStatus1)
    }
    fd
  })
  
  output$heatmap <- renderPlotly({
    fd <- filtered_summary()
    validate(
      need(nrow(fd) > 0, "No data available for the selected filters.")
    )
    color_scale <- switch(input$orderStatus1,
                          "Completed" = scale_fill_gradient(low = "blue", high = "red"),
                          "Cancelled" = scale_fill_gradient(low = "green", high = "yellow"),
                          scale_fill_gradient(low = "skyblue", high = "blue"))
    
    p <- ggplot(fd, aes(x = Month, y = Product.Type, fill = Sales, text = paste("Sales:", Sales))) +
      geom_tile(color = "white") +
      color_scale +
      labs(title = "Heatmap of Sales", x = "Month", y = "Product Type", fill = "Sales Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    ggplotly(p, tooltip = c("x", "y", "fill", "text"))
  })
  
  # Sales Analysis by Product Type
  filtered_data2 <- reactive({
    req(input$product_type2)
    cleaned_data %>%
      filter(Product.Type %in% input$product_type2) %>%
      group_by(Product.Type, Year = year(Purchase.Date), 
               Month = factor(format(Purchase.Date, "%b"), levels = month.abb)) %>%
      summarise(Total_Quantity = sum(Quantity, na.rm = TRUE), 
                Total_Revenue = sum(Total.Price, na.rm = TRUE), 
                .groups = "drop") %>%
      arrange(Year, match(Month, month.abb))
  })
  
  output$maxRevenueMonth <- renderText({
    req(input$visualization == "Sales Analysis by Product Type")
    data2 <- filtered_data2()
    validate(need(nrow(data2) > 0, "No data available."))
    max_row <- data2 %>% filter(Total_Revenue == max(Total_Revenue, na.rm = TRUE)) %>% slice(1)
    paste("The month with the highest revenue among selected product type(s) is", 
          max_row$Month, max_row$Year, 
          "with a total revenue of $", round(max_row$Total_Revenue, 2))
  })
  
  output$quantityPlot <- renderPlotly({
    req(input$visualization == "Sales Analysis by Product Type")
    data2 <- filtered_data2()
    validate(need(nrow(data2) > 0, "No data available."))
    
    p <- ggplot(data2, aes(x = Month, y = Total_Quantity, color = Product.Type, 
                           linetype = as.factor(Year), group = interaction(Product.Type, Year))) +
      geom_line(size = 1) + geom_point(size = 2) +
      labs(title = "Monthly Sales Quantity by Product Type", x = "Month", y = "Total Quantity", 
           color = "Product Type", linetype = "Year") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "color", "linetype"))
  })
  
  output$revenuePlot <- renderPlotly({
    req(input$visualization == "Sales Analysis by Product Type")
    data2 <- filtered_data2()
    validate(need(nrow(data2) > 0, "No data available."))
    
    p <- ggplot(data2, aes(x = Month, y = Total_Revenue, color = Product.Type, 
                           linetype = as.factor(Year), group = interaction(Product.Type, Year))) +
      geom_line(size = 1) + geom_point(size = 2) +
      labs(title = "Monthly Sales Revenue by Product Type", x = "Month", y = "Total Revenue", 
           color = "Product Type", linetype = "Year") +
      theme_minimal()
    ggplotly(p, tooltip = c("x", "y", "color", "linetype"))
  })
  
  # Customer Behavior Analysis
  filtered_data3 <- reactive({
    req(input$visualization == "Customer Behavior Analysis")
    cleaned_data %>%
      filter(Age_Group == input$age_group3,
             Purchase.Date >= input$date_range3[1] & Purchase.Date <= input$date_range3[2],
             Order.Status == input$order_status3)
  })
  
  output$relationship_plot3 <- renderPlotly({
    fd3 <- filtered_data3()
    validate(need(nrow(fd3) > 0, "No data available for the selected filters."))
    
    p <- if (input$relationship3 == "Product Type vs Add-On Purchase") {
      ggplot(fd3, aes(x = Product.Type, fill = Add_On)) +
        geom_bar(position = "dodge") +
        labs(title = "Add-On Purchases by Product Type", x = "Product Type", y = "Count") +
        theme_minimal()
    } else {
      ggplot(fd3, aes(x = Product.Type, fill = Shipping.Type)) +
        geom_bar(position = "dodge") +
        labs(title = "Shipping Type by Product Type", x = "Product Type", y = "Count") +
        theme_minimal()
    }
    ggplotly(p, tooltip = c("x", "fill", "count"))
  })
  
  output$filtered_data3 <- renderTable({
    fd3 <- filtered_data3()
    validate(need(nrow(fd3) > 0, "No data available."))
    fd3 %>%
      select(Customer.ID, Age, Gender, Loyalty.Member, Product.Type, SKU, Order.Status, Total.Price, Add.on.Total)
  })
  
  # Graphical Query
  reactiveData4 <- reactive({
    req(input$visualization == "Graphical Query")
    cleaned_data %>%
      filter(
        Age_Group %in% input$age_group4,
        Purchase.Date >= input$date_range4[1] & Purchase.Date <= input$date_range4[2],
        Order.Status %in% input$order_status4,
        Rating %in% input$rating4
      )
  })
  
  output$relationship_plot4 <- renderPlotly({
    data4 <- reactiveData4()
    validate(need(nrow(data4) > 0, "No data available."))
    
    p <- if (input$relationship4 == "Product Type vs Shipping Type") {
      ggplot(data4, aes(x = Product.Type, fill = Shipping.Type)) +
        geom_bar(position = "dodge") +
        labs(title = "Shipping Type by Product Type", x = "Product Type", y = "Count") +
        theme_minimal()
    } else {
      ggplot(data4, aes(x = Product.Type, fill = Add_On)) +
        geom_bar(position = "dodge") +
        labs(title = "Add-On Purchases by Product Type", x = "Product Type", y = "Count") +
        theme_minimal()
    }
    ggplotly(p, tooltip = c("x", "fill", "count"))
  })
  
  output$filtered_data4 <- renderTable({
    data4 <- reactiveData4()
    validate(need(nrow(data4) > 0, "No data available."))
    data4 %>%
      select(Customer.ID, Age, Gender, Loyalty.Member, Product.Type, SKU, Order.Status, Total.Price, Add.on.Total)
  })
  
  
  reactiveData5 <- reactive({
    req(input$visualization == "Detailed Customer Behavior")
    cleaned_data %>%
      filter(
        Age_Group %in% input$age_group5,
        Purchase.Date >= input$date_range5[1] & Purchase.Date <= input$date_range5[2],
        Order.Status %in% input$order_status5,
        Rating %in% input$rating5
      )
  })
  
  output$relationship_plot5 <- renderPlotly({
    data5 <- reactiveData5()
    validate(need(nrow(data5) > 0, "No data available."))
    
    p <- if (input$relationship5 == "Product Type vs Add-On Purchase") {
      ggplot(data5, aes(x = Product.Type, fill = Add_On)) +
        geom_bar(stat = "count", position = "dodge") +
        labs(title = "Add-On Purchases by Product Type", x = "Product Type", y = "Count")
    } else if (input$relationship5 == "Product Type vs Shipping Type") {
      ggplot(data5, aes(x = Product.Type, fill = Shipping.Type)) +
        geom_bar(stat = "count", position = "dodge") +
        labs(title = "Shipping Type by Product Type", x = "Product Type", y = "Count")
    } else if (input$relationship5 == "Total Price by SKU") {
      grouped_data <- data5 %>%
        group_by(SKU, Gender) %>%
        summarise(Total_Price = sum(Total.Price, na.rm = TRUE), .groups = 'drop')
      ggplot(grouped_data, aes(x = SKU, y = Total_Price, fill = Gender)) +
        geom_col(position = position_dodge()) +
        labs(title = "Total Price by SKU and Gender", x = "SKU", y = "Total Price")
    } else {
      grouped_data <- data5 %>%
        group_by(SKU, Gender) %>%
        summarise(Total_Quantity = sum(Quantity, na.rm = TRUE), .groups = 'drop')
      ggplot(grouped_data, aes(x = SKU, y = Total_Quantity, fill = Gender)) +
        geom_col(position = position_dodge()) +
        labs(title = "Quantity by SKU and Gender", x = "SKU", y = "Quantity")
    }
    
    ggplotly(p, tooltip = c("x", "y", "fill"))
  })
  
  output$filtered_data5 <- renderTable({
    data5 <- reactiveData5()
    validate(need(nrow(data5) > 0, "No data available."))
    data5 %>%
      select(Customer.ID, Age, Gender, Loyalty.Member, Product.Type, SKU, Order.Status, Total.Price, Add.on.Total)
  })
}

shinyApp(ui = ui, server = server)