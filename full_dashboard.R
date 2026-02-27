library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(tidyverse)
library(dplyr)
library(plotly)
library(shinyTime)

pizza<-read.csv("D:/3rd Year/Semestor 02/DSC3163- Advanced Database Management System/Dashboard_Assign/Data/pizza_data.csv")
pizza$Price<-gsub("\\$","",pizza$Price)

pizza$Size <- gsub("\\s*\\(.*\\)", "", pizza$Size)
pizza$Price<-as.numeric(pizza$Price)
pizza_price<-pizza %>%
  group_by(Type, Size) %>%
  summarise(
    Average_Price= mean(Price, na.rm = TRUE)
  )

pizza_size<- pizza %>%
  group_by(Size) %>%
  summarise(
    Count= n()
  )

pizza_com_type<- pizza %>%
  group_by(Company,Type) %>%
  summarize(Count= n())

pizza_sales<- read.csv("D:/3rd Year/Semestor 02/DSC3163- Advanced Database Management System/Dashboard_Assign/Data/pizza_sales.csv")
pizza_sales$order_date<-gsub("-", "/", pizza_sales$order_date)
pizza_sales$order_date<-as.Date(pizza_sales$order_date, tryFormats = c("%d/%m/%Y", "%d-%m-%Y"))
pizza_sales$order_date<- as.Date(format(pizza_sales$order_date, "2023-%m-%d"))

pizza_sales$month <- format(pizza_sales$order_date, "%b") # Extract month as abbreviation
pizza_sales$month <- factor(pizza_sales$month, levels = month.abb)

pizza_intent<- read.csv("D:/3rd Year/Semestor 02/DSC3163- Advanced Database Management System/Dashboard_Assign/Data/pizza_intent_dataset.csv")
pizza_intent<-pizza_intent[-3]

pizza_intent_item<-pizza_intent %>%
  group_by(label) %>%
  summarise(count = n())

pizza_intent_item<- pizza_intent_item[order(pizza_intent_item$count,decreasing = TRUE),]

all_ingredients <- paste(pizza_sales$pizza_ingredients, collapse = ", ")
ingredient_list <- unlist(strsplit(all_ingredients, ", "))
ingredient_count <- table(ingredient_list)
sorted_ingredient_count <- sort(ingredient_count, decreasing = TRUE)
sorted_ingredient_count<- data.frame(sorted_ingredient_count)
sorted_ingredient_count<-sorted_ingredient_count[1:20,]

# Define UI for the application
ui <- dashboardPage(
  dashboardHeader(title = "MOON Pizza"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Order", tabName = "order", icon = icon("calendar")),
      menuItem("Menu", tabName = "menu", icon = icon("utensils")),
      menuItem("Performance Monitoring",tabName = "performance",icon = icon("tachometer-alt")),
      menuItem("Customer Review",tabName = "customer_review",icon = icon("list"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "order",
        fluidRow(
          column(
            width = 5,
            box(
              title = "Order",
              status = "primary",
              width = 12,
              p(strong("Order Details")),
              selectInput("Type", label = "Type", choices = list("Dine-in", "Pick-up", "Delivery")),
              dateInput("date", label = "Date", format = "dd-mm-yyyy", startview = "month"),
              textInput("name", label = "Name", placeholder = "Your Name"),
              numericInput("order_no", label = "Order No", value = 1, min = 1),
              selectInput("pizza_type", label = "Choose Pizza Type:", choices = c(
                "", "Cheeses Pizza", "Specialty Pizzas", "Feast Pizzas", "Cheese Pizza", 
                "Classic Recipe Pizzas", "New Recipe Pizzas", "Skinny Pizzas", 
                "Gluten-Free Pizzas", "Express Favorites"
              )),
              uiOutput("size_input"),
              uiOutput("quantity_input"),
              selectInput("paid", label = "Paid", choices = list("Yes", "No")),
              actionButton("add_order_btn", label = "Add Order", icon = icon("plus"))
            )
          ),
          column(
            width = 7,
            fluidRow(
              box(
                title = "Dine-in Orders",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                DTOutput("dine_in")
              ),
              box(
                title = "Pickup Orders",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                DTOutput("pickup")
              ),
              box(
                title = "Delivery Orders",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                DTOutput("delivery")
              )
            )
          )
        )
      ),
      tabItem(
        tabName = "menu",
        fluidPage(
          fluidRow(
            column(
              width = 5,  
              box(
                title = "Average Price",
                status = "primary",
                solidHeader = TRUE,
                width = 12,
                fluidPage(
                  fluidRow(
                    column(5, offset = 0, style = 'padding:3px;', selectInput("type", label = "Pizza Type", choices = unique(pizza$Type))),
                    column(4, offset = 1, style = 'padding:1px;', selectInput("size", label = "Pizza Size", choices = unique(pizza$Size)))
                  ),
                  tableOutput("averageprice")
                )
              ),
              box(
                title = "Max Sales Size",
                status = "primary",
                solidHeader = TRUE,
                width = 12,  # Full width in the column
                plotOutput("size")
              )
            ),
            # Right Column: Competitor Box
            column(
              width = 7,  # One-third of the row width
              box(
                title = "Competitor",
                status = "info",
                solidHeader = TRUE,
                width = 12,  # Full width in the column
                height = "700px",  # Adjust height as needed
                box(title = "Dominos",status = "primary",plotlyOutput("dominos",height = "250px")),
                box(title = "Pizza Hut",status = "primary",plotlyOutput("pizza_hut",height = "250px")),
                box(title = "Godfather",plotlyOutput("godfather",height = "250px")),
                box(title = "IMO",plotlyOutput("imo",height = "250px"))
              )
            )
          )
        )
      ),
      tabItem(tabName = "performance",
        h2("Performance Analysis"),
        h3(dateRangeInput("date_range",label = "Select a Date Range",format = "dd-mm-yyyy",startview = "month",separator = "to",start = "2023-01-01",end = "2023-12-31",min = "2023-01-01",max = "2023-12-31")),
        h3("Number of Days in the Selected Date Range:",textOutput("num_days")),
          fluidRow(
            valueBoxOutput("total_revenue",width = 3),
            valueBoxOutput("total_pizza_sales",width = 3),
            valueBoxOutput("total_orders",width = 3),
            valueBoxOutput("average_order",width = 3)
          ),
          fluidRow(
            box(title = "Hourly trend for total Pizzas sold", status = "primary", solidHeader = TRUE,
              plotOutput("hourly_trend", height = 250), width = 6),
            box(title = "Weekly trend for total orders", status = "primary", solidHeader = TRUE,
                    plotOutput("weekly_trend", height = 250), width = 6)
          ),
          fluidRow(
            box(
              plotOutput("monthly_sales_plot",height = "400px"),
              width = 6
            ),
            box(title = "Monthly sales pizza by size",
              selectInput("month",label = "Select a month",choices =list("Jan","Feb","Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")),
              plotOutput("month_size",height = "300px"),
              width = 6
            )
          ),
        fluidRow(
          box(title = "Most Used Toppings",
            plotOutput("toppings"),
            width = 12
          )
        )
      ),
      tabItem(
        tabName = "customer_review",
        fluidRow(
          column(
            width = 5,
            box(title = "Customer Question/Review About",
                status = "primary",
                width = 12,
                plotOutput("about")
            )
          ),
          column(
            width = 7,
            box(
              title = "Customer Questions & Complains",
              width = 12,
              selectInput("review",label = "Select a label",choices = unique(pizza_intent$label)),
              DTOutput("review")
            )
          )
        )
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$size_input <- renderUI({
    if (input$pizza_type != "") {
      selectInput("pizza_size", "Choose Size:", choices = c("Small", "Medium", "Large", "X-Large", "Personal", "Mini", "Jumbo", "X Large"))
    }
  })
  
  output$quantity_input <- renderUI({
    if (!is.null(input$pizza_size)) {
      numericInput("pizza_quantity", "Enter Quantity:", value = 1, min = 1)
    }
  })
  
  # Reactive values to store data for each order type
  orders <- reactiveValues(
    dinein = data.frame(
      Order_No = integer(),
      Type = character(),
      Name = character(),
      Pizza_Type = character(),
      Pizza_Size = character(),
      Quantity = integer(),
      Paid = character(),
      stringsAsFactors = FALSE
    ),
    pickup = data.frame(
      Order_No = integer(),
      Type = character(),
      Name = character(),
      Pizza_Type = character(),
      Pizza_Size = character(),
      Quantity = integer(),
      Paid = character(),
      stringsAsFactors = FALSE
    ),
    delivery = data.frame(
      Order_No = integer(),
      Type = character(),
      Name = character(),
      Pizza_Type = character(),
      Pizza_Size = character(),
      Quantity = integer(),
      Paid = character(),
      stringsAsFactors = FALSE
    )
  )
  
  observeEvent(input$add_order_btn, {
    new_order <- data.frame(
      Order_No = input$order_no,
      Type = input$Type,
      Name = input$name,
      Pizza_Type = input$pizza_type,
      Pizza_Size = input$pizza_size,
      Quantity = input$pizza_quantity,
      Paid = input$paid,
      stringsAsFactors = FALSE
    )
    
    if (input$Type == "Dine-in") {
      orders$dinein <- rbind(orders$dinein, new_order)
    } else if (input$Type == "Pick-up") {
      orders$pickup <- rbind(orders$pickup, new_order)
    } else if (input$Type == "Delivery") {
      orders$delivery <- rbind(orders$delivery, new_order)
    }
  })
  
  # Render Data Tables
  output$dine_in <- renderDT({
    datatable(orders$dinein,options = list(pageLength=5))
  })
  
  output$pickup <- renderDT({
    datatable(orders$pickup,options = list(pageLength=5))
  })
  
  output$delivery <- renderDT({
    datatable(orders$delivery,options = list(pageLength=5))
  })
  
  output$averageprice<-renderTable({
    filtered_data <- pizza_price%>%
      filter(Type == input$type, Size == input$size) %>%
      summarise(Average_Price= mean(Average_Price, na.rm = TRUE))
    
    data.frame("Pizza_Type"=input$type,"Pizza_size"=input$size, "Price"=paste0("$",mean(filtered_data$Average_Price)))
  })
  
  output$size<-renderPlot({
    pct<-round(pizza_size$Count/sum(pizza_size$Count)*100)
    pie(pizza_size$Count,labels = paste(paste(pizza_size$Size, pct),"%",sep = ""), col = rainbow(length(pizza_size$Count)),main = "Pie chart by size")
  })
  
  output$dominos<- renderPlotly({
    dominos_data<- pizza_com_type %>%
      filter(Company=="Domino's Pizza")
    dominos_data<- dominos_data[,-1]
    dominos_data$Percentage <- round((dominos_data$Count / sum(dominos_data$Count)) * 100, 2)
    plot_ly(
      dominos_data,
      labels = ~Type,
      values = ~Count,
      #textinfo = "label+percent",
      hoverinfo = "label+Count+percent",
      marker = list(colors =c("#E8C547", "#F3A712", "#DA3E52")),
      textposition = "outside"
    ) %>%
      add_pie(hole = 0.5) %>%
      layout(
        showlegend = TRUE
      )
  })
  
  
  output$pizza_hut<- renderPlotly({
    pizza_hut_data<- pizza_com_type %>%
      filter(Company=="Pizza Hut")
    pizza_hut_data<- pizza_hut_data[,-1]
    pizza_hut_data$Percentage <- round((pizza_hut_data$Count / sum(pizza_hut_data$Count)) * 100, 2)
    plot_ly(
      pizza_hut_data,
      labels = ~Type,
      values = ~Count,
      hoverinfo = "label+Count+percent",
      marker = list(colors =c("#5A5353", "#6A0572", "#3C1642")),
      textposition = "outside"
    ) %>%
      add_pie(hole = 0.5) %>%
      layout(
        showlegend = TRUE
      )
  })
  output$godfather<- renderPlotly({
    godfather_data <-pizza_com_type %>% 
      filter(Company=="Godfather's Pizza")
    godfather_data<- godfather_data[,-1]
    godfather_data$Percentage <- round((godfather_data$Count / sum(godfather_data$Count)) * 100, 2)
    plot_ly(
      godfather_data,
      labels = ~Type,
      values = ~Count,
      hoverinfo = "label+Count+percent",
      marker = list(colors =c("#E8C547", "#F3A712", "#DA3E52")),
      textposition = "outside"
    ) %>%
      add_pie(hole = 0.5) %>%
      layout(
        showlegend = TRUE
      )
  })
  
  output$imo<- renderPlotly({
    imo_data<- pizza_com_type %>%
      filter(Company=="IMO's Pizza")
    imo_data<- imo_data[,-1]
    imo_data$Percentage <- round((imo_data$Count / sum(imo_data$Count)) * 100, 2)
    plot_ly(
      imo_data,
      labels = ~Type,
      values = ~Count,
      hoverinfo = "label+Count+percent",
      marker = list(colors =c( "#6A0572")),
      textposition = "outside"
    ) %>%
      add_pie(hole = 0.5) %>%
      layout(
        showlegend = TRUE
      )
  })
  
  
  filtered_data <- reactive({
    pizza_sales %>%
      filter(
        order_date >= input$date_range[1],
        order_date <= input$date_range[2]
      )
  })
  output$num_days <- renderText({
    # Extract start and end dates from the input
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Calculate the number of days
    num_days <- as.numeric(difftime(end_date, start_date, units = "days"))
    
    # Display the result
    paste(num_days)
  })
  
  # Value boxes
  output$total_revenue <- renderValueBox({
    valueBox(sum(filtered_data()$total_price), "Total Revenue", icon = icon("dollar-sign"), color = "green")
  })
  
  # Value box: Total Pizza Sales
  output$total_pizza_sales <- renderValueBox({
    valueBox(
      sum(filtered_data()$quantity, na.rm = TRUE),
      "Total Pizza Sales", icon = icon("utensils"), color = "orange"
    )
  })
  
  # Value box: Total Orders
  output$total_orders <- renderValueBox({
    valueBox(
      n_distinct(filtered_data()$order_id),
      "Total Orders", icon = icon("shopping-cart"), color = "red"
    )
  })
  
  # Value box: Progress (placeholder)
  output$average_order <- renderValueBox({
    start_date <- input$date_range[1]
    end_date <- input$date_range[2]
    
    # Calculate the number of days
    num_days <- as.numeric(difftime(end_date, start_date, units = "days"))
    
    valueBox(
      round(sum(filtered_data()$total_price)/num_days,2),
      "Average Order", icon = icon("tag"), color = "blue"
    )
  })
  
  # Hourly Trend Plot
  output$hourly_trend <- renderPlot({
    hourly_data <- filtered_data() %>%
      group_by(order_time = format(as.POSIXct(order_time, format = "%H:%M:%S"), "%H")) %>%
      summarise(total_pizzas = sum(quantity, na.rm = TRUE), .groups = "drop")
    
    ggplot(hourly_data, aes(x = as.numeric(order_time), y = total_pizzas)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(x = "Hour of the Day", y = "Number of Pizzas Sold") +
      theme_minimal()
  })
  
  # Weekly Trend Plot
  output$weekly_trend <- renderPlot({
    weekly_data <- filtered_data() %>%
      mutate(week = format(order_date, "%U")) %>%
      group_by(week) %>%
      summarise(total_orders = n_distinct(order_id), .groups = "drop")
    
    ggplot(weekly_data, aes(x = as.numeric(week), y = total_orders)) +
      geom_line(color = "blue") +
      geom_point(color = "red") +
      labs(x = "Week Number", y = "Total Orders") +
      theme_minimal()
  })
  
  output$monthly_sales_plot <- renderPlot({
    # Summarize data
    monthly_sales <- pizza_sales %>%
      group_by(month, pizza_category) %>%
      summarise(total_quantity = sum(quantity))
    
    # Create stacked bar plot
    ggplot(monthly_sales, aes(x = month, y = total_quantity, fill = pizza_category)) +
      geom_bar(stat = "identity",width = 0.7) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Month",
        y = "Sales Quantity",
        fill = "pizza_category",
        title = "Monthly Sales Quantity by Category"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  output$month_size<- renderPlot({
    month_data<- pizza_sales %>%
      filter(month==input$month) %>%
      group_by(pizza_size) %>%
      summarise(count=sum(quantity,na.rm = TRUE),.groups = "drop") 
    
    ggplot(month_data,aes(x=pizza_size,y=count,fill=pizza_size))+
      geom_bar(stat = "identity",width = 0.7)+
      scale_fill_brewer(palette = "Set2")+
      labs(x="Pizza Size",y="Quantity Sold", title =paste("Sales for ",input$month))+
      theme_minimal()
    
  })
  
  output$toppings<- renderPlot({
    ggplot(sorted_ingredient_count, aes(x = ingredient_list, y = Freq, size = Freq)) +
      geom_point(alpha = 1.5) +
      labs(title = "Frequency of Ingredients",
           x = "Ingredient",
           y = "Frequency",
           size = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      coord_flip()
  })
  
  output$about<-renderPlot({
    ggplot(pizza_intent_item, aes(x = label, y = count,fill=label)) +
      geom_bar(stat = "identity",width = 0.7) +
      scale_fill_brewer(palette = "Set2") +
      labs(
        x = "Label",
        y = "Count",
        title = "Customer Question & Complain"
      ) +
      theme_minimal(base_size = 14) +
      theme(legend.position = "bottom")
  })
  
  output$review<- renderDT({
    review_data<- pizza_intent %>%
      filter(pizza_intent$label==input$review)
    
    datatable(review_data)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

 
