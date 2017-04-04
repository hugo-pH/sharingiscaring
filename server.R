library(shiny)
library(rdrop2)
library(lubridate)
library(tidyverse)
library(stringr)
library(wesanderson)
library(ggthemes)

theme_set(theme_bw(20))
source("functions.R")
#https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# Dropbox folders
drop.folder <- "la_lista_de_la_compra"
raw.responses.dir <- file.path(drop.folder, "responses_raw")
db.responses.dir <- file.path(drop.folder, "responses_db")
orders.file.name <- c("current_orders.csv")
purchase.file.name <- c("purchases.csv")
# source("global.R")
users <- read.csv("./users.csv") %>% unlist() %>% as.character()


shinyServer(function(input, output, session){
  action <- reactive({
    input$action
  })
  
  #########################
  ## render UI functions ##
  #########################
  
  
  output$chooseFormType <- renderUI({
    
    if(action() == "Add order"){
      tagList(
        selectInput("name", "Who",
                    c(" ",  users)),
        textInput("item", "What"),
        selectInput("priority", "Priority",
                    c("",  "urgent", "take it easy")),
        
        actionButton("submit_order", "Submit order", class = "btn-primary"),
        
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        )
      )
    }else if(action() == "Add purchase"){
      tagList(
        # id = "form_buy",
        h3 = "I bought something",
        selectInput("buy.name", "Who",
                    c(" ",  users)),
        checkboxInput("newitemcheck", "The item was not in the list", F ),
        uiOutput("new.item.check"),
        ## TODO input for bought objects already in the list
        conditionalPanel(
          condition = "input.newitemcheck == true",
          textInput("new.purchase.item", "What")
          
        ),
        textInput("price", "How much"),
        
        actionButton("submit_purchase", "Submit purchase", class = "btn-primary"),
        
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        )
      )
    }else if(action() == "Remove order"){
      tagList(
        uiOutput("showOrdersID"),
        actionButton("remove_order", "Remove order", class = "btn-primary"),
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        )
      )
    }else if(action() == "Remove purchase"){
      tagList(
        uiOutput("showPurchasesID"),
        actionButton("remove_purchase", "Remove order", class = "btn-primary"),
        shinyjs::hidden(
          span(id = "submit_msg", "Submitting..."),
          div(id = "error",
              div(br(), tags$b("Error: "), span(id = "error_msg"))
          )
        )
      )
    }
  })
  
  output$new.item.check <- renderUI({
    data <- load.orders.data(drop.folder = drop.folder, file.name = orders.file.name)
    items <-  data$item
    conditionalPanel(
      condition = "input.newitemcheck == false",
      selectInput("list.purchase.item", "What",
                  items)
    )
  })
  
  
  output$showPurchasesID <- renderUI({
    data <- load.purchase.data(drop.folder = drop.folder, 
                               db.responses.dir = db.responses.dir,
                               file.ending = purchase.file.name)
    items <-  data$id
    
    
    selectInput("remove.purchase.id", "Select ID of item to be removed",
                items)
  })
  
  output$showOrdersID <- renderUI({
    data <- load.orders.data(drop.folder = drop.folder, file.name = orders.file.name)
    items <-  data$id
    
    selectInput("remove.order.id", "Select ID of item to be removed",
                items)
  })
  
  # Show the responses already submitted
  output$OrdersData <- DT::renderDataTable(
    load.orders.data(drop.folder = drop.folder, file.name = orders.file.name) %>% 
      mutate(date = as.character(date)),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )
  
  
  output$PurchasesData <- DT::renderDataTable(
    load.purchase.data(drop.folder = drop.folder, 
                       db.responses.dir = db.responses.dir,
                       file.ending = purchase.file.name) %>% 
      mutate(date = as.character(date)) %>% 
      arrange(desc(date)),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )
  
  
  # Allow user to download responses
  output$downloadPurchBtn <- downloadHandler(
    filename = function() { 
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(load.purchase.data(drop.folder = drop.folder, 
                                   db.responses.dir = db.responses.dir,
                                   file.ending = purchase.file.name), file, row.names = FALSE)
    }
  )  
  
  # Allow user to download responses
  output$downloadOrdersBtn <- downloadHandler(
    filename = function() { 
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(load.orders.data(drop.folder = drop.folder, 
                                         file.name = orders.file.name), file, row.names = FALSE)
    }
  )  
  
  output$table.orders <- renderUI({

    # if(action() %in% c("Add purchase", "Remove purchase")){
      
    # }else if(action() %in% c("Add order", "Remove order")){
      div(
        h2("Current orders"),
        downloadButton("downloadOrdersBtn", "Download Orders"), br(), br(),
        DT::dataTableOutput("OrdersData") 
      )
    
  
    # }
  })


  
  output$table.purchases <- renderUI({
    div(
      id = "adminPanel",
      h2("Previous purchases"),
      downloadButton("downloadPurchBtn", "Download purchases"), br(), br(),
      DT::dataTableOutput("PurchasesData")
      
    )
  })
  
  
  output$plot.time.purchases <- renderPlot({
    d <- load.purchase.data(drop.folder = drop.folder, 
                            db.responses.dir = db.responses.dir,
                            file.ending = purchase.file.name)
    
    d %>% 
      arrange(date) %>% 
      mutate(
        time_d =  as.numeric(difftime(date, date[1], units= "day"))
      ) %>% 
      group_by(name) %>% 
      mutate(paid = cumsum(price)) %>% 
      ggplot(aes(x = time_d, y = paid, colour = name)) +
      theme_solarized(20) +
      scale_color_manual(name = "Who", values = wes_palette("Cavalcanti")) +
      theme(legend.position = "top") +
      geom_point(size = 5) +
      geom_line() +
      xlim(0, 31) +
      ylim(0, NA) +
      xlab("Time (days)") +
      ylab("Money-money €")
    
    
  })  
  
  output$plot.total.purchases <- renderPlot({
    d <- load.purchase.data(drop.folder = drop.folder, 
                      db.responses.dir = db.responses.dir,
                      file.ending = purchase.file.name)
    
    d %>% 
      group_by(name) %>% 
      summarise(
        total = sum(price) 
      ) %>%  
      ggplot(aes(x = name, y = total, colour = name)) +
      theme_solarized(20) +
      scale_color_manual(values = wes_palette("Cavalcanti")) +
      geom_point(size = 5) +
      theme(legend.position = "none") +
      ylim(0, NA) +
      xlab("Who") +
      ylab("Money-money €")
    
  })
  
  output$table.total.paid <-renderDataTable({
    d <- load.purchase.data(drop.folder = drop.folder, 
                            db.responses.dir = db.responses.dir,
                            file.ending = purchase.file.name)
    
    d %>% 
      group_by(name) %>% 
      summarise(
        total = sum(price) 
      )
    
  })
  
  output$adjust.payments <-renderTable({
    # browser()
    d <- load.purchase.data(drop.folder = drop.folder, 
                            db.responses.dir = db.responses.dir,
                            file.ending = purchase.file.name)
    
    total.per.person <- d %>% 
      group_by(name) %>% 
      summarise(
        total = sum(price) 
      )
    total.month <- sum(total.per.person$total)
    total.month.per.person <- total.month / length(unique(total.per.person$name))
    out.table <- total.per.person %>% 
      mutate(total_paid_month = total.month,
             total_to_paid_per_person = total.month.per.person,
             balance = total_to_paid_per_person - total)
    out.table
  })
  
  ####################
  ### Server stuff ###
  ####################
  
  
  
  ### Retrieve forms data ###
  
  ### Orders
  # Gather all the form inputs (and add timestamp)
  orderformData <- reactive({
    data <- data_frame(
      name = input$name,
      item = input$item,
      date = Sys.time(),
      priority = input$priority
    )
    data
  })
  
  ### Purchases
  # Gather all the form inputs (and add timestamp)
  purchaseformData <- reactive({
    data <- data_frame(
      name = input$buy.name,
      item = NA,
      date = Sys.time(),
      price = as.numeric(input$price),
      id = NA
    )
    
    if(input$newitemcheck){
      data$item <- input$new.purchase.item
    }else{
      
      data$item <- input$list.purchase.item
    }
    data
  })
  
  
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_order, {
    # browser()
    catchy("submit_order", f = update.current.orders, 
           args = list(data = orderformData(), 
                       drop.folder = drop.folder, 
                       file.name = orders.file.name))
  })
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_order, {
    # Update orders table
    output$OrdersData <- DT::renderDataTable(
      load.orders.data(drop.folder = drop.folder,                                           
                       file.name = orders.file.name) %>% 
        mutate(date = as.character(date)),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
      
      
    )
    
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_purchase, {
    # browser()
    catchy("submit_purchase", 
           f = update.purchase.answers, 
            args  = list(data = purchaseformData(),
                         drop.folder = drop.folder,
                         orders.file.name = orders.file.name,
                         purchase.file.name = purchase.file.name,
                         new.item = input$newitemcheck,
                         db.responses.dir = db.responses.dir
                         ))
    
  })
  
  observeEvent(input$submit_purchase, {
    # browser()
    # Update orders table
    output$OrdersData <- DT::renderDataTable(
      load.orders.data(drop.folder = drop.folder,                                           
                       file.name = orders.file.name) %>% 
        mutate(date = as.character(date)),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
      
      
    )
    # Update purchases table
    output$PurchasesData <- DT::renderDataTable(
      load.purchase.data(drop.folder = drop.folder, 
                         db.responses.dir = db.responses.dir,
                         file.ending = purchase.file.name) %>% 
        mutate(date = as.character(date)) %>% 
        arrange(desc(date)),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    )
    
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$remove_order, {
    catchy("remove_order", 
           f = remove.orders, 
           args = list(
             data = load.orders.data(drop.folder = drop.folder, 
                                     file.name = orders.file.name), 
             drop.folder = drop.folder,
             orders.file.name = orders.file.name,
             purchase.file.name = purchase.file.name,
             db.responses.dir = db.responses.dir,
             id = input$remove.order.id))
  })
  # When the Submit button is clicked, submit the response
  observeEvent(input$remove_order, {
    # Show the responses already submitted
    output$OrdersData <- DT::renderDataTable(
      load.orders.data(drop.folder = drop.folder, 
                       file.name = orders.file.name) %>% 
        mutate(date = as.character(date)),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    )
  })
  
  
  observeEvent(input$remove_purchase, {
    catchy("remove_purchase", 
           f = remove.purchases, 
           args = list(
             data = load.purchase.data(drop.folder = drop.folder, 
                                       db.responses.dir = db.responses.dir,
                                       file.ending = purchase.file.name), 
             drop.folder = drop.folder,
             orders.file.name = orders.file.name,
             purchase.file.name = purchase.file.name,
             db.responses.dir = db.responses.dir,
             id = input$remove.purchase.id))
  })
  observeEvent(input$remove_purchase, {
    # browser()
    output$PurchasesData <- DT::renderDataTable(
      load.purchase.data(drop.folder = drop.folder, 
                         db.responses.dir = db.responses.dir,
                         file.ending = purchase.file.name) %>% 
        mutate(date = as.character(date)) %>% 
        arrange(desc(date)),
      rownames = FALSE,
      options = list(searching = FALSE, lengthChange = FALSE)
    )
    
  })
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
}
)