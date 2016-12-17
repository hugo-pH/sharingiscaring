library(shiny)
library(rdrop2)
library(lubridate)
library(tidyverse)
library(stringr)
#https://github.com/daattali/shiny-server/blob/master/mimic-google-form/app.R
# Dropbox folders
drop.folder <- "la_lista_de_la_compra"
raw.responses.dir <- file.path(drop.folder, "responses_raw")
db.responses.dir <- file.path(drop.folder, "responses_db")
# source("global.R")

# Function to catch errors in a given submit action
catchy <- function(submit_type, f, args){
  
  # User-experience stuff
  shinyjs::disable(submit_type)
  shinyjs::show("submit_msg")
  shinyjs::hide("error")
  
  # Save the data (show an error message in case of error)
  tryCatch({
    f(args)
    # update.current.orders(orderformData())
    shinyjs::reset("form")
    shinyjs::hide("form")
    shinyjs::show("thankyou_msg")
  },
  error = function(err) {
    shinyjs::html("error_msg", err$message)
    shinyjs::show(id = "error", anim = TRUE, animType = "fade")
  },
  finally = {
    shinyjs::enable(submit_type)
    shinyjs::hide("submit_msg")
  })
}
  

# get a formatted string of the timestamp (exclude colons as they are invalid
# characters in Windows filenames)
humanTime <- function() {
  format(Sys.time(), "%Y%m%d-%H%M%OS")
}


load.current.orders.data  <- function() {
  data <-  drop_read_csv(file.path(drop.folder, "current_orders.csv"), 
                         colClasses = c("numeric", 
                                        "character", 
                                        "character", 
                                        "POSIXct", 
                                        "character")) %>% 
    mutate(id = seq_along(date)) %>% 
    select(id, name, item, date, priority)
  data
}

update.current.orders <- function(data) {
  data.updated <- bind_rows(
    load.current.orders.data(),
    data
  )
    filePath <- file.path(tempdir(), "current_orders.csv")
    write.csv(data.updated, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, dest = drop.folder)
  }

remove.orders <- function(args) {
  data.updated <- args$data %>% 
    filter(id != args$id)
    # filter(!(date == args$date & name == args$name & item == args$item))
    
  filePath <- file.path(tempdir(), "current_orders.csv")
  write.csv(data.updated, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = drop.folder)
}

remove.purchases <- function(args) {
  date <- Sys.Date()
  month <- month(date,label = T)
  Y <- year(date)
  file.name <- paste(Y, month, "purchases.csv", sep = "_")
  
  data.updated <- args$data %>% 
    filter(id != args$id)
  filePath <- file.path(tempdir(), file.name)
  # filePath <- file.path(db.responses.dir, Y, month, file.name)
  write.csv(data.updated, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = file.path(db.responses.dir, Y, month))
}

load.purchase.data <- function() {
  date <- Sys.Date()
  month <- month(date,label = T)
  Y <- year(date)
  file.name <- paste(Y, month, "purchases.csv", sep = "_")
  
  n_files <- drop_dir(file.path(db.responses.dir, Y, month)) %>% nrow()
    # if there is not any data create an empty data.frame  
    if(n_files == 0){
      data <- data_frame(
        name = character(0),
        item = character(0),
        date = .POSIXct(character(0)),
        price = numeric(0),
        id = numeric(0)
      )
    }else{
      data <-drop_read_csv(file.path(db.responses.dir, Y, month, file.name), 
                           colClasses = c("character", 
                                          "character", 
                                          "POSIXct", 
                                          "numeric", 
                                          "numeric"))  %>% 
        mutate(id = seq_along(date)) 
    }
  data
}


handle_files <- function(){
  # make sure there are folders for current year and month
  current.folders.year <- drop_dir(file.path(db.responses.dir)) %>% 
    mutate(folder = str_split(path, "/")[[1]][[4]])
  
  
  if(Y %in% current.folders.year$folder){
    print("is already there")
  }else{
    drop_create(file.path(db.responses.dir, Y))
  }
  
  current.folders.month <- drop_dir(file.path(db.responses.dir, Y)) %>% 
    mutate(folder = str_split(path, "/")[[1]][[5]])
  
  
  if(month %in% current.folders.month$folder){
    print("is already there2")
  }else{
    drop_create(file.path(db.responses.dir, Y, month))
  }
}


update.purchase.answers <- function(data, new.item) {
  # data <- pur
  # pur.data = purchaseformData()
  date <- Sys.Date()
  month <- month(date,label = T)
  Y <- year(date)
  new.item <- data$item
  # handle_files()
  # # make sure there are folders for current year and month
  current.folders.year <- drop_dir(file.path(db.responses.dir)) %>%
    mutate(folder = str_split(path, "/")[[1]][[4]])
  # handle_files()
# 
  if(Y %in% current.folders.year$folder){
    print("is already there")
  }else{
    drop_create(file.path(db.responses.dir, Y))
    }

  current.folders.month <- drop_dir(file.path(db.responses.dir, Y)) %>%
    mutate(folder = str_split(path, "/")[[1]][[5]])


  if(month %in% current.folders.month$folder){
    print("is already there2")
  }else{
    drop_create(file.path(db.responses.dir, Y, month))
  }
  
  # data <- data_frame(1, "H", "coriander", Sys.time(), 2)
  # colnames(data) <- colnames(d)
  # Update the purchase file, add new elements
  data.updated <- rbind(
    # pur.data,
    # purchaseformData(),
    load.purchase.data(),
    data[[1]]
  )
  print("loaded second time")
  filePath <- file.path(tempdir(), paste(Y, month, "purchases.csv", sep = "_"))
  write.csv(data.updated, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = file.path(db.responses.dir, Y, month))
  
  if(data[[2]] == FALSE){
  # browser()
  item.to.delete <- data[[1]]$item
  # delete from the orders file the item that was purchased
  data.orders <-load.current.orders.data() %>% 
    filter(item != item.to.delete)
  
  filePath <- file.path(tempdir(), "current_orders.csv")
  write.csv(data.orders, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = drop.folder)
  }
}



shinyServer(function(input, output, session){
  action <- reactive({
    input$action
  })
  
  output$chooseFormType <- renderUI({
    
    if(action() == "Add purchase"){
      tagList(
        # id = "form_buy",
        h3 = "I bought something",
        selectInput("buy.name", "Who",
                    # items
                    c("",  "E", "A", "H")
        ),
        checkboxInput("newitemcheck", "The item was not in the list", F ),
        uiOutput("buycheck"),
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
    }else if(action() == "Add order"){
      tagList(
        # id = "form_need",
        
        selectInput("name", "Who",
                    c("",  "E", "A", "H")),
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
    }else if(action() == "Remove order"){
      tagList(
        # id = "form_need",
        
        # selectInput("name", "Who",
                    # c("",  "E", "A", "H")),
        # textInput("item", "What"),
        # selectInput("priority", "Priority",
                    # c("",  "urgent", "take it easy")),
        uiOutput("showOrdersID"),
        # uiOutput("showOrdersDate"),
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
        # id = "form_need",
        
        # selectInput("name", "Who",
                    # c("",  "E", "A", "H")),
        # textInput("item", "What"),
        # selectInput("priority", "Priority",
        # c("",  "urgent", "take it easy")),
        uiOutput("showPurchasesID"),
        # uiOutput("showPurchaseDate"),
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
  
  # debug(catchy)
  
  
  
  # submit another response
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  })
  
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_order, {
    catchy("submit_order", f = update.current.orders, args = list(data = orderformData()))
  })
  # When the Submit button is clicked, submit the response
  observeEvent(input$submit_purchase, {
    # browser()
    catchy("submit_purchase", 
           f = update.purchase.answers, 
            args  = list(data = purchaseformData(), 
                         new.item = input$newitemcheck
                         ))
  })
  # When the Submit button is clicked, submit the response
  observeEvent(input$remove_order, {
    catchy("remove_order", 
           f = remove.orders, 
           args = list(
             data = load.current.orders.data(), 
             id = input$remove.order.id))
  })
  
  observeEvent(input$remove_purchase, {
    catchy("remove_purchase", 
           f = remove.purchases, 
           args = list(
             data = load.purchase.data(), 
             id = input$remove.purchase.id))
  })
  
  # # When the Submit button is clicked, submit the response
  # observeEvent(input$submit_order, {
  #   catchy("remove_purchase")
  # })
    
  output$buycheck <- renderUI({
    data <- load.current.orders.data()
      
      items <-  data$item
    conditionalPanel(
      condition = "input.newitemcheck == false",
      selectInput("list.purchase.item", "What",
                  items)
    )
  })
  
  
  output$showPurchasesID <- renderUI({
    data <- load.purchase.data()
    items <-  data$id
    
    
    selectInput("remove.purchase.id", "Select ID of item to be removed",
                  items)
  })
  
  output$showOrdersID <- renderUI({
    data <- load.current.orders.data()
    items <-  data$id
    
    selectInput("remove.order.id", "Select ID of item to be removed",
                items)
  })
  # output$showOrdersDate <- renderUI({
  #   data <- load.current.orders.data()
  #   dates <-  data$date
  #   
  #   selectInput("remove.item.date", "When",
  #               dates)
  # })
  # 
  # remove.purchase.id
  # output$showPurchases <- renderUI({
  #   data <- load.purchase.data()
  #   items <-  data$item
  #   
  #   selectInput("remove.purchase", "What",
  #               items)
  # })
  # 
  # output$showPurchaseDate <- renderUI({
  #   data <- load.purchase.data()
  #   dates <-  data$date
  #   
  #   selectInput("remove.purchase.date", "When",
  #               dates)
  # })
  # 
  
  # Show the responses already submitted
  output$OrdersData <- DT::renderDataTable(
    load.current.orders.data() %>% 
      mutate(date = as.character(date)),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )
  
  
  output$PurchasesData <- DT::renderDataTable(
    load.purchase.data() %>% 
      mutate(date = as.character(date)),
    rownames = FALSE,
    options = list(searching = FALSE, lengthChange = FALSE)
  )
  
  
  # Allow user to download responses
  output$downloadPurchBtn <- downloadHandler(
    filename = function() { 
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(load.purchase.data(), file, row.names = FALSE)
    }
  )  
  
  # Allow user to download responses
  output$downloadOrdersBtn <- downloadHandler(
    filename = function() { 
      sprintf("%s.csv", humanTime())
    },
    content = function(file) {
      write.csv(load.current.orders.data(), file, row.names = FALSE)
    }
  )  
  
  output$Tables <- renderUI({
    if(action() %in% c("Add purchase", "Remove purchase")){
      div(
        id = "adminPanel",
        h2("Previous purchases"),
        downloadButton("downloadPurchBtn", "Download purchases"), br(), br(),
        DT::dataTableOutput("PurchasesData")
        
      )
    }else if(action() %in% c("Add order", "Remove order")){
      div(
        h2("Current orders"),
        downloadButton("downloadOrdersBtn", "Download Orders"), br(), br(),
        DT::dataTableOutput("OrdersData") 
      )
    }
  })
  

  
  
}
)