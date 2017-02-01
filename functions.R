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

##################
### Load files ###
##################

load.orders.data  <- function(drop.folder, file.name) {
  print(drop.folder)
  print(file.name)
  file.name.path <- file.path(drop.folder, file.name)
  print(file.name.path)
  data <-  drop_read_csv(file.name.path, 
                         colClasses = c("numeric", 
                                        "character", 
                                        "character", 
                                        "POSIXct", 
                                        "character")) %>% 
    mutate(id = seq_along(date)) %>% 
    select(id, name, item, date, priority)
  return(data)
}



load.purchase.data <- function(drop.folder, db.responses.dir, file.ending) {
  date <- Sys.Date()
  month <- month(date,label = T)
  Y <- year(date)
  file.name <- paste(Y, month, file.ending, sep = "_")
  
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



update.current.orders <- function(arg.list) {
  # browser()
  data.updated <- bind_rows(
    load.orders.data(drop.folder = arg.list$drop.folder,
                     file.name = arg.list$file.name),
    arg.list$data
  )
  filePath <- file.path(tempdir(), arg.list$file.name)
  # filePath <- file.path(tempdir(), "current_orders.csv")
  write.csv(data.updated, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = arg.list$drop.folder)
}

remove.orders <- function(arg.list) {
  db.responses.dir <- arg.list$db.responses.dir
  data  <- arg.list$data
  drop.folder = arg.list$drop.folder
  orders.file.name = arg.list$orders.file.name
  purchase.file.name = arg.list$purchase.file.name
  id.rm = arg.list$id
  
  data.updated <- data %>% 
    filter(id != id.rm)
  # filter(!(date == args$date & name == args$name & item == args$item))
  
  filePath <- file.path(tempdir(), orders.file.name)
  write.csv(data.updated, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = drop.folder)
}


update.purchase.answers <- function(arg.list) {
  # browser()
  db.responses.dir <- arg.list$db.responses.dir
  new.item <- arg.list$new.item
  data  <- arg.list$data
  drop.folder = arg.list$drop.folder
  orders.file.name = arg.list$orders.file.name
  purchase.file.name = arg.list$purchase.file.name
 
  date <- Sys.Date()
  month <- month(date,label = T)
  Y <- year(date)
  # browser()
  handle_files(db.responses.dir, Y = Y, month = month)
  # Update the purchase file, add new elements
  data.updated <- rbind(
    load.purchase.data(drop.folder =  drop.folder,
                       db.responses.dir = db.responses.dir,
                        file.ending = purchase.file.name),
    data
  )
  print("loaded second time")
  filePath <- file.path(tempdir(), paste(Y, month, purchase.file.name, sep = "_"))
  write.csv(data.updated, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = file.path(db.responses.dir, Y, month))
  
  if(new.item == FALSE){
    # browser()
    item.to.delete <- data$item
    # delete from the orders file the item that was purchased
    data.orders <-load.orders.data(drop.folder = drop.folder,
                                   file.name = orders.file.name) %>% 
      filter(item != item.to.delete)
    
    filePath <- file.path(tempdir(), orders.file.name)
    write.csv(data.orders, filePath, row.names = FALSE, quote = TRUE)
    # Upload the file to Dropbox
    drop_upload(filePath, dest = drop.folder)
  }
}



remove.purchases <- function(arg.list) {
  
  db.responses.dir <- arg.list$db.responses.dir
  data  <- arg.list$data
  drop.folder = arg.list$drop.folder
  orders.file.name = arg.list$orders.file.name
  purchase.file.name = arg.list$purchase.file.name
  id.rm = arg.list$id
  date <- Sys.Date()
  month <- month(date,label = T)
  Y <- year(date)
  file.name <- paste(Y, month, purchase.file.name, sep = "_")
  
  data.updated <- data %>% 
    filter(id != id.rm)
  filePath <- file.path(tempdir(), file.name)
  # filePath <- file.path(db.responses.dir, Y, month, file.name)
  write.csv(data.updated, filePath, row.names = FALSE, quote = TRUE)
  # Upload the file to Dropbox
  drop_upload(filePath, dest = file.path(db.responses.dir, Y, month))
}



handle_files <- function(db.responses.dir, Y, month){
  # make sure there are folders for current year and month
  n.current.folders.year <- drop_dir(file.path(db.responses.dir)) %>% 
    nrow()

  
  if(n.current.folders.year > 0){
    print("is already there")
  }else{
    drop_create(file.path(db.responses.dir, Y))
  }
  
  n.current.folders.month <- drop_dir(file.path(db.responses.dir, Y)) %>% 
    nrow()
  
  if(n.current.folders.month > 0){
    print("is already there2")
  }else{
    drop_create(file.path(db.responses.dir, Y, month))
  }
}


