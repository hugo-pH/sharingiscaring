library(shiny)
fluidPage(
  shinyjs::useShinyjs(),
  title = "Home needs",
  div(id = "header",
      h1("The ultimate shoppinglist app"),
      h4("")),
  fluidRow(
    column(12,
           radioButtons("action", "What do you want to do?", 
                        choices = c("Add order", "Remove order" , "Add purchase", "Remove purchase"), 
                        selected = "Add order"),
           uiOutput("chooseFormType"), 
           shinyjs::hidden(
             div(
               id = "thankyou_msg",
               h3("Thanks, your response was submitted successfully!"),
               actionLink("submit_another", "Submit another response")
             )
           )
    )
  ),
  fluidRow(
    column(12, uiOutput("Tables"))
    )
)
  