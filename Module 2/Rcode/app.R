library(shiny)




ui <- fluidPage(
  titlePanel("Simple Body Fat Calculator"),
  sidebarLayout(
    mainPanel(
      h5("This is a simple and easy calculator for bodyfat. 
        You just need to enter your abdomen circumstance and your age to get the result!"),
      numericInput("ab", "Abdomen circumstance(cm)", 85.36), #the default is Phelps' data
      selectInput("age_range", "Select the your age range",
                  choices = c("22-39","40-59","60+"))
    ),
    mainPanel(
      h2("The result is:"),
      textOutput("output")
    )))
server <- function(input, output){
  output$output <- renderText({
    switch(input$age_range,
           "22-39" = input$ab * 0.55587 -33.64708 ,
           "40-59" = input$ab * 0.55587 + 1.39555 -33.64708,
           "60+" = input$ab * 0.55587 + 3.38473 -33.64708)
    
  })
  
}
app <- shinyApp(ui, server)
app