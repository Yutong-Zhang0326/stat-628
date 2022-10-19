library(shiny)
library(tidyverse)

ui <- fluidPage(  
    titlePanel(h1(id = "title", "Simple Body Fat Calculator", align = "center")),
    sidebarPanel(
        h5("This is a simple and easy calculator for bodyfat. 
        You just need to enter your abdomen circumstance and your age to get the result!"),
        hr(),
        h2("Please enter your information"),
        numericInput("ab", "Abdomen circumstance(cm)", 85.36), #the default is Phelps' data
        selectInput("age_range", "Select the your age range",
                    choices = c("22-39","40-59","60+")),
        submitButton()
    ),
    mainPanel(
        h2("Your body fat percentage is:"),
        htmlOutput("output")
    ))


categorization = function(bodyfat){
    case_when(bodyfat >=2 & bodyfat < 5 ~ "Body fat percentage category: Low Body Fat Risk",
              bodyfat >= 5 & bodyfat < 8 ~ "Body fat percentage category: Ultra Lean",
              bodyfat >=8 & bodyfat < 12 ~ "Body fat percentage category: Lean",
              bodyfat >= 12 & bodyfat < 20 ~ "Body fat percentage category: Moderately Lean",
              bodyfat >= 20 & bodyfat < 30 ~ "Body fat percentage category: Excess Fat",
              bodyfat >= 30 ~ "Body fat percentage category: High Body Fat Risk",
              TRUE ~ "Error: Abnormal output. Please enter your real information!")
}


server <- function(input, output){
    output$output <- renderUI({
        ab = input$ab
        age_range = input$age_range
        if (age_range == "22-39"){
            out2239 = paste(h2(round(ab * 0.55587 -33.64708, 2),"%"))
            HTML(paste(out2239, h4(categorization(round(ab * 0.55587 -33.64708, 2))), h4("A healthy body fat percentage range for your age is: 8-18.2%")))
        }else if (age_range == "40-59"){
            out4059 = paste(h2(round(ab * 0.55587 + 1.39555 -33.64708, 2),"%"))
            HTML(paste(out4059, h4(categorization(round(ab * 0.55587 + 1.39555 -33.64708, 2))), h4("A healthy body fat range for your age is: 8-22.1%")))
        }
        else{
            out60 = paste(h2(round(ab * 0.55587 + 3.38473 -33.64708, 2),"%"))
            HTML(paste(out60, h4(categorization(round(ab * 0.55587 + 3.38473 -33.64708, 2))), h4("A healthy body fat range for your age is: 8-22.6%")))
        }
    })
    
}
app <- shinyApp(ui, server)
app