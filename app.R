library(shiny)
library(shinyBS)

# Define UI for application that takes height and weight as inputs
ui <- fluidPage(
  titlePanel("BMI Calculator"),
  br(),
  numericInput("Weight", "Weight (in kg):", 'a'),
  br(),
  numericInput("Height", "Height (in cm):", 'b'),
  br(),
  actionButton("Cal", "Calculate"),
  bsModal("modalnew", "Output", "Cal", size = "large",
          htmlOutput("BMI"),
          br(),
          htmlOutput("conclusion"),
          br(),
          htmlOutput("table")
  )
)

# Define server logic required to calculate BMI and to show output
server <- function(input, output, session) {
  
  output$BMI <- renderText({
    bmi <- round((input$Weight/(input$Height/100))/(input$Height/100),2)
    
    if(bmi < 18.5) { conclusion <- "Underweight" } 
    else if(bmi >= 18.5 && bmi <= 24.9) { conclusion <- "Normal weight" } 
    else if(bmi >= 25 && bmi <= 29.9) { conclusion <- "Overweight" } 
    else { conclusion <- "Obese" }
    
    output$conclusion <- renderText({paste0("Conclusion: ", conclusion)})
    
    Ranges <- c("Less than 18.5", ">= 18.5 & <= 24.9", ">= 25 & <= 29.9", "30 & Above")
    Conclusions <- c("Underweight", "Normal weight", "Overweight", "Obese")
    dt <- data.frame(Ranges, Conclusions)
    output$table <- renderTable({dt})
    
    paste0("Your BMI is ", bmi)
  })
  
  observeEvent(input$close, {
    toggleModal(session, "modalnew", toggle = "close") 
  })
}

# Run the application 
shinyApp(ui = ui, server = server)