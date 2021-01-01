### Data Science Capstone : Final Project App: Next Word Prediction


library(shiny)


shinyUI(fluidPage(
    
    # Application title
    titlePanel("Data Science Capstone: Final Project App - Word Prediction"),
    navbarPage("Section",
               tabPanel("App Description",
                        mainPanel(
                            includeHTML("App.html"),
                        )
               ), 
               #First Tab    
               tabPanel("Project Instruction",
                        mainPanel(
                            includeHTML("Instructions.html")
                        )
               ),
               #Second Tab
               tabPanel("Predict Word App",
                        sidebarPanel(
                            textInput("inputString", "Enter your word or phrase here", value = ""),
                            helpText("Please click on the below button to get your next word"),
                            submitButton("Predict My Word")),
                        mainPanel(
                            h2("Let me think!!! Your next word it should be"),
                            verbatimTextOutput("prediction"),
                            h4("Your complete phrase is:"),
                            textOutput('text1')
                        )
               )
    )
))

