library(shiny)

# Define UI for application that draws a histogram
shinyUI(
    fluidPage(

        # Application title
        titlePanel("NGram Text Prediction Algorithm"),

        # 
        textInput(
            inputId="text", 
            label = h3("Start typing!"), 
            value = "Enter text..."
        ),

        hr(),

        verbatimTextOutput("value")
    )
)
