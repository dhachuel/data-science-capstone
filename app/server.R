library(shiny)
library(tokenizers)


applyNGram <- function(input_text){
    return(tokenizers::tokenize_words(input_text)[[1]])
}

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    # Expression that generates a histogram. The expression is
    # wrapped in a call to renderPlot to indicate that:
    #
    #  1) It is "reactive" and therefore should re-execute automatically
    #     when inputs change
    #  2) Its output type is a plot

    output$value <- renderText({ paste(applyNGram(input_text = input$text), collapse=' | ' ) })
    
})


