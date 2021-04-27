#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define server logic to read selected file ----
server <- function(input, output) {
    dataframe <- reactive({
        # input$file1 will be NULL initially. After the user selects
        # and uploads a file, it will be a data frame with 'name',
        # 'size', 'type', and 'datapath' columns. The 'datapath'
        # column will contain the local filenames where the data can
        # be found.
        inFile <- input$file1
        if (is.null(inFile))
            return(NULL)
        data <- read.csv(inFile$datapath, header = F, sep = ";")
    })
    
    output$contents <- renderTable({
        dataframe()
        })
    
    output$dimensions <- renderTable({
        dims <- dim(dataframe())
    })
    
    observeEvent(input$file1,{
        print(paste0("File loaded: ", input$file1$name))
        print(paste0(toString(dim(dataframe()))))
    })
}

