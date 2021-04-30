library(shiny)
library(ggplot2)

runApp(list(
    ui = shinyUI(pageWithSidebar(
        headerPanel('Uploading Files'),
        sidebarPanel( 
            fileInput('file1', 'Choose file to upload',
                      accept = c('text/csv',
                                 'text/comma-separated-values',
                                 'text/tab-separated-values',
                                 'text/plain','.csv','.tsv')),
            uiOutput('buttonsUI'), br(),
            uiOutput('downloadUI')
        ),
        mainPanel(
            tableOutput('readytable'),
            plotOutput('testplot')
        )
    )), 
    server = shinyServer(function(input, output) {
        # variables to control the sequence of processes 
        controlVar <- reactiveValues(fileReady = FALSE, tableReady = FALSE)
        # to keep the data upload
        dat <- NULL
        # handle the file reading
        observeEvent(input$file1, {
            controlVar$fileReady <- FALSE
            if (is.null(input$file1))
                return()
            inFile <- input$file1
            dat <<- read.csv(inFile$datapath, sep = ';', header=FALSE)
            if(!is.data.frame(dat))
                return()
            controlVar$fileReady <- TRUE
        })
        # show buttons only when file is uploaded
        output$buttonsUI <- renderUI({
            if (controlVar$fileReady)
                div(
                    textInput('text1','Type what will be in column 6'),
                    actionButton('go','go')
                )
        })
        # show a download button only if data is ready
        output$downloadUI <- renderUI({
            if (controlVar$tableReady)
                downloadButton('downloadData', 'Download')
        })
        # add columns to dat and run some script on it
        observeEvent(input$go, {
            controlVar$tableReady <- FALSE
            if (!is.null(input$text1))
                dat[,6] <<- input$text1
            # simulate running a cool script on dat
            Sys.sleep(2)
            controlVar$tableReady <- TRUE  
        })
        
        # render table after uploading file or running the script
        output$readytable <- renderTable({
            input$go
            if (controlVar$fileReady || controlVar$tableReady)
                dat
        })
        
        output$testplot <- renderPlot({
            if(!controlVar$fileReady)
                return()
            time = dat[,2]
            P1 = dat[,3]
            P2 = dat[,5]
            plot(time, P1, col="red", ylim=c(0,12), type='l')
            lines(time, P2, col="blue")
        })
        
        # handle the download button
        output$downloadData <- downloadHandler(
            filename = function() { 'newData.csv' },
            content = function(file) {
                write.csv(dat, file)
            }
        )
    })
))