library(shiny)
library(ggplot2)


dataCleanUp <- function(df){
    n = 4
    for (i in 2 : (length(df[ , 1])-(n+1))){
        #check for zeros
        if (df[i, 3] == 0)
            #if the position is zero, set it to the last position
            df[i, 3] = df[i-1, 3]
        if (df[i, 5] == 0)
            #if the position is zero, set it to the last position
            df[i, 5] = df[i-1, 5]
        #Correct position changes
        #P1
        if (df[i, 3] != df[i-1, 3])
            #If in the next n entries any position is different, go back to the last position
            #Only count stable positions
            if(any(df[(i+1):(i+n), 3] != df[i, 3]))
                df[i, 3] = df[i-1,3]
        #P2
        if (df[i, 5] != df[i - 1, 5])
            #If in the next n entries any position is different, go back to the last position
            #Only count stable positions
            if (any(df[(i + 1):(i + n), 5] != df[i, 5]))
                df[i, 5] = df[i - 1, 5]
    }
    return(split(df, df$V1))
}


runApp(list(
    ui = shinyUI(pageWithSidebar(
        headerPanel('MK Evaluation'),
        sidebarPanel( 
            fileInput('file1', 'Choose file to upload',
                      accept = c('text/csv',
                                 'text/comma-separated-values',
                                 'text/tab-separated-values',
                                 'text/plain','.csv','.tsv')), br(),
            uiOutput('debugUI'),
            uiOutput('TrackListUI'),
            uiOutput('buttonsUI')
        ),
        mainPanel(
            tableOutput('readytable'),
            plotOutput('testplot')
        )
    )), 
    server = shinyServer(function(input, output) {
        #TODO:
        #Clean up data:
            #rank zero when falling down
            #randomness when overtaking, make sure position is the same in either left or right frames
        #Separate by tracks
        #Track selection    DONE
        #Upload track list?

        
        # variables to control the sequence of processes 
        controlVar <- reactiveValues(fileReady = FALSE, trackReady = FALSE)
        # to keep the data upload
        dat <- NULL
        # handle the file reading
        observeEvent(input$file1, {
            controlVar$fileReady <- FALSE
            if (is.null(input$file1))
                return()
            inFile <- input$file1
            dat <<- read.csv(inFile$datapath, sep = ',', header=FALSE)
            trackdata <<- dataCleanUp(dat)
            if(!is.data.frame(dat))
                return()
            controlVar$fileReady <- TRUE
        })
        
        #Debug option to display data in table
        output$debugUI <- renderUI({
            if (controlVar$fileReady)
                checkboxInput("ShowTable", "Show Table", FALSE)
        })
        
        #Show Tracklist only when file is uploaded
        output$TrackListUI <- renderUI({
            if (controlVar$fileReady)
                #Check how many tracks there are
                div(
                    selectInput('tracks', 'Tracks', seq(1, tail(dat[,1],1)+1,1))
                )
        })
        
        #Control variable to check if a track has been selected
        observeEvent(input$tracks, {
            controlVar$trackReady <- FALSE
            if (is.null(input$tracks))
                return()
            if (input$tracks > 0)
                controlVar$trackReady <- TRUE
        }
        )
        
        # show buttons only when file is uploaded
        output$buttonsUI <- renderUI({
            if (controlVar$fileReady)
                div(
                    actionButton('go','go')
                )
        })
        

        # Simulate running script on dat when pushing go
        observeEvent(input$go, {
            # simulate running a cool script on dat
            Sys.sleep(2)
            controlVar$trackReady <- TRUE  
        })
        
        
        
        # render table after uploading file or running the script
        # Left for testing, will be eventually removed
        output$readytable <- renderTable({
            if (is.null(input$ShowTable))
                return()
            if (controlVar$fileReady && input$ShowTable)
                if (controlVar$trackReady)
                    trackdata[[as.integer(input$tracks)]]
        })
        
        #Testplot, should be changed to ggplot later
        output$testplot <- renderPlot({
            if(!controlVar$fileReady)
                return()
            if(!controlVar$trackReady)
                return()
            data = trackdata[[as.integer(input$tracks)]]
            time = data[,2]
            P1 = data[,3]
            P2 = data[,5]
            plot(time, P1, col="red", ylim=c(0,12), type='l', xlab="Time", ylab="Position", lwd=2)
            lines(time, P2, col="blue", lwd = 2)
            legend("topright", legend=c("Player 1", "Player 2"), col=c("red", "blue"), lty=c(1,1))
            
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