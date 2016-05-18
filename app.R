library(shiny)
library(Hmisc)
library(car)
library(stats)
options(shiny.maxRequestSize=100*1024^2)

ui <- fluidPage(
  
  
  
  sidebarPanel(
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    tags$hr(),
    checkboxInput('header', 'Header', TRUE),
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 ','),
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"')
  ),
  mainPanel(
    tabsetPanel(
      
      tabPanel(
        "content",
        dataTableOutput('contents')
      ),
      tabPanel("Summary",
               h4("Structure of the data"),
               verbatimTextOutput("structure"),
               h4("Data Summary"),
               verbatimTextOutput("summary")
              
               #  tableOutput('contents')
      ),
      tabPanel(
        "Correlation",
        plotOutput("corr"),
        h4("Correlation Table"),
        verbatimTextOutput("corrTable")
      ),
   
      tabPanel( "Histogram",
                uiOutput("xval"),
                plotOutput("hist"),
               verbatimTextOutput("indSummary")
      ),
      tabPanel(
        "Barchart",
        uiOutput("xval1"),
        plotOutput("bar")
      ),
      tabPanel(
        
        "Multibarchart",
        uiOutput("multibarColorBy"),
        uiOutput("multibaryval"),
        plotOutput("multibar")
      )
      
      
    )
  )
  
)



server <-  function(input, output) {
  
  output$structure <- renderPrint({
    gsub("factor","String",sapply(datasetInput(),class))
  })
  
  output$contents <- renderDataTable({
    
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, nrows = 20, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
  datasetInput<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  })
  
  numberOfdCol <- reactive({
   noCol <- ncol(datasetInput())
   
  })
  
  
  output$summary <- renderPrint({
   summary(datasetInput())
  })
  
  
  output$xval <- renderUI({selectInput("x","x-Value",c(names(datasetInput())[sapply(datasetInput(), class) == "integer"]))})
  #output$yval <- renderUI({selectInput("y","y-value",c(names(datasetInput())))})
  
  output$xval1 <- renderUI({selectInput("x1","Value",c(names(datasetInput())))})
  #output$yval1 <- renderUI({selectInput("y1","y-value",c(names(datasetInput())[sapply(datasetInput(), class) == "integer"]))})
  
  
  output$corr <- renderPlot({
    pairs(datasetInput())
    
      })
  
  ndata <- reactive({datasetInput()[sapply(datasetInput(),is.numeric)]})
  output$corrTable <- renderPrint({
    cor(ndata())
    
  #  print(numberOfdCol())
    
  })
  output$bar <- renderPlot({
    title <- "name"
    counts <- table(datasetInput()[,input$x1])
    barplot(counts,xlab=input$x1 )
  })
  
  output$hist <- renderPlot({
    title <- "name"
     hist(datasetInput()[,input$x])
  })
  
  output$indSummary <- renderPrint({
    
    summary(datasetInput()[,input$x])
  })
  output$multibarColorBy <- renderUI({selectInput("ColorBy","Color By",c(names(datasetInput())[sapply(datasetInput(), class) == "factor"]))})
  output$multibaryval <- renderUI({selectInput("yvalgg","y-value",c(names(datasetInput())[sapply(datasetInput(), class) == "integer"]))})
  
  
  output$multibar <- renderPlot({
    abc <- ggplot(datasetInput(),aes(datasetInput()[,input$yvalgg],fill = datasetInput()[,input$ColorBy])) + geom_bar(width = 5)
    abc
  })
}



shinyApp(ui = ui, server = server)
