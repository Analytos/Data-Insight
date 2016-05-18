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
               verbatimTextOutput("structure"),
               verbatimTextOutput("summary")
              
               #  tableOutput('contents')
      ),
      tabPanel(
        "Correlation",
        plotOutput("corr"),
        verbatimTextOutput("corrTable")
      ),
   
      tabPanel( "Histogram",
                uiOutput("xval"),
               # uiOutput("yval"),
                plotOutput("hist"),
               verbatimTextOutput("indSummary")
      ),
      tabPanel(
        "Barchart",
        uiOutput("xval1"),
        uiOutput("yval1"),
        plotOutput("box")
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
  
  output$xval1 <- renderUI({selectInput("x1","Value",c(names(datasetInput())[sapply(datasetInput(), class) == "factor"]))})
  output$yval1 <- renderUI({selectInput("y1","y-value",c(names(datasetInput())[sapply(datasetInput(), class) == "integer"]))})
  
  
  output$corr <- renderPlot({
    pairs(datasetInput())
    
      })
  
  
  output$corrTable <- renderPrint({
    cor(iris$Sepal.Length, iris$Petal.Length)
    
  #  print(numberOfdCol())
    
  })
  output$box <- renderPlot({
    title <- "name"
    barplot(datasetInput()[,input$x1]
         #ylab=input$y1,
         #xlab=input$x1
         )
  })
  
  output$hist <- renderPlot({
    title <- "name"
     hist(datasetInput()[,input$x])
  })
  
  output$indSummary <- renderPrint({
    
    summary(datasetInput()[,input$x])
  })
  
}



shinyApp(ui = ui, server = server)
