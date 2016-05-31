library(shiny)
library(Hmisc)
library(car)
library(stats)
library(sm)
library(vcd)
library(randomForest)
options(shiny.maxRequestSize=100*1024^2)

ui <- fluidPage(
  
 
  headerPanel(
    list(tags$head( tags$style("body {background-color: #FDFDFD;}"),
                    tags$title("Data Quality analysis")
                    ))),
  #tags$head(tags$img(rel = "icon" ,src="ANALYTOS300PX.png")),
  
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
               dataTableOutput("summary")
              
               #  tableOutput('contents')
      ),
      tabPanel(
        "Correlation",
        plotOutput("corr"),
        h4("Correlation Table"),
        verbatimTextOutput("corrTable")
      ),
   
  
      tabPanel(
        "Outliers",
        uiOutput("BPColorBy"),
        uiOutput("BPxval"),
        plotOutput("boxplot")
      ),
      tabPanel(
        "VariableImportance",
        plotOutput("Variable_importance_function"),
        tableOutput("var.imp")
        
      ),
      tabPanel(
        "Data insight",
      
        tags$div(
          height = "500px",
          tags$div(
            "Bar Chart",
            style="display:inline-block ",
            float = "left",
            uiOutput("xval1"),
            plotOutput("bar")
          ),

          
          tags$div(
            "Histogram",
            style="display:inline-block",
            uiOutput("xval"),
            plotOutput("hist")
           # verbatimTextOutput("indSummary")
          )
        ),
        tags$div(
          tags$div(
            "Multi Bar Chart",
            style="display:inline-block",
            float = "right",
            uiOutput("multibarColorBy",width = "50px"),
            uiOutput("multibaryval", width = "50px"),
            plotOutput("multibar")
          ),
          tags$div(
            "Density",
            style="display:inline-block",
            uiOutput("DGColorBy"),
            uiOutput("DGxval"),
            plotOutput("densityGraph")
          )
        )
      )
      
      
    )
  )
  
)



server <-  function(input, output) {
  
  
  output$Variable_importance_function <- renderPlot({
    Variable_importance=randomForest(datasetInput(),na.rm=T)
    varImpPlot(Variable_importance,sort = T)
    
  })
  
  
  output$var.imp<- renderTable({
    Variable_importance=randomForest(datasetInput(),na.rm=T)
    var.imp = data.frame(importance(Variable_importance,type = 2)) 
    var.imp$Variables <- row.names(var.imp)
    var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),]
    
    
  })
 
  output$BPColorBy <- renderUI({selectInput("BPColor","value 1",c(names(datasetInput())[sapply(datasetInput(), class) == "factor"]))})
  output$BPxval <- renderUI({selectInput("BPxvalgg","value 2",c(names(datasetInput())[sapply(datasetInput(), class) == "integer" || sapply(datasetInput(),class) == "numeric"|| sapply(datasetInput(),class) == "int"]))})
  
  
  output$boxplot <- renderPlot ({
    # sm.density.compare(datasetInput()[,input$DGxvalgg], datasetInput()[,input$DGColor], xlab=datasetInput()[,input$DGColor],col=topo.colors(3))
    # legend("topright", inset=.02,legend= unique(datasetInput()[,input$DGColor]), fill=topo.colors(3), horiz=FALSE, cex=0.8)
    boxplot(datasetInput()[,input$BPxvalgg] ~ datasetInput()[,input$BPColor] ,col=topo.colors(3))
    legend("bottomright", inset=.02,legend= unique(datasetInput()[,input$BPColor]), fill=topo.colors(3), horiz=FALSE, cex=0.8)
    
    
  })
  
  
  
  datasetInput<-reactive({
    
    inFile <- input$file1
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header=input$header, sep=input$sep, 
             quote=input$quote)
  }) 
  
  output$DGColorBy <- renderUI({selectInput("DGColor","Color By",c(names(datasetInput())[sapply(datasetInput(), class) == "factor"]))})
  output$DGxval <- renderUI({selectInput("DGxvalgg","x-value",c(names(datasetInput())[sapply(datasetInput(), class) == "integer" || sapply(datasetInput(),class) == "numeric"|| sapply(datasetInput(),class) == "int"]))})
  
  
  output$densityGraph <- renderPlot ({
   # sm.density.compare(datasetInput()[,input$DGxvalgg], datasetInput()[,input$DGColor], xlab=datasetInput()[,input$DGColor],col=topo.colors(3))
   # legend("topright", inset=.02,legend= unique(datasetInput()[,input$DGColor]), fill=topo.colors(3), horiz=FALSE, cex=0.8)
   qplot(datasetInput()[,input$DGxvalgg], data=datasetInput(), geom = "density",alpha=I(.7),fill=datasetInput()[,input$DGColor])
    
  },height = 300, width = 400 )
  
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
  

  
  numberOfdCol <- reactive({
   noCol <- ncol(datasetInput())
   
  })
  
  
  output$summary <- renderDataTable({
   summary(datasetInput())
  })
  
  
  output$xval <- renderUI({selectInput("x","x-Value",c(names(datasetInput())[sapply(datasetInput(), class) == "integer" || sapply(datasetInput(),class) == "numeric"|| sapply(datasetInput(),class) == "int"]))})
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
    barplot(counts,xlab=input$x1,col = "lightseagreen" )
  },height = 300, width = 400)
  
  output$hist <- renderPlot({
    title <- "name"
     hist(datasetInput()[,input$x] , col = "lightskyblue1")
  },height = 300, width = 400)
  
  output$indSummary <- renderPrint({
    
    summary(datasetInput()[,input$x])
  })
  output$multibarColorBy <- renderUI({selectInput("ColorBy","Color By",c(names(datasetInput())[sapply(datasetInput(), class) == "factor"]))})
  output$multibaryval <- renderUI({selectInput("yvalgg","y-value",c(names(datasetInput())[sapply(datasetInput(), class) == "integer" || sapply(datasetInput(),class) == "numeric"|| sapply(datasetInput(),class) == "int"]))})
  
  
  output$multibar <- renderPlot({
    abc <- ggplot(datasetInput(),aes(datasetInput()[,input$yvalgg],fill = datasetInput()[,input$ColorBy])) + geom_bar()
    abc
  },height = 300, width = 400)
}



shinyApp(ui = ui, server = server)
