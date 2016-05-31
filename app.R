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
              
               h4(textOutput("Rowinfo" )),
               h4(textOutput("Colinfo" )),
               h4("Structure of the data"),
               tableOutput("structure"),
               h4("Data Summary"),
               tableOutput("summary")
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
 
  output$BPColorBy <- renderUI({
    
    a <- c(names(datasetInput())[sapply(datasetInput(), class) == "factor"])
    if(length(a) == 0) {
      selectInput("BPColor","Categories",c("None"))  
    }
    else
    {
    selectInput("BPColor","Categories",c("None",names(datasetInput())[sapply(datasetInput(), class) == "factor"]))
    }
     })
  
  output$BPxval <- renderUI({selectInput("BPyvalgg","Y values",c(names(datasetInput())[sapply(datasetInput(), class) == "integer" || sapply(datasetInput(),class) == "numeric"|| sapply(datasetInput(),class) == "int"]))})
  
  
  output$boxplot <- renderPlot ({
    
    catg<- input$BPColor
    print(catg)
    if(catg == "None")
    {
      boxplot(datasetInput()[,input$BPyvalgg],col=topo.colors(3))
      #legend("bottomright", inset=.02,legend= unique(datasetInput()[,input$BPColor]), fill=topo.colors(3), horiz=FALSE, cex=0.8)
      
    }
    else
    {
    boxplot(datasetInput()[,input$BPyvalgg] ~ datasetInput()[,input$BPColor] ,col=topo.colors(3))
    legend("bottomright", inset=.02,legend= unique(datasetInput()[,input$BPColor]), fill=topo.colors(3), horiz=FALSE, cex=0.8)
    
    }
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
  qplot(datasetInput()[,input$DGxvalgg], data=datasetInput(), geom = "density",alpha=I(.7),fill=datasetInput()[,input$DGColor])
    
  },height = 300, width = 400 )
  
  

  output$structure <- renderTable({
    types <- gsub("factor","String",sapply(datasetInput(),class))
    require(reshape2)
    df <- melt(data.frame(types))
    colnames(df) <- c( "Data types")
    df
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
  
  
  output$summary <- renderTable({
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
  
  
  
  output$Rowinfo <- renderText({
    
    paste( "Number of Rows",dim(datasetInput())[1])
  })
  
  
  output$Colinfo <- renderText({
    paste( "Number of Columns",dim(datasetInput())[2])
  })
  
  
  output$multibarColorBy <- renderUI({selectInput("ColorBy","Color By",c(names(datasetInput())[sapply(datasetInput(), class) == "factor"]))})
  output$multibaryval <- renderUI({selectInput("yvalgg","y-value",c(names(datasetInput())[sapply(datasetInput(), class) == "integer" || sapply(datasetInput(),class) == "numeric"|| sapply(datasetInput(),class) == "int"]))})
  
  
  output$multibar <- renderPlot({
    abc <- ggplot(datasetInput(),aes(datasetInput()[,input$yvalgg],fill = datasetInput()[,input$ColorBy])) + geom_bar()
    abc
  },height = 300, width = 400)
}



shinyApp(ui = ui, server = server)
