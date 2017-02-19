library(solvebio)
library(data.table)
library('corrplot')
library(shiny)
library(plotly)
library(shinythemes)

ui <- shinyUI(fluidPage(

  titlePanel("Gene Expression Analysis"),
  mainPanel(
    tabsetPanel(
      
      tabPanel("Analysis",
               plotOutput("plot",  height=600,
                          click = "plot_click",
                          brush = "plot_brush"
                          ),
              
               plotOutput("correlation",  height=600,
                          click = "plot_click",
                          brush = "plot_brush"
                          ),
 
               plotlyOutput("heat"),
               plotlyOutput("correlationply"),
               plotlyOutput("correlationTR"),
               verbatimTextOutput("info")
               ),
      tabPanel("correlation pairs", DT::dataTableOutput('genepairs'))
    )
  ),

  sidebarPanel(
    selectInput("expr_dataset", "Please choose your expression dataset:",
                choices = c("solvebio:NanoString/1.0.0/ExprMatrix"),
                selected = "solvebio:NanoString/1.0.0/ExprMatrix"),  width = 7)
))

server <- shinyServer(function(input, output, session) {

  datasetName <- reactive({
    input$expr_dataset
  })

  cols <- reactive({
    colnames(Dataset.query(datasetName()))
  })

  observe({ print(datasetName())})

  # observe({ print(cols())})

  expr_dt <- reactive({
    if (is.null(datasetName())) {
      return(NULL)}
    as.data.table(Dataset.query(datasetName(), limit = 10)) # paginate=TRUE))
  })

  observe({ print(dim(expr_dt()))})
  # observe({ print(colnames(expr_dt()))})

  genes <- reactive({
    if (is.null(expr_dt())) {
      return(NULL)}
    expr_dt()[,gene]
  })

  # observe({ print(length(genes()))})
  # observe({ print(genes())})

  expr_m <- reactive({
    if (is.null(expr_dt())) {
      return(NULL)}
    expr_dt()[,'_id':=NULL]
    expr_dt()[,'_commit':=NULL]
    expr_dt()[,gene:=NULL]
    as.matrix(expr_dt())
  })

  row_labels <- reactive({genes()})

  observe({ print(row_labels())})
  observe({ print(colnames(expr_m()))})
  
  output$plot <- renderPlot({
    if (is.null(expr_dt())) {
      return(NULL)}
    input$plot
    heatmap(expr_m(), labRow = row_labels())
  })

  output$heat <- renderPlotly({
    plot_ly(z = expr_m(), type = "heatmap", y = row_labels())
  })

  output$correlationply <- renderPlotly({
    plot_ly(z = cor(expr_m()), type = "heatmap", x = colnames(expr_m()), y = colnames(expr_m()))
  })
  
  output$correlationTR <- renderPlotly({
    plot_ly(z = cor(t(expr_m())), type = "heatmap", x = row_labels(), y = row_labels())
  })

  output$correlation <- renderPlot({
    if (is.null(expr_dt())) {
      return(NULL)}
    input$correlation
    corrplot(cor(expr_m()))
  })

  output$info <- renderText({

    xy_range_str <- function(e) {
      if(is.null(e)) return("NULL\n")
       paste0("xmin=", round(e$xmin, 1), " xmax=", round(e$xmax, 1), 
              " ymin=", round(e$ymin, 1), " ymax=", round(e$ymax, 1))
    }
    
    paste0(
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  
  genepairslist <- reactive({
    as.data.frame(as.table(cor(expr_m())))
  })
  
  observe({ print(genepairslist())})
  
  output$genepairs <- DT::renderDataTable({
    # DT::datatable(subset(genepairslist(), abs(Freq) > 0.5))
    DT::datatable(genepairslist())
    
  })

})
shinyApp(ui = ui, server = server)
