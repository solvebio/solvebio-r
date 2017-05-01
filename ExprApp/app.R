library(solvebio)
library(data.table)
library('corrplot')
library(shiny)
library(plotly)
library(shinythemes)

pt_ann = Dataset.query("solvebio:NanoString/1.0.0/SampleAnnot", paginate = TRUE)

make0 <-function (m, c){
  m[which(abs(m)<c)]=0
  return(m)
}

set_row_names <- function(m, v) {
  row.names(m) <- v
  return(m)
}

ui <- shinyUI(fluidPage(

  titlePanel("Gene Expression Analysis"),
  mainPanel(
    tabsetPanel(
      tabPanel("Select corr",
              sliderInput("cutoff", "Correlation cutoff", min = 0, max = 1, value = 0.95),
              plotlyOutput("customplot",  height=600),
              uiOutput("corrplot")),
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
      tabPanel("patient correlations", DT::dataTableOutput('ptpairs')),
      tabPanel("gene correlations", DT::dataTableOutput('genepairs'),
               sliderInput("cutoff", "Correlation cutoff", min = 0, max = 1, value = 0.95))
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

#   # observe({ print(cols())})

  expr_dt <- reactive({
    if (is.null(datasetName())) {
      return(NULL)}
    as.data.table(Dataset.query(datasetName(), limit = 30)) # paginate=TRUE))
  })

#   # observe({ print(dim(expr_dt()))})
#   # observe({ print(colnames(expr_dt()))})

  genes <- reactive({
    if (is.null(expr_dt())) {
      return(NULL)}
    expr_dt()[,gene]
  })

#   # observe({ print(length(genes()))})
#   # observe({ print(genes())})

  expr_m_unnamed <- reactive({
    if (is.null(expr_dt())) {
      return(NULL)}
    expr_dt()[,'_id':=NULL]
    expr_dt()[,'_commit':=NULL]
    expr_dt()[,gene:=NULL]
    as.matrix(expr_dt())
  })

  expr_m <- reactive({set_row_names(expr_m_unnamed(), genes())})
  
 # observe({ print(expr_m())})

 expr_mT <- reactive({ t(expr_m())})
 observe({ print(expr_mT())})

 cor_mT <- reactive({ cor(expr_mT()) })

 # observe({ print(cor_mT())})

 cor_mT_nodiag <- reactive({ cor_mT() - diag(dim(cor_mT())[1]) })

 # observe({ print(cor_mT_nodiag())})

 maxcor_mT_nodiag <- reactive({ apply(cor_mT_nodiag(), 1, max) }) 

 # observe({ print(maxcor_mT_nodiag())})

 maxcor_mT_nodiag_ind <- reactive({ which(abs(maxcor_mT_nodiag())>0.5) })

 # observe({ print(maxcor_mT_nodiag_ind())})

 cor_mT_05 <- reactive({ cor_mT()[maxcor_mT_nodiag_ind(), maxcor_mT_nodiag_ind()] })

 # observe({ print(cor_mT_05())})

 cor_mT_05_signif <- reactive({ cor_mT_05() - diag(dim(cor_mT_05())[1]) })

# observe({ corrplot(make0(cor_mT_05_signif(), input$cutoff)) })

output$customplot <- renderPlotly({
  plot_ly(z = make0(cor_mT_05_signif(), input$cutoff), type = "heatmap", x = colnames(cor_mT_05_signif()), y = colnames(cor_mT_05_signif()))
})

  # row_labels <- reactive({genes()})

  # observe({ print(row_labels())})
  # observe({ print(colnames(expr_m()))})

  output$plot <- renderPlot({
    if (is.null(expr_dt())) {
      return(NULL)}
    input$plot
    heatmap(expr_m(), labRow = rownames(expr_m())) # row_labels())
  })

  output$heat <- renderPlotly({
    plot_ly(z = expr_m(), type = "heatmap", x = colnames(expr_m()), y = rownames(expr_m())) # y = row_labels())
  })

  output$correlationply <- renderPlotly({
    plot_ly(z = cor(expr_m()), type = "heatmap", x = colnames(expr_m()), y = colnames(expr_m()))
  })

  output$correlationTR <- renderPlotly({
    plot_ly(z = cor_mT(), type = "heatmap", x = colnames(cor_mT()), y = colnames(cor_mT()))
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

  ptpairslist <- reactive({
    as.data.frame(as.table(cor(expr_m())))
  })

  genepairslist <- reactive({
    as.data.frame(as.table(cor_mT()))
  })
  # observe({ print(ptpairslist())})
  # observe({ print(genepairslist())})

  output$ptpairs <- DT::renderDataTable({
    DT::datatable(ptpairslist())
  })

  output$genepairs <- DT::renderDataTable({
    DT::datatable(genepairslist())
  })

 })
shinyApp(ui = ui, server = server)
