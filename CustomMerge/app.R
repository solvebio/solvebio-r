library(solvebio)
library(data.table)

library(shiny)

ui <- shinyUI(fluidPage(
  
  titlePanel("My R Shiny App"),
      mainPanel(
        tabsetPanel(
          tabPanel("Analysis", plotOutput("plot")),
          tabPanel("Merged Table", DT::dataTableOutput('table1'))
        )
      ),
  sidebarPanel(
 
    fileInput(
      'file',
      'Choose file to upload.'
    ),

    selectInput("datasetInput1", "Dataset 1",
          choices = c("GWAS/GWAS", "ClinVar/3.7.4-2017-01-04/Combined", "ClinVar/Variants"),
            selected = "GWAS/GWAS"),
    selectInput("datasetInput2", "Dataset 2",
                choices = c("GWAS/GWAS", "ClinVar/3.7.4-2017-01-04/Combined", 
                            "ClinVar/Variants"), selected = "GWAS/GWAS"),
    uiOutput("choose_columns1"),
    uiOutput("choose_columns2")
)))

server <- shinyServer(function(input, output, session) {
  
  datasetName1 <- reactive({
      input$datasetInput1
  })
 
  datasetName2 <- reactive({
    input$datasetInput2
  })  
   
  cols1 <- reactive({
    colnames(Dataset.query(datasetName1()))
  })

  cols2 <- reactive({
    colnames(Dataset.query(datasetName2()))
  })
  
  observe({ print(datasetName1())})
  observe({ print(datasetName2())})
  
  observe({ print(cols1())})
  observe({ print(cols2)})
 
  output$choose_columns1 <- renderUI({

    colnames1 <- cols1()
    
    selectInput("columns1", "Choose columns", 
                choices  = cols1(),
                multiple = TRUE)
  })
  
  output$choose_columns2 <- renderUI({

    colnames2 <- cols2()
    
    selectInput("columns2", "Choose columns", 
                choices  = cols2(),
                multiple = TRUE)
  })
  
  inFile <- reactive({
    if (is.null(input$file)) {
      return(NULL)
    } else {
      input$file
    }
  })
  
  genesf <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      read.table(inFile()$datapath, header = FALSE)
    }
  })
  
  filter_to_q <- reactive({
    list(list('gene_symbol__in', paste(genesf()[,1], sep = ",")))
  })
  
  observe({ print(list(list('gene_symbol__in', paste(genesf()[,1], sep = ",")))) })
  
  query1 <- reactive({
    if (is.null(inFile())) {
      return(NULL)}
    Dataset.query(datasetName1(),filters=filter_to_q(), 
                  fields = input$columns1
                  )
  })
  
  query2 <- reactive({
    if (is.null(inFile())) {
      return(NULL)}
    Dataset.query(datasetName2(),filters=filter_to_q(), 
                  fields = input$columns2
                  )
  })
  
  observe({ print(colnames(query1()))})
  
  merged <- reactive({
    if (is.null(inFile())) {
      return(NULL)}
    merge(query1(), query2(), by='variant_sbid')
  })

  output$table1 <- DT::renderDataTable({
    DT::datatable(merged())
  })
})  
# # Run the application 
shinyApp(ui = ui, server = server)