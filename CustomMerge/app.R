library(solvebio)
library(data.table)
# genes = c("MYBPC3", "RDH5", "MYO15A", "FZD4", "DMD", "GLIS3", "ISPD", "C3", "APOB", "ATP6V0A2", "KIF5A", "SNCA", "LDLR", "ANK1", "FBN2")
genes=c("BRCA2", "BRCA1")
man_genes_filter = list(list('gene_symbol__in', paste(genes)))
# clinvar_genes_q = Dataset.query('ClinVar/3.7.4-2017-01-04/Combined', filters=man_genes_filter, fields=c("variant_sbid", "phenotype", "clinical_significance", "gene_symbol"))
# clinvar_genes_dt = as.data.table(clinvar_genes_q)

library(shiny)

ui <- shinyUI(fluidPage(
  
  titlePanel("My R Shiny App"),
      mainPanel(
        # tableOutput("table1")
        DT::dataTableOutput('table1')
        # uiOutput("results")
      ),
  sidebarPanel(
    
    fileInput(
      'file',
      'Choose file to upload.'
    ),

    selectInput("datasetInput1", "Dataset 1",
          choices = c("GWAS/GWAS", "ClinVar/3.7.4-2017-01-04/Combined", "ClinVar/Variants")),
    selectInput("datasetInput2", "Dataset 2",
                choices = c("GWAS/GWAS", "ClinVar/3.7.4-2017-01-04/Combined", "ClinVar/Variants"))
    
        
    # selectInput(
    #   "y_input", 
    #   label = h5("Select Time Series/Response Variable"),
    #   ""
    # ),
    
    # textInput(
    #   "genes_filter",
    #   label = "genes filter"
    # )
    
)))

server <- shinyServer(function(input, output, session) {
  
  datasetName1 <- reactive({
      input$datasetInput1
  })
 
  datasetName2 <- reactive({
    input$datasetInput2
  })  
   
  # observe({
  #     updateSelectInput(
  #       session,
  #       "datasetInput")
  # 
  # })
  
  observe({ print(datasetName1())})
  observe({ print(datasetName2())})
  
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
    Dataset.query(datasetName1(),filters=filter_to_q(), fields=c("variant_sbid", "phenotype", "clinical_significance", "gene_symbol"))
  })
  
  query2 <- reactive({
    if (is.null(inFile())) {
      return(NULL)}
    Dataset.query(datasetName2(),filters=filter_to_q(), fields=c("variant_sbid", "phenotype", "clinical_significance", "gene_symbol"))
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
  # output$table1 <- reactive({renderDataTable(query1)})
  # output$table1 <- DT::renderDataTable(query1)
  # observe({ print(head(query1()))})
  # observe({ print(head(query2()))})

  
  # observe({
  #   print(paste(genes_filter()[,1]))
  # })
  
  # observe({
  #   updateTextInput(
  #     session,
  #     "genes_filter",
  #     myData()[,1]
  #   )
  # })
  
  # observe({
  #   updateSelectInput(
  #     session,
  #     "y_input",
  #     choices=names(myData()))
  #   
  # })
  
})


# 
# ui <- shinyUI(fluidPage(
#   titlePanel("test demo"),
#   sidebarLayout(
#     mainPanel(
#       # tableOutput("results")
#       uiOutput("results")
#     ),
#     sidebarPanel(
#       uiOutput("dataset1"),
#       uiOutput("dataset2")
#       # selectInput("datasetInput", "Dataset 1",
#       #             choices = c("GWAS", "ClinVar", "CIViC")),
#       # selectInput("datasetInput", "Dataset 2",
#       #             choices = c("GWAS", "ClinVar", "CIViC")),
#       # selectInput("var", 
#       #             label = "Choose a variable to display",
#       #             choices = colnames(clinvar_genes_dt),
#       #             selected = "gene_symbol"),
#       # textInput("genesfilter", "Genes filter")
#   )
#   ),
#   fluidRow(
#     column(12, 
#            dataTableOutput('table1')
#     )
#   )
#   
#   ))
# 
# 
# # Define server logic required to draw a histogram
# server <- function(input, output)  {
#   # genes_filter <- reactive({
#   #   # input$genesfilter
#   #   list(list('gene_symbol__in', paste(input$genesfilter)))
#   # })
#   # 
#   # observe({ print(genes_filter()) }) 
#   # 
#   # output$text1 <- renderText({ 
#   #   paste("You have selected", input$var)
#   # })
#   
#   output$dataset1 <- renderUI({
#     selectInput("dataset1", "Dataset 1",
#                 choices = c("GWAS/GWAS", "ClinVar/3.7.4-2017-01-04/Combined", "CIViC/CIViC"),
#                 selected = "GWAS/GWAS")
#   })
#   
#   # data <- reactive({
#   #   Dataset.query(input$dataset1)
#   # })
#   # 
#   dataset <- reactive({
#     get(load(Dataset.query(input$dataset1)))
#   })
#   
#   
#   observe({print(head(dataset()))})
#   output$dataset2 <- renderUI({
#     selectInput("dataset2", "Dataset 2",
#                 choices = c("GWAS/GWAS", "ClinVar/3.7.4-2017-01-04/Combined", "CIViC"),
#                 selected = "GWAS/GWAS")
#   })
#   
#   observe({ print(input$dataset1)})
#   observe({ print(input$dataset2)})
#   
#   # query1 <- reactive({
#   #   paste(input$dataset1, ',filters=',genes_filter(), 
#   #         ',fields=c("variant_sbid", "phenotype", "clinical_significance", "gene_symbol")')
#   # })
#   # 
#   # observe({ print(query1()) })
#   # 
#   # query2 <- reactive({
#   #   paste(input$dataset2, ',filters=',genes_filter(), 
#   #         ',fields=c("variant_sbid", "phenotype", "clinical_significance", "gene_symbol")')
#   # })
#   # 
#   # observe({ print(query2()) })
#   
#   
#   #output$table <- renderDataTable(as.data.table(Dataset.query(query)))
# #   output$results <- renderUI({
# #   # renderTable(Dataset.query(query))
# #     renderDataTable(data)
# #   })
#  }  
#   #output$table <- renderDataTable(as.data.table(Dataset.query
#   # ('ClinVar/3.7.4-2017-01-04/Combined', filters=genes_filter(), 
#   # fields=c("variant_sbid", "phenotype", "clinical_significance", "gene_symbol")))
#   # )
#   
# 
# # Run the application 
shinyApp(ui = ui, server = server)