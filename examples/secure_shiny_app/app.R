require(shiny)
require(shinydashboard)
require(DT)
library(tidyverse)
require(solvebio)

client_id <- "your SolveBio app client ID"

server <- function(input, output, session) {
    makeSolveBioLink <- function(dataset_id) {
        # Create a link on SolveBio for a dataset.
        url <- tags$a("Open on SolveBio", href=paste0('https://my.solvebio.com/data/', dataset_id))
        return(as.character(url))
    }

    retrieveDatasets <- reactive({
        # Get a list of datasets in the current user's personal vault.
        env = session$userData$solvebio_env

        vault = Vault.get_personal_vault(env=env)
        datasets <- Vault.datasets(vault$id, env=env)
        shiny::validate(need(nrow(datasets)>0, "No datasets found."))
        return(datasets)
    })

    output$dataset_list = DT::renderDataTable({
        data <- retrieveDatasets()
        # Only show a few columns in the table.
        data <- data[,c(1, 11, 3, 13, 7, 5)]
        data <- data%>%
            mutate(solvebio_url=makeSolveBioLink(dataset_id))%>%
            select(id, path, filename, description, solvebio_url, dataset_documents_count)

        DT::datatable(data,
                      selection='single',
                      escape=FALSE,
                      filter='none',
                      options = list(scrollX = TRUE)
                      )
    })

    retrieveData <- reactive({
        env = session$userData$solvebio_env

        data <- retrieveDatasets()
        s = input$dataset_list_rows_selected
        shiny::validate(
                        need(!is.null(input$dataset_list_rows_selected),
                             "Select a dataset")
                        )
        dataset_id <- data[s,1]
        y <- Dataset.query(dataset_id, env=env)
    })

    output$dataset_preview = DT::renderDataTable({
        data <- retrieveData()
        DT::datatable(data,
                      selection='single',
                      escape=FALSE,
                      filter='top',
                      options = list(scrollX = TRUE)
                      )
    })
}

ui <- dashboardPage(
                    dashboardHeader(title="SolveBio"),
                    dashboardSidebar(disable=TRUE),
                    dashboardBody(
                                  fluidPage(
                                            fluidRow(
                                                     box(width = 12, title = "Datasets in your personal vault ",
                                                         DT::dataTableOutput('dataset_list')
                                                         )
                                                     ),
                                            fluidRow(
                                                     box(width = 12,title = "Preview of selected dataset ",
                                                         DT::dataTableOutput('dataset_preview')
                                                         )
                                                     )
                                            )

                                  )
                    )


# Wrap your base server and return a new secure server function
server <- solvebio::Application.shinyServer(client_id, server)

shinyApp(ui = ui, server = server)
