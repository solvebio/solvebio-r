library(shiny)
library(shinydashboard)
library(DT)
library(tidyverse)
library(solvebio)

CLIENT_ID <- Sys.getenv('CLIENT_ID', unset='your SolveBio app client ID')
# Client secret is optional
CLIENT_SECRET <- Sys.getenv('CLIENT_SECRET')


server <- function(input, output, session) {
    retrieveDatasets <- reactive({
        # Get a list of datasets in the current user's personal vault.
        env = session$userData$solvebio_env

        vault = Vault.get_personal_vault(env=env)
        datasets <- Vault.datasets(vault$id, limit=1000, env=env)
        shiny::validate(need(nrow(datasets)>0, "No datasets found."))
        return(datasets)
    })

    output$dataset_list = DT::renderDataTable({
        data <- retrieveDatasets()
        # Only show a few columns in the table.
        data <- data[,c("id", "path", "filename", "description", "dataset_documents_count", "created_at", "updated_at")]
        # Add a link to SolveBio web
        data <- data %>%
            mutate(url = paste0('<a href="https://my.solvebio.com/data/', id, '" target="_blank">Open on SolveBio</a>')) %>%
            select(id, path, filename, description, url, dataset_documents_count, created_at, updated_at)

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
        dataset_id <- data[s,c("id")]
        Dataset.query(dataset_id, limit=100, env=env)
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
                                                     box(width = 12,title = "Preview of selected dataset (first 100 records) ",
                                                         DT::dataTableOutput('dataset_preview')
                                                         )
                                                     )
                                            )

                                  )
                    )


# Wrap your base server and return a new protected server function
protected_server <- solvebio::protectedServer(server, client_id=CLIENT_ID, client_secret=CLIENT_SECRET)

shinyApp(ui = ui, server = protected_server)
