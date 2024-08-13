library(shiny)
library(shinyjs)
library(shinydashboard)
library(DT)
library(tidyverse)
library(solvebio)

# NOTE: If SOLVEBIO_ACCESS_TOKEN exists this will override OAuth2 configuration
# Example Shiny app client ID
CLIENT_ID <- 'YOUR CLIENT ID'
# CLIENT_ID <- Sys.getenv('CLIENT_ID')
CLIENT_SECRET <- Sys.getenv('CLIENT_SECRET')  # OPTIONAL
BASE_URL <- "https://<DOMAIN>.edp.aws.quartz.bio"

server <- function(input, output, session) {
    retrieveDatasets <- reactive({
        # Get a list of datasets in the current user's personal vault.
        env = session$userData$solvebio_env

        # Get list of all datasets you can access
        # vault = Vault.get_personal_vault(env=env)
        # datasets <- Vault.datasets(vault$id, limit=1000, env=env)

        # Find all accessible datasets
        datasets <- GlobalSearch.search(limit=1000, filters = '[["type","dataset"]]', vault_scope="access", paginate=TRUE, env=env)

        shiny::validate(need(nrow(datasets)>0, "No datasets found."))
        return(datasets)
    })

    output$dataset_list = DT::renderDataTable({
        data <- retrieveDatasets()
        # Only show a few columns in the table.
        data <- data[,c("id", "vault", "path", "name", "created_at", "updated_at")]
        # Add a link to SolveBio web
        data <- data %>%
            mutate(url = paste0('<a href="', BASE_URL, '/data/', id, '" target="_blank">Open in EDP</a>')) %>%
            select(id, path, name, url, created_at, updated_at)

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
                    dashboardHeader(title="QuartzBio EDP Shiny Example"),
                    dashboardSidebar(disable=TRUE),
                    dashboardBody(
                                  # Optional code for token cookie support
                                  shiny::tags$head(
                                                   shiny::tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/js-cookie/2.2.0/js.cookie.js")
                                                   ),
                                  useShinyjs(),
                                  extendShinyjs(text = solvebio::protectedServerJS(),
                                                functions = c("enableCookieAuth", "getCookie", "setCookie", "rmCookie")),
                                  fluidPage(
                                            fluidRow(
                                                     box(width = 12, title = "Accessible datasets ",
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
protected_server <- solvebio::protectedServer(server, client_id=CLIENT_ID, client_secret=CLIENT_SECRET, base_url=BASE_URL)

options(shiny.port = 3838)
shinyApp(ui = ui, server = protected_server)
