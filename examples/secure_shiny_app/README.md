# Example: Secure Shiny App

In this example we create a simple Shiny app wrapped by SolveBio's "protected server".
This requires users to authorize the app (with their SolveBio account) before interacting with it.


## Install Dependencies

To install the local dependencies for this app, run the `init.R` script:


    Rscript init.R


If you are a developer working on solvebio-r, make sure to override your local installation by manually installing it from your local repo. To do this, run `R CMD INSTALL .` from the root of the repo.


## Run Locally

Obtain a SolveBio client ID and create an `.Renviron` file in the app's directory with the following:

    CLIENT_ID=your-client-id


Run the Shiny app from your command-line:

    R -e "shiny::runApp(port=3838)"

Open [http://127.0.0.1:3838](http://127.0.0.1:3838) in your browser.


## Deploy to shinyapps.io

First, [create a shinyapps account](https://www.shinyapps.io/admin/#/signup). Follow the instructions to install `rsconnect` and log in with your credentials.

Make sure you create your SolveBio app and set up your `.Renviron` file (see the section above) before deploying.

To deploy, open R in the app's directory and run:

    library(rsconnect)
    deployApp()


This may take a few minutes, and should automatically open up your browser to the app URL.
