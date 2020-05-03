
library(shiny)

library(scales)
library(dplyr)
library(DT)
library(tidyverse)
library(lubridate)
library(shinyjs)
library(shinyWidgets)
library(rio)
library(rsconnect)

source("aw_util.R")
source("aw_MergeModules.R")
source("aw_Modules.R")
source("aw_helpers.R")


appCSS <- "
#loading-content {
  position: absolute;
  background: #000000;
  opacity: 0.9;
  z-index: 100;
  left: 0;
  right: 0;
  height: 100%;
  text-align: center;
  color: #FFFFFF;
}
"
ui <- fluidPage(
    useShinyjs(),
    inlineCSS(appCSS),

    # Loading message
    div(
      id = "loading-content",
      h2("Loading Air Quality Watch...")
    ),

    fluidRow(
        column(3,

             panel(

               column(12,
                    
               titlePanel(title=div(img(src="images//banner_airquality.jpg",width = "100%")),windowTitle = "Air Quality Watch"),


               selectizeInput("SI_var", 'Select stations you are interested in (Max 4)', choices = seedDataset2$EoICode,
                              multiple = TRUE, options = list(maxItems = 4), width = "90%"),

               shinyjs::disabled(  actionButton("AB_load", label = "Step1- Load stations data", width = "90%")),
                  helpText("Select stations and load into the system, syetem will find out the available pollutants recorded by these stations.")
               ),
             )

        ),

        column(
          width = 9,

          panel(

            mapplot_mod_ui("mapplot1")
          ),
        )
    ),

      tabsetPanel(id = "pollutant",
               # tabPanel('All', aw_MergeModulesUI("id5")),
                tabPanel('PM2.5', aw_MergeModulesUI(id = "id1")),
                tabPanel('PM10', aw_MergeModulesUI("id2")),
                tabPanel('SO2', aw_MergeModulesUI("id3")),
                tabPanel('NO2', aw_MergeModulesUI("id4"))


       ),


)

server <- function(input, output,session) {


  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")
  show("app-content")


    observe({
      req(input$SI_var)
      stations <- input$SI_var
      if (length(stations) >0) {
        shinyjs::enable("AB_load")
      } else {
        shinyjs::disable("AB_load")
      }
    })
    observeEvent(input$AB_load, {
        req(input$SI_var)

        stations <- input$SI_var
        polList <- c('PM2.5','PM10','SO2','NO2')
        Ids <- c("id1","id2","id3","id4")
        My_Printlog("OBSERVE EVENT ......")

        selectedStationsDataset <- getDatasetofSelectedStation(input$SI_var)
        # Strange behavir - Shiny is not accepting loop .. results are getting impacted
       # for(i in 1:length(polList))
        #{


          mod_filename <-getFileList(stations,polList[1],longName= 2)
          data_module1 <- callModule( module = aw_MergeModules,
                                      id = Ids[1],
                                      stn_name = mod_filename,
                                      pol_name = polList[1],
                                      getDatasetofSelectedStation(input$SI_var))

          mod_filename <-getFileList(stations,polList[2],longName= 2)

          data_module1 <- callModule( module = aw_MergeModules,
                                      id = Ids[2],
                                      stn_name = mod_filename,
                                      pol_name = polList[2],
                                      getDatasetofSelectedStation(input$SI_var))
          mod_filename <-getFileList(stations,polList[3],longName= 2)

          data_module1 <- callModule( module = aw_MergeModules,
                                      id = Ids[3],
                                      stn_name = mod_filename,
                                      pol_name = polList[3],
                                      getDatasetofSelectedStation(input$SI_var))
          mod_filename <-getFileList(stations,polList[4],longName= 2)

          data_module1 <- callModule( module = aw_MergeModules,
                                      id = Ids[4],
                                      stn_name = mod_filename,
                                      pol_name = polList[4],
                                      getDatasetofSelectedStation(input$SI_var))
        #}

      })


      observeEvent(input$SI_var, {

      req(input$SI_var)
      stations <- input$SI_var

      # execute scatterplot module
      res <- callModule(mapplot_mod_server,
                        "mapplot1",
                        dataset = getDatasetofSelectedStation(input$SI_var))
      })

}

# Run the application
shinyApp(ui = ui, server = server)
