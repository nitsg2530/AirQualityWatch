
aw_MergeModulesUI <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 3,

        panel(

          shinyjs::disabled(checkboxGroupInput(ns("AW_CB_Stations"), "No station is available for this pollutant, load stations data first from Step 1",
                             #choiceNames =  list("Fine particulates (PM2.5)", "Particulates (PM10)",  "Sulphur dioxide (SO2)", "Nitrogen dioxide (NO2)"),
                             choiceNames = "...you will see available stations here",
                             choiceValues = "",
                             inline = TRUE)),
          actionButton(ns("AB_load2"), label = "Step2- Load pollutant data ", width = "90%"),
          helpText("Please load pollutant data for available and selected stations"),

          varselect_mod_ui(ns("plot1_vars")),
        #  tags$img(src=n"")

        ),


      ),
      column(
        width = 9,

        panel(

          scatterplot_mod_ui(ns("plots"))
        ),
      )


    )
  )
}

aw_MergeModules <- function(input, output, session,  stn_name, pol_name,selectedStationsDataset){ 

  My_Printlog("Module Server ....")
  My_Printlog(pol_name) 
  My_Printlog(stn_name)
   observe({

    updateCheckboxGroupInput(session, "AW_CB_Stations",
                             label = paste("Available station(s) for analysis" ),
                             choices = stn_name,
                             selected = stn_name, inline = TRUE
    )
  })

  observeEvent(input$AB_load2, {

    stn_name2 = input$AW_CB_Stations


    data_set <-getDataSetForPollutants(stn_name2,pol_name, longName= 3)
    if(!is.null(data_set)){
      data_set <- data_set %>% drop_na()
      data_set$CalendarDates <- decimal_date(ymd(paste(data_set$Year, data_set$Month, data_set$Day, sep="-")))
      data_set$Week_Days <- wday(ymd(paste(data_set$Year, data_set$Month, data_set$Day, sep="-")))
      data_set$Days_in_year <- yday(ymd(paste(data_set$Year, data_set$Month, data_set$Day, sep="-")))
      str(data_set)

      plot1vars <- callModule(varselect_mod_server, "plot1_vars",data_set, pol_name)
      res <- callModule(scatterplot_mod_server,
                        "plots",
                        dataset = data_set,
                        plot1vars = plot1vars,
                        selectedStationsDataset)
    }

  })

}



