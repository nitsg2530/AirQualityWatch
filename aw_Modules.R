
varselect_mod_ui <- function(id) {
  ns <- NS(id)
  shinyjs::useShinyjs()
  
# assemble UI elements
  tagList(
    hr(),
     shinyjs::disabled(selectInput(ns("AggOpt"), "Aggregation options",
                choices = c( "Raw hourly data"="aw_RawAgg" ,
                             "Daily averages"="aw_DailyAvg" ,
                             "Daily maximum"="aw_DailyMax" ,
                             "Hours / day threshold is exceeded"="aw_tholdHrDay",
                             "Hours / year threshold is exceeded"="aw_tholdHrYr",
                             "Days / year daily ave concentration threshold is exceeded"="aw_tholdYrDayAv"),
                width = '95%'
    )),
    shinyjs::disabled(selectInput(ns("TmScale"), "Time scale options",
                c("Calendar time (Default)" = "calTime",
                  "Date within the year (Jan 1st-Dec 31)" = "DateYr",
                  "Day within the week (0-7days)" = "DayWk",
                "Hour in the day (0-24 Hrs)" ="HrDay"),
                width = '95%',
                selected="CalTime")),

    shinyjs::disabled(numericInput(
      ns("thVar"),
      'Threshold value',
      25, min=0,
      width = '50%')),
    shinyjs::disabled(textOutput(ns("HelpText1"))),
    h6("*Getty images used")
  )
}


varselect_mod_server <- function(input, output, session,data_set, pol_name){


  observeEvent(input$AggOpt,{
    if(input$AggOpt == "aw_tholdHrDay" || input$AggOpt == "aw_tholdHrYr" ||input$AggOpt ==  "aw_tholdYrDayAv"){

      shinyjs::enable("thVar")
      shinyjs::show("thVar")
      shinyjs::show("HelpText1")
    }
    else
    {
      shinyjs::hide("thVar")
      shinyjs::hide("HelpText1")
      #shinyjs::disabled("thVar")
    }
    shinyjs::enable("AggOpt")
    shinyjs::enable("TmScale")
    shinyjs::enable("HelpText1")
  })
  output$HelpText1 <- renderText({
    "This is user defined threshold value, this will change the polt and data results for number of pollutants. EU defined threshold limits will be shown over the plot as applicable"
  })
   return(
    list(
      xvar = reactive({
        if(input$TmScale == "DayWk"){
          xvar = "Week_days"
        }
        else if(input$TmScale == "DateYr"){
          xvar = "Days_in_year"
        }
        else if(input$TmScale == "HrDay"){
          xvar = "Hour"
        }

        else{
          xvar = "CalendarDates"
        }
        }),
      yvar = reactive({yvar = "Concentration"}),
      df = reactive({  data_set }),
      TmScale = reactive({input$TmScale}),
      thVar = reactive({renderText({ input$thVar })}),
      AggOpt = reactive({input$AggOpt}),
      polName = reactive({pol_name})

    )

  )

}


scatterplot_mod_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
      column(
        width = 12,
        h3("Main Plot"),
        # h3("Some text here..."),
        h3( textOutput(ns("caption"))),
        # helpText("Some help text .."),
        plotOutput(ns("plot1")),

      ),

    ),

    fluidRow(
      hr(),
      column(
        width = 6,
        shinyjs::disabled(checkboxGroupInput(ns("AW_CB_HeadDBOnly"), "Check if you want a compact Report (Only 10 records) ",
                           choiceNames = "Yes, only 10 records",
                           choiceValues = "HeadDBOnly",
                           selected = "HeadDBOnly",
                           inline = TRUE)),
      ),
      column(
        width = 6,
        shinyjs::disabled(downloadButton(ns("Report_download"), label = "Download Report")),
        shinyjs::disabled(downloadButton(ns("CSV_download"), label = "Download CSV data")),
      )
    ),
    fluidRow(
      column(
        width = 12,
        hr(),
        h3("Air Watch data"),
        DT::dataTableOutput(ns("mytable1")),
      )
    )
  )
}


scatterplot_mod_server <- function(input,
                                   output,
                                   session,
                                   dataset,
                                   plot1vars,
                                   selectedStationsDataset){

    df2 <- reactive({

      return(getAggregatedtDataset(plot1vars$df(),TmScale =plot1vars$TmScale(),thVar=plot1vars$thVar(),AggOpt =plot1vars$AggOpt()))
    })

    plot1_obj <- reactive({

        p <- scatter_airwatch( df2(),  TmScale =plot1vars$TmScale(),thVar=plot1vars$thVar(),AggOpt =plot1vars$AggOpt(),polName=plot1vars$polName() )
        shinyjs::enable("Report_download")
        shinyjs::enable("CSV_download")
        shinyjs::enable("AW_CB_HeadDBOnly")
      return(p)
    })


    output$caption <- renderText({
      getCaption(TmScale =plot1vars$TmScale(),AggOpt =plot1vars$AggOpt(),polName=plot1vars$polName())
    })


  output$mytable1 <- DT::renderDataTable({
    withProgress(message = 'Generating data table...', style = "notification", value = 0.1, {

      incProgress(0.5)
      if(plot1vars$AggOpt() == "aw_RawAgg" || plot1vars$AggOpt() == "aw_DailyAvg"|| plot1vars$AggOpt() == "aw_DailyMax" ){
        spreField = "Concentration"
        x_val = "CalendarDates"

        incProgress(0.5)
      }
      else if(plot1vars$AggOpt() == "aw_tholdHrDay" ){

        spreField = "Hours per day"
        x_val = "CalendarDates"
        incProgress(0.5)
      }
      else if(plot1vars$AggOpt() == "aw_tholdHrYr" ){

        incProgress(0.5)
        spreField = "Hours per year"
        x_val = "Year"

      }
      else if(plot1vars$AggOpt() == "aw_tholdYrDayAv" ){

        incProgress(0.5)
        spreField = "Days Per Year"
        x_val = "Year"

      }
      incProgress(0.5)
      data_wide <- df2() %>%
        mutate(row_id=1:n()) %>%
        ungroup() %>%
        select(row_id,x_val, AirQualityStationEoICode, spreField,Week_Days ,Days_in_year, Hour) %>%
        spread( AirQualityStationEoICode, spreField) %>%
        select(-row_id)

      plot1vars$TmScale()

      incProgress(0.5)
      if(plot1vars$TmScale()== "calTime")
      {
        removeCols = c("Week_Days","Days_in_year","Hour")
      }
      else if(plot1vars$TmScale()== "DateYr")
      {
        incProgress(0.5)
        if("Year" %in% colnames(data_wide))
        {
          removeCols = c("Week_Days","Year","Hour")
        }
        else if("CalendarDates" %in% colnames(data_wide))
        {
          removeCols = c("Week_Days","CalendarDates","Hour")
        }
      }
      else if(plot1vars$TmScale()== "DayWk")
      {
        incProgress(0.5)
        if("Year" %in% colnames(data_wide))
        {
          removeCols = c("Days_in_year","Year","Hour")
        }
        else if("CalendarDates" %in% colnames(data_wide))
        {
          removeCols = c("Days_in_year","CalendarDates","Hour")
        }

      }
      else if(plot1vars$TmScale()== "HrDay")
      {
        incProgress(0.5)

        if("Year" %in% colnames(data_wide))
        {
          removeCols = c("Week_Days","Days_in_year","Year")
        }
        else if("CalendarDates" %in% colnames(data_wide))
        {
          removeCols = c("Week_Days","Days_in_year","CalendarDates")
        }
      }
      data_wide <- data_wide %>%
        select(-removeCols)
      incProgress(0.5)
      DT::datatable(data_wide )
    })

  })



  output$plot1 <- renderPlot({
    plot1_obj()
  })

  output$CSV_download <- downloadHandler(

      filename = function(){
        paste("AirWatchData","csv",sep=".")
      } ,
      content = function(file) {
        data_wide <- dataset %>%
          mutate(row_id=1:n()) %>%
          ungroup() %>%
          select(row_id,CalendarDates, AirQualityStationEoICode, Concentration) %>%
          spread( AirQualityStationEoICode, Concentration) %>%
          select(-row_id)
       write.csv(data_wide,file)

      }

  )

    output$Report_download <- downloadHandler(
       filename = function(){
        paste("AirWatchReport","docx",sep=".")
      } ,
      content = function(file) {
        tempReport <- file.path("aw_Report.Rmd")

        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        data_wide <- df2() %>%
          mutate(row_id=1:n()) %>%
          ungroup() %>%
          select(row_id,CalendarDates, AirQualityStationEoICode, Concentration) %>%
          spread( AirQualityStationEoICode, Concentration) %>%
          select(-row_id)
        if(input$AW_CB_HeadDBOnly == "HeadDBOnly"){
          data_wide <- head(data_wide, n=10)
        }
        mapDataset <- selectedStationsDataset %>%
          select(EoICode,StationName,Latitude,Longitude,StationArea,StationType)
        params <- list(plotData =  df2(),tableData = mapDataset,tableData2 = data_wide, plotCode = "plot(1)", plotObj = plot1_obj())#, plotObj2 =selectedStationsDataset)

        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )

      }
    )



}


mapplot_mod_ui <- function(id) {
  ns <- NS(id)

  tagList(
    fluidRow(
     

      column(
        width = 4,
        h3("Selected stations..."),
        panel(
          plotOutput(ns("mapplot1"),height = "185px"),
        )


      ),
      column(
        width = 8,

        panel(
        DT::dataTableOutput(ns("stationtable1")),)
      ),

    )
  )
}


mapplot_mod_server <- function(input,
                                   output,
                                   session,
                                   dataset){

  mapplot1_obj <- reactive({
    p <- map_airwatch(dataset)

    return(p)
  })


  output$stationtable1 <- DT::renderDataTable({
    g_stations<-dataset
    dataset1 <- dataset %>%
      select(StationName,EoICode,StationType,StationArea)
    DT::datatable(dataset1,rownames = FALSE,class = "display",options = list(dom = 't',paging = FALSE,searching = FALSE,ordering=FALSE,selection =FALSE))
  })
  output$mapplot1 <- renderPlot({
    mapplot1_obj()
  })

}
