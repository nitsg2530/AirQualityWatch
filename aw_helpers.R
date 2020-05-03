plot_labeller <- function(l, varname) {
  if (varname == "Concentration") {
    res <- dollar(l)
  } else {
    res <- comma(l)
  }
  return(res)
}

getX_var<- function(TmScale,AggOpt){
  X_Var = "CalendarDates"
  if(TmScale == "calTime"){
    X_Var = "CalendarDates"
    if(AggOpt == "aw_tholdHrYr"){
      X_Var = "Year"
    }
    if(AggOpt == "aw_tholdYrDayAv"){
      X_Var = "Year"
    }
  }
  else if(TmScale == "DateYr"){
    X_Var = "Days_in_year"
  }
  else if(TmScale == "DayWk"){
    X_Var = "Week_Days"
  }
  else if(TmScale == "HrDay"){
    X_Var = "Hour"
  }

  return(X_Var)
}

getY_var<- function(AggOpt){
  Y_Var = "Concentration"
  if(AggOpt == "aw_RawAgg"){
    Y_Var = "Concentration"
  }
  else if(AggOpt == "aw_DailyAvg"){
    Y_Var = "Concentration"
  }
  else if(AggOpt == "aw_DailyMax"){
    Y_Var = "Concentration"
  }
  else if(AggOpt == "aw_tholdHrDay"){
    Y_Var = "Hours per day"
  }
  else if(AggOpt == "aw_tholdHrYr"){
    Y_Var = "Hours per year"
  }
  else if(AggOpt == "aw_tholdYrDayAv"){
    Y_Var ="Days Per Year"
  }
  return(Y_Var)
}
getAggregatedtDataset<- function(data_set,TmScale,thVar,AggOpt){

  withProgress(message = 'Generating Data...', style = "notification", value = 0.1, {
    if(AggOpt == "aw_RawAgg"){
      incProgress(0.5)
      df <- data_set
    }
    else if(AggOpt == "aw_DailyAvg"){
      incProgress(0.5)
      df <- data_set %>%
        group_by(CalendarDates,AirQualityStationEoICode) %>%
        summarise(Concentration = mean(Concentration, na.rm=TRUE),Days_in_year =mean(Days_in_year),Week_Days =mean(Week_Days),Hour =mean(Hour))
    }
    else if(AggOpt == "aw_DailyMax"){
      incProgress(0.5)
      df <-data_set %>%
         group_by(CalendarDates,AirQualityStationEoICode) %>%
         summarise(Concentration = max(Concentration, na.rm=TRUE),Days_in_year =max(Days_in_year),Week_Days =max(Week_Days),Hour =max(Hour) )
      #for symmetry I am using max for Days_in_year etc mean also return the same results
    }
    else if(AggOpt == "aw_tholdHrDay"){
      incProgress(0.5)
      df <-data_set %>%
       group_by(CalendarDates, AirQualityStationEoICode) %>%
       filter(Concentration > as.numeric(thVar())) %>%
       summarise ("Hours per day" = n() ,Days_in_year =max(Days_in_year),Week_Days =max(Week_Days),Hour =max(Hour)  )

    }
    else if(AggOpt == "aw_tholdHrYr"){
      incProgress(0.5)
      #Year / ANS
      df <-data_set %>%
        group_by(Year,AirQualityStationEoICode) %>%
        filter(Concentration > as.numeric(thVar())) %>%
        summarise ("Hours per year" = n() ,Days_in_year =max(Days_in_year),Week_Days =max(Week_Days),Hour =max(Hour) )

    }
    else if(AggOpt == "aw_tholdYrDayAv"){
      incProgress(0.5)

      data_set2 <- data_set
      df <- data_set %>%
        group_by(CalendarDates,AirQualityStationEoICode) %>%
        summarize(Mean = mean(Concentration, na.rm=TRUE))

      incProgress(0.5)
      new_dataset <- df %>%
         inner_join(data_set2, by=c("CalendarDates","AirQualityStationEoICode"))

      incProgress(0.5)

      new_dataset <- new_dataset %>%
         select(Year,CalendarDates,AirQualityStationEoICode,Mean,Week_Days,Days_in_year,Hour)

      incProgress(0.5)
        Gpby_dataset <- new_dataset %>%
         group_by(Year,AirQualityStationEoICode) %>%
         filter(Mean > as.numeric(thVar())) %>%
         summarise ("Days Per Year" = n(),Days_in_year =max(Days_in_year),Week_Days =max(Week_Days),Hour =max(Hour))

        incProgress(0.5)
       df <- Gpby_dataset %>% drop_na()

    }

    incProgress(0.5)
  })
  return (df)
}
getCaption <- function(TmScale,AggOpt,polName){

  if(AggOpt == "aw_RawAgg"){
    ystr ="Hourly concentration"
  }
  else if(AggOpt =="aw_DailyAvg"){
    ystr ="Daily averages concentration"
  }
  else if(AggOpt =="aw_DailyMax"){
    ystr ="Daily maximum concentration"
  }
  else if(AggOpt =="aw_tholdHrDay"){
    ystr ="Number of hours per day for which the threshold is exceeded"
  }
  else if(AggOpt =="aw_tholdHrYr"){
    ystr ="Number of hours per year for which the threshold is exceeded"
  }
  else if(AggOpt =="aw_tholdYrDayAv"){
    ystr ="Number of days per year for which the daily average concentration exceeds the threshold"
  }

  if(TmScale =="calTime"){
    xstr ="Calendar dates"
  }
  else if(TmScale == "DateYr"){
    xstr ="date within the year (Jan 1st - Dec 31)"
  }
  else if(TmScale == "DayWk"){
    xstr ="day within the week (Mon - Sunday)"
  }
  else if(TmScale == "HrDay"){
    xstr ="hour in the day (0 - 24 Hrs)"
  }
  retStr = paste(ystr,"of",polName,"for",xstr,sep = " ")
}

getThresholdValueforHozLine <- function(TmScale,AggOpt,polName){

  if(polName =="PM2.5"){

  }
  else if(polName =="PM10"){

    if(AggOpt == "aw_DailyAvg"){
      return(50)

    }
    else if(AggOpt =="aw_tholdYrDayAv"){
      return(35)
    }

  }
  else if(polName =="SO2"){
    if(AggOpt =="aw_RawAgg"){
      return(350)
    }
    else if(AggOpt =="aw_DailyAvg"){
      return(125)
    }
    else if(AggOpt =="aw_tholdHrYr")
    {
      return(24)
    }
    else if(AggOpt =="aw_tholdYrDayAv")
    {
      return(3)
    }

  }
  else if(polName =="NO2"){
    if(AggOpt =="aw_RawAgg"){
      return(200)
    }

  }

  return (999)


}


scatter_airwatch <- function(dataset, TmScale,thVar,AggOpt,polName) {
  withProgress(message = 'Generating plot...', style = "notification", value = 0.1, {
    yvar2 <- getY_var(AggOpt)
    xvar2 <- getX_var(TmScale,AggOpt)
    x <- rlang::sym(xvar2)
    y <- rlang::sym(yvar2)
    t <- rlang::sym(TmScale)
    a <- rlang::sym(AggOpt)
    pol <- rlang::sym(polName)
    incProgress(0.5)
    eu_thold <- getThresholdValueforHozLine(t,a,pol)
    incProgress(0.5)
    p <- ggplot(dataset, aes(x = !!x, y = !!y, color =AirQualityStationEoICode  ) )+

      incProgress(0.5)
#    scale_x_continuous(labels = function(l) plot_labeller(l, varname = xvar)) +
 #   scale_y_continuous(labels = function(l) plot_labeller(l, varname = yvar)) +
    theme(axis.title = element_text(size = rel(1.2)),
          axis.text = element_text(size = rel(1.1)))
  if(TmScale == "calTime")
    p <- p + geom_line(alpha = 1/3)
  else
    p <- p + geom_jitter(alpha = 1/2)
  if(eu_thold !=999)
     p <- p + geom_hline(yintercept=eu_thold, linetype="dashed")
    incProgress(0.5)

  })
  return(p)
}



map_airwatch <- function(dataset) {

  p <- ggplot() + geom_polygon(data = cz, aes(x=long, y = lat,group = group,), fill = "gray", color = "black") +
    geom_point(data =dataset,aes(x = dataset$Longitude , y = dataset$Latitude ), color = "Red", size = 3) +
    geom_text(aes(x = dataset$Longitude +0.1, y = dataset$Latitude +0.0001,label = dataset$StationName, hjust = 0), color = "red", size = 5)+

    theme(legend.position = "none")+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())+
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())+
    theme(plot.margin = unit(c(0, 0, 0, 0), "pt"))+
    coord_fixed(1.3)

  return(p)
}
