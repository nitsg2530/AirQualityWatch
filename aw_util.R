
##___________RUN MODE______________________
#FileDirectory <- "RData"
FileDirectory <-"CorCSV"
FileType <-".csv"
#FileType <- ".RData"
##__________________________________________

#print(paste(paste(FileDirectory, "__Stations",sep="/"),FileType,sep=""))
seedDataset2 <- import(paste(paste(FileDirectory, "__Stations",sep="/"),FileType,sep=""))


#cz <- map_data("world", "Czech Republic") # we already did this, but we can do it again


getDatasetofSelectedStation <- function(stations){
  df <- seedDataset2 %>%
    filter(EoICode %in%  stations)
  return(df)
}



getDataSetForPollutants <- function(stations, polList=c('PM2.5','PM10','SO2','NO2'), longName=2){

  withProgress(message = 'Reading data files...', style = "notification", value = 0.1, {
    My_Printlog("Inside ...........getDataSetForPollutants")
    myfiles <- getFileList(stations,polList,longName)
    incProgress(0.5)
    dataset <- do.call("rbind", lapply(myfiles, read.csv, header = TRUE))

    incProgress(0.5)
    if(class(dataset) ==  "data.frame")
    {
      My_Printlog("Returning data set ")
      return(dataset)
    }
    else
    {
      My_Printlog("ERROR:  Returning NULL data set ")
      return(NULL)
    }
  })
}
getFileList <- function(stations, polList=c('PM2.5','PM10','SO2','NO2'),longName=2){

  withProgress(message = 'Reading files...', style = "notification", value = 0.1, {
    s_len = length(stations)
    p_len = length(polList)
    retList = vector()
    for (i in 1:s_len){
      incProgress(0.5)
      for(j in 1:p_len){
        incProgress(0.5)
        rdatafname <- paste (FileDirectory,"/",stations[i],sep="", "_",polList[j], FileType)
         if(file.exists(rdatafname) ){
            if(longName== 1){
              retList[length(retList)+1] <- paste (stations[i],sep="", "_",polList[j], FileType)
            }
            else if(longName== 2){
              retList[length(retList)+1] <- stations[i]
            }
            else if(longName== 3){

              retList[length(retList)+1] <- paste (FileDirectory,"/",stations[i],sep="", "_",polList[j], FileType)
            }
            else if(longName== 4){
              retList[length(retList)+1] <- paste (stations[i],sep="", "_",polList[j])
            }
        }
      }

    }
    My_Printlog("FILES ARE COMING !!")
    My_Printlog(unique(retList))
  })
  return (unique(retList))
}

My_Printlog <- function (str, ...){

  print(str,...)

}

