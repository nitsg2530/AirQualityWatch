#Convert CSV to RData files in a working folder
#IMP - Run it in 2 steps and take data backup
#--------------------------------------------------

library("rio")

FileDir <- "C:\\Users\\ng253\\Documents\\Glasgow\\Courses\\R Programming\\Project\\OnlyCSV"
DestDir <- "C:\\Users\\ng253\\Documents\\Glasgow\\Courses\\R Programming\\Project\\CorCSV"

DestRData <- "C:\\Users\\ng253\\Documents\\Glasgow\\Courses\\R Programming\\Project\\CorRData"

# STEP1........................................
allCSV_Files <- list.files(FileDir)
print(allCSV_Files)
for (i in 1:length(allCSV_Files))
{
  csvfname <- allCSV_Files[i]
  if(csvfname != "__Stations.csv")
  {
    filename = paste(FileDir,csvfname,sep="\\")
    print(filename)
    df <- read.csv(filename)
  
  
     df <- df %>%
       group_by(Year,Month,Day,Hour, AirQualityStationEoICode,AirPollutant) %>%
       summarize(Concentration = mean(Concentration))
  
     df<- drop_na(df)
     print(paste(DestDir,csvfname,sep="\\"))
     write.csv(df, file = paste(DestDir,csvfname,sep="\\"),row.names=FALSE)
     
  }
  else
  {
    print("STATION FILE")
  }
 #  Write CSV in R
  
}
# STEP1........................................
allCSV_Files <- list.files(DestDir)


for (i in 1:length(allCSV_Files))
{
  csvfname <- allCSV_Files[i]
  
  if(csvfname != "__Stations.csv")
  {
    rdatafname <- str_replace(csvfname, ".csv", ".RData")
    
    convert(paste(DestDir,csvfname,sep="\\"), paste(DestRData,rdatafname,sep="\\"))
    print(rdatafname)
    print(csvfname)
    print("...................")
  }
}

# STEP2........................................
#Take a break here ....


allCSV_Files <- list.files(DestDir)

for (i in 1:length(allCSV_Files))
{
  csvfname <- allCSV_Files[i]
  
  if(csvfname != "__Stations.csv")
  {
    rdatafname <- str_replace(csvfname, ".csv", ".RData")
    f1 <- paste(DestDir,csvfname,sep="\\")
    f2 <- paste(DestRData,rdatafname,sep="\\")
    print(f1)
    print(f2)
    x <- import(f1)
    y <- import(f2)
    
    # confirm data match
    if(!all.equal(x, y, check.attributes = FALSE))
    {
      print(csvfname)
      print("******************conversion failed ")
    }
    else
    {
      print(csvfname)
      print("<3")
    }
  }
}
print("_________DONE")