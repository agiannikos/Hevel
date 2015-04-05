#--Remove everything from memory--------------------------#
rm(list=ls())

##--INPUTS------------------------------------------------#
#---------------------------------------------------------#

##--Set Working Directory----------------------------------
#---Working directory is the directory that this file is--#
#---located. Under the same directory the script----------#
#---AOI_FUNCTIONS.R have to be placed---------------------#
setwd("G:/RDEV")

#--Define AOI Tool. Values: 01-AOI-001 OR 01-AOI-002-------
aoiTool <- "01-AOI-001"

#--AOI Directory for zip files-----------------------------
#--The Main folder that are stored AOI Data---------------#
#--The format is the following: "c:/folder/" -------------#
aoidataMainDir <- "G:/Temp/7_AOI/AOI_1/"

#--Sub folders with dates. Should be defined as array-----#
#--This in case we need to read data from two or more-----#
#--folders. The format is the following. c("03_2015")-----#
#--for one folder or c("03_2015","04_2015","05_2015")-----#
#--for two or more.---------------------------------------#
aoiDataSubDir  <- c("03_2015", "04_2015")

##--Dates--------------------------------------------------
#--Define the dates for the statistics.-------------------#
#--Date format: mm.dd.yyyy (month.day.year)---------------#
fromDate <- "03.19.2015"
toDate  <- "03.22.2015"

#--END OF INPUTS------------------------------------------#
#---------------------------------------------------------#

##----Libraries--------------------------------------------
library(dplyr)

##----Sources for Functions--------------------------------
source("./AOI_FUNCTIONS.R")
source("./GC_FUNCTIONS.R")
source("./GC_STATS_PIR_FILE_AND_FOLDER.R")
#---------------------------------------------------------#

##-------Calculations--------------------------------------
#--Read Data from the defined file------------------------#
pirData <- NULL
for (pirF in pirFile){
        pirTempData  <- read.csv(paste(pirDir,pirF, sep=""), colClasses=c("GLS_BATCH_NR"="character"))
        pirData <- rbind(pirData,pirTempData)
}

pirData <- distinct(pirData)

file_list <- getFileList(mainDirectory = aoidataMainDir ,subDirArray = aoiDataSubDir)
plot <- getAOICumulativePlotsWithDensityPlots(pirData=pirData, 
                                              aoiTool=aoiTool, 
                                              fromDate=fromDate, 
                                              toDate=toDate, 
                                              aoiFilesList=file_list)


plot