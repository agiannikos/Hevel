#--Remove everything from memory--------------------------#
rm(list=ls())

##--INPUTS------------------------------------------------#
#---------------------------------------------------------#

##--Set Working Directory----------------------------------
#---Working directory is the directory that this file is -#
#---located. Under the same directory the script----------#
#---AOI_FUNCTIONS.R have to be placed---------------------#
setwd("G:/RDEV")

#--Define AOI Tool. Values: 01-AOI-001 OR 01-AOI-002-------
aoiTool <- "01-AOI-001"

##--Dates--------------------------------------------------
#--Define the dates for the statistics.-------------------#
#--Date format: mm.dd.yyyy (month.day.year)---------------#
fromDate <- "03.14.2015"
toDate  <- "03.14.2015"

#--Excel File Name-----------------------------------------
#--if the file already exist it will be replaced.---------#
#--The excel file will be saved in output folder which is-#
#--located in the working directory-----------------------#
excelName  <- "AOI_001_03_14_2015.xlsx"

#--AOI Directory for zip files-----------------------------
#--The Main folder that are stored AOI Data---------------#  
aoidataMainDir <- "W:/????????????????????/7_AOI/AOI_1/"

#--Sub folders with dates. Should be defined as array-----#
#--This in case we need to read data from two or more-----#
#--folders. The format is the following. c("03_2015")-----#
#--for one folder or c("03_2015","04_2015","05_2015")-----#
#--for two or more.---------------------------------------#
aoiDataSubDir  <- c("02_2015", "03_2015")

#--END OF INPUTS------------------------------------------#
#---------------------------------------------------------#


##----Libraries--------------------------------------------
library(ggplot2)
library(dplyr)
options(java.parameters = "-Xmx3000m")
library(xlsx)

##----Sources for Functions--------------------------------
source("./AOI_FUNCTIONS.R")
source("./AOI_STATS_PIR_FILE_AND_FOLDER.R")
#---------------------------------------------------------#

##-------Calculations--------------------------------------
#--Read Data from the defined file------------------------#
pirData <- NULL
for (pirF in pirFile){
        pirTempData  <- read.csv(paste(pirDir,pirF, sep=""), colClasses=c("GLS_BATCH_NR"="character"))
        pirData <- rbind(pirData,pirTempData)
}

pirData <- distinct(pirData)

#--Get the pallets for the glasses passed from AOI--------#

listOfSubstratesFromPir <- getGlassesFromMultiplePallets_by_dates(pirData = pirData, 
                                                                  fromDate =fromDate, 
                                                                  toDate =toDate, 
                                                                  aoiTool =aoiTool)

#--AOI zip files List
file_list <- getFileList(mainDirectory = aoidataMainDir ,subDirArray = aoiDataSubDir)

defectsPerGlass <- getSubstratesDataFromZip(file_list =file_list,
                                            pirSubstateListPerPallet = listOfSubstratesFromPir 
) %>%
        select(BATCH_ID,SUBSTRATE_ID,Defect.Class:AOI_RESULT,-Images.Reflection)

#--Delete variables that are not needed anymore in order--#
#--to release memory--------------------------------------#
rm(list=setdiff(ls(),c("defectsPerGlass","missingSubstrates","excelName")))
gc()
#--Write Excel Files---------------------------------------


write.xlsx2(defectsPerGlass, file = paste("./output/",excelName,sep=""),
           sheetName = "Raw Data",row.names = FALSE
)

if(nrow(missingSubstrates)>0){
        
        write.xlsx2(missingSubstrates, file = paste("./output/",excelName,sep=""), 
                   sheetName = "missingSubstrates", append = TRUE
        )
}