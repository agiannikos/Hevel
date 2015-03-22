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
aoiTool <- "01-AOI-002"

##--Dates--------------------------------------------------
#--Define the dates for the statistics.-------------------#
#--Date format: mm.dd.yyyy (month.day.year)---------------#
fromDate <- "03.01.2015"
toDate  <- "03.05.2015"

#--Access Database Name------------------------------------
#--The Database should be placed at the folder:-----------#
#--[folder of this script]/output/------------------------#
accessName  <- "AOI_DATA.accdb"

#--AOI Directory for zip files-----------------------------
#--The Main folder that are stored AOI Data---------------#  
aoidataMainDir <- "W:/????????????????????/7_AOI/AOI_2/"

#--Sub folders with dates. Should be defined as array-----#
#--This in case we need to read data from two or more-----#
#--folders. The format is the following. c("03_2015")-----#
#--for one folder or c("03_2015","04_2015","05_2015")-----#
#--for two or more.---------------------------------------#
aoiDataSubDir  <- c("03_2015")

#--END OF INPUTS------------------------------------------#
#---------------------------------------------------------#

##----Libraries--------------------------------------------
library(dplyr)
library(RODBC)

##----Sources for Functions--------------------------------
source("./AOI_FUNCTIONS.R")
source("./AOI_STATS_PIR_FILE_AND_FOLDER.R")
#---------------------------------------------------------#

##-------Calculations--------------------------------------
#--Read Data from the defined file------------------------#
pirData  <- read.csv(paste(pirDir,pirFile, sep=""))

#--Get the pallets for the glasses passed from AOI--------#

listOfSubstratesFromPir <- getGlassesFromMultiplePallets_split_by_dates(pirData = pirData, 
                                                                        fromDate = fromDate, 
                                                                        toDate = toDate, 
                                                                        aoiTool = aoiTool)

#--AOI zip files List
file_list <- getFileList(mainDirectory = aoidataMainDir ,subDirArray = aoiDataSubDir)

defectsPerGlass <- getSubstratesDataFromZip(file_list =file_list,
                                            pirSubstateListPerPallet = listOfSubstratesFromPir 
) %>%
        select(BATCH_ID,SUBSTRATE_ID,Defect.Class:MONTH,-Images.Reflection)

#--Delete variables that are not needed in order----------#
#--to release memory--------------------------------------#
rm(list=setdiff(ls(),c("defectsPerGlass","missingSubstrates","accessName","deleteDuplicatesFromTable")))
gc()
#--Store Data to the database------------------------------

mycon <- odbcConnectAccess2007(paste("./output/",accessName,sep=""))

sqlSave(channel = mycon, dat = defectsPerGlass, tablename = "RowData", append = TRUE, rownames = FALSE,  colnames = FALSE, addPK = TRUE)
sqlSave(channel = mycon, dat = missingSubstrates, tablename = "missingSubstrates", append = TRUE, rownames = FALSE,  colnames = FALSE, addPK = FALSE)

rowDataColsArray <- c("BATCH_ID", "SUBSTRATE_ID", "DefectClass", "Layer", "Quality", "XPosition", "YPosition", "Width",
                      "Length", "Size", "ID", "X", "DATE", "AOI_RESULT", "AOI_TOOL", "YEAR", "MONTH")

# deleteDuplicatesFromTable(odbcConnection = mycon, tableName = "RowData",tableColsArray = rowDataColsArray)

close(mycon)