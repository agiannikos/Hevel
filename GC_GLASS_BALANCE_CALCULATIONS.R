#--Remove everything from memory--------------------------#
rm(list=ls())

##--INPUTS------------------------------------------------#
#---------------------------------------------------------#

##--Set Working Directory----------------------------------
#---Working directory is the directory that this file is -#
#---located. Under the same directory the script----------#
#---AOI_FUNCTIONS.R have to be placed---------------------#
setwd("G:/RDEV")

##--Dates--------------------------------------------------
#--Define the dates for the statistics.-------------------#
#--Date format: mm.dd.yyyy (month.day.year)---------------#
fromDate <- "03.09.2015"
toDate  <- "03.15.2015"
week <- "Week 13"

#--END OF INPUTS------------------------------------------#
#---------------------------------------------------------#

##----Libraries--------------------------------------------
library(dplyr)

##----Sources for Functions--------------------------------
source("./GC_FUNCTIONS.R")
source("./GC_STATS_PIR_FILE_AND_FOLDER.R")
#---------------------------------------------------------#

##-------Calculations--------------------------------------
#--Read Data from the defined file------------------------#
pirData  <- read.csv(paste(pirDir,pirFile, sep=""))


pallets <- getPalletsPerDates(pirData = pirData,fromDate = fromDate, toDate = toDate)

balance <- getSubstratesBalanceForGivenPalletIDS(pirData = pirData, palletIDs = pallets)
