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
fromDate <- "03.03.2015"
toDate  <- "03.04.2015"

#--Define AOI Tool: Values: 01-AOI-001 OR 01-AOI-002-------
aoiTool <- "01-AOI-001"

#----END OF INPUTS----------------------------------------#
#---------------------------------------------------------#


##----Libraries--------------------------------------------
library(ggplot2)
library(dplyr)
#library(xlsx)

##----Sources for Functions--------------------------------
source("./AOI_FUNCTIONS.R")
source("./AOI_STATS_PIR_FILE_AND_FOLDER.R")
#---------------------------------------------------------#

##-------Calculations--------------------------------------
#--Read Data from the defined file------------------------#
pirData  <- read.csv(paste(pirDir,pirFile, sep=""))