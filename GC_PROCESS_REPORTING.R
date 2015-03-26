#--Remove everything from memory--------------------------#
rm(list=ls())

##--INPUTS------------------------------------------------#
#---------------------------------------------------------#

##--Set Working Directory----------------------------------
#---Working directory is the directory that this file is -#
#---located. Under the same directory the script----------#
#---AOI_FUNCTIONS.R have to be placed---------------------#
setwd("E:/RDEV")

##--Dates--------------------------------------------------
#--Define the dates for the statistics.-------------------#
#--Date format: mm.dd.yyyy (month.day.year)---------------#
fromDate <- "03.09.2015"
toDate  <- "03.15.2015"

#--END OF INPUTS------------------------------------------#
#---------------------------------------------------------#

##----Libraries--------------------------------------------
library(dplyr)
library(RODBC)
library(ggplot2)
library(gridExtra)

##----Sources for Functions--------------------------------
source("./GC_FUNCTIONS.R")
source("./GC_STATS_PIR_FILE_AND_FOLDER.R")
#---------------------------------------------------------#

##-------Calculations--------------------------------------
#--Read Data from the defined file------------------------#
pirData  <- read.csv(paste(pirDir,pirFile, sep=""))

GCIData <- pirData %>%
        filter(as.Date(GCI_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
               as.Date(GCI_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y")) %>%
        select(SUBSTRATE_ID,
               contains("GLS", ignore.case = TRUE), 
               contains("GCI", ignore.case = TRUE), 
               contains("AOI", ignore.case = TRUE),
               contains("GMS", ignore.case = TRUE)
               )

GCHData  <- pirData %>%
        filter(as.Date(GCH_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
               as.Date(GCH_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y")) %>%
        select(SUBSTRATE_ID,
               contains("GCH", ignore.case = TRUE)
        )


GCI01 <- GCIData %>%
        filter(GCI_TOOL=="01-GCI-001") %>%
        mutate(GCI_TIME_END=format(as.Date(GCI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))


GCI02 <- GCIData %>%
        filter(GCI_TOOL=="01-GCI-002") %>%
        mutate(GCI_TIME_END=format(as.Date(GCI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))

GCH01 <- GCHData %>%
        filter(GCH_TOOL=="01-GCH-001") %>%
        mutate(GCH_TIME_END=format(as.Date(GCH_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))
GCH02 <- GCHData %>%
        filter(GCH_TOOL=="01-GCH-002") %>%
        mutate(GCH_TIME_END=format(as.Date(GCH_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))

p <-  plotBoxPlot_with_density(plotData = GCH01, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_ULTSON_CONDUCT", yAxisLabel = "Conductivity [µS]", 
                                        xAxisLabel = "Date", opt_plotBinWidth=2)
p

