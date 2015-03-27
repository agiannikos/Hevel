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
library(ggplot2)
library(gridExtra)
library(ReporteRs)

##----Sources for Functions--------------------------------
source("./GC_FUNCTIONS.R")
source("./GC_STATS_PIR_FILE_AND_FOLDER.R")
#---------------------------------------------------------#

##-------Calculations--------------------------------------
#--Read Data from the defined file------------------------#
pirData  <- read.csv(paste(pirDir,pirFile, sep=""))



#--First Slide--------------------------------------------#
doc = pptx(template = "G:/Temp/report_example_template.pptx")
doc = addSlide(doc, "Title Slide")
doc = addTitle(doc,paste("Pre Front-End Process Report for    ", week, " (", fromDate, " - ", toDate, ")", sep=""))

##--GCI----------------------------------------------------
##-------------------------------------------------------##

##--DATA-------------------------------------------------##
GCIData <- pirData %>%
        filter(as.Date(GCI_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
               as.Date(GCI_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y")) %>%
        select(SUBSTRATE_ID,
               contains("GLS", ignore.case = TRUE), 
               contains("GCI", ignore.case = TRUE), 
               contains("AOI", ignore.case = TRUE),
               contains("GMS", ignore.case = TRUE)
        )

GCI01 <- GCIData %>%
        filter(GCI_TOOL=="01-GCI-001") %>%
        mutate(GCI_TIME_END=format(as.Date(GCI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))


GCI02 <- GCIData %>%
        filter(GCI_TOOL=="01-GCI-002") %>%
        mutate(GCI_TIME_END=format(as.Date(GCI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))

##--Plots and Slides-------------------------------------##
#--Define binwidths for each plot.-----------------------##
gciDefaultBinwiths <- TRUE
if (gciDefaultBinwiths){
        gciR3CondBW <- NULL
        gciBrCondBW <- NULL
        gciR1Temp  <- NULL
        gciR2Temp  <- NULL
        gciR3Temp  <- NULL        
}else{
        gciR3CondBW <- 0.05
        gciBrCondBW <- 0.3
        gciR1Temp  <- 0.5
        gciR2Temp  <- 0.5
        gciR3Temp  <- 0.5
}
#--GCI01 R-3 Conductivity---------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCI01, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_RINSE3_DIW_CONDUCT", yAxisLabel = "Conductivity [µS]", 
                                  xAxisLabel = "Date", opt_plotBinWidth=gciR3CondBW, opt_HLA=2, opt_HHLA=4, opt_SP=1)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI01 Rinse-3 Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCI02 R-3 Conductivity---------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCI02, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_RINSE3_DIW_CONDUCT", yAxisLabel = "Conductivity [µS]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gciR3CondBW, opt_HLA=2, opt_HHLA=4, opt_SP=1)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI02 Rinse-3 Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCI01 Brush Conductivity-------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCI01, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_BRUSH_DTG_CONDUCT", yAxisLabel = "Conductivity [mS]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gciBrCondBW, opt_HLA=9.5, opt_HHLA=10, opt_LLA=6, opt_LLLA=5, opt_SP=8)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI01 Brush Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCI02 Brush Conductivity-------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCI02, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_BRUSH_DTG_CONDUCT", yAxisLabel = "Conductivity [mS]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gciBrCondBW, opt_HLA=9.5, opt_HHLA=10, opt_LLA=6, opt_LLLA=5, opt_SP=8)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI02 Brush Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCI01 R1-Temperature-----------------------------------# 
gPlot <- plotBoxPlot_with_density(plotData = GCI01, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_RINSE1_TANK_TEMP", yAxisLabel = "Temperature [°C]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gciR1Temp, opt_HLA=39, opt_HHLA=41, opt_LLA=35, opt_LLLA=33, opt_SP=37)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI01 Rinse-1 Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCI02 R1-Temperature------------------------------------# 
gPlot <- plotBoxPlot_with_density(plotData = GCI02, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_RINSE1_TANK_TEMP", yAxisLabel = "Temperature [°C]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gciR1Temp, opt_HLA=39, opt_HHLA=41, opt_LLA=35, opt_LLLA=33, opt_SP=37)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI02 Rinse-1 Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCI01 R2-Temperature-----------------------------------# 
gPlot <- plotBoxPlot_with_density(plotData = GCI01, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_RINSE2_TANK_TEMP", yAxisLabel = "Temperature [°C]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gciR2Temp, opt_HLA=38, opt_HHLA=40, opt_LLA=34, opt_LLLA=32, opt_SP=36)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI01 Rinse-2 Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCI02 R2-Temperature-----------------------------------# 
gPlot <- plotBoxPlot_with_density(plotData = GCI02, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_RINSE2_TANK_TEMP", yAxisLabel = "Temperature [°C]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gciR2Temp, opt_HLA=38, opt_HHLA=40, opt_LLA=34, opt_LLLA=32, opt_SP=36)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI02 Rinse-2 Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCI01 R3-Temperature-----------------------------------# 
gPlot <- plotBoxPlot_with_density(plotData = GCI01, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_RINSE3_TANK_TEMP", yAxisLabel = "Temperature [°C]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gciR3Temp, opt_HLA=37, opt_HHLA=39, opt_LLA=33, opt_LLLA=31, opt_SP=35)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI01 Rinse-3 Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCI02 R3-Temperature-----------------------------------# 
gPlot <- plotBoxPlot_with_density(plotData = GCI02, X_Variable = "GCI_TIME_END", Y_Variable = "GCI_RINSE3_TANK_TEMP", yAxisLabel = "Temperature [°C]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gciR3Temp, opt_HLA=37, opt_HHLA=39, opt_LLA=33, opt_LLLA=31, opt_SP=35)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCI02 Rinse-3 Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

##--Remove GCI DATA---------------------------------------#
rm(list=c("GCIData","GCI01","GCI02"))
rm(list=ls(pattern = "gci"))


##--GCH----------------------------------------------------
##-------------------------------------------------------##

##--DATA-------------------------------------------------##
GCHData  <- pirData %>%
        filter(as.Date(GCH_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
               as.Date(GCH_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y")) %>%
        select(SUBSTRATE_ID,
               contains("GCH", ignore.case = TRUE)
        )
GCH01 <- GCHData %>%
        filter(GCH_TOOL=="01-GCH-001") %>%
        mutate(GCH_TIME_END=format(as.Date(GCH_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"),GCH_BRUSH_DTG_CONDUCT=GCH_BRUSH_DTG_CONDUCT/10, 
               GCH_RINSE3_DIW_CONDUCT=GCH_RINSE3_DIW_CONDUCT/10,GCH_RINSE1_TANK_TEMP=GCH_RINSE1_TANK_TEMP/10, GCH_RINSE2_TANK_TEMP=GCH_RINSE2_TANK_TEMP/10,
               GCH_RINSE3_TANK_TEMP=GCH_RINSE3_TANK_TEMP/10)
GCH02 <- GCHData %>%
        filter(GCH_TOOL=="01-GCH-002") %>%
        mutate(GCH_TIME_END=format(as.Date(GCH_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"),GCH_BRUSH_DTG_CONDUCT=GCH_BRUSH_DTG_CONDUCT/10, 
               GCH_RINSE3_DIW_CONDUCT=GCH_RINSE3_DIW_CONDUCT/10, GCH_RINSE1_TANK_TEMP=GCH_RINSE1_TANK_TEMP/10, GCH_RINSE2_TANK_TEMP=GCH_RINSE2_TANK_TEMP/10,
               GCH_RINSE3_TANK_TEMP=GCH_RINSE3_TANK_TEMP/10)

##--Plots and Slides-------------------------------------##
#--Define binwidths for each plot.-----------------------##
gchDefaultBinwiths <- TRUE
if (gchDefaultBinwiths){
        gchR3CondBW <- NULL
        gchUlSonCondBW <- NULL
        gchBrCondBW <- NULL
        gchM2Temp  <- NULL
        gchUlSonTemp  <- NULL
        gchM6Temp  <- NULL        
}else{
        gchR3CondBW <- 0.05
        gchUlSonCondBW <- 2
        gchBrCondBW <- 0.1
        gchM2Temp  <- 0.5
        gchUlSonTemp  <- 0.1
        gchM6Temp  <- 0.5
}


#--GCH01 R-3 Conductivity---------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH01, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_RINSE3_DIW_CONDUCT", yAxisLabel = "Conductivity [µS]", 
                                  xAxisLabel = "Date", opt_plotBinWidth=gchR3CondBW, opt_LLA=0, opt_LLLA=0, opt_HLA=2, opt_HHLA=3, opt_SP=1)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH01 Rinse-3 Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH02 R-3 Conductivity---------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH02, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_RINSE3_DIW_CONDUCT", yAxisLabel = "Conductivity [µS]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gchR3CondBW, opt_LLA=0, opt_LLLA=0, opt_HLA=2, opt_HHLA=3, opt_SP=1)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH02 Rinse-3 Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH01 Ultrasonic Conductivity---------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH01, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_ULTSON_CONDUCT", yAxisLabel = "Conductivity [µS]", 
                                  xAxisLabel = "Date", opt_plotBinWidth=gchUlSonCondBW, opt_HLA=200, opt_HHLA=300, opt_SP=150)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH01 Ultrasonic Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH02 Ultrasonic Conductivity---------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH02, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_ULTSON_CONDUCT", yAxisLabel = "Conductivity [µS]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gchUlSonCondBW, opt_HLA=200, opt_HHLA=300, opt_SP=150)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH02 Ultrasonic Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH01 Brush Conductivity---------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH01, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_BRUSH_DTG_CONDUCT", yAxisLabel = "Conductivity [mS]", 
                                  xAxisLabel = "Date", opt_plotBinWidth=gchBrCondBW, opt_LLA=6, opt_LLLA=5, opt_HLA=10, opt_HHLA=11, opt_SP=8)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH01 Brush Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH02 Brush Conductivity---------------------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH02, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_BRUSH_DTG_CONDUCT", yAxisLabel = "Conductivity [mS]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gchBrCondBW, opt_LLA=6, opt_LLLA=5, opt_HLA=10, opt_HHLA=11, opt_SP=8)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH02 Brush Conductivity")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH01 M2–Pre Cleaning Temperature----------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH01, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_RINSE1_TANK_TEMP", yAxisLabel = "Temperature [°C]", 
                                  xAxisLabel = "Date", opt_plotBinWidth=gchM2Temp, opt_HLA=47, opt_HHLA=48, opt_LLA=43, opt_LLLA=42, opt_SP=45)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH01 M2–Pre Cleaning Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH02 M2–Pre Cleaning Temperature----------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH02, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_RINSE1_TANK_TEMP", yAxisLabel = "Temperature [°C]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gchM2Temp, opt_HLA=47, opt_HHLA=48, opt_LLA=43, opt_LLLA=42, opt_SP=45)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH02 M2–Pre Cleaning Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH01 M5–Ultrasonic Temperature----------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH01, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_RINSE2_TANK_TEMP", yAxisLabel = "Temperature [°C]", 
                                  xAxisLabel = "Date", opt_plotBinWidth=gchUlSonTemp, opt_HLA=32, opt_HHLA=33, opt_LLA=28, opt_LLLA=27, opt_SP=30)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH01 M5–Ultrasonic Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH02 M5–Ultrasonic Temperature----------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH02, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_RINSE2_TANK_TEMP", yAxisLabel = "Temperature [°C]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gchUlSonTemp, opt_HLA=32, opt_HHLA=33, opt_LLA=28, opt_LLLA=27, opt_SP=30)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH02 M5–Ultrasonic Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH01 M6–Cascade Rinse Temperature----------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH01, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_RINSE3_TANK_TEMP", yAxisLabel = "Temperature [°C]", 
                                  xAxisLabel = "Date", opt_plotBinWidth=gchM6Temp, opt_HLA=30, opt_HHLA=32, opt_LLA=24, opt_LLLA=22, opt_SP=27)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH01 M6–Cascade Rinse Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)

#--GCH02 M6–Cascade Rinse Temperature----------------------#
gPlot <- plotBoxPlot_with_density(plotData = GCH02, X_Variable = "GCH_TIME_END", Y_Variable = "GCH_RINSE3_TANK_TEMP", yAxisLabel = "Temperature [°C]",
                                  xAxisLabel = "Date", opt_plotBinWidth=gchM6Temp, opt_HLA=30, opt_HHLA=32, opt_LLA=24, opt_LLLA=22, opt_SP=27)
doc = addSlide(doc, "Title and Content")
doc = addTitle(doc,"GCH02 M6–Cascade Rinse Temperature")
doc = addPlot(doc = doc , fun = print, x = gPlot)
rm(gPlot)


##--Remove GCH Data---------------------------------------#
rm(list=c("GCHData","GCH01","GCH02"))
rm(list=ls(pattern = "gch"))

#--Save File-----------------------------------------------
writeDoc(doc, "G:/Temp/report_example.pptx" )
