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
from_date <- "03.01.2015"
to_date  <- "03.19.2015"

#--Define AOI Tool: Values: 01-AOI-001 OR 01-AOI-002-------
aoiTool <- "01-AOI-002"

#--Excel File Name-----------------------------------------
#--if the file already exist it will be replaced.---------#
#--The excel file will be saved in output folder which is-#
#--located in the working directory-----------------------#
excelName  <- "AOI_0021.xlsx"

#----END OF INPUTS----------------------------------------#
#---------------------------------------------------------#


##----Libraries--------------------------------------------
library(ggplot2)
library(dplyr)
library(xlsx)

##----Sources for Functions--------------------------------
source("./AOI_FUNCTIONS.R")
source("./AOI_STATS_PIR_FILE_AND_FOLDER.R")
#---------------------------------------------------------#

##-------Calculations--------------------------------------
#--Read Data from the defined file------------------------#
pirData  <- read.csv(paste(pirDir,pirFile, sep=""))

#--Extract Data from PIR FILE-----------------------------#
plotData <- glassesPerDatesPerTool(pirData=pirData, tool = aoiTool, fromDate= from_date, toDate = to_date)

dataforbothtools <- glassesPerDates(pirData=pirData, fromDate= from_date, toDate = to_date)

##--Grouped & Summarised Data------------------------------
#--Number of IO/NIO glasses per pallet for the given period
by_batch_status_count <- plotData %>%
        group_by(DATE, GLS_BATCH_NR,AOI_RESULT) %>%
        summarise(COUNT=n()) %>%
        arrange(DATE,GLS_BATCH_NR)

# View(by_batch_status_count)

#--Glasses per pallet passed through AOI------------------#
glasses_to_AOI_per_batch <- by_batch_status_count %>% 
        group_by(DATE, GLS_BATCH_NR) %>%
        summarise(GLASSES_TO_AOI=sum(COUNT)) %>%
        arrange(DATE,GLS_BATCH_NR)

# View(glasses_to_AOI_per_batch)

#--Total & Average defects per batch(pallet)--------------#
by_batch_status_sum <- plotData %>%
        group_by(DATE, GLS_BATCH_NR,AOI_RESULT) %>%
        summarise(TOTAL_DEFECTS=sum(AOI_DEFECTS_TOTAL),AVG_DEFECTS=TOTAL_DEFECTS/n()) %>%
        arrange(DATE,GLS_BATCH_NR)

# View(by_batch_status_sum)

#--Write Excel Files---------------------------------------


write.xlsx2(data.frame(by_batch_status_count), file = paste("./output/",excelName,sep=""),
            sheetName = "Summarized Data",row.names = FALSE)

write.xlsx2(plotData, file = paste("./output/",excelName,sep=""), 
            sheetName = "Raw Data", append = TRUE)

write.xlsx2(dataforbothtools, file = paste("./output/",excelName,sep=""), 
            sheetName = "Both Tools", append = TRUE)


##--Plot Data----------------------------------------------
g <- ggplot(data = by_batch_status_count,aes(x =GLS_BATCH_NR,y=COUNT,fill=AOI_RESULT))
g <- g + geom_bar(stat="identity", position="dodge")
g <- g + xlab("Batch ID") + ylab("Number of Substrates")
g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))
g <- g + theme(axis.title=element_text(size=16,face="bold"))
g <- g + geom_text(aes(label=COUNT),position = position_dodge(width=1),vjust=1,size=5)
g






