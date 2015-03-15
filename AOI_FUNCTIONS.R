##----Libraries--------------------------------------------
library(dplyr)
library(stringr)


#--Get all glasses that have passed from the defined AOI in the given period of time---- 
glassesPerDates <- function(pirData,tool,fromDate, toDate){
        
        glassesPerDates <- pirData %>%
                filter(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
                       as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y")
                ) %>%
                select(SUBSTRATE_ID,PROD_ENG,GLS_BATCH_NR,contains("AOI",ignore.case = TRUE)) %>%
                mutate(DATE=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))
        
        return(glassesPerDates)
}

#--Get all glasses that have passed AOI from a given pallet----
glassesPerPallet <- function(pirData,palletNo){
        glassesPerPallet <- pirData %>%
                filter(GLS_BATCH_NR==palletNo) %>%
                select(SUBSTRATE_ID,PROD_ENG,GLS_BATCH_NR,contains("AOI",ignore.case = TRUE)) %>%
                mutate(DATE=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))
        
        return(glassesPerPallet)
}

#--Get the list of files from a given array of subdirectories----
getFileList <- function(mainDirectory,subDirArray,fileExtnsion=".zip"){
        oldwd <- getwd()
        file_list <-NULL
        for (pf in subDirArray){
                filePath <-paste(mainDirectory,pf,sep="")
                setwd(filePath)
                files <- list.files(pattern=fileExtnsion)
                file_list <- rbind(file_list,data.frame(filePath,filesName=files))
        }
        setwd(oldwd) 
        return(file_list)        
}

#--Get defects data from AOI zip files--------------------#
getSubstratesDataFromZip <- function(file_list, pirSubstateListPerPallet){
        file_list$substrateID <- sub("_.*","",file_list$filesName)
        matchSubstrateID <- match(pirSubstateListPerPallet$SUBSTRATE_ID,file_list$substrateID)
        data_list <- file_list[matchSubstrateID,]
        oldWd <- getwd()
        
        tempWD <- as.character(data_list[1,1])
        setwd(tempWD)
        substrateData <- data.frame(read.delim(unz(data_list[1,2],"defects.tsv"), header=T))
        
        substrateData$SUBSTRATE_ID <- as.character(data_list[1,3])
        glassID <- as.character(data_list[1,3])
        substrateData$BATCH_ID <- filter(pirSubstateListPerPallet,SUBSTRATE_ID==glassID)[1,3]
        for (i in 2:nrow(data_list))
        {
                tempWD <- as.character(data_list[i,1])
                setwd(tempWD)
                tempData <- read.delim(unz(data_list[i,2],"defects.tsv"), header=T)
                tempData$SUBSTRATE_ID <- as.character(data_list[i,3])
                glassID <- as.character(data_list[i,3])
                tempData$BATCH_ID <- filter(pirSubstateListPerPallet,SUBSTRATE_ID==glassID)[1,3]
                
                substrateData=rbind(substrateData, tempData)
                rm(tempData)
        }
        setwd(oldWd)
        return(substrateData)
}

#--Get pallet number from substrate ID
getPalletNoFromGlassID <- function(pirData,substrateID){
        palletID <- pirData %>%
                filter(SUBSTRATE_ID==substrateID) %>%
                select(SUBSTRATE_ID,GLS_BATCH_NR)
        return(palletID)
}

#--Plot Bar Chart---------------------------------------------------------------
plotBarChart <- function(data,x,y,colorCategory,xAxisLab, yAxisLabel,variableForLabelsOnGraph){
        ##--Plot Data----------------------------------------------
        g <- ggplot(data = data,aes(x =x,y=y,fill=colorCategory))
        g <- g + geom_bar(stat="identity", position="dodge")
        g <- g + xlab(xAxisLab) + ylab(yAxisLabel)
        g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))
        g <- g + theme(axis.title=element_text(size=16,face="bold"))
        g <- g + geom_text(aes(label=variableForLabelsOnGraph),position = position_dodge(width=1),vjust=1,size=5)
        g
}

#--Find The Pallet IDs between two defined dates-----------
getPalletID_by_Dates <- function(pirData,fromDate,toDate,aoiTool){
        filter(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
               as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y")
        ) 
        return(getPalletID_by_Dates)
}
