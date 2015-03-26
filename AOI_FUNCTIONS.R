##--This files contains all the functions that are--------#
#--required for the excecution of the other scripts.------#
#--This file shoud not be modified------------------------#

##----Libraries--------------------------------------------
library(dplyr)
library(stringr)
library(RODBC)


#--Get all glasses that have passed from the defined AOI in the given period of time---- 
glassesPerDatesPerTool <- function(pirData,tool,fromDate, toDate){
        
        glassesPerDatesPerTool <- pirData %>%
                filter(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
                       as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y"),
                       AOI_TOOL==tool
                ) %>%
                select(SUBSTRATE_ID,PROD_ENG,GLS_BATCH_NR,contains("AOI",ignore.case = TRUE)) %>%
                mutate(DATE=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))
        
        return(glassesPerDatesPerTool)
}

#--Get all glasses that have passed from both tools in the given period of time---- 
glassesPerDates <- function(pirData,fromDate, toDate){
        
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
                filter(GLS_BATCH_NR==palletNo,!is.na(AOI_DEFECTS_TOTAL)) %>%
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
                if (file.exists(filePath)){
                        setwd(filePath)
                        files <- list.files(pattern=fileExtnsion)
                        file_list <- rbind(file_list,data.frame(filePath,filesName=files))
                }
                
        }
        setwd(oldwd) 
        return(file_list)        
}

#--Get defects data from AOI zip files--------------------#
#--file_list is the list of files in AOI directrory and---#
#--pirSubstateListPerPallet is the list of substrates in--#
#--the PIR file-------------------------------------------#
getSubstratesDataFromZip <- function(file_list, pirSubstateListPerPallet){
        substrateData <- NULL
        naData <- NULL
        
        #Take the substrate ID from file name and put in a new column with the name "substrateID" 
        file_list$substrateID <- sub("_.*","",file_list$filesName)
        
        #Compare the two lists by substrate ID and return the substrates that exists in both lists
        matchSubstrateID <- match(pirSubstateListPerPallet$SUBSTRATE_ID,file_list$substrateID)
        #take the substrates that are in both lists
        data_list <- file_list[matchSubstrateID,]
        
        #take the substrates that are in pir file but not in AOI directory
        missingSubstrates <<- pirSubstateListPerPallet[is.na(matchSubstrateID),]
#         View(missingSubstrates)
        
        data_list <- data_list[!is.na(data_list$substrateID),]
#         View(data_list)
        
        oldWd <- getwd()
        
        # if there are substrates in data_list read the data from zip files
        if (nrow(data_list)>0){
                tempWD <- as.character(data_list[1,1])
                #                 print(tempWD)
                if (!is.na(tempWD)){
                        #                         print("tempWD-1")
                        setwd(tempWD)
                        substrateData <- data.frame(read.delim(unz(data_list[1,2],"defects.tsv"), header=T))
                        #                         print(as.character(data_list[1,2]))
                        #                         print(ncol(substrateData))
                        
                      
                        
                        if(ncol(substrateData)>11){
                                substrateData <- substrateData %>%
                                        mutate(Layer=NA) %>%
                                        select(Defect.Class,
                                               Layer,
                                               Quality,
                                               X.Position=3, #"xpos.mm.",
                                               Y.Position=4, #"ypos.mm.",
                                               Width=6, #"width.mm.",
                                               Length=7, #"length.mm.",
                                               Size=5, #"size.mm.",
                                               ID,
                                               Images.Reflection=131,#"Images.R",
                                               X
                                        )
                        }
                        
                        if(ncol(substrateData)<=10){
                                substrateData <- substrateData %>%
                                        mutate(Defect.Class=NA, Layer=NA, Quality=NA, X.Position=NA, Y.Position=NA, ID=NA,Images.Reflection=NA,X=NA) %>%
                                        select(Defect.Class,
                                               Layer,
                                               Quality,
                                               X.Position,
                                               Y.Position,
                                               Width=3, #"width.mm.",
                                               Length=2, #"length.mm.",
                                               Size=4, #"size.mm.",
                                               ID,
                                               Images.Reflection,
                                               X
                                        )
                        }
                        
                        substrateData$SUBSTRATE_ID <- as.character(data_list[1,3])
                        glassID <- as.character(data_list[1,3])
                        substrateData$BATCH_ID <- filter(pirSubstateListPerPallet,SUBSTRATE_ID==glassID)[1,3]
                }
                #                 print(paste("data_list rows:",nrow(data_list)))
                for (i in 2:nrow(data_list))
                {
                        tempWD <- as.character(data_list[i,1])
                        #                         print("tempWD-2")
                        #                         print(tempWD)
                        setwd(tempWD)
                        tempData <- read.delim(unz(data_list[i,2],"defects.tsv"), header=T)
                        #                         print(data_list[i,2])
                        #                         print(ncol(tempData))
                        if(ncol(tempData)>11){
                                tempData <- tempData %>%
                                        mutate(Layer=NA) %>%
                                        select(Defect.Class,
                                               Layer,
                                               Quality,
                                               X.Position=3, #"xpos.mm.",
                                               Y.Position=4, #"ypos.mm.",
                                               Width=6, #"width.mm.",
                                               Length=7, #"length.mm.",
                                               Size=5, #"size.mm.",
                                               ID,
                                               Images.Reflection=131,#"Images.R",
                                               X
                                        )
                        }
                        
                        if(ncol(tempData)<=10){
                                tempData <- tempData %>%
                                        mutate(Defect.Class=NA, Layer=NA, Quality=NA, X.Position=NA, Y.Position=NA, ID=NA,Images.Reflection=NA,X=NA) %>%
                                        select(Defect.Class,
                                               Layer,
                                               Quality,
                                               X.Position,
                                               Y.Position,
                                               Width=3, #"width.mm.",
                                               Length=2, #"length.mm.",
                                               Size=4, #"size.mm.",
                                               ID,
                                               Images.Reflection,
                                               X
                                        )
                        }
                        
                        tempData$SUBSTRATE_ID <- as.character(data_list[i,3])
                        glassID <- as.character(data_list[i,3])
                        tempData$BATCH_ID <- filter(pirSubstateListPerPallet,SUBSTRATE_ID==glassID)[1,3]
                        substrateData=rbind(substrateData, tempData)
                        rm(tempData)
                }
                
                setwd(oldWd)
        }
        
        #         print(paste("missingSubs rows:",nrow(missingSubstrates)))
        
        if (nrow(missingSubstrates)>0){
                naData <- data.frame(BATCH_ID=missingSubstrates$GLS_BATCH_NR,
                                     SUBSTRATE_ID=missingSubstrates$SUBSTRATE_ID,
                                     Defect.Class=NA,
                                     Layer=NA,
                                     Quality=NA,
                                     X.Position=NA,
                                     Y.Position=NA,
                                     Width=NA,
                                     Length=NA,
                                     Size=NA,
                                     ID=NA,
                                     X=NA,
                                     Images.Reflection=NA
                )
        }
        #                 View(naData)
        #                 View(substrateData)
        
        substrateData=rbind(substrateData, naData)
        takeSubstrateIDs <- match(substrateData$SUBSTRATE_ID,pirSubstateListPerPallet$SUBSTRATE_ID)
        substrateData$DATE <- pirSubstateListPerPallet[takeSubstrateIDs,"DATE"]
        substrateData$AOI_RESULT <- pirSubstateListPerPallet[takeSubstrateIDs,"AOI_RESULT"]
        substrateData$AOI_TOOL <- pirSubstateListPerPallet[takeSubstrateIDs,"AOI_TOOL"]
        substrateData$YEAR <- pirSubstateListPerPallet[takeSubstrateIDs,"YEAR"]
        substrateData$MONTH <- pirSubstateListPerPallet[takeSubstrateIDs,"MONTH"]
        #                 View(substrateData)
        return(substrateData)
}

#--Get pallet number from substrate ID
getPalletNoFromGlassID <- function(pirData,substrateID){
        palletID <- pirData %>%
                filter(SUBSTRATE_ID==substrateID) %>%
                select(SUBSTRATE_ID,GLS_BATCH_NR)
        return(palletID)
}

#--Plot Bar Chart-----------------------------------------#
plotBarChart <- function(data,x,y,colorCategory,xAxisLab, yAxisLabel,variableForLabelsOnGraph){
        ##--Plot Data-------------------------------------#
        g <- ggplot(data = data,aes(x =x,y=y,fill=colorCategory))
        g <- g + geom_bar(stat="identity", position="dodge")
        g <- g + xlab(xAxisLab) + ylab(yAxisLabel)
        g <- g + theme(axis.text.x = element_text(angle = 45, hjust = 1, size=12))
        g <- g + theme(axis.title=element_text(size=16,face="bold"))
        g <- g + geom_text(aes(label=variableForLabelsOnGraph),position = position_dodge(width=1),vjust=1,size=5)
        g
}

#--Find The Pallet IDs between two defined dates----------#
getPalletID_by_Dates <- function(pirData,fromDate,toDate,aoiTool){
        
        getPalletID_by_Dates<- pirData %>%
                filter(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
                       as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y"),
                       AOI_TOOL==aoiTool
                ) %>%
                distinct(GLS_BATCH_NR) %>%
                select(GLS_BATCH_NR)
        
        return(getPalletID_by_Dates)
}

#--The following function returns the list of the glasses-#
#--that have passed from the defined aoi at the defined---#
#--dates. It returns whole pallets of glasses even if-----#
#--have loaded at different dates that the defined--------#

getGlassesFromMultiplePallets_by_dates <- function(pirData,fromDate,toDate,aoiTool){
        
        pallets <- getPalletID_by_Dates(pirData = pirData, fromDate = fromDate, toDate = toDate, aoiTool = aoiTool)
        
        palletArray  <- as.character(pallets[,1])
        
        glasslist <- pirData %>%
                filter(GLS_BATCH_NR %in% palletArray, AOI_TOOL== aoiTool) %>%
                arrange(GLS_BATCH_NR,SUBSTRATE_ID) %>%
                select(SUBSTRATE_ID,PROD_ENG,GLS_BATCH_NR,contains("AOI",ignore.case = TRUE)) %>%
                mutate(DATE=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"),
                       YEAR=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%Y"),
                       MONTH=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m")
                )
        
        
        return(glasslist)
}

#--The following function returns the list of the glasses-#
#--that have passed from the defined aoi at the defined---#
#--dates. The difference from the previous function-------#
#--is that excludes the glasses from a pallet if they-----#
#--have not loaded at the defined dates-------------------# 

getGlassesFromMultiplePallets_split_by_dates <- function(pirData,fromDate,toDate,aoiTool){
        
        pallets <- getPalletID_by_Dates(pirData = pirData, fromDate = fromDate, toDate = toDate, aoiTool = aoiTool)
        
        palletArray  <- as.character(pallets[,1])
        glasslist <- pirData %>%
                filter(GLS_BATCH_NR %in% palletArray, 
                       AOI_TOOL== aoiTool, 
                       as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"), 
                       as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y")) %>%
                arrange(GLS_BATCH_NR,SUBSTRATE_ID) %>%
                select(SUBSTRATE_ID,PROD_ENG,GLS_BATCH_NR,contains("AOI",ignore.case = TRUE)) %>%
                mutate(DATE=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"),
                       YEAR=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%Y"),
                       MONTH=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m")
                )
        
        
        return(glasslist)
}

#--The following function counts the number of glasses----#
#--that have passed from the defined aoi at the defined---#
#--time period--------------------------------------------#
totalGlasses_by_dates_by_tool <- function(pirData,fromDate,toDate,aoiTool){
        totalGlasses <- pirData %>%
                filter(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
                       as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y"),
                       AOI_TOOL==aoiTool
                ) %>%
                select(SUBSTRATE_ID,PROD_ENG,GLS_BATCH_NR,contains("AOI",ignore.case = TRUE)) %>%
                mutate(DATE=format(as.Date(AOI_TIME_END,"%m.%d.%Y %H:%M:%S"),"%m.%d.%Y"))
        
        return(nrow(totalGlasses))
}

##--The following function checks the database for--------#
#--duplicates and deletes them----------------------------#
##--This function needs 32 bit R Version and is not-------#
#--working for tables with many records-------------------#
#--ColumnNames should be provited with the----------------# 
#--following format---------------------------------------#
# tableColsArray <- c("BATCH_ID", "SUBSTRATE_ID", "DefectClass", "Layer", "Quality", "XPosition", "YPosition", "Width",
#                       "Length", "Size", "ID", "X", "DATE", "AOI_RESULT", "AOI_TOOL", "YEAR", "MONTH")

deleteDuplicatesFromTable <- function(odbcConnection,tableName, tableColsArray){
        
        addROWIDqry <- paste("ALTER TABLE ", tableName, " ADD COLUMN RID COUNTER;", sep="")
        sqlQuery(odbcConnection,addROWIDqry)
        
        leftWhereClause  <- ""
        for (c in tableColsArray){
                if (leftWhereClause==""){
                        leftWhereClause  <- paste("(","L.", c, sep="")
                        
                }else{
                        leftWhereClause <- paste(leftWhereClause, " & ", "L.", c, sep="")
                }
        }
        leftWhereClause <- paste(leftWhereClause,")",sep="")
        
        rightWhereClause  <- ""
        for (c in tableColsArray){
                if (rightWhereClause==""){
                        rightWhereClause  <- paste("(", tableName, ".", c, sep="")
                        
                }else{
                        rightWhereClause <- paste(rightWhereClause, " & ", tableName , ".", c, sep="")
                }
        }
        rightWhereClause <- paste(rightWhereClause,")",sep="")
        
        qry <- paste("DELETE ", tableName, ".* FROM ", tableName, " WHERE ((((SELECT COUNT (*) FROM ",
                     tableName, " AS L  WHERE ", leftWhereClause, " = ", rightWhereClause,
                     " AND (L.RID <= ", tableName,".RID)))>1));",sep="")
        
        
        #         sqlQuery(odbcConnection,qry)
        
        deleteRIDqry <- paste("ALTER TABLE ", tableName, " DROP COLUMN RID;", sep="")
        #         sqlQuery(odbcConnection,deleteRIDqry)
}


##--The following function checks the database for--------#
#--duplicates and deletes them----------------------------#
##--This function needs 32 bit R Version.-----------------#
deleteDuplicatesFromTableManyRecords <- function(odbcConnection,tableName){
        
        copyDistinctDataToTempTableQry <- paste("SELECT DISTINCT ", tableName, ".* INTO tempTable FROM ", tableName,";", sep="")
        sqlQuery(odbcConnection,copyDistinctDataToTempTableQry)
        
        deleteOriginalTableQry <- paste("DROP TABLE ",tableName,";")
        sqlQuery(odbcConnection,deleteOriginalTableQry)
        
        copyTempTableToOriginalTableQry <- paste("SELECT DISTINCT tempTable.* INTO ", tableName, " FROM tempTable;", sep="")
        sqlQuery(odbcConnection,copyTempTableToOriginalTableQry)
        
        deleteTempTableQry <- paste("DROP TABLE tempTable;")
        sqlQuery(odbcConnection,deleteTempTableQry)
        
}