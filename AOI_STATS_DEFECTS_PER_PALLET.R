##--INPUTS------------------------------------------------#
#---------------------------------------------------------#
rm(list=ls())
##--Set Working Directory----------------------------------
#---Working directory is the directory that this file is -#   
#---located. Under the same directory the script----------#
#---AOI_FUNCTIONS.R have to be placed---------------------#
setwd("G:/RDEV")

##--Pallet Number------------------------------------------
palletNo <- "/40509517765"

#--AOI Directory for zip files-----------------------------
#--The Main folder that are stored AOI Data---------------#  
aoidataMainDir <- "W:/????????????????????/7_AOI/AOI_1/"

#--Sub folders with dates. Should be defined as array-----#
#--This in case we need to read data from two or more-----#
#--folders. The format is the following. c("03_2015")-----#
#--for one folder or c("03_2015","04_2015","05_2015")-----#
#--for two or more.---------------------------------------#
aoiDataSubDir  <- c("02_2015", "03_2015")

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

#--Excel File Name-----------------------------------------
#--if the file already exist it will be replaced.---------#
#--The excel file will be saved in output folder which is-#
#--located in the working directory-----------------------#
#--if you define it as pallet.xlsx then the file will be--#
#--saved with pellet ID like "140819301665.xlsx"----------#
excelName <- paste(palletNo,".xlsx",sep="")

pirData  <- read.csv(paste(pirDir,pirFile, sep=""))

#--Extract Data from PIR FILE-----------------------------#
substratesList <- glassesPerPallet(pirData=pirData,palletNo=palletNo) 



file_list <- getFileList(mainDirectory = aoidataMainDir ,subDirArray = aoiDataSubDir)

defectsPerPalletPerGlass <- getSubstratesDataFromZip(file_list =file_list,
                                                     pirSubstateListPerPallet = substratesList 
                                                     ) %>%
        select(BATCH_ID,SUBSTRATE_ID,Defect.Class:AOI_RESULT,-Images.Reflection)

write.xlsx(defectsPerPalletPerGlass, file = paste("./output/",excelName,sep=""),
           sheetName = "Raw Data",row.names = FALSE
           )


#--Group by Batch ID and defect---------------------------#
plotData <- defectsPerPalletPerGlass %>%
        group_by(BATCH_ID,Defect.Class) %>%
        summarise(COUNT=n())
View(plotData)

#--Save to excel file-------------------------------------#
dir.create(file.path("./", "output"), showWarnings = FALSE)

write.xlsx(plotData, file = paste("./output/",excelName,sep=""), sheetName = "Summary", append = TRUE)

if(nrow(missingSubstrates)>0){
        
        write.xlsx(missingSubstrates, file = paste("./output/",excelName,sep=""), 
                   sheetName = "missingSubstrates", append = TRUE
                   )
}

substrates <- substratesList %>%
        select(GLS_BATCH_NR,SUBSTRATE_ID,AOI_RESULT) %>%
        arrange(SUBSTRATE_ID)

write.xlsx(substrates, file = paste("./output/",excelName,sep=""), sheetName = "substrates", append = TRUE)

if (nrow(defectsPerPalletPerGlass[!is.na(defectsPerPalletPerGlass$Defect.Class),])>0){
        
        #--Plot Total Defects per Pallet---------------------------
        g <- ggplot(data = defectsPerPalletPerGlass,aes(x =Defect.Class))
        g <- g + geom_histogram(binwidth = 1,aes(fill = ..count..))
        g <- g + xlab("Defect Type") + ylab("Number of Defects")+ggtitle(paste("Batch ID: ",palletNo,sep=""))
        g <- g + theme(axis.title=element_text(size=16,face="bold"))
        g
        
        ggsave(filename = paste(palletNo,"_defects.png",sep=""),plot = g,path = "./output/", scale = 1,dpi = 600)
        
        #-----Plot cumulative plot of defects per pallet-----------
        p<-ggplot(
                data=defectsPerPalletPerGlass, aes(
                        x=X.Position,
                        y=Y.Position,
                        color=Defect.Class,
                        shape=Layer
                )
        )
        
        p <- p+layer(geom="point")
        p <- p+xlab("X")+ylab("Y")
        p <- p + coord_fixed(ratio=1)
        p <- p + xlim(1100,0)+ylim(1300,0)
        p <- p + ggtitle(paste("Batch ID: ",palletNo,sep="")) + theme(plot.title=element_text(size=12))
        p
        
        ggsave(filename = paste(palletNo,"_cumulative.png",sep=""),plot = p,path = "./output/", scale = 1,dpi = 600)
}else{
        print("Nothing to plot")
}
