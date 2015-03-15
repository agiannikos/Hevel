##--INPUTS------------------------------------------------#
#---------------------------------------------------------#

##--Set Working Directory----------------------------------
#---Working directory is the directory that this file is -#   
#---located. Under the same directory the script----------#
#---AOI_FUNCTIONS.R have to be placed---------------------#
setwd("G:/RDEV")

#--AOI Directory for zip files-----------------------------
aoidataMainDir <- "W:/????????????????????/7_AOI/AOI_1/"
aoiDataSubDir  <- c("02_2015","03_2015")

##--Pallet Number------------------------------------------
palletNo <- "140820301765"

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

#--Extract Data from PIR FILE-----------------------------#
substratesList <- glassesPerPallet(pirData=pirData,palletNo=palletNo) %>%
        filter(!is.na(AOI_DEFECTS_TOTAL))


file_list <- getFileList(mainDirectory = aoidataMainDir ,subDirArray = aoiDataSubDir )

defectsPerPalletPerGlass <- getSubstratesDataFromZip(file_list =file_list ,pirSubstateListPerPallet = substratesList )

plotData <- defectsPerPalletPerGlass %>%
        group_by(BATCH_ID,Defect.Class) %>%
        summarise(COUNT=n())
View(plotData)

g <- ggplot(data = defectsPerPalletPerGlass,aes(x =Defect.Class))
g <- g + geom_histogram(binwidth = 1,aes(fill = ..count..))
g <- g + xlab("Defect Type") + ylab("Number of Defects")+ggtitle(paste("Batch ID: ",palletNo,sep=""))
g <- g + theme(axis.title=element_text(size=16,face="bold"))
g

