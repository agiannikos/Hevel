##--INPUTS------------------------------------------------#
#---------------------------------------------------------#
##--Set Working Directory----------------------------------
#---Working directory is the directory that this file is -#   
#---located. Under the same directory the script----------#
#---AOI_FUNCTIONS.R have to be placed---------------------#
setwd("E:/RDEV")

##--Substrate ID------------------------------------------
SubstrateID <-"HVL1P11503030421"
#----END OF INPUTS----------------------------------------#
#---------------------------------------------------------#

##----Libraries--------------------------------------------
#library(xlsx)

##----Sources for Functions--------------------------------
source("./AOI_FUNCTIONS.R")
source("./AOI_STATS_PIR_FILE_AND_FOLDER.R")
#---------------------------------------------------------#

##-------Calculations--------------------------------------
#--Read Data from the defined file------------------------#
pirData  <- read.csv(paste(pirDir,pirFile, sep=""))

#--Extract Data from PIR FILE-----------------------------#
palletID <- getPalletNoFromGlassID(pirData,SubstrateID)
View(palletID)