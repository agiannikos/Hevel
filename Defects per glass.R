#-----Load Libraties
library(ggplot2)


#-----Set working directory
setwd("W:/Результаты/7_AOI/AOI_1/02_2015")
#-----Read data
file_list <- list.files(pattern="_2015_02_10_")

mydata = read.delim(unz(file_list[1],"defects.tsv"), header=T)
gls_serial <- strsplit(file_list[1], "_201*")[[1]][1]
mydata$glassID <- gls_serial

for (i in 2:length(file_list))
{
        tempData <- read.delim(unz(file_list[i],"defects.tsv"), header=T)
        gls_serial <- strsplit(file_list[i], "_201*")[[1]][1]
        tempData$glassID <- gls_serial
        mydata=rbind(mydata, tempData)
}

#-----Remove unwanted data


#-----Plot data with ggplot2
q <- qplot(x=glassID, data=mydata, geom="histogram", fill=Defect.Class)
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,size=10))
q <- q + ggtitle(paste("Data: ",getwd(), " - Glasses: ", length(file_list)))

rm(tempData,file_list,gls_serial,i)
q