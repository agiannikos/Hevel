#-----Load Libraties
library(ggplot2)


#-----Set working directory
setwd("W:/Результаты/7_AOI/AOI_1/02_2015")
#-----Read data
file_list <- list.files(pattern=".zip")

panelinfo = read.delim(unz(file_list[1],"panelinfo.tsv"), header=T,nrow=1)

for (i in 2:length(file_list)) {
        panelinfo=rbind(panelinfo, read.delim(unz(file_list[i],"panelinfo.tsv"), header=T, nrow=1))
}


#-----Plot data with ggplot2
q <- qplot(x=Date, data=panelinfo, geom="histogram", fill=Quality, position="dodge")
q <- q + theme(axis.text.x = element_text(angle = 90, hjust = 0.5, vjust = 0.5,size=10))
q <- q +stat_bin(binwidth=1, geom="text", aes(label=..count..), vjust=0.5, hjust=0, position="dodge")
q <- q + ggtitle(paste("Data: ",getwd(), " - Total Glasses: ", length(file_list)))
q