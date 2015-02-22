#-----Load Libraries
library(ggplot2)


#-----Set working directory
setwd("W:/Результаты/7_AOI/AOI_1/02_2015")
#-----Read data
file_list <- list.files(pattern="_2015_02_16_")

mydata = read.delim(unz(file_list[1],"defects.tsv"), header=T)

for (i in 2:length(file_list))
{
        mydata=rbind(mydata, read.delim(unz(file_list[i],"defects.tsv"), header=T))
}

#-----Plot data with ggplot2
p<-ggplot(
        data=mydata, aes(
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
p <- p + ggtitle(paste("Data: ",getwd(), " - Glasses: ", length(file_list)))

p