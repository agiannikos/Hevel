library(ggplot2)
library(gridExtra)
library(lazyeval)
library(stringr)

source("./AOI_FUNCTIONS.R")


##--This function creates a boxplot and a density plot----#
#---for the given variables

plotBoxPlot_with_density <- function(plotData, X_Variable, Y_Variable, yAxisLabel, xAxisLabel, 
                                     opt_y_axisMin=NULL, opt_y_axisMax=NULL, opt_plotBinWidth=NULL, opt_densityPlotLabel="Density",
                                     opt_HLA=NULL, opt_HHLA=NULL, opt_LLA=NULL,opt_LLLA=NULL,opt_SP=NULL){
        
        axis_label_font_size <- 12
        axis_ticks_label_size <- 10
        axis_ticks_label_angle <- 45
        plot_line_width <- 0.2
        plot_line_width_multi <- 2
        
        if (is.null(opt_y_axisMax)){
                opt_y_axisMax=ceiling(max(plotData[,Y_Variable],opt_HLA,opt_HHLA))
        }
        
        if (is.null(opt_y_axisMin)){
                opt_y_axisMin=floor(min(plotData[,Y_Variable],opt_LLA,opt_LLLA))
        }
        
        gcPlot <- ggplot(data = plotData, aes_string(x = X_Variable, y=Y_Variable))
        
        if (!is.null(opt_HLA)) gcPlot <- gcPlot + geom_hline(yintercept=opt_HLA, color="yellow",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_LLA)) gcPlot <- gcPlot + geom_hline(yintercept=opt_LLA, color="yellow",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_HHLA)) gcPlot <- gcPlot + geom_hline(yintercept=opt_HHLA, color="red",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_LLLA)) gcPlot <- gcPlot + geom_hline(yintercept=opt_LLLA, color="red",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_SP)) gcPlot <- gcPlot + geom_hline(yintercept=opt_SP, color="green",lwd=plot_line_width*plot_line_width_multi)
        gcPlot <- gcPlot + geom_boxplot(outlier.colour = "red",lwd=plot_line_width)
        
        gcPlot <- gcPlot + xlab(xAxisLabel) + ylab(yAxisLabel)
        gcPlot <- gcPlot + scale_x_discrete(expand=c(0.02,0)) + scale_y_continuous(expand=c(0.02,0), limits = c(opt_y_axisMin,opt_y_axisMax))
        gcPlot <- gcPlot + theme(axis.title=element_text(size=axis_label_font_size,face="bold"), 
                                 axis.text.x = element_text(angle = axis_ticks_label_angle, hjust = 1, size=axis_ticks_label_size),
                                 axis.text.y = element_text(size=axis_ticks_label_size),
                                 plot.margin=unit(c(0.5,0,0.5,0.5), "cm")
        )
        #gcPlot
        gciGrob  <- ggplotGrob(gcPlot)
        
        
        #http://stackoverflow.com/questions/17370460/scatterplot-with-alpha-transparent-histograms-in-r
        
        gcDensityPlot  <- ggplot(data=plotData, aes_string(x = Y_Variable))  
        
        if (!is.null(opt_HLA)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_HLA, color="yellow",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_LLA)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_LLA, color="yellow",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_HHLA)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_HHLA, color="red",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_LLLA)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_LLLA, color="red",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_SP)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_SP, color="green",lwd=plot_line_width*plot_line_width_multi)
        gcDensityPlot <- gcDensityPlot + stat_bin(binwidth = opt_plotBinWidth, colour = "black", fill= "grey",lwd=plot_line_width) 
        
        gcDensityPlot <- gcDensityPlot + scale_x_continuous(expand=c(0.02,0), labels=NULL, limits = c(opt_y_axisMin,opt_y_axisMax)) + scale_y_continuous(expand=c(0.02,0))
        gcDensityPlot  <- gcDensityPlot + xlab(NULL) + ylab(opt_densityPlotLabel)  
        gcDensityPlot <- gcDensityPlot + theme(axis.title=element_text(size=axis_label_font_size,face="bold"), 
                                               axis.text.x = element_text(angle = axis_ticks_label_angle, hjust = 1, size=axis_ticks_label_size),
                                               axis.ticks.y=element_blank(),
                                               plot.margin=unit(c(0.5,0.5,0.5,-0.15), "cm")
        ) + coord_flip()
        
        
        
        #gcDensityPlot 
        gciDensityGrob  <- ggplotGrob(gcDensityPlot)
        
        #http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot
        maxGridHeigth  <-  grid::unit.pmax(gciDensityGrob$heights[2:5],gciGrob$heights[2:5])
        
        gciGrob$heights[2:5] <- as.list(maxGridHeigth)
        gciDensityGrob$heights[2:5] <- as.list(maxGridHeigth)
        
        plot <- arrangeGrob(gciGrob,gciDensityGrob,ncol=2,nrow=1, widths=c(6,2), heigths=c(2,6))
        
        return(plot)
        
}

plotBoxPlot_with_density_line <- function(plotData, X_Variable, Y_Variable, yAxisLabel, xAxisLabel, 
                                          opt_y_axisMin=NULL, opt_y_axisMax=NULL, opt_plotBinWidth=NULL, opt_densityPlotLabel="Density",
                                          opt_HLA=NULL, opt_HHLA=NULL, opt_LLA=NULL,opt_LLLA=NULL,opt_SP=NULL){
        
        axis_label_font_size <- 12
        axis_ticks_label_size <- 10
        axis_ticks_label_angle <- 45
        plot_line_width <- 0.2
        plot_line_width_multi <- 2
        
        if (is.null(opt_y_axisMax)){
                opt_y_axisMax=ceiling(max(plotData[,Y_Variable],opt_HLA,opt_HHLA))
        }
        
        if (is.null(opt_y_axisMin)){
                opt_y_axisMin=floor(min(plotData[,Y_Variable],opt_LLA,opt_LLLA))
        }
        
        gcPlot <- ggplot(data = plotData, aes_string(x = X_Variable, y=Y_Variable))
        
        if (!is.null(opt_HLA)) gcPlot <- gcPlot + geom_hline(yintercept=opt_HLA, color="yellow",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_LLA)) gcPlot <- gcPlot + geom_hline(yintercept=opt_LLA, color="yellow",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_HHLA)) gcPlot <- gcPlot + geom_hline(yintercept=opt_HHLA, color="red",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_LLLA)) gcPlot <- gcPlot + geom_hline(yintercept=opt_LLLA, color="red",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_SP)) gcPlot <- gcPlot + geom_hline(yintercept=opt_SP, color="green",lwd=plot_line_width*plot_line_width_multi)
        gcPlot <- gcPlot + geom_boxplot(outlier.colour = "red",lwd=plot_line_width)
        
        gcPlot <- gcPlot + xlab(xAxisLabel) + ylab(yAxisLabel)
        gcPlot <- gcPlot + scale_x_discrete(expand=c(0.02,0)) + scale_y_continuous(expand=c(0.02,0), limits = c(opt_y_axisMin,opt_y_axisMax))
        gcPlot <- gcPlot + theme(axis.title=element_text(size=axis_label_font_size,face="bold"), 
                                 axis.text.x = element_text(angle = axis_ticks_label_angle, hjust = 1, size=axis_ticks_label_size),
                                 axis.text.y = element_text(size=axis_ticks_label_size),
                                 plot.margin=unit(c(1,0,1,1), "cm")
        )
        #gcPlot
        gciGrob  <- ggplotGrob(gcPlot)
        
        
        #http://stackoverflow.com/questions/17370460/scatterplot-with-alpha-transparent-histograms-in-r
        
        gcDensityPlot  <- ggplot(data=plotData, aes_string(x = Y_Variable))  
        
        if (!is.null(opt_HLA)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_HLA, color="yellow",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_LLA)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_LLA, color="yellow",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_HHLA)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_HHLA, color="red",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_LLLA)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_LLLA, color="red",lwd=plot_line_width*plot_line_width_multi)
        if (!is.null(opt_SP)) gcDensityPlot <- gcDensityPlot + geom_vline(xintercept=opt_SP, color="green",lwd=plot_line_width*plot_line_width_multi)
        gcDensityPlot <- gcDensityPlot + geom_density(colour = "black", fill= "grey",lwd=plot_line_width) 
        
        gcDensityPlot <- gcDensityPlot + scale_x_continuous(expand=c(0.02,0), labels=NULL, limits = c(opt_y_axisMin,opt_y_axisMax)) + scale_y_continuous(expand=c(0.02,0))
        gcDensityPlot  <- gcDensityPlot + xlab(NULL) + ylab(opt_densityPlotLabel)  
        gcDensityPlot <- gcDensityPlot + theme(axis.title=element_text(size=axis_label_font_size,face="bold"), 
                                               axis.text.x = element_text(angle = axis_ticks_label_angle, hjust = 1, size=axis_ticks_label_size),
                                               axis.ticks.y=element_blank(),
                                               plot.margin=unit(c(1,1,1,-0.15), "cm")
        ) + coord_flip()
        
        
        
        #gcDensityPlot 
        gciDensityGrob  <- ggplotGrob(gcDensityPlot)
        
        #http://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot
        maxGridHeigth  <-  grid::unit.pmax(gciDensityGrob$heights[2:5],gciGrob$heights[2:5])
        
        gciGrob$heights[2:5] <- as.list(maxGridHeigth)
        gciDensityGrob$heights[2:5] <- as.list(maxGridHeigth)
        
        #         plot <- grid.arrange(arrangeGrob(gciGrob,gciDensityGrob,ncol=2,nrow=1, widths=c(6,2), heigths=c(2,6)))
        plot <- arrangeGrob(gciGrob,gciDensityGrob,ncol=2,nrow=1, widths=c(6,2), heigths=c(2,6))
        
        
        print(paste("plot Class:",class(plot)))
        
        return(plot)
        
}

getPalletsPerDates <- function(pirData,fromDate, toDate){
        pallets <- pirData %>%
                filter(as.Date(GLS_TIME_END,"%m.%d.%Y %H:%M:%S")>=as.Date(fromDate,"%m.%d.%Y"),
                       as.Date(GLS_TIME_END,"%m.%d.%Y %H:%M:%S")<=as.Date(toDate,"%m.%d.%Y")
                ) %>%
                distinct(GLS_BATCH_NR) %>%
                select(GLS_BATCH_NR)
        return(pallets)     
}

getSubstratesPerToolForGivenPalletIDs <- function(pirData,palletIDs,toolColumn ,toolName){
        
        toolCriteria <- interp(~column==tool, tool=toolName, column=as.name(toolColumn)) #lazyeval library
        substrates <- pirData %>%
                filter_(~GLS_BATCH_NR %in% palletIDs, toolCriteria) 
        
        return(substrates)
                
}

getSubstratesBalanceForGivenPalletIDS <- function(pirData, palletIDs_df){
        
        palletArray  <- as.character(palletIDs_df[,1])
        balance <- pirData %>%
                filter(GLS_BATCH_NR %in% palletArray) %>%
                select(SUBSTRATE_ID, GLS_BATCH_NR, GLS_TIME_END, GLS_TOOL, GLS_RESULT, GCI_TIME_END, GCI_TOOL, GCI_RESULT, GMS_TIME_END, GMS_TOOL, GMS_RESULT, AOI_TIME_END, AOI_TOOL, AOI_RESULT)
        
        gls <- balance %>% 
                select(PALLET_ID=GLS_BATCH_NR, SUBSTRATE_ID, TOOL_TIME_END=GLS_TIME_END, TOOL=GLS_TOOL, TOOL_RESULT=GLS_RESULT) %>%        
                mutate(TOOL_CAT=str_extract(balance$GLS_TOOL,"[A-Z]{3}"), LOADING_LINE=str_extract(balance$GLS_TOOL,"[0-2]+$"))        
        
        gci <- balance %>% 
                select(PALLET_ID=GLS_BATCH_NR, SUBSTRATE_ID, TOOL_TIME_END=GCI_TIME_END, TOOL=GCI_TOOL, TOOL_RESULT=GCI_RESULT) %>%       
                mutate(TOOL_CAT=str_extract(balance$GCI_TOOL,"[A-Z]{3}"), LOADING_LINE=str_extract(balance$GCI_TOOL,"[0-2]+$"))
        
        gms <- balance %>% 
                select(PALLET_ID=GLS_BATCH_NR, SUBSTRATE_ID, TOOL_TIME_END=GMS_TIME_END, TOOL = GMS_TOOL, TOOL_RESULT = GMS_RESULT) %>%       
                mutate(TOOL_CAT=str_extract(balance$GMS_TOOL,"[A-Z]{3}"), LOADING_LINE=str_extract(balance$GMS_TOOL,"[0-2]+$"))
        
        aoi <- balance %>% 
                select(PALLET_ID=GLS_BATCH_NR, SUBSTRATE_ID, TOOL_TIME_END=AOI_TIME_END, TOOL = AOI_TOOL, TOOL_RESULT = AOI_RESULT) %>%       
                mutate(TOOL_CAT=str_extract(balance$AOI_TOOL,"[A-Z]{3}"), LOADING_LINE=str_extract(balance$AOI_TOOL,"[0-2]+$"))
        
        balanceF <- bind_rows(gls,gci,gms,aoi) %>%
                group_by(PALLET_ID, LOADING_LINE, TOOL, TOOL_RESULT) %>%
                summarise(SUBSTRATES=n()) 
        
        return(balance)
        
}


getAOICumulativePlotsWithDensityPlots <- function(pirData, aoiTool, fromDate, toDate, aoiFilesList){
        
        opt_densityPlotLabel="Density"
        axis_label_font_size <- 12
        axis_ticks_label_size <- 10
        axis_ticks_label_angle <- 45
        plot_line_width <- 0.2
        plot_line_width_multi <- 2
        opt_y_axisMin <- 0
        opt_y_axisMin <- 1300
        opt_x_axisMin <- 0
        opt_x_axisMin <- 1100
        
        
        listOfSubstratesFromPir <- getGlassesFromMultiplePallets_by_dates(pirData = pirData, 
                                                                          fromDate = fromDate, 
                                                                          toDate = toDate, 
                                                                          aoiTool = aoiTool)
        
        
        defectsPerGlass <- getSubstratesDataFromZip(file_list =aoiFilesList,
                                                    pirSubstateListPerPallet = listOfSubstratesFromPir 
        ) %>%
                select(BATCH_ID,SUBSTRATE_ID,Defect.Class:MONTH,-Images.Reflection)
        
        #-----Plot data with ggplot2
        p<-ggplot(data=defectsPerGlass, aes(x=X.Position, y=Y.Position, colour=Defect.Class, alpha=1/50, shape=Layer))
        
        p <- p+layer(geom="point")
        p <- p+xlab("X")+ylab("Y")
        p <- p + coord_fixed(ratio=1)
        p <- p + xlim(1100,0)+ylim(1300,0)
        #p <- p + ggtitle(paste("Data: ",getwd(), " - Glasses: ", length(file_list)," - Date:",date_for_plot)) + theme(plot.title=element_text(size=12))
        
        dplot <- ggplot(data = defectsPerGlass, aes(x = X.Position))
        dplot <- dplot + geom_density(colour = "black", fill= "grey",lwd=plot_line_width) 
#         dplot <- dplot + scale_x_continuous(expand=c(0.02,0), labels=NULL, limits = c(opt_y_axisMin,opt_y_axisMax)) + scale_y_continuous(expand=c(0.02,0))
#         dplot <- dplot + xlab(NULL) + ylab(opt_densityPlotLabel)  
#         dplot <- dplot + theme(axis.title=element_text(size=axis_label_font_size,face="bold"), 
#                                                axis.text.x = element_text(angle = axis_ticks_label_angle, hjust = 1, size=axis_ticks_label_size),
#                                                axis.ticks.y=element_blank(),
#                                                plot.margin=unit(c(1,1,1,-0.15), "cm"))
                                               
        return(dplot)
        
        
}