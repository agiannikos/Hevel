
plotBoxPlot_with_density <- function(plotData, X_Variable, Y_Variable, yAxisLabel, xAxisLabel, 
                                     opt_y_axisMin=0, opt_y_axisMax=NULL, opt_plotBinWidth=0.05, opt_densityPlotLabel="Density",
                                     opt_HLA=NULL, opt_HHLA=NULL, opt_LLA=NULL,opt_LLLA=NULL,opt_SP=NULL){
        
        axis_label_font_size <- 12
        axis_ticks_label_size <- 10
        axis_ticks_label_angle <- 45
        plot_line_width <- 0.2
        plot_line_width_multi <- 2
        
        if (is.null(opt_y_axisMax)){
                opt_y_axisMax=ceiling(max(plotData[,Y_Variable],opt_HLA,opt_HHLA))
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
        gcDensityPlot <- gcDensityPlot + stat_bin(binwidth = opt_plotBinWidth, colour = "black", fill= "grey",lwd=plot_line_width) 
        
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

plotBoxPlot_with_density_line <- function(plotData, X_Variable, Y_Variable, yAxisLabel, xAxisLabel, 
                                     opt_y_axisMin=0, opt_y_axisMax=NULL, opt_plotBinWidth=0.05, opt_densityPlotLabel="Density",
                                     opt_HLA=NULL, opt_HHLA=NULL, opt_LLA=NULL,opt_LLLA=NULL,opt_SP=NULL){
        
        axis_label_font_size <- 12
        axis_ticks_label_size <- 10
        axis_ticks_label_angle <- 45
        plot_line_width <- 0.2
        plot_line_width_multi <- 2
        
        if (is.null(opt_y_axisMax)){
                opt_y_axisMax=ceiling(max(plotData[,Y_Variable],opt_HLA,opt_HHLA))
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