
opt_densityPlotLabel="Density"
axis_label_font_size <- 12
axis_ticks_label_size <- 10
axis_ticks_label_angle <- 45
plot_line_width <- 0.2
plot_line_width_multi <- 2
opt_y_axisMin <- 0
opt_y_axisMax <- 1300
opt_x_axisMin <- 0
opt_x_axisMax <- 1100


#-----Plot data with ggplot2
splot <- ggplot(data=dat, aes(x=X.Position, y=Y.Position, colour=Defect.Class, alpha=1/50, shape=Layer))
splot <- splot + layer(geom="point")
splot <- splot + xlab("X")+ylab("Y")
#splot <- splot + coord_fixed(ratio=1)
#splot <- splot + xlim(1100,0)+ylim(1300,0)
splot <- splot + theme(legend.position = "none")
#p <- p + ggtitle(paste("Data: ",getwd(), " - Glasses: ", length(file_list)," - Date:",date_for_plot)) + theme(plot.title=element_text(size=12))

#         g_legend<-function(a.gplot){
#                 tmp <- ggplot_gtable(ggplot_build(a.gplot))
#                 leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
#                 legend <- tmp$grobs[[leg]]
#                 return(legend)
#         }
#         
#         legend <- g_legend(splot)

tdplot <- ggplot(data = dat, aes(x = X.Position))
tdplot <- tdplot + geom_density(fill= "grey", colour = "dark grey", lwd=plot_line_width) 
tdplot <- tdplot + 
        scale_x_continuous(expand=c(0.02,0), labels=NULL, limits = c(opt_x_axisMin, opt_x_axisMax)) + 
        scale_y_continuous(expand=c(0.02,0),labels=NULL) 
tdplot <- tdplot + xlab(NULL) + ylab(opt_densityPlotLabel)
tdplot <- tdplot + theme(plot.margin=unit(c(1,1,1,1),"cm"))

#         tdplot <- tdplot + theme(axis.title=element_text(size=axis_label_font_size,face="bold"), 
#                                                axis.text.x = element_text(angle = axis_ticks_label_angle, hjust = 1, size=axis_ticks_label_size),
#                                                axis.ticks.y=element_blank(),
#                                                plot.margin=unit(c(1,1,1,-0.15), "cm"))

rdplot <- ggplot(data = dat, aes(x = Y.Position))
rdplot <- rdplot + geom_density(fill= "grey", colour = "dark grey", lwd=plot_line_width) 
rdplot <- rdplot + 
        scale_x_continuous(expand=c(0.02,0), labels=NULL, limits = c(opt_y_axisMin,opt_y_axisMax)) + 
        scale_y_continuous(expand=c(0.02,0),labels=NULL)
rdplot <- rdplot + xlab(NULL) + ylab(opt_densityPlotLabel)
rdplot <- rdplot + theme(plot.margin=unit(c(1,1,1,1), "cm")) + coord_flip()

gsplot <- ggplotGrob(splot)
gtdplot <- ggplotGrob(tdplot)
grdplot <- ggplotGrob(rdplot)

maxGridWidths  <-  grid::unit.pmax(gsplot$widths[2:5],gtdplot$widths[2:5])

splot$widths[2:5] <- as.list(maxGridWidths)
tdplot$widths[2:5] <- as.list(maxGridWidths)        
gsplot$

plot <- arrangeGrob(gtdplot,  gsplot, ncol=1, widths=c(3,1), heights=c(1,3))
plot

