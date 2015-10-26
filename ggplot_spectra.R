library(reshape2)
library(ggplot2)
library(readr)
setwd("~/trash")
spec <- read_csv("~/Dropbox/AfSIS_MASTER_FILES/VS/VS_combined/data/VS_MIR_spectra.csv")
#Ensure spectra starts from column 2.

wave<-as.numeric(substr(colnames(spec[,-c(1)]),2,19))
colnames(spec) <- c("SSN",wave)
spec.melt<-melt(spec[1:20,],id=c("SSN"))

#By spectra
p<-ggplot(data=spec.melt, aes(x=as.numeric(as.vector(variable)),y=value,group=SSN))+
    geom_line(size=0.34,aes(col=as.numeric(variable)))+scale_colour_gradient(high="red",low="green")+
    ggtitle("Raw MIR spectra")+
    xlim(c(4000,600))+
    ylim(c(0,3))+ 
    xlab(expression("Wavenumbers cm"^-1))+
    ylab("Absorbance")+
    #theme with white background
    theme_bw() +
    #eliminates background, gridlines, and chart border
    theme(
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
    )
p<-p+theme(legend.position = "none")
p

#print the plot in png file
png(file="~/Downloads/KWS_Marsabit/Raw spectra.png")
p
dev.off()

#Version 2
p<-ggplot(data=spec.melt, aes(x=as.numeric(as.vector(variable)),y=value,group=SSN))+
    geom_line(size=0.34,aes(col=as.numeric(variable)))+scale_colour_gradientn(colours=c("blue1","red"))+
    ggtitle("Raw MIR spectra")+
    xlim(c(4000,600))+
    ylim(c(0,3))+ 
    xlab(expression("Wavenumbers cm"^-1))+
    ylab("Absorbance")+
    #theme with white background
    theme_bw() +
    #eliminates background, gridlines, and chart border
    theme(
        plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
    )
p<-p+theme(legend.position = "none")
p

#print the plot in png file
png(file="~/Downloads/KWS_Marsabit/Raw spectra_v2.png")
p
dev.off()