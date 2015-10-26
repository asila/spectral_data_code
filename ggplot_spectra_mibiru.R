library(reshape2)
library(ggplot2)
library(readr)

spec <- read_csv("~/Dropbox/AfSIS_MASTER_FILES/Mubiru/plant/alpha_Uganda-NARO flat table plants.csv")
#Ensure spectra starts from column 2.
wave<-as.numeric(substr(colnames(spec[,-c(1)]),2,19))
colnames(spec) <- c("SSN",wave)
spec.melt<-melt(spec[1:20,],id=c("SSN"))

#Version 2
p<-ggplot(data=spec.melt, aes(x=as.numeric(as.vector(variable)),y=value,group=SSN))+
    geom_line(size=0.34,aes(col=as.numeric(variable)))+scale_colour_gradientn(colours=c("green4","yellow2","greenyellow","yellow2"))+
    ggtitle("Raw MIR spectra")+
    xlim(c(4000,600))+
    ylim(c(0,5))+ 
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
png(file="~/Dropbox/AfSIS_MASTER_FILES/Mubiru/plant/Plants raw spectra.png")
p
dev.off()

spec <- read.csv("~/Training/Ghana/MIR and X-ray spectroscopy training/Kumasi_kwandaso_raw_spectra.csv")[,-c(2:4)]
strsplit(as.vector(spec[,1]),".",fixed=TRUE,perl=FALSE)
#Ensure spectra starts from column 2.
wave<-as.numeric(substr(colnames(spec[,-c(1)]),2,19))
colnames(spec) <- c("SSN",wave)
spec.melt<-melt(spec[1:20,],id=c("SSN"))
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
png(file="~/Dropbox/AfSIS_MASTER_FILES/Mubiru/soil/Soil raw spectra.png")
p
dev.off()


