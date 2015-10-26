library(soil.spec)
library(readr)

#Read OFRA 2 MIR data
#set working directory
setwd("~/Models/aFSIS/data")
#set name of new spectra
new <- "VS"
afs<-read_csv("~/Dropbox/AfSIS_reporting_data/Seperated_datasets/Calibration_Htsxt_MIR.csv")
new<-read_csv("~/Dropbox/AfSIS_MASTER_FILES/VS/VS_combined/data/VS_MIR_spectra.csv")
#rename new colnames
#new spectra starts which column?
hd.0<-as.vector(colnames(new))
k0<-menu(hd.0,graphics=TRUE,title="Select where column names with spectra begins")
wave.0 <-  paste0("m",round(as.numeric(substr(colnames(new[,2:length(hd.0)]),2,19)),1))
colnames(new) <-c("SSN",wave.0)
#Getting matching names in the two tables and rbind
p.0<-which(colnames(new)%in%colnames(afs))
afsn.0<-rbind(afs,new[,p.0])

#Store the file
write.table(afsn.0,file="afsis plus new spectra.csv",sep=",",row.names=FALSE)

z<-ncol(afsn)
afsn<-afsn.0[,-c(1,z)]
colnames(afsn)<-as.numeric(substr(colnames(afsn),2,16))

#Obtain derivatives
afsn<-as.matrix(afsn)
de.s<-trans(afsn,tr="derivative",order=1,gap=21)
der.s<-as.matrix(de.s$trans)


#########Run PCA#################################
pc<-prcomp(der.s)
imp<-summary(pc)$importance
pcs<-pc$x[,1:10]
pcs.ssn<-cbind(as.vector(afsn[,1]),pcs)
colnames(pcs.ssn)<-c("SSN",colnames(pcs))
ypc<-as.data.frame(pcs.ssn)
write.table(ypc,file="afsis pc scores plus new spectra.csv",sep=",",row.names=FALSE)
ypc<-read.csv("afsis pc scores plus new spectra.csv")

png(file=paste0("~/Dropbox/AfSIS_MASTER_FILES/VS/VS_combined/figs/AfSIS and", new,".png"),width=600,height=600)
par(mfrow=c(1,1))
plot(ypc[,2:3],col="blue",pch="",main="PCA scores for afsis and new soil spectra",cex.lab=1.2,xlab=paste0("PC1 explains ", round(imp[2,1],3)*100, " % total variation"),ylab=paste0("PC2 explains ", round(imp[2,2],3)*100, " % total variation"))
k<-nrow(afs)
points(ypc[1:k,2:3],col="blue",pch=19)
points(ypc[-c(1:k),2:3],col="red",pch=19)
legend("bottomright",pch=19,col=c("blue","red"),c("AfSIS",new),bty="n",text.col=c("blue","red"))
dev.off()
