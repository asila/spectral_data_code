library(soil.spec)
library(readr)

#Read OFRA 2 MIR data
afs<-read_csv("~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_2combine/combine mir soil.csv")
ref<-read.csv("~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_2combine/Actual_reference_data.csv")
codes<-read.csv("~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_2combine/4 sites all predicted.csv")[,c("SSN","Country","Site")]

str(codes)
afsn<-afs[,-c(1,1766)]
colnames(afsn)<-as.numeric(substr(colnames(afsn),2,16))

#Obtain derivatives
afsn<-as.matrix(afsn)
de.s<-trans(afsn,tr="derivative",order=1,gap=21)
der.s<-as.matrix(de.s$trans)


#########Run PCA#################################
pc<-prcomp(der.s)
imp<-summary(pc)$importance
pcs<-pc$x[,1:10]
pcs.ssn<-cbind(as.vector(afs[,1]),pcs)
colnames(pcs.ssn)<-c("SSN",colnames(pcs))
ypc<-merge(codes,pcs.ssn)
write.table(ypc,file="~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_2combine/pc scores plus codes.csv",sep=",",row.names=FALSE)
ypc<-read.csv("~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_2combine/pc scores plus codes.csv")

k<-which(ypc$SSN%in%ref$SSN)
png(file="~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_2combine/Calibration and Prediction.png",width=600,height=600)
plot(ypc[,4:5],col="blue",pch="",main="PCA scores for soil spectra",cex.lab=1.2,xlab=paste0("PC1 explains ", round(imp[2,1],3)*100, " % total variation"),ylab=paste0("PC2 explains ", round(imp[2,2],3)*100, " % total variation"))
points(ypc[-k,4:5],col="blue",pch=19)
points(ypc[k,4:5],col="red",pch=19)
legend("topleft",pch=19,col=c("blue","red"),c("Prediction","Calibration"),bty="n",text.col=c("blue","red"))
dev.off()

#Show by site 
ref1<-subset(codes,codes$Site=="Uganda")
k1<-which(ypc$SSN%in%ref1$SSN)
ref2<-subset(codes,codes$Site=="Rwanda")
k2<-which(ypc$SSN%in%ref2$SSN)
ref3<-subset(codes,codes$Site=="Nigeria")
k3<-which(ypc$SSN%in%ref3$SSN)
png(file="~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_2combine/PCA by sites.png",width=600,height=600)
plot(ypc[,4:5],col="blue",pch="",main="PCA scores for soil spectra by site",cex.lab=1.2,xlab=paste0("PC1 explains ", round(imp[2,1],3)*100, " % total variation"),ylab=paste0("PC2 explains ", round(imp[2,2],3)*100, " % total variation"))
points(ypc[k1,4:5],col="blue",pch=19)
points(ypc[k2,4:5],col="red",pch=19)
points(ypc[k3,4:5],col="purple",pch=19)

legend("topleft",pch=19,col=c("blue","red","purple"),c("Uganda","Rwanda","Nigeria"),bty="n",text.col=c("blue","red","purple"))
dev.off()

############ Plants ####################
#Read OFRA 2 MIR data
afs<-read_csv("~/Dropbox/AfSIS_MASTER_FILES/ofra/OFRA_plant samples/U&N_plant mir data.csv")
ref<-read.csv("~/Dropbox/AfSIS_MASTER_FILES/ofra/OFRA_plant samples/Ref_uganda_Nigeria.csv")
codes<-read.csv("~/Dropbox/AfSIS_MASTER_FILES/ofra/OFRA_plant samples/samplecodes.csv")[,c("SSN","Country","Site")]

str(codes)
afsn<-afs[,-c(1,1766)]
colnames(afsn)<-as.numeric(substr(colnames(afsn),2,16))

#Obtain derivatives
afsn<-as.matrix(afsn)
de.s<-trans(afsn,tr="derivative",order=1,gap=21)
der.s<-as.matrix(de.s$trans)


#########Run PCA#################################
pc<-prcomp(der.s)
imp<-summary(pc)$importance
pcs<-pc$x[,1:10]
pcs.ssn<-cbind(as.vector(afs[,1]),pcs)
colnames(pcs.ssn)<-c("SSN",colnames(pcs))
ypc<-merge(codes,pcs.ssn)
write.table(ypc,file="~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_Plant samples/pc scores plus codes.csv",sep=",",row.names=FALSE)
ypc<-read.csv("~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_Plant samples/pc scores plus codes.csv")

k<-which(ypc$SSN%in%ref$SSN)
png(file="~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_Plant samples/Calibration and Prediction by site.png",width=1200,height=600)
par(mfrow=c(1,2))
plot(ypc[,4:5],col="yellowgreen",pch="",main="PCA scores for foliar spectra",cex.lab=1.2,xlab=paste0("PC1 explains ", round(imp[2,1],3)*100, " % total variation"),ylab=paste0("PC2 explains ", round(imp[2,2],3)*100, " % total variation"))
points(ypc[-k,4:5],col="yellowgreen",pch=19)
points(ypc[k,4:5],col="brown",pch=19)
legend("topleft",pch=19,col=c("yellowgreen","brown"),c("Prediction","Calibration"),bty="n",text.col=c("yellowgreen","brown"))
#dev.off()

#Show by site 
ref1<-subset(codes,codes$Site=="Uganda")
k1<-which(ypc$SSN%in%ref1$SSN)

ref3<-subset(codes,codes$Site=="Nigeria")
k3<-which(ypc$SSN%in%ref3$SSN)
#png(file="~/Dropbox/AfSIS_MASTER_FILES/ofra/ofra_Plant samples/PCA by sites.png",width=600,height=600)
plot(ypc[,4:5],col="blue",pch="",main="PCA scores for foliar spectra by site",cex.lab=1.2,xlab=paste0("PC1 explains ", round(imp[2,1],3)*100, " % total variation"),ylab=paste0("PC2 explains ", round(imp[2,2],3)*100, " % total variation"))
points(ypc[k1,4:5],col="blue",pch=19)
points(ypc[k2,4:5],col="red",pch=19)
points(ypc[k3,4:5],col="purple",pch=19)

legend("topleft",pch=19,col=c("blue","purple"),c("Uganda","Nigeria"),bty="n",text.col=c("blue","purple"))
dev.off()

