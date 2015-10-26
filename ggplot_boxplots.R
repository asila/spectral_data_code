library(ggplot2)
pred<-read.csv("~/Dropbox/AfSIS_MASTER_FILES/KWS_Marsabit/KWS Marsabit All Predictions.csv")
pred$Cluster <- as.factor(pred$Cluster)
ph<-ggplot(pred,aes(x=Cluster,y=pH,dodge=Cluster,ylab=""))+geom_boxplot(width=0.3,noth=TRUE)
ph<-ph + labs(x="Sampling Clusters",y="pH",title="Soil pH levels over sampling clusters")
ph<-ph + theme(text = element_text(siz=18))
png("~/Dropbox/AfSIS_MASTER_FILES/KWS_Marsabit/figs/pH.png")
ph
dev.off()
pg<-ggplot(pred,aes(x=Cluster,y=Total.Carbon,ylab=""))+geom_boxplot(width=0.3,noth=TRUE)
pg<-pg + labs(x="Sampling Clusters",y="Total Carbon (%)",title="Soil Total Carbon distribution over sampling clusters")
pg<-pg + theme(text = element_text(siz=18))

png("~/Dropbox/AfSIS_MASTER_FILES/KWS_Marsabit/figs/t.carbon.png")
pg
dev.off()
pc<-ggplot(pred,aes(x=Cluster,y=Clay,ylab=""))+geom_boxplot(width=0.3,noth=TRUE)
pc<-pc + labs(x="Sampling Clusters",y="Clay (%)",title="Clay content distribution over sampling clusters")
pc<-pc + theme(text = element_text(siz=18))
png("~/Dropbox/AfSIS_MASTER_FILES/KWS_Marsabit/figs/Clay.png")
pc
dev.off()

ps<-ggplot(pred,aes(x=Cluster,y=Sand,ylab=""))+geom_boxplot(width=0.3,noth=TRUE)
ps<-ps + labs(x="Sampling Clusters",y="Sand (%)",title="Sand content distribution over sampling clusters")
ps<-ps + theme(text = element_text(siz=18))

png("~/Dropbox/AfSIS_MASTER_FILES/KWS_Marsabit/figs/Sand.png")
ps
dev.off()