library("readr")
ref<-read_csv("~/Dropbox/AfSIS_MASTER_FILES/VS/VS_combined/data/All_vs_reference_data.csv")
pred<-read_csv("~/Dropbox/AfSIS_MASTER_FILES/VS/VS_combined/data/All Predictions.csv")
rang1=round(apply(na.omit(ref[,-1]),2,range),1)
rrr<-NULL
for ( i in 1:ncol(rang1)){
	rr<-paste(rang1[1,i],rang1[2,i],sep="-")
	rrr<-c(rrr,rr)
}

rang2=round(apply(na.omit(pred[,-1]),2,range),1)
rrr2<-NULL
for ( i in 1:ncol(rang2)){
	rr2<-paste(rang2[1,i],rang2[2,i],sep="-")
	rrr2<-c(rrr2,rr2)
}

cbind(colnames(ref[,-1]),rrr,rrr2)
ng1=round(apply(na.omit(ref[,-1]),2,range),1)
