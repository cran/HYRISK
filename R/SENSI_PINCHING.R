SENSI_PINCHING <-
function(Z0,Z0p,mode="global",threshold=NULL,level=NULL,disc=0.01){
if (mode=="global"){
	unc0<-UNCERTAINTY(Z0,disc)
	unc<-UNCERTAINTY(Z0p,disc)
} else if (mode=="proba"){
	p0<-PROBA_INTERVAL(Z0,threshold)
	p<-PROBA_INTERVAL(Z0p,threshold)
	unc0<-p0$Pupp-p0$Plow
	unc<-p$Pupp-p$Plow
} else if (mode=="quantile"){
	q0<-QUAN_INTERVAL(Z0,level)
	q<-QUAN_INTERVAL(Z0p,level)
	unc0<-q0$Qupp-q0$Qlow
	unc<-q$Qupp-q$Qlow
}
sensi<-100*(1-unc/unc0)
return(sensi)
}
