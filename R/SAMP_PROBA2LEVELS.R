SAMP_PROBA2LEVELS <-
function(X,PROBA,Y,ALPHA,choice_opt,param_opt=NULL){
n2lev=length(Y)
Ysamp<-NULL
Ysampmin<-NULL
Ysampmax<-NULL
Xsamp<-NULL
noptim<-NULL
for (i in 1:n2lev){
	if (Y[[i]]$type=="proba"){
		Ysamp[i]=SAMP_PROBA(Y[[i]],ALPHA[i])
	} else if (Y[[i]]$type=="fixed") {
		Ysamp[i]=Y[[i]]$param
	} else if (Y[[i]]$type=="possi") {
		noptim=c(noptim,i)
		TEMP=SAMP_POSSI(Y[[i]],ALPHA[i])
		Ysamp[i]=0
		Ysampmin=c(Ysampmin,TEMP[1])
		Ysampmax=c(Ysampmax,TEMP[2])
	}
}

if (length(noptim)==0){
	Xsamp=do.call(X$qfun,as.list(c(PROBA,Ysamp)))
	return (c(Xsamp,Xsamp))
} else {
	for (q in 1:2){
		PROBLAW<-function(Z){
		Ysamp[noptim]=Z
		return ((-2*q+3)*do.call(X$qfun,as.list(c(PROBA,Ysamp))))
		}
		Xsamp[q]=(-2*q+3)*OPTIMIZ(choice_opt,Ysampmin,Ysampmax,PROBLAW,param_opt)
	}
return(Xsamp)
}
}
