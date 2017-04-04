PROPAG_fun <-
function(rr,N,INP,FUN,choice_opt,param_opt=NULL){
Z<-NULL
for (q in 1:2){
	Xr<-NULL
	optim<-NULL
	ndunno<-NULL
	updunno<-NULL
	lodunno<-NULL
	midunno<-NULL
	for (i in 1:length(INP)){
		if ((INP[[i]]$type=="possi")){
			if (INP[[i]]$monoton=="incr"){
				Xr[i]=SAMP_POSSI(INP[[i]],rr[i])[q]
			} else if (INP[[i]]$monoton=="decr"){
				Xr[i]=SAMP_POSSI(INP[[i]],rr[i])[3-q]
			} else if (INP[[i]]$monoton=="dunno"){
				ndunno=c(ndunno,i)
				lodunno=c(lodunno,SAMP_POSSI(INP[[i]],rr[i])[1])
				updunno=c(updunno,SAMP_POSSI(INP[[i]],rr[i])[2])
				Xr[i]=0
			}
		} else if (INP[[i]]$type=="impr proba"){
			subtype<-NULL
			for (j in INP[INP[[i]]$param]){
				subtype=c(subtype,j$type)
			}
			if (("possi" %in% subtype)&(!("proba" %in% subtype))&(!("impr proba" %in% subtype))){
				if (INP[[i]]$monoton=="incr"){
					Xr[i]=SAMP_PROBA2LEVELS(INP[[i]],rr[i],INP[INP[[i]]$param],rr[INP[[i]]$param],choice_opt,param_opt)[q]
				} else if (INP[[i]]$monoton=="decr"){
					Xr[i]=SAMP_PROBA2LEVELS(INP[[i]],rr[i],INP[INP[[i]]$param],rr[INP[[i]]$param],choice_opt,param_opt)[3-q]
				} else if (INP[[i]]$monoton=="dunno"){
					ndunno=c(ndunno,i)
					lodunno=c(lodunno,SAMP_PROBA2LEVELS(INP[[i]],rr[i],INP[INP[[i]]$param],rr[INP[[i]]$param],choice_opt,param_opt)[1])
					updunno=c(updunno,SAMP_PROBA2LEVELS(INP[[i]],rr[i],INP[INP[[i]]$param],rr[INP[[i]]$param],choice_opt,param_opt)[2])
					Xr[i]=0
				}
			} else if (("proba" %in% subtype)&(!("possi" %in% subtype))&(!("impr proba" %in% subtype))) {
				l=length(INP[INP[[i]]$param])
				rrbis=matrix(runif(N*l),ncol=l)
				Xrbis<-NULL
				for (k in 1:N){
					Xrbis[k]=SAMP_PROBA2LEVELS(INP[[i]],rr[i],INP[INP[[i]]$param],rrbis[k,],choice_opt,param_opt)[1]
				}
				Xrbound=c(quantile(Xrbis,0.05),quantile(Xrbis,0.95))
				if (INP[[i]]$monoton=="incr"){
					Xr[i]=Xrbound[q]
				} else if (INP[[i]]$monoton=="decr"){
					Xr[i]=Xrbound[3-q]
				} else if (INP[[i]]$monoton=="dunno"){
					ndunno=c(ndunno,i)
					lodunno=c(lodunno,Xrbound[1])
					updunno=c(updunno,Xrbound[2])
					Xr[i]=0
				}
			} else if (("fixed" %in% subtype)&(!("possi" %in% subtype))&(!("proba" %in% subtype))&(!("impr proba" %in% subtype))) {
				Xr[i]=SAMP_PROBA2LEVELS(INP[[i]],rr[i],INP[INP[[i]]$param],rr[INP[[i]]$param],choice_opt,param_opt)[q]
			} else if ("impr proba" %in% subtype) {
				stop("Error: imprecise probabilistic subvariable(s) of a variable")
			} else {
				stop("Error: possibilistic and probabilistic subvariables of the same variable")
			}
		} else if ((INP[[i]]$type=="proba")){
			Xr[i]=SAMP_PROBA(INP[[i]],rr[i])
		} else if ((INP[[i]]$type=="fixed")){
			Xr[i]=INP[[i]]$param
		} else if (INP[[i]]$level==2){
			Xr[i]=0
		}
	}
	if (length(ndunno)==0) {
		Z[q]<-FUN(Xr)
	} else {
		if (identical(lodunno,updunno)) {
			Xr[ndunno]=lodunno			
			Z[q]<-FUN(Xr)
		} else {
			FUN2<-function(X) {
			Xr[ndunno]=X
			return(FUN(Xr)*(-2*q+3))
			}
			Z[q]=(-2*q+3)*OPTIMIZ(choice_opt,lodunno,updunno,FUN2,param_opt)
		}
	}
}
return(Z)
}
