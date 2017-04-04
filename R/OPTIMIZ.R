OPTIMIZ <-
function(choice_opt,lobound,upbound,fn,param_opt=NULL){

mid=(lobound+upbound)/2

#METHOD L-BFGS-B
if (choice_opt=="L-BFGS-B"){
	fnsc=pmax(abs(fn(lobound)),abs(fn(upbound)))
	parsc=pmax(abs(lobound),abs(upbound))
	OPT<-(optim(par=mid,fn=fn,gr=NULL,method="L-BFGS-B",lower=lobound,upper=upbound, control=list(fnscale=fnsc,parscale=parsc))$value)

#METHOD L-BFGS-B MULTISTART
} else if (choice_opt=="L-BFGS-B_MULTI"){
	mult_start=matrix(0,ncol=length(lobound),nrow=param_opt)
	for (i in 1:length(lobound)){
		mult_start[,i]=runif(param_opt, min=lobound[i], max=upbound[i])
	}
	fnsc=pmax(abs(fn(lobound)),abs(fn(upbound)))
	parsc=pmax(abs(lobound),abs(upbound))
	OPT0<-NULL
	for (i in 1:nrow(mult_start)){
		OPT0[i]<-(optim(par=mult_start[i,],fn=fn,gr=NULL,method="L-BFGS-B",lower=lobound,upper=upbound, control=list(fnscale=fnsc,parscale=parsc))$value)
	}
	OPT<-(min(OPT0))
#METHOD GENOUD
} else if (choice_opt=="GENOUD"){
	bounds=cbind(lobound,upbound)
	A=capture.output(OPT<-((genoud(fn=fn,nvars=length(lobound),Domains=bounds,boundary.enforcement=2,pop.size=param_opt[1],max.generations=param_opt[2],solution.tolerance=param_opt[3]))$value))

}

return(OPT)

}
