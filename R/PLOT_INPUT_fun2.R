PLOT_INPUT_fun2 <-
function(rr,which,N,input,choice_opt,param_opt){

Xr<-NULL
Xrbis<-NULL

inp=input[[which]]

l=length(input[inp$param])
rrbis=matrix(runif(N*l),ncol=l)

for (k in 1:N){
	Xrbis[k]=SAMP_PROBA2LEVELS(inp,rr[which],input[inp$param],rrbis[k,],choice_opt,param_opt)[1]
}
Xr=c(quantile(Xrbis,0.05,na.rm=T),quantile(Xrbis,0.95,na.rm=T))

return(Xr)

}