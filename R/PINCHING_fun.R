PINCHING_fun <-
function(which,value,N,input,FUN,choice_opt="L-BFGS-B",param_opt=NULL,corr=1.e-2,NL=10,mode="IRS"){
inputp<-input
inputp[[which]]=CREATE_INPUT(name=input[[which]]$name,type="fixed",param=c(value),monoton="dunno")
inputp[[which]]$level=input[[which]]$level
Z0p<-PROPAG(N=N,inputp,FUN,choice_opt,param_opt,corr,NL=NL,mode=mode)
return(Z0p)
}
