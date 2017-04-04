PLOT_INPUT_fun <-
function(rr,which,input,choice_opt,param_opt){

Xr<-NULL
inp=input[[which]]
Xr=SAMP_PROBA2LEVELS(inp,rr[which],input[inp$param],rr[inp$param],choice_opt,param_opt)

return(Xr)

}