SAMP_PROBA <-
function(X,PROBA){

Xsamp=do.call(X$qfun,as.list(c(PROBA,X$param)))
return (Xsamp) 

}
