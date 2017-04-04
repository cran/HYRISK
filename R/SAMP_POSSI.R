SAMP_POSSI <-
function(X,ALPHA){

if (X$distr=="interval"){
	Xsamp_min=X$param[1]
	Xsamp_max=X$param[2]
} else {
	TEMP<-cut(X$fuzzy,level=ALPHA)
	Xsamp_min=min(TEMP)
	Xsamp_max=max(TEMP)
}
return (c(Xsamp_min,Xsamp_max)) 
}
