SUMMARY_1CDF <-
function(Z0,aversion=0.5){
##F2 (Confidence Index) approach by Dubois & Guyonnet 2011
Z<-aversion*Z0[2,]+(1-aversion)*Z0[1,]
return(Z)
}
