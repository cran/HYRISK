UNCERTAINTY <-
function(Z0,disc=0.01){
threshold<-seq(disc,1-disc,by=disc)
F1<- quantile(Z0[1,],threshold)
F2<- quantile(Z0[2,],threshold)
unc<-sum(abs(F1-F2))
return(unc)
}
