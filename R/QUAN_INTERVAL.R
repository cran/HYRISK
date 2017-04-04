QUAN_INTERVAL <-
function(Z0,level=0.5){
F1<- quantile(Z0[1,],level)
F2<- quantile(Z0[2,],level)
return(list(Qlow=min(F1,F2),Qupp=max(F1,F2)))
}
