comp.lsmeans <- function ( lsm, lsd, description.length=4,
                           description.precision=2)
{
  sorted <- sort(lsm,decreasing=T)
  l <- length(sorted)
  cmp  <- NULL
  grps <- 0
  for (i in 1:l) {
    tmp <- matrix ('',ncol=1,nrow=l)
    for (j in i:l) 
      tmp[j] <- if (abs(sorted[i]-sorted[j]) < lsd) 1+grps else ""
    if (i==1) 
    { add <- 1 } 
    else 
    { add <- 0 
      for (j in i:l) 
      { if ( (tmp[j,1]!="") && (cmp[j,grps]=="")) add <- 1 }
    }
    if (add) 
    {colnames(tmp)<-names(sorted[i]) 
     cmp<-cbind(cmp,tmp); grps=grps+1;}
  }
  rownames(cmp) <- paste (format(names(sorted),digits=description.length),
                          round(sorted,description.precision))
  cmp
}
