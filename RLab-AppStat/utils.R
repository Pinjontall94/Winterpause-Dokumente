
check.residuals <- function ( m )  {
  res <- residuals ( m )
  fit <- fitted    ( m )
  par(mfrow=c(2,2),mar=c(4,4,4,1))
  hist  ( res , probability=T, main="Histogram" )
  x <- seq(min(res),max(res),length.out=100)                    
  lines ( x,dnorm(x,mean(res),sd(res)),lwd=2,col="darkgreen")    
  plot  ( res~fit,     main="Fitted-values" )    
  lines ( c(min(fit),max(fit)) , c(0,0),col="darkgreen",lwd=2 )        
  qqnorm( res,         main="QQ-Normal plot")    
  qqline( res ,col="darkgreen",lwd=2)                                  
  plot  ( res, main="Index plot")                
  lines (lowess((1:length(fit)), res),col="darkgreen",lwd=2)                  
}

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
