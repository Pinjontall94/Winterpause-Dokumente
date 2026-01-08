
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
