
# plots a ms plot (adopted from p. cirillo)
ms_plot <- function(data,p=4) {
  par(mfrow = c(2, 2)) 
  x=abs(data)
  for (i in 1:p) {
    y=x^i
    S=cumsum(y)
    M=cummax(y)
    R=M/S
    plot(1:length(x),R,type='l', col=2, lwd=3, ylim=c(0,1),xlab='n', ylab='Rn', 
         main=paste("ms plot for p=",i))
  }
  par(mfrow = c(1, 1))
}