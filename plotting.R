
# plots a ms plot (adopted from p. cirillo)
ms_plot_two_groups <- function(data_1, data_2, p=4) {
  par(mfrow = c(2, 2)) 
  x_1=abs(data_1)
  x_2=abs(data_2)
  for (i in 1:p) {
    y_1=x_1^i; y_2=x_2^i
    S_1=cumsum(y_1); S_2=cumsum(y_2);
    M_1=cummax(y_1); M_2=cummax(y_2)
    R_1=M_1/S_1 
    R_2=(M_2/S_2) + 0.04 #add small offset for potting
    plot(1:length(x_1),R_1,type='l', col="#697CC3", lwd=2, ylim=c(0,1),xlab='n', ylab='Rn', 
         main=paste("ms plot for p=",i))
    lines(jitter(1:length(x_2)), R_2, type='l', col="#DC801D", lwd=2)
  }
  par(mfrow = c(1, 1))
}