


# what is the ratio between percent and irradiance

dfi2 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfa_2.csv")
dfi8 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfa_8.csv")
dfp2 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/perc_march/pm_dfa_2.csv")
dfp8 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/perc_march/pm_dfa_8.csv")

plot(dfi2$Combined~dfp2$Combined)


### PERCENTAGES FOR FIG6 Anomalies

par(mfrow=c(3,5))
par(oma = c(2,3,0.8,2))
par(mar=c(1.5,1,0,1.5))

#plot 5 times
y_lim = c(-30,90)
for (i in 1:5){
  plot(smooth.spline(dfi2$Combined~dfi2$Elevation1), type = 'l', col='slategrey',
       ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = 5,
       lty = 1,xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8, xaxt='n',yaxt='n')
  axis(2,cex.axis=1.8, labels=NA)
  axis(1,cex.axis=1.8, labels=NA)
  abline(h=0,lty=3)
  # abline(h=50,lty=3)
  par(new = T)
  # y_lim = c(-10,26.6)
  y_lim2 = y_lim/3
  plot(smooth.spline(dfp2$Combined~dfp2$Elevation1), type = 'l', col='red',
       ylim=y_lim2, ylab=NA, lwd = 2,
       lty = 1,xlab=NA,cex.lab=1.3, cex.axis=1.8, axes=F)
  axis(4, cex.axis=1.8,cex.lab=1.3)
}
# south (plot 5 times)
for (i in 1:5){
  plot(smooth.spline(dfi8$Combined~dfi8$Elevation1), type = 'l', col='slategrey',
       ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = 5,
       lty = 1,xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8, xaxt='n',yaxt='n')
  axis(2,cex.axis=1.8, labels=NA)
  axis(1,cex.axis=1.8, labels=NA)
  abline(h=0,lty=3)
  # abline(h=50,lty=3)
  par(new = T)
  # y_lim = c(-10,26.6)
  y_lim2 = y_lim/3
  plot(smooth.spline(dfp8$Combined~dfp8$Elevation1), type = 'l', col='red',
       ylim=y_lim2, ylab=NA, lwd = 2,
       lty = 1,xlab=NA,cex.lab=1.3, cex.axis=1.8, axes=F)
  axis(4, cex.axis=1.8,cex.lab=1.3)
}



### FIG 5 PERCENTAGE

dfi2 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfa_2.csv")
dfp2 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/perc_march/pm_dfa_2.csv")

par(mfrow=c(1,2))
y_lim = c(-150,110) #y_lim = c(-30,100)
plot(smooth.spline(dfi2$Combined~dfi2$Elevation1), type = 'l', col='slategrey',
     ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = 5,
     lty = 1,xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8)
abline(h=0,lty=3)
# abline(h=50,lty=3)
par(new = T)
# y_lim = c(-10,26.6)
y_lim2 = y_lim/3
plot(smooth.spline(dfp2$Combined~dfp2$Elevation1), type = 'l', col='red',
     ylim=y_lim2, ylab=NA, lwd = 2,
     lty = 1,xlab=NA,cex.lab=1.3, cex.axis=1.8, axes=F)
axis(4, cex.axis=1.8,cex.lab=1.3)



### FIG 7 PERCENTAGE (^change to -150,110)
y_lim = c(-150,110) #y_lim = c(-30,100)
plot(smooth.spline(dfi2$Combined~dfi2$Elevation1), type = 'l', col='slategrey',
     ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = 5,
     lty = 1,xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8)
abline(h=0,lty=3)
# abline(h=50,lty=3)
par(new = T)
# y_lim = c(-10,26.6)
y_lim2 = y_lim/3
plot(smooth.spline(dfp2$Combined~dfp2$Elevation1), type = 'l', col='red',
     ylim=y_lim2, ylab=NA, lwd = 2,
     lty = 1,xlab=NA,cex.lab=1.3, cex.axis=1.8, axes=F)
axis(4, cex.axis=1.8,cex.lab=1.3, at=c(-40,-30,-20,-10,0,10,20,30))
axis(4, cex.axis=1.8,cex.lab=1.3, at=c(-40,-20))
