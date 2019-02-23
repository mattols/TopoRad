





# plot different dem

###############################################
p.sw.dems <- function(df, mod = 7, y_lim = c(-400,50), dem_ls = c("WV", "ASTER","SRTM", "ALOS", "WV30"), placeLegend = 'bottomleft'){
  # output from dfts
  # plot different DEMs against elevation
  
  # set plot params
  # par(mfrow=c(1,1))
  # par(mar=c(5,5.8,2,2))
  l_ty = c(2,3,4,1,1)
  l_wd = c(3,3,3,5,3)
  l_ty = c(1,2,3,4,1)
  l_wd = c(6,3,3,2,3)
  #cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4')
  cols = c('slategray1','slategray2','slategray3','slategray4', 'slategrey')
  cols = c('slategray4','slategray3','slategray3','slategray3','slategray4')
  cols = c('azure3','lightskyblue3','deepskyblue2','firebrick','azure4')
  # create list of dem types
  # dem_ls  <- levels(as.factor(df$dem))
  # dem_ls <- c("WV", "ASTER","SRTM", "ALOS", "WV30")
  
  # exclude any NA values
  dfg <- df[df$dem == dem_ls[1],]
  dfg <- dfg[!is.na(dfg[,7]),]
  
  # make plots
  plot(smooth.spline(dfg[,mod]~dfg[,9]), type = 'l', col='white',
       ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = l_wd[1],
       lty = l_ty[1],xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8)
  for(j in dem_ls){
    dfg <- df[df$dem == j,]
    dfg <- dfg[!is.na(dfg[,7]),]
    lines(smooth.spline(dfg[,mod]~dfg[,9]), lwd=l_wd[match(j,dem_ls)],
          lty=l_ty[match(j,dem_ls)], col=cols[match(j,dem_ls)])
  }
  abline(h=0,lty=3)
  # abline(v = dfg$Elevation[which.min(abs(dfg$Distance.Elevation))], lty=3, col='red')
  if(!is.null(placeLegend)){
    legend(placeLegend,dem_ls,col=cols,lwd=l_wd,
           lty=l_ty,cex=1.4,bty='n')
  }
}

# north glacie
dfdem2 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dftf_2.csv")

par(mfrow=c(3,2))
# par(mar=c(5,5.8,2,2))
par(oma=c(0,0,0,0))
par(mar=c(4.5,5,0.3,0.3))
p.sw.dems(dfdem2, mod = 1, y_lim=c(-200,200))
p.sw.dems(dfdem2, mod = 7, y_lim = c(-380,10),dem_ls = c("WV","ASTER","SRTM", "ALOS", "WV30"))


###############################################
p.sw.dem.anom <- function(df, mod = 7, y_lim = c(-100,100), dem_ls = c("ASTER","SRTM", "ALOS", "WV30"), placeLegend = 'bottomleft'){
  # output from dfts
  # plot different DEMs against elevation
  
  # set plot params
  l_ty = c(2,3,4,1)
  l_wd = c(3,3,2,3)
  colsd = c('lightskyblue3','deepskyblue2','firebrick','azure4')
  # create list of dem types
  
  # exclude any NA values
  dfg <- df[df$dem == dem_ls[1],]
  dfg <- dfg[!is.na(dfg[,7]),]
  
  dfg$L = (dfg$Elevation - min(dfg$Elevation))
  
  # make plots
  plot(smooth.spline(dfg[,mod]~dfg[,10]), type = 'l', col='white',
       ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = l_wd[1],
       lty = l_ty[1],xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8)
  for(j in dem_ls){
    dfg <- df[df$dem == j,]
    dfg <- dfg[!is.na(dfg[,7]),]
    lines(smooth.spline(dfg[,mod]~dfg[,10]), lwd=l_wd[match(j,dem_ls)],
          lty=l_ty[match(j,dem_ls)], col=colsd[match(j,dem_ls)])
  }
  abline(h=0,lty=3)
  # abline(v = dfg$Elevation[which.min(abs(dfg$Distance.Elevation))], lty=3, col='red')
  if(!is.null(placeLegend)){
    legend(placeLegend,dem_ls,col=colsd,lwd=l_wd,
           lty=l_ty,cex=1.4,bty='n')
  }
}



# north glacier anomaly
dfadem2 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dfa_8.csv")
p.sw.dem.anom(dfadem2, mod = 7, y_lim = c(-150,110), dem_ls = c("ASTER","SRTM", "ALOS", "WV30"))

#


dfgadem <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dfga.csv")

p.sw.box(dfdem2, mod_select = c("WV","ASTER","SRTM", "ALOS", "WV30"))

