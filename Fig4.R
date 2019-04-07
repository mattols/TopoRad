





# plot different dem

###############################################
p.sw.dems <- function(df, ax = c(1,2), mod = 7, all_mods = FALSE, y_lim = c(-400,50), dem_ls = c("WV", "ASTER","SRTM", "ALOS", "WV30"), placeLegend = 'bottomleft'){
  # output from dfts
  # plot different DEMs against elevation
  
  # set plot params
  # par(mfrow=c(1,1))
  # par(mar=c(5,5.8,2,2))
  l_ty = c(2,3,4,1,1)
  l_wd = c(3,3,3,5,3)
  l_ty = c(1,2,3,4,1)
  l_wd = c(6,3,3,2,3)
  l_ty = c(2,1,4,1)
  l_wd = c(3,2,3,4)
  #cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4')
  cols = c('slategray1','slategray2','slategray3','slategray4', 'slategrey')
  cols = c('slategray4','slategray3','slategray3','slategray3','slategray4')
  cols = c('azure3','lightskyblue3','deepskyblue2','firebrick','azure4')
  cols = c('snow4','black','firebrick','slategrey')
  # create list of dem types
  # dem_ls  <- levels(as.factor(df$dem))
  # dem_ls <- c("WV", "ASTER","SRTM", "ALOS", "WV30")
  
  # only 3 main DEMS
  l_ty = c(2,1,4,1)[1:length(dem_ls)]
  l_wd = c(3,2,3,4)[1:length(dem_ls)]
  cols = c('snow4','black','firebrick','slategrey')[1:length(dem_ls)]
  
  if (length(dem_ls)==2){
    l_ty = c(4,1)
    l_wd = c(3,3)
    cols = c('firebrick','black')
  }
  
  if (all_mods){
    par(mfrow=c(2,2))
    par(mar=c(4.5,5,0.8,0.3))
    yy = list(c())
    for (i in c(1,4,6,7)){
      mod = i
      # exclude any NA values
      dfg <- df[df$dem == dem_ls[1],]
      dfg <- dfg[!is.na(dfg[,7]),]
      
      # make plots
      plot(smooth.spline(dfg[,mod]~dfg[,9]), type = 'l', col='white',
           ylim=c(-350,100), ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = l_wd[1],
           lty = l_ty[1],xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8)
      for(j in dem_ls){
        dfg <- df[df$dem == j,]
        dfg <- dfg[!is.na(dfg[,7]),]
        lines(smooth.spline(dfg[,mod]~dfg[,9]), lwd=l_wd[match(j,dem_ls)],
              lty=l_ty[match(j,dem_ls)], col=cols[match(j,dem_ls)])
      }
      abline(h=0,lty=3)
      # abline(v = dfg$Elevation[which.min(abs(dfg$Distance.Elevation))], lty=3, col='red')
      if(i==6){
        legend(placeLegend,dem_ls,col=cols,lwd=l_wd,
               lty=l_ty,cex=1.4,bty='n')
      }
    }
    
  } else{
    # exclude any NA values
    dfg <- df[df$dem == dem_ls[1],]
    dfg <- dfg[!is.na(dfg[,7]),]
    
    # make plots
    plot(smooth.spline(dfg[,mod]~dfg[,9]), type = 'l', col='white',
         ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = l_wd[1],
         lty = l_ty[1],xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8, axes = F)
    axis(1,cex.axis=1.7,labels=NA)
    axis(2,cex.axis=1.7, labels=NA)
    for (i in ax){axis(i,cex.axis=1.7)}
    box()
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
}

# north glacie
dfdem2 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dftf_2.csv")

par(mfrow=c(1,1))
# par(mar=c(5,5.8,2,2))
par(oma=c(0,0,0,0))
par(mar=c(4.5,5,0.3,0.3))
p.sw.dems(dfdem2, mod = 4, y_lim=c(-300,10))
p.sw.dems(dfdem2, mod = 7, y_lim = c(-380,10),dem_ls = c("WV","ASTER","SRTM", "ALOS", "WV30"))
p.sw.dems(dfdem2, mod = 7, y_lim = c(-380,10),dem_ls = c("ASTER","SRTM", "ALOS", "WV30"))

par(mfrow=c(2,2))
par(oma=c(4,4,0,0.4))
par(mar=c(1,1,0.4,0.4))
p.sw.dems(dfdem2, ax = 2, mod = 7, y_lim = c(-380,10),dem_ls = c("WV30","WV"))
p.sw.dems(dfdem2, ax = NULL, mod = 7, y_lim = c(-380,10),dem_ls = c("ASTER","WV"))
p.sw.dems(dfdem2, mod = 7, y_lim = c(-380,10),dem_ls = c("SRTM","WV"))
p.sw.dems(dfdem2, ax=1, mod = 7, y_lim = c(-380,10),dem_ls = c("ALOS","WV"))
# par(new=T)
# par(mfrow=c(1,1))
# par(oma=c(0,0,0,0))
# text(2,expression(Mean~change~'in'~irradiance~(Wm^2)),cex=1.3)
p.sw.dems(dfdem2, all_mods = TRUE, y_lim = c(-380,10),dem_ls = c("ASTER","SRTM", "ALOS", "WV30"))

# plot 4 glaciers with each dem
dfdem2 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dftf_2.csv")
dfdem8 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dftf_8.csv")
dfdem9 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dftf_9.csv")
dfdem4 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dftf_4.csv")
par(mfrow=c(2,2))
par(mar=c(4.5,5,0.8,0.3))
p.sw.dems(dfdem2, mod = 7, y_lim = c(-500,50),dem_ls = c("ASTER","SRTM", "ALOS", "WV30"), placeLegend = NULL)
p.sw.dems(dfdem8, mod = 7, y_lim = c(-500,50),dem_ls = c("ASTER","SRTM", "ALOS", "WV30"))
p.sw.dems(dfdem9, mod = 7, y_lim = c(-500,50),dem_ls = c("ASTER","SRTM", "ALOS", "WV30"), placeLegend = NULL)
p.sw.dems(dfdem4, mod = 7, y_lim = c(-500,50),dem_ls = c("ASTER","SRTM", "ALOS", "WV30"), placeLegend = NULL)

dfgadem <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dfga.csv")
p.sw.dems(dfgadem, mod = 7, y_lim = c(-500,50),dem_ls = c("ASTER","SRTM", "ALOS", "WV30"), placeLegend = NULL)

###############################################
p.sw.dem.anom <- function(df, mod = 7, y_lim = c(-100,100), dem_ls = c("ASTER","SRTM", "ALOS", "WV30"), placeLegend = 'bottomleft'){
  # output from dfts
  # plot different DEMs against elevation
  
  # set plot params
  l_ty = c(2,3,4,1)
  l_wd = c(3,3,2,3)
  colsd = c('lightskyblue3','deepskyblue2','firebrick','azure4')
  
  l_ty = c(2,1,4,1)
  l_wd = c(3,2,3,4)
  colsd = c('snow4','black','firebrick','slategrey')
  colsd = c('snow4','royalblue4','firebrick','slategrey')
  
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


# Fig 7
# north glacier anomaly
dfadem2 <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dfa_2.csv")
p.sw.dem.anom(dfadem2, mod = 7, y_lim = c(-150,110), dem_ls = c("ASTER","SRTM", "ALOS", "WV30"), placeLegend = "bottom")

mean(dfadem2[dfadem2$dem =="WV30" | dfadem2$dem =="ALOS" | dfadem2$dem =="SRTM",]$Combined,na.rm=T)
sd(dfadem2[dfadem2$dem =="WV30" | dfadem2$dem =="ALOS" | dfadem2$dem =="SRTM",]$Combined,na.rm=T)

cor(dfadem2[dfadem2$dem =="ASTER",]$Combined,dfadem2[dfadem2$dem =="SRTM",]$Combined[1:31822])
length(dfadem2[dfadem2$dem =="WV30",]$Combined)

p.sw.dem.anom(dfgadem, mod = 7, y_lim = c(-150,110), dem_ls = c("ASTER","SRTM", "ALOS", "WV30"))

# compare distributions of dems
colsd = c('snow4','black','firebrick','slategrey')
hist(mask(crop(aster_dem,glaciers[2,]),glaciers[2,]), col=colsd[1])
hist(mask(crop(srtm_dem,glaciers[2,]),glaciers[2,]), col=colsd[2], add=T)
hist(mask(crop(alos_dem,glaciers[2,]),glaciers[2,]), col=colsd[3], add=T)
hist(mask(crop(wv_dem,glaciers[2,]),glaciers[2,]), col=colsd[4])

plot(getValues(mask(crop(alos_dem,glaciers[2,]),glaciers[2,]))~getValues(mask(crop(srtm_dem,glaciers[2,]),glaciers[2,]))[1:88548])

plot(getValues(mask(crop(alos_dem,glaciers[2,]),glaciers[2,])))

# resample ALOS and stack
alos2      <- resample(alos_dem, srtm_dem, method='bilinear')
d_stk <- stack(aster_dem,srtm_dem, alos2)
dmask = mask(crop(d_stk,glaciers[2,]),glaciers[2,])

plot(getValues(dmask[[1]])~getValues(mean(dmask)))


dfgadem <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dfga.csv")

p.sw.box(dfdem2, mod_select = c("WV","ASTER","SRTM", "ALOS", "WV30"))

