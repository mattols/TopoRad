#
#
#
# Fig 5 - Anomalies
#
# # # # # # # # # # # # # # # # # # # # # #


##########################################
# iterate through anomalies for all glaciers
for (i in 1:10){
  dfa <- read.csv(paste0("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dec21_all/dfa_",i,".csv"))
  p.sw.anom(dfa,y_lim = c(-100,200))
  legend("bottomleft",legend=i)
  print(i)
}

###############################################
p.anom.fig5 <- function(df, y_lim = c(-100,150), mods_plot4 = c(1,4,5,6,7), drop_res = c(1,4), topovar=7, ax_bottom = TRUE, placeLegend = 6, tnames=F, multi_glacier=FALSE){
  # plot anomaly values from dataframe
  
  # set plot params
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
  col_names = unlist(lapply(names(df), function(x) {gsub("\\.", " ", x)}))
  
  # create resolution list
  if (multi_glacier == FALSE){
    res_ls  <- as.numeric(levels(as.factor(df$Resolution)))
  } else{
    # for all
    res_ls  <- as.numeric(levels(as.factor(round(df$Resolution,-1))))
    df$Resolution = round(df$Resolution,-1)
  }
  
  if (length(res_ls) > 1 & !is.null(drop_res)){
    res_ls <- res_ls[-drop_res]
  }
  l_ty = c(1,5,4,3)    #seq(1, length(res_ls))
  l_wd = c(4, rep(2, length(res_ls) - 1))
  
  mod_select = mods_plot4
  
  for (j in mod_select){
    # select data
    dfg = df[df$Resolution == res_ls[1],]
    
    # make plots
    plot(smooth.spline(dfg[,j]~dfg$Enorm), type = 'l', col='white',
         ylim=y_lim, ylab=expression(SW~anomaly~from~'8m'~(Wm^2)), lwd = l_wd[1],
         lty = l_ty[1],xlab='', axes=F)
    if(tnames){
      legend("topright",legend = col_names[j],cex=1.4,bty='n')
    }
    
    box()
    if (ax_bottom){
      if (match(j,mod_select) == 1 | length(mod_select)==1){
        axis(2,cex.axis=1.8)
        axis(1,cex.axis=1.8)
        
      } else{
        axis(2,cex.axis=1.8, labels=F)
        axis(1,cex.axis=1.8)
      }
    } else{
      if (match(j,mod_select) == 1){
        axis(2,cex.axis=1.8)
        axis(1,cex.axis=1.8, labels=F)
      } else{
        axis(2,cex.axis=1.8, labels=F)
        axis(1,cex.axis=1.8, labels=F)
      }
    }
    
    if (j == placeLegend){
      # place legend at terrain reflected
      legend('topleft',legend = round(res_ls,-1),col=cols[placeLegend],lwd=l_wd,
             lty=l_ty,cex=1.8,bty='n')
    }
    
    for(rr in res_ls){
      # iterate through all resolutions
      dfg = df[df$Resolution == rr,]
      
      lines(smooth.spline(dfg[,j]~dfg$Enorm), lwd=l_wd[match(rr,res_ls)],
            lty=l_ty[match(rr,res_ls)], col=cols[j])
    }
    abline(h=0,lty=3)
  }
}


##########################################
# show all anomalies for glacier 2 (north) for each season (plot all 5 variables)
# 1, 4, 7 drops 20m 150m and 1000m
dfa2j <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dfa_2.csv")
dfa2m <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfa_2.csv")
dfa8m <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfa_8.csv")
dfa8j <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dfa_8.csv")
dfa2d <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dec21_all/dfa_2.csv")
dfa2m <- add.enorm(dfa2m)
dfa8m <- add.enorm(dfa8m)
dfa8j <- add.enorm(dfa8j)
dfa2d <- add.enorm(dfa2d)
dfa2j <- add.enorm(dfa2j)

par(mfrow=c(3,5))
par(oma = c(2,3,0.8,0.5))
par(mar=c(1.5,1,0,0))
# anomalies for each season
# march
p.anom.fig5(dfa2m,y_lim = c(-30,90), drop_res = c(1,4,7), ax_bottom = FALSE)
p.anom.fig5(dfa8m,y_lim = c(-30,90), drop_res = c(1,4,7), placeLegend = 0)
# june
p.anom.fig5(dfa2j,y_lim = c(-30,90), drop_res = c(1,4,7), ax_bottom = FALSE)
p.anom.fig5(dfa8j,y_lim = c(-30,90), drop_res = c(1,4,7), placeLegend = 0)

p.anom.fig5(dfa2j,y_lim = c(-30,120), drop_res = c(1,4,7), ax_bottom = FALSE, placeLegend = 0)
p.anom.fig5(dfa2d,y_lim = c(-140,200), drop_res = c(1,4,7), placeLegend = 0)



# change line types!


#
#### PLOT ANOMALY FOR ALL GLACIERS AT ONE RESOLUTION
dfga_j <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dfga.csv")
dfga_m <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfga.csv")

###############################################
p.anom.glaciers <- function(df, y_lim = c(-40,100), res = 2){
  res_ls  <- as.numeric(levels(as.factor(round(df$Resolution,-1))))
  df$Resolution = round(df$Resolution,-1)
  # drop_res = c(1,4,7)
  # if (length(res_ls) > 1 & !is.null(drop_res)){
  #   res_ls <- res_ls[-drop_res]
  # }
  df = df[df$Resolution == res_ls[res],]
  colss = brewer.pal(10, "Spectral")
  #colss = paste0(rep('gray',10),round(seq(10,80,length.out=10)))
  l_ty = 1 #c(2,3,4,1)
  l_wd = 3 #c(3,3,2,3)
  
  # select data
  dfg = df[df$Gn == 2,]
  # normalize elevation (0-1)
  dfg$Enorm = dfg$Elevation1 - min(dfg$Elevation1)
  dfg$Enorm = dfg$Enorm/max(dfg$Enorm)
  plot(smooth.spline(dfg[,7]~dfg$Enorm), type = 'l', col=colss[j],
       ylim=y_lim, ylab='', lwd = l_wd,
       lty = l_ty,xlab='',cex.axis=1.8,cex.lab=1.3)
  abline(h=0,lty=3)
  for (j in 2:10){
    # select data
    dfg = df[df$Gn == j,]
    # normalize elevation (0-1)
    dfg$Enorm = dfg$Elevation1 - min(dfg$Elevation1)
    dfg$Enorm = dfg$Enorm/max(dfg$Enorm)
    # make plots
    
    lines(smooth.spline(dfg[,7]~dfg$Enorm), lwd=l_wd,
          lty=l_ty, col=colss[j])
  }
  #legend("bottomleft", legend=seq(1,10), col=colss,lty=1,lwd=2)
}

p.anom.glaciers(dfga_m,  y_lim = c(-30,100), res=2) # 30 m
p.anom.glaciers(dfga_m,  y_lim = c(-30,100), res=3) # 90 m

# old methods
p.sw.anom(dfga_m, y_lim = c(-30,100), drop_res = c(1,4,7), plot4 = FALSE, topovar = 7, multi_glacier = TRUE)
p.anom.fig5(dfga_m, y_lim = c(-40,100), drop_res = c(1,4,7), topovar = 7, multi_glacier = TRUE)


###############################################
add.enorm <- function(df){
  res_ls  <- as.numeric(levels(as.factor(round(df$Resolution,-1))))
  df$Resolution = round(df$Resolution,-1)
  df$Enorm = NA
  for (i in 1:length(res_ls)){
    print(paste("resolution",i,'of',length(res_ls)))
    for (j in 1:10){
      # select data
      dfg = df[df$Resolution == res_ls[i] & df$Gn == j,]
      # normalize elevation (0-1)
      dfg$Enorm = dfg$Elevation1 - min(dfg$Elevation1,na.rm=T)
      dfg$Enorm = dfg$Enorm/max(dfg$Enorm,na.rm=T)
      df[df$Resolution == res_ls[i] & df$Gn == j,]$Enorm = dfg$Enorm
    }
  }
  return(df)
}


##################
# Normalize Enorm (create)
dfga_m <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfga.csv")
dfga_m2 <- add.enorm(dfga_m)

p.anom.fig5(dfga_m2, y_lim = c(-30,100), mods_plot4 = 7, ax_bottom = TRUE, placeLegend = 7, drop_res = c(1,4,7), topovar = 7, multi_glacier = TRUE)


# # # # # # # # # # #
# MAP OF RESOLUTIONS
colsgrad = rev(brewer.pal(11, "RdBu"))
brk = c(-125,-100,-75,-50,-25,0,25,50,75,100,125)
res_n = c(8,30,90,250,500)
ress_ls =  c(1,4,12,33,65)

for (i in ress_ls){
  location.variables(demL, glaciers[2,], resampleFactor = i)
  date = ISOdate(2017, 3, 21, 0)
  tfstk <- sw.daily(date)
  tfcom = tfstk
  tfcom[values(tfcom) > 125] = 125
  tfcom[values(tfcom) < -125] = -125
  # png(paste0('F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/figures/R_figs2/g2_r',i, '.png'))
  if (match(i, ress_ls)==1){
    plot(tfcom[[7]], col=colsgrad, legend=F, cex.axis=1.6, 
         breaks = brk)
  } else{
    plot(tfcom[[7]], col=colsgrad, legend=F, yaxt='n', cex.axis=1.6,
         breaks = brk)
    axis(2, labels=NA)
  }
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "grey")
  plot(tfcom[[7]], col=colsgrad, add=T, legend=F, cex.axis=1.3,cex.lab=1.5,
       breaks = brk)
  #plot(glacier,add=T)
  legend("topright",legend=paste(res_n[match(i,ress_ls)],'m'),cex=1.5, bty='n')
  # dev.off()
}

# Legend
par(mfrow=c(1,1))
plot(tfcom[[7]], col=colsgrad, legend=F, cex.axis=1.6, 
     breaks = brk)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey")
plot(tfcom[[7]], col=colsgrad, add=T, cex.axis=1.3,cex.lab=1.5,
     breaks = brk)