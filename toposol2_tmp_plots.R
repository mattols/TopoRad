#
#
#
#
#
#



########################
## Plot maps (glacier 2)
location.variables(demL, glaciers[2,], resampleFactor = 4)
date = ISOdate(2017, 3, 21, 0)
tfstk <- sw.daily(date)
# plot 5 main
plot(tfstk[[1]], col=colsgrad[1:8], breaks = c(-350,-200,-150,-100,-60,-10,0,10,60))
plot(glacier,add=T)
legend("bottomleft", legend = names(stkvar),bty='n',cex=1.5)
plot(tfstk[[4]], col=rev(blues9), breaks = c(-350,-250,-150,-100,-50,-25,-10,0))
plot(glacier,add=T) 
plot(tfstk[[5]], col=rev(blues9), breaks = c(-30,-25,-20,-15,-10,-5,0))
plot(glacier,add=T)
plot(tfstk[[6]], col=colsgrad[6:length(colsgrad)], breaks = c(0,10,20,30,40,50,75))
plot(glacier,add=T)
plot(tfstk[[7]], col=colsgrad, breaks = c(-250,-150,-100,-50,-25,0,25,50,100))
plot(glacier,add=T)


tfstk@data



###############################################################
###############################################################
dftf <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dftf_8.csv")
dfa <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dfa_8.csv")

#####################
## FIG 2 - diagram
# use ts_plot_test.R


#####################
## FIG 4 - maps or boxplots?
#

# plot each resolution (for what glacier?)
for (i in c(1,4,12,33,65,130)){
  location.variables(demL, shape = glaciers[2,], resampleFactor = i)  #(resample north facing glacier to 20m (factor of 3))
  tfstk = sw.daily(date = ISOdate(2017, 6, 21, 0)) 
  plot(tfstk[[7]], col=colsgrad[1:5], breaks = c(-250,-150,-100,-75,-50,-25,0,25,50,100))
  plot(glacier,add=T)
}

# boxplots
p.sw.box(dfa, drop_res = c(1,4))
p.sw.box(dftf)


#####################
## FIG 6 - all 10 glaciers
#
# june 21
p.sw.anom(dfga, y_lim = c(-50,120), plot4 = FALSE, topovar = 7, multi_glacier = TRUE)
#p.sw.box(dfga)
# with all
par(mfrow=c(2,2))
dfga_j <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dfga.csv")
p.sw.anom(dfga_j, y_lim = c(-50,200), plot4 = FALSE, topovar = 7, multi_glacier = TRUE)
dfga_m <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfga.csv")
p.sw.anom(dfga_m, y_lim = c(-50,200), plot4 = FALSE, topovar = 7, multi_glacier = TRUE, placeLegend = NULL)
p.sw.anom(dfga, y_lim = c(-50,200), plot4 = FALSE, topovar = 7, multi_glacier = TRUE)

#boxplots of all 10 glacier anomalies
p.sw.box(dfga_j, drop_res = c(1,4), multi_glacier=TRUE)


#####################
## FIG 7
#

#####################
## FIG 8
#




# percentage?
# mean and sd across resolution
# 









### ????
# obtain lat/lon values for improved length (instead of elevation)
dft <- sw.res.dataframe(tfstk)
p.sw.elv(dft, y_lim = c(-250,150))

# create raster of lat vals
r = rasterToPoints(dem,spatial=T)
dd = dem
dd@data@values = r@coords[,2]
ex = extent(glacier)
ex@ymin = ex@ymin-0.001
ex@ymax = ex@ymax+0.001
dd = mask(crop(dd, extent(ex)),glacier)

# change elevation values to lat values
dft[,9] = getValues(dd)

# make it slope length? elevation and lat??






###########################################################
## EXPLORE

# ROUGH MAP
plot(crop(demL,glaciers))
plot(glaciers,add=T,col=alpha('deepskyblue2',0.7))
text(glaciers, labels = match(glaciers@data$RGIId,glaciers@data$RGIId),cex=2)



# glaciers loop
par(mfrow=c(2,3))
for (i in 1:10){
  location.variables(demL, glaciers[i,], resampleFactor = 4)
  date = ISOdate(2017, 3, 21, 0)
  tfstk <- sw.daily(date)
  tfcom = tfstk
  tfcom[values(tfcom) > 100] = 100
  tfcom[values(tfcom) < -250] = -250
  plot(tfcom[[c(1,4)]], col=colsgrad, 
       breaks = c(-250,-150,-100,-50,-25,0,25,50,100))
  #plot(glacier,add=T)
  legend("bottomleft",legend=paste('glacier',i),cex=1.5)
}

# resolution loop
par(mfrow=c(1,1))
res_n = c(8,30,90,250,500,1000)
res_n = c(30,90,250,500,1000)
ress_ls =  c(1,4,12,33,65,130)
ress_ls =  c(4,12,33,65,130)
for (i in ress_ls){
  location.variables(demL, glaciers[2,], resampleFactor = i)
  date = ISOdate(2017, 3, 21, 0)
  tfstk <- sw.daily(date)
  tfcom = tfstk
  tfcom[values(tfcom) > 100] = 100
  tfcom[values(tfcom) < -250] = -250
  # png(paste0('F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/figures/R_figs2/g2_r',i, '.png'))
  plot(tfcom[[7]], col=colsgrad, legend=F,
       breaks = c(-250,-150,-100,-50,-75,-25,0,25,50,75,100))
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
         "grey")
  plot(tfcom[[7]], col=colsgrad, add=T, legend=F,
       breaks = c(-250,-150,-100,-50,-75,-25,0,25,50,75,100))
  #plot(glacier,add=T)
  legend("topright",legend=paste(res_n[match(i,ress_ls)],'m'),cex=1.5, bty='n')
  # dev.off()
}





########################
## Plot maps (glacier 2)
location.variables(demL, glaciers[8,], resampleFactor = 4)
date = ISOdate(2017, 3, 21, 0)
tfstk <- sw.daily(date)
ex = extent(glacier)
ex@xmin = ex@xmin - 0.01
ex@xmax = ex@xmax + 0.01
ex@ymin = ex@ymin - 0.001
ex@ymax = ex@ymax + 0.001
# tfstk_g8 = tfstk
# dfa2m <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfa_2.csv")
# p.sw.anom(dfa2m,y_lim = c(-30,130))
dfa8m <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfa_8.csv")
p.sw.anom(dfa8m,y_lim = c(-30,130))

colsgrad2 = c(colsgrad[1:5],colsgrad[7:length(colsgrad)])
brk = c(-150,-100,-50,-75,-25,0,25,50,75,100)
brk = c(-100,-75,-50,-25,-15,0,15,25,50,75,100)
# plot 5 main
tfcom = tfstk
# tfcom[values(tfcom) > 100] = 100
# tfcom[values(tfcom) < -150] = -150
sa = tfcom[[1]]
sa[values(sa)>100] = 100
sa[values(sa) < -100] = -100
plot(sa, col=colsgrad2, legend=F, axes=F,
     breaks = brk)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey")
plot(sa, col=colsgrad, add=T, legend=F,
     breaks = brk)
plot(rasterToContour(crop(dem,ex),levels=seq(4500,8000,100)),lwd=1.6,add=T,col='grey41')
axis(1, cex.axis = 1.5)
axis(2, cex.axis = 1.5)
#
ts = tfcom[[4]]
ts[values(ts) < -100] = -100
plot(ts, col=colsgrad2, legend=F, axes=F,
     breaks = brk)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey")
plot(ts, col=colsgrad2, add=T, legend=F,
     breaks = brk)
plot(rasterToContour(crop(dem,ex),levels=seq(4500,8000,100)),lwd=1.6,add=T,col='grey41')
axis(1, cex.axis = 1.5)
axis(2, labels = NA)
#
df = tfcom[[5]]
plot(df, col=colsgrad2, legend=F, axes=F,
     breaks = brk)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey")
plot(df, col=colsgrad, add=T, legend=F,
     breaks = brk)
plot(rasterToContour(crop(dem,ex),levels=seq(4500,8000,100)),lwd=1.6,add=T,col='grey41')
axis(1, cex.axis = 1.5)
axis(2, labels = NA)
#
rf = tfcom[[6]]
plot(rf, col=colsgrad2, legend=F, axes=F,
     breaks = brk)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey")
plot(rf, col=colsgrad2, add=T, legend=F,
     breaks = brk)
plot(rasterToContour(crop(dem,ex),levels=seq(4500,8000,100)),lwd=1.6,add=T,col='grey41')
axis(1, cex.axis = 1.5)
axis(2, labels = NA)
com = tfcom[[7]]
com[values(com)>100] = 100
com[values(com) < -100] = -100
plot(com, col=colsgrad2, legend=F, axes=F,
     breaks = brk)
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = 
       "grey")
plot(com, col=colsgrad2, add=T, legend=F,
     breaks = brk)
plot(rasterToContour(crop(dem,ex),levels=seq(4500,8000,100)),lwd=1.6,add=T,col='grey41')
axis(1, cex.axis = 1.5)
axis(2, labels = NA)






















plot(tfstk[[1]], col=colsgrad[1:8], breaks = c(-350,-200,-150,-100,-60,-10,0,10,60))
plot(glacier,add=T)
legend("bottomleft", legend = names(stkvar),bty='n',cex=1.5)
plot(tfstk[[4]], col=rev(blues9), breaks = c(-350,-250,-150,-100,-50,-25,-10,0))
plot(glacier,add=T) 
plot(tfstk[[5]], col=rev(blues9), breaks = c(-30,-25,-20,-15,-10,-5,0))
plot(glacier,add=T)
plot(tfstk[[6]], col=colsgrad[6:length(colsgrad)], breaks = c(0,10,20,30,40,50,75))
plot(glacier,add=T)
plot(tfstk[[7]], col=colsgrad, breaks = c(-250,-150,-100,-50,-25,0,25,50,100))
plot(glacier,add=T)




