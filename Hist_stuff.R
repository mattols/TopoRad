




############################################################################
p.hist.dem <- function(df, y_lim = c(0,0.01)){
  # histogram of different dems
  colsd = c('azure3','lightskyblue3','deepskyblue2','firebrick','azure4')
  #colsd = c('lightskyblue3','deepskyblue2','firebrick','azure4')
  l_ty = c(1,2,3,4,1)
  l_wd = c(6,3,3,2,3)
  dem_ls = c("WV", "ASTER","SRTM", "ALOS", "WV30")
  
  dens = density(df[df$dem == dem_ls[5],]$Combined, bw=1)
  # dens$y = length(df[df$dem == dem_ls[2],]$Combined)/sum(dens$y) * dens$y
  plot(dens, col=colsd[5], main="", ylim=y_lim, lwd=2, lty=l_ty[5],
       xlab=expression(Mean~irradiance~change~(Wm^-2)))
  polygon(dens, col=alpha('lightgray',0.3),border=alpha('gray',0.3))
  for (j in 2:4){
    dens = density(df[df$dem == dem_ls[j],]$Combined, bw=1)
    lines(dens, col=colsd[j], lwd=4, lty=l_ty[j])
  }
  legend("topleft", legend=dem_ls[2:5], col= colsd[2:5], lwd=c(rep(4,3),2), lty=l_ty[2:5],cex=1.3)
}

dff <- read.csv(paste0("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dftf_2.csv"))
p.hist.dem(dff)

# plot for all glaciers
for (i in 1:10){
  dff <- read.csv(paste0("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/dftf_",i,".csv"))
  p.hist.dem(dff)
  legend("left",paste("G",i), bty='n')
}

# plot hist of all
cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
brk2 = seq(min(dfg$Combined),max(dfg$Combined),length.out = length(brk))
hc = hist(dfg$Combined, breaks=brk2)
## plot all histograms together
hist(dfg$Combined, breaks=brk2, col=cols[7], ylim=c(0,2200), main="", xlab=expression(Mean~change~'in'~irradiance~(Wm^-2)))
brk3 = seq(min(dfg$Incidence.angle),max(dfg$Incidence.angle),length.out = length(brk))
hist(dfg$Incidence.angle, breaks=brk3, col=cols[1], add=T)
brk4 = seq(min(dfg$Topographic.shading),max(dfg$Topographic.shading),length.out = length(brk))
hist(dfg$Topographic.shading, breaks=brk4, col=cols[4], add=T)
brk5 = seq(min(dfg$Diffuse.sky),max(dfg$Diffuse.sky),length.out = length(brk))
hist(dfg$Diffuse.sky, breaks=brk5, border=cols[5],col=cols[5], add=T)
brk6 = seq(min(dfg$Terrain.reflected),max(dfg$Terrain.reflected),length.out = length(brk))
hist(dfg$Terrain.reflected, breaks=brk6, border=cols[6],col=cols[6], add=T)
box()
legend('topleft', names(dfg)[c(1,4:7)], col=cols[c(1,4:7)], cex=1.5,bty='n',lwd=2)








############################################################################
p.hist.anom <- function(df, y_lim = c(0,0.05), placeLegend = 'topleft', drop_res = c(1,4,7)){
  # histogram of resolutions
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
  col_names = unlist(lapply(names(df), function(x) {gsub("\\.", " ", x)}))
  
  topo_var = 7
  res_ls  <- as.numeric(levels(as.factor(df$Resolution)))
  if (length(res_ls) > 1 & !is.null(drop_res)){
    res_ls <- res_ls[-drop_res]
  }
  l_ty = c(1,2,4,3)    #seq(1, length(res_ls))
  l_wd = c(4, rep(2, length(res_ls) - 1))
  
  dens = density(df[df$Resolution == res_ls[1],]$Combined, bw=1, na.rm=T)
  # dens$y = length(df[df$dem == dem_ls[2],]$Combined)/sum(dens$y) * dens$y
  plot(dens, col=cols[7], main="", ylim=y_lim, lwd=4, lty=l_ty[1],
       xlab=expression(Mean~irradiance~change~(Wm^-2)))
  #polygon(dens, col=alpha('lightgray',0.3),border=alpha('gray',0.3))
  for (j in 1:length(res_ls)){
    dens = density(df[df$Resolution == res_ls[j],]$Combined, na.rm=T)
    lines(dens, col=cols[j+3], lwd=2, lty=l_ty[j])
  }
  legend(placeLegend,legend = round(res_ls,-1),col=cols,lwd=l_wd,
         lty=l_ty,cex=1.4,bty='n')
}

dfm <- read.csv(paste0("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfa_2.csv"))
p.hist.anom(dfm)












# try to plot elv with ggplot2
par(mfrow=c(1,1))
plot(density(df[df$dem == dem_ls[2],]$Combined, bw=1), 
     col=colsd[2], main="", ylim=c(0,0.01), lwd=2, lty=l_ty[2],
     xlab=expression(Mean~irradiance~change~(Wm^-2)))
lines(density(df[df$dem == dem_ls[3],]$Combined, bw=1), col=colsd[3], lwd=2, lty=l_ty[3])
lines(density(df[df$dem == dem_ls[4],]$Combined, bw=1), col=colsd[4], lwd=2, lty=l_ty[4])
lines(density(df[df$dem == dem_ls[5],]$Combined, bw=1), col=colsd[5], lwd=2, lty=l_ty[5])
#abline(v=0,lty=1)
legend("topleft", legend=dem_ls[2:5], col= colsd[2:5], lwd=2, lty=l_ty[2:5],cex=1.5)






#hist of 
dfg = dfdem2

hist(dfdem2$Elevation, breaks = seq(min(dfdem2$Elevation),max(dfdem2$Elevation)), add=T,
     axes=F,col='grey',border='grey')
# exclude any NA values
dfg <- dfg[!is.na(dfg[,7]),]

# make plots
par(mar=c(4.5,5,0.3,5))
plot(smooth.spline(dfg[,1]~dfg[,9]), type = 'l', col='white', ylim=c(-350,50),
     ylab=expression(Mean~change~'in'~irradiance~(Wm^2)),
     lty = 1,xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8)

for(j in mod_select){
  lines(smooth.spline(dfg[,j]~dfg[,9]), lwd=l_wd[j],
        lty=l_ty[j], col=cols[j])
}

abline(h=0,lty=3)
par(new=T)
d = density(dfdem2$Elevation)
plot(d, type = 'n',axes=F, ylab='',xlab='')
polygon(d, col=alpha('lightgray',0.3),border=alpha('gray',0.3))
axis(4,cex.lab=1.3,cex.axis=1.5)
text(4, "Density")



g <- ggplot(data = )


mod_select = c(1,4,6,7)
cols2 = cols[mod_select]
dmelt <- melt(dfg,id = "Elevation")
#names(cols2) <- levels(dmelt$variable)
ggplot(dfg, aes(x=Elevation)) +
  geom_hline(yintercept = 0, linetype='dashed') +
  geom_smooth(aes(y=Incidence.angle, colour = cols[1]), method = loess, span = 0.1) +
  geom_smooth(aes(y=Topographic.shading, colour = cols[4]), method = loess, span = 0.1) +
  geom_smooth(aes(y=Diffuse.sky, colour = cols[5]),method = loess, span = 0.1) +
  geom_smooth(aes(y=Terrain.reflected, colour = cols[6]), method = loess, span = 0.1) +
  geom_smooth(aes(y=Combined, colour = cols[7]), method = loess, span = 0.1) +
  ylab(expression(Mean~irradiance~change~(Wm^-2))) +
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1)) + 
  theme_classic() + theme(text = element_text(size = 20))

p.sw.elv(dfg)

geom_hline(yintercept = 0, linetype='dashed') +
  geom_line() + scale_fill_manual(name = "", values = cols2) +
  facet_wrap(.~variable, ncol=2, scales = "fixed") + 
  scale_x_discrete(name = "DEM types", labels=c("WV","ASTER","SRTM", "ALOS", "WV30")) +
  ylab(expression(Mean~irradiance~change~(Wm^-2))) +
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1)) + 
  theme_classic() + theme(text = element_text(size = 20))



dfg <- df[df$dem == dem_ls[1],]
dfg <- dfg[!is.na(dfg[,7]),]

dfg$L = (dfg$Elevation - min(dfg$Elevation))


for(j in dem_ls){
  dfg <- df[df$dem == j,]
  dfg <- dfg[!is.na(dfg[,7]),]
  lines(smooth.spline(dfg[,mod]~dfg[,10]), lwd=l_wd[match(j,dem_ls)],
        lty=l_ty[match(j,dem_ls)], col=cols[match(j,dem_ls)])
}  
















###########################











# boxplots for all resolutions
mod_select = c(1,4,6,7)
cols2 = cols[mod_select]
dmelt <- melt(dfdem2,id.vars='dem', measure.vars=mod_select)
names(cols2) <- levels(dmelt$variable)
ggplot(dmelt, aes(x=factor(dem), y=value,fill=variable)) +
  geom_hline(yintercept = 0, linetype='dashed') +
  geom_boxplot() + scale_fill_manual(name = "", values = cols2) +
  facet_wrap(.~variable, ncol=2, scales = "fixed") + 
  scale_x_discrete(name = "DEM types", labels=c("WV","ASTER","SRTM", "ALOS", "WV30")) +
  ylab(expression(Mean~irradiance~change~(Wm^-2))) +
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1)) + 
  theme_classic() + theme(text = element_text(size = 20))

# boxplots for all dem types
dfga_m <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dfga.csv")
mod_select = c(1,4,6,7)
cols2 = cols[mod_select]
dmelt <- melt(dfga_m,id.vars='Glacier', measure.vars=mod_select)
names(cols2) <- levels(dmelt$variable)
ggplot(dmelt, aes(x=factor(dem), y=value,fill=variable)) +
  geom_hline(yintercept = 0, linetype='dashed') +
  geom_boxplot() + scale_fill_manual(name = "", values = cols2) +
  facet_wrap(.~variable, ncol=2, scales = "fixed") + 
  scale_x_discrete(name = "DEM types", labels=c("WV","ASTER","SRTM", "ALOS", "WV30")) +
  ylab(expression(Mean~irradiance~change~(Wm^-2))) +
  theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1)) + 
  theme_classic() + theme(text = element_text(size = 20))


### problem need to select all 10 glaciers of a given resolution
# read in another dfga for march?








## area weighted elevation
dfe = as.data.frame(table(round(dfg$Elevation)))
dfge = as.data.frame(dfg$Elevation); names(dfge) = "E1"
dfge$E2 = round(dfge$E1)
dfge$freq = dfe$Freq[match(dfge$E2, dfe$Var1)]
dfge$Aratio = dfge$freq/max(dfge$freq)
dfge$Eratio = dfge$E1/max(dfge$E1)
dfge$Enorm = dfge$E1 - min(dfge$E1)
dfge$Ew1 = dfge$Eratio * dfge$freq
dfge$Ew2 = dfge$Eratio / dfge$freq
dfge$Ew3 = dfge$freq / dfge$Eratio
dfge$Ew4 = dfge$E1 * dfge$Aratio
# plot results
dfge$C = dfg$Combined
dfsc = as.data.frame(scale(dfge))
cols = brewer.pal(6, 'Set1')
plot(smooth.spline(dfg$Combined~dfsc$Ew1), type = 'l', col=cols[1],lwd=2)
abline(h=0,lty=3)
lines(smooth.spline(dfg$Combined~dfsc$Ew2),col=cols[2],lwd=2)
lines(smooth.spline(dfg$Combined~dfsc$Ew3),col=cols[3],lwd=2)
lines(smooth.spline(dfg$Combined~dfsc$E1),col=cols[4],lwd=2)
lines(smooth.spline(dfg$Combined~dfsc$Enorm),col=cols[5],lty=2,lwd=2)
#lines(smooth.spline(dfg$Combined~dfsc$Ew4),col=cols[6],lwd=2)
legend('bottomright', c("E1","E2","Ew3","E1","Enorm","Ew4"),col=cols,lwd=2)

# hist
brk = seq(min(round(dfge$E1,-1))-10,max(round(dfge$E1,-1))+10,10)
h = hist(dfge$E1, breaks = brk)
plot(h, col='deepskyblue2')
dfge$freq2 = h$counts[match(dfge$E2, round(h$mids,-1))]


