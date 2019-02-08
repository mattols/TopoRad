




#### START
spt1 = SpatialPoints(coordinates(data.frame(86.925,28.07)),proj4string=crs(dem))
spt2 = SpatialPoints(coordinates(data.frame(86.943,28.05)),proj4string=crs(dem))
spt3 = SpatialPoints(coordinates(data.frame(86.948,28.04)),proj4string=crs(dem))
spt4 = SpatialPoints(coordinates(data.frame(86.942,28.023)),proj4string=crs(dem))
spt5 = SpatialPoints(coordinates(data.frame(86.956,28.062)),proj4string=crs(dem))


# WATCH MODELS / RUN FROM MOMENT LOOP START


# ASSIGN MATRIX
model_flat = array(0,dim=dim(d_mat))
model_flat_base = array(0,dim=dim(d_mat))
model_flat_sh = array(0,dim=dim(d_mat))
model_inc_sr = array(0,dim=dim(d_mat))
model_inc_sh = array(0,dim=dim(d_mat))
model_vf_base = array(0,dim=dim(d_mat))
model_vf = array(0,dim=dim(d_mat))
model_ref = array(0,dim=dim(d_mat))

print(paste("-> Starting day", all_dates[i]))
# CREATE MOMENTS DURING DAYLIGHT HOURS (15 MIN)
# dl = daylength(lat, lon, JD(all_dates[i]), tmz)
# dy = all_dates[i] - 6 * 3600
# sunrise = floor(dl[1,1]) * 3600
# sunset = floor(dl[1,2] + 1) * 3600
# jd = JD(seq(dy+sunrise, dy+sunset, as.difftime(15, units="mins")))

# MAIN FIX
jd=JD(seq(ISOdate(2017,4,21,0),ISOdate(2017,4,21,23),by="15 mins"))
## sun position and vector
sv = sunvector(jd,lat,lon,tmz)
sp1=sunpos(sv)
## daylight zenith<=90
sp=sp1[which(sp1[,2]<=90),]
sv=sv[which(sp1[,2]<=90),]

# SOLAR GEOMETRY
height = array(d_mat)
visibility = 28 # Figure out a method?
RH = 60
tempK = 278.15
zenith=sp[,2]
az_noon = which.min(abs(180-sunpos(sv))[,1])
azimuth_eq = c(sunpos(sv)[,1][1:az_noon]-180,sunpos(sv)[,1][(az_noon+1):length(sunpos(sv)[,1])]-180)

# ZENITH ON A FLAT PLANE !!!!!@#$%^ NOT SURE ABOUT THIS>>>
cos_sfc=sv%*%as.vector(normalvector(0,0))
cos_sfc[cos_sfc<0]=0
#cos_sfc = as.matrix(cos_sfc, ncol=ncol(dem))

# moment loop
sh = NULL
for (m in 1:length(sv[,1])){
  # TOPOGRAPHIC SHADING (cast shadow)
  #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
  sh = doshade(d_mat, sv[m,], dl=dem_res)
  
  # INCIDENT ANGLE
  Z_rad  = radians(zenith)[m]
  Azimuth_rad = radians(azimuth_eq)[m]
  exposures  = a - radians(180)
  cos_inc = acos((cos(Z_rad) * cos(s)) +
                   (sin(Z_rad) * sin(s) * cos(Azimuth_rad - exposures)))
  
  cos_inc = as.matrix(cos_inc)
  # cos_inc[cos_inc < 0] = 0
  cos_inc[cos_inc > radians(90)] = radians(90)
  cos_inc = cos(cos_inc)
  # svv = sv
  # svv[,1:2] = sv[,1:2]*-1
  # hish = hillshading(cgr, svv[m,])
  # cos_inc = cos(hish)
  #################################################################################
  # INSOLATION
  Idirdif = insolation(zenith[m],jd[m],height,visibility,RH,tempK,0.002,0.45)
  Ib = matrix(Idirdif[,1],nrow=nrow(dem),ncol=ncol(dem))
  Id = matrix(Idirdif[,2],nrow=nrow(dem),ncol=ncol(dem))
  alphaT = 0.6 #0.45
  Iglob = (Ib * sh + Id * VF_mat )*cos_sfc[m] #use cos_inc or cos_sfc[m] ?
  #Ir = Iglob * (1 - VF_mat) * alphaT
  Ir = Iglob * (as.matrix((1 + cos(s))/2) - VF_mat) * alphaT # Dozier (Hetrick 1993)
  #Ir = (Ib*sh + Id * VF_mat) * 0.45 * (1 - VF_mat) * cos_sfc[m]# x albedo
  
  print(paste("Moment",m,"of",length(zenith)," - Ib:",round(mean(Ib,na.rm=T),2),' - Id:',round(mean(Id,na.rm=T),2),
              ' - Ir:',round(mean(Ir,na.rm=T),2)))
  
  # ADD TO MODELS
  model_flat =    model_flat + (Ib)*cos_sfc[m]
  model_flat_base = model_flat_base + (Ib + Id)*cos_sfc[m]
  model_flat_sh = model_flat_sh + (Ib * sh)*cos_sfc[m]
  model_inc_sr =  model_inc_sr + (Ib) * cos_inc
  model_inc_sh =  model_inc_sh + (Ib * sh)*cos_inc
  model_vf_base = model_vf_base + (Ib * sh + Id)*cos_inc
  model_vf =      model_vf + (Ib * sh + Id * VF_mat )*cos_inc
  model_ref =     model_ref + (Ib * sh + Id * VF_mat + Ir)*cos_inc
  ######################################################################################
  
  # par(mfrow=c(2,2))
  # plot(make_raster((Ib)*cos_sfc[m], dem), main=paste("model flat", m, "of", length(zenith)), col= colsgrad)
  # plot(glacier, lwd=2,add=T)
  # plot(make_raster((Ib) * cos_inc, dem), main="model slope", col= colsgrad)
  # plot(glacier, lwd=2,add=T)
  # plot(make_raster((Ib * sh)*cos_inc, dem), main="model slope and shade", col= colsgrad)
  # plot(glacier, lwd=2,add=T)
  # plot(make_raster((Ib * sh + Id * VF_mat + Ir * (1 - VF_mat))*cos_inc, dem), main="model combined", col= colsgrad)
  # plot(glacier, lwd=2,add=T)
  
  # ### PLOTS >>>
  if (m == 1){ # could do 2 plots -- compare Ib Id Ir
    #png(paste('images/models_g',g,'_',round(dem_res),'.png',sep=''))
    plot(1:length(zenith),rep(NA,length(zenith)),ylim=c(0,1100),
         main='June 21',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour')
    legend('topright',c('flat','flat_sh','slope','slope_sh','viewf','refl'),
           pch=seq(13,18),cex=1.2)
    legend('topleft',paste('DEM resolution:',round(dem_res),'m'),bty='n',cex=0.8)
    points(m,mean((Ib)*cos_sfc[m],na.rm=T),pch=13)
    points(m,mean((Ib * sh)*cos_sfc[m],na.rm=T),pch=14)
    points(m,mean((Ib) * cos_inc,na.rm=T),pch=15)
    points(m,mean((Ib * sh)*cos_inc,na.rm=T),pch=16)
    points(m,mean((Ib * sh + Id * VF_mat )*cos_inc,na.rm=T),pch=17)
    # points(m,mean((Ib * sh + Id)*cos_inc,na.rm=T),pch=17)
    #points(m,mean((Ib * sh + Id * VF_mat + Ir * (1 - VF_mat))*cos_inc,na.rm=T),pch=18)
    #points(m,mean((Ib * sh + Id * VF_mat + Ir * (as.matrix((1 + cos(s))/2) - VF_mat))*cos_inc,na.rm=T),pch=18)
    points(m,mean((Ib * sh + Id * VF_mat + Ir)*cos_inc,na.rm=T),pch=18)
  } else{
    points(m,mean((Ib)*cos_sfc[m],na.rm=T),pch=13)
    points(m,mean((Ib * sh)*cos_sfc[m],na.rm=T),pch=14)
    points(m,mean((Ib) * cos_inc,na.rm=T),pch=15)
    points(m,mean((Ib * sh)*cos_inc,na.rm=T),pch=16)
    points(m,mean((Ib * sh + Id * VF_mat )*cos_inc,na.rm=T),pch=17)
    # points(m,mean((Ib * sh + Id)*cos_inc,na.rm=T),pch=17)
    # points(m,mean((Ib * sh + Id * VF_mat + Ir * (1 - VF_mat))*cos_inc,na.rm=T),pch=18)
    #points(m,mean((Ib * sh + Id * VF_mat + Ir * (as.matrix((1 + cos(s))/2) - VF_mat))*cos_inc,na.rm=T),pch=18)
    points(m,mean((Ib * sh + Id * VF_mat + Ir)*cos_inc,na.rm=T),pch=18)
  }
  ## END PLOTS >>>
}
# SAVE PLOT
#dev.off() # for plotting models

print(paste("Raster Math...", rf))

# DAILY MEAN
model_flat =    model_flat / length(zenith)
model_flat_base = model_flat_base / length(zenith)
model_flat_sh = model_flat_sh / length(zenith)
model_inc_sr =  model_inc_sr / length(zenith)
model_inc_sh =  model_inc_sh / length(zenith)
model_vf_base = model_vf_base / length(zenith)
model_vf =      model_vf / length(zenith)
model_ref =     model_ref / length(zenith)

############################################
############################################
# CREATE MODELS
slope_asp = model_inc_sh - model_flat_sh
tot_sh = model_flat_sh - model_flat
cast_sh = model_inc_sh - model_inc_sr
sh_rel = (tot_sh - cast_sh)
#sh_rel = ((model_inc_sr - model_flat) - slope_asp)
comb = model_ref - model_flat_base
viewf = model_vf - model_vf_base
refl = model_ref - model_vf
refl = model_ref - model_vf_base
#comb = slope_asp + cast_sh + viewf + refl
############################################
############################################

# REMAKE RASTER
tot_sh = make_raster(tot_sh, dem)
cast_sh = make_raster(cast_sh, dem)
sh_rel = make_raster(sh_rel, dem)
slope_asp = make_raster(slope_asp, dem)
comb = make_raster(comb, dem)
viewf = make_raster(viewf, dem)
refl = make_raster(refl, dem)

# SAVE TO STACK & MASK
ex = extent(glacier)
ex@ymin = ex@ymin-0.001
ex@ymax = ex@ymax+0.001
new_stk = stack(slope_asp, sh_rel, cast_sh, tot_sh, 
                viewf, refl, comb, dem, Vf_dem)

names(new_stk) = c("Slope_Asp","Sh_rel","Cast_sh","Shading",
                   "Viewf", "Ref", "Combined", "DEM", 'Vf')

new_stk = crop(new_stk, extent(ex))
new_stk = mask(new_stk, glacier)





################################################################################################

### PLOTS PLOTS PLOTS


# PLOT PARAMS
########################################################################################
par(mfrow=c(1,1))
par(mar=c(5,5.8,2,1))
#cols = c('steelblue',rev(brewer.pal(4,'Reds')[3]),'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
cols = c('steelblue',rev(brewer.pal(6,'Reds')[3:5]),'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
y_lim = c(-250,80)
l_ty = c(5,1,1,1,1,4,1)
l_wd = c(3,4,4,4,3,3,5)
########################################################################################

dfg = as.data.frame(new_stk)
dfg = dfg[!is.na(dfg[,1]),]
mod = c(1:7)
mod_name = c('Slope & aspect','Shaded Relief','Cast Shadows','Total shading','Diffuse Sky','Reflected','Combined')

plot(smooth.spline(dfg[,1]~dfg[,8]), type = 'l', col='white',
     ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = l_wd[1],
     lty = l_ty[1],xlab='Distance from mid-elevation (m)',cex.lab=1.3, cex.axis=1.8)
for(j in mod){
  lines(smooth.spline(dfg[,j]~dfg[,8]), lwd=l_wd[match(j,mod)],
        lty=l_ty[match(j,mod)], col=cols[match(j,mod)])
}
abline(h=0,lty=3)
legend('bottomleft',mod_name,col=cols,lwd=l_wd,lty=l_ty,cex=1.4,bty='n')


# use tot_shading for north glacier

plot(new_stk[[1]], col=colsgrad[1:8], breaks = c(-250,-200,-150,-100,-60,-10,0,10,60))
plot(glacier,add=T)



plot(tfstk[[1]], col=colsgrad[1:8], breaks = c(-250,-200,-150,-100,-60,-10,0,10,60))
plot(glacier,add=T)
plot(spt3,add=T,pch=20)
#






















# moment loop
for (m in 1:length(sv[,1])){
  # TOPOGRAPHIC SHADING (cast shadow)
  #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
  sh = doshade(d_mat, sv[m,], dl=dem_res)
  
  # INCIDENT ANGLE
  Z_rad  = radians(zenith)[m]
  Azimuth_rad = radians(azimuth_eq)[m]
  exposures  = a - radians(180)
  cos_inc = acos((cos(Z_rad) * cos(s)) +
                   (sin(Z_rad) * sin(s) * cos(Azimuth_rad - exposures)))
  
  cos_inc = as.matrix(cos_inc)
  # cos_inc[cos_inc < 0] = 0
  cos_inc[cos_inc > radians(90)] = radians(90)
  cos_inc = cos(cos_inc)
  # svv = sv
  # svv[,1:2] = sv[,1:2]*-1
  # hish = hillshading(cgr, svv[m,])
  # cos_inc = cos(hish)
  #################################################################################
  # INSOLATION
  Idirdif = insolation(zenith[m],jd[m],height,visibility,RH,tempK,0.002,0.45)
  Ib = matrix(Idirdif[,1],nrow=nrow(dem),ncol=ncol(dem))
  Id = matrix(Idirdif[,2],nrow=nrow(dem),ncol=ncol(dem))
  alphaT = 0.6 #0.45
  Iglob = (Ib * sh + Id * VF_mat )*cos_sfc[m] #use cos_inc or cos_sfc[m] ?
  #Ir = Iglob * (1 - VF_mat) * alphaT
  Ir = Iglob * (as.matrix((1 + cos(s))/2) - VF_mat) * alphaT # Dozier (Hetrick 1993)
  #Ir = (Ib*sh + Id * VF_mat) * 0.45 * (1 - VF_mat) * cos_sfc[m]# x albedo
  
  print(paste("Moment",m,"of",length(zenith)," - Ib:",round(mean(Ib,na.rm=T),2),' - Id:',round(mean(Id,na.rm=T),2),
              ' - Ir:',round(mean(Ir,na.rm=T),2)))
  
  # ### PLOTS >>>
  if (m == 1){ # could do 2 plots -- compare Ib Id Ir
    #png(paste('images/models_g',g,'_',round(dem_res),'.png',sep=''))
    plot(1:length(zenith),rep(NA,length(zenith)),ylim=c(0,1100),
         main='June 21',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour')
    legend('topright',c('flat','flat_sh','slope','slope_sh','diff_open','diff_vf','refl'),
           pch=c(13,14,15,16,17,17,18),cex=1.2)
    legend('topleft',paste('DEM resolution:',round(dem_res),'m'),bty='n',cex=0.8)
    points(m,mean((Ib)*cos_sfc[m],na.rm=T),pch=13)
    points(m,mean((Ib * sh)*cos_sfc[m],na.rm=T),pch=14)
    points(m,mean((Ib) * cos_inc,na.rm=T),pch=15)
    points(m,mean((Ib * sh)*cos_inc,na.rm=T),pch=16)
    points(m,mean((Ib * sh + Id * VF_mat )*cos_inc,na.rm=T),pch=17)
    #points(m,mean((Ib * sh + Id)*cos_inc,na.rm=T),pch=17)
    # points(m,mean((Ib * sh + Id * VF_mat + Ir * (1 - VF_mat))*cos_inc,na.rm=T),pch=18)
    # points(m,mean((Ib * sh + Id * VF_mat + Ir * (as.matrix((1 + cos(s))/2) - VF_mat))*cos_inc,na.rm=T),pch=18)
    points(m,mean((Ib * sh + Id * VF_mat + Ir)*cos_inc,na.rm=T),pch=18)
  } else{
    points(m,mean((Ib)*cos_sfc[m],na.rm=T),pch=13)
    points(m,mean((Ib * sh)*cos_sfc[m],na.rm=T),pch=14)
    points(m,mean((Ib) * cos_inc,na.rm=T),pch=15)
    points(m,mean((Ib * sh)*cos_inc,na.rm=T),pch=16)
    points(m,mean((Ib * sh + Id * VF_mat )*cos_inc,na.rm=T),pch=17)
    #points(m,mean((Ib * sh + Id)*cos_inc,na.rm=T),pch=17)
    # points(m,mean((Ib * sh + Id * VF_mat + Ir * (1 - VF_mat))*cos_inc,na.rm=T),pch=18)
    # points(m,mean((Ib * sh + Id * VF_mat + Ir * (as.matrix((1 + cos(s))/2) - VF_mat))*cos_inc,na.rm=T),pch=18)
    points(m,mean((Ib * sh + Id * VF_mat + Ir)*cos_inc,na.rm=T),pch=18)
  }
  ## END PLOTS >>>
}










### DIFFERENCE

# moment loop
for (m in 1:length(sv[,1])){
  # TOPOGRAPHIC SHADING (cast shadow)
  #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
  sh = doshade(d_mat, sv[m,], dl=dem_res)
  
  # INCIDENT ANGLE
  Z_rad  = radians(zenith)[m]
  Azimuth_rad = radians(azimuth_eq)[m]
  exposures  = a - radians(180)
  cos_inc = acos((cos(Z_rad) * cos(s)) +
                   (sin(Z_rad) * sin(s) * cos(Azimuth_rad - exposures)))
  
  cos_inc = as.matrix(cos_inc)
  # cos_inc[cos_inc < 0] = 0
  cos_inc[cos_inc > radians(90)] = radians(90)
  cos_inc = cos(cos_inc)
  # svv = sv
  # svv[,1:2] = sv[,1:2]*-1
  # hish = hillshading(cgr, svv[m,])
  # cos_inc = cos(hish)
  #################################################################################
  # INSOLATION
  Idirdif = insolation(zenith[m],jd[m],height,visibility,RH,tempK,0.002,0.45)
  Ib = matrix(Idirdif[,1],nrow=nrow(dem),ncol=ncol(dem))
  Id = matrix(Idirdif[,2],nrow=nrow(dem),ncol=ncol(dem))
  alphaT = 0.6 #0.45
  Iglob = (Ib + Id * VF_mat )*cos_sfc[m] #use cos_inc or cos_sfc[m] ?
  #Ir = Iglob * (1 - VF_mat) * alphaT
  Ir = Iglob * (as.matrix((1 + cos(s))/2) - VF_mat) * alphaT # Dozier (Hetrick 1993)
  #Ir = (Ib*sh + Id * VF_mat) * 0.45 * (1 - VF_mat) * cos_sfc[m]# x albedo
  
  print(paste("Moment",m,"of",length(zenith)," - Ib:",round(mean(Ib,na.rm=T),2),' - Id:',round(mean(Id,na.rm=T),2),
              ' - Ir:',round(mean(Ir,na.rm=T),2)))
  
  
  model_flat =    (Ib)*cos_sfc[m]
  model_flat_base = (Ib + Id)*cos_sfc[m]
  model_flat_sh = (Ib * sh)*cos_sfc[m]
  model_inc_sr =  (Ib) * cos_inc
  model_inc_sh =  (Ib * sh)*cos_inc
  model_vf_base = (Ib * sh + Id)*cos_inc
  model_vf =      (Ib * sh + Id * VF_mat )*cos_inc
  model_ref =     (Ib * sh + Id * VF_mat + Ir)*cos_inc
  # CREATE MODELS
  slope_asp = model_inc_sh - model_flat_sh
  tot_sh = model_flat_sh - model_flat
  cast_sh = model_inc_sh - model_inc_sr
  sh_rel = (tot_sh - cast_sh)
  #sh_rel = ((model_inc_sr - model_flat) - slope_asp)
  comb = model_ref - model_flat_base
  viewf = model_vf - model_vf_base
  refl = model_ref - model_vf
  refl = model_ref - model_vf_base
  # REMAKE RASTER
  tot_sh = make_raster(tot_sh, dem)
  cast_sh = make_raster(cast_sh, dem)
  sh_rel = make_raster(sh_rel, dem)
  slope_asp = make_raster(slope_asp, dem)
  comb = make_raster(comb, dem)
  viewf = make_raster(viewf, dem)
  refl = make_raster(refl, dem)
  
  # SAVE TO STACK & MASK
  ex = extent(glacier)
  ex@ymin = ex@ymin-0.001
  ex@ymax = ex@ymax+0.001
  new_stk = stack(slope_asp, sh_rel, cast_sh, tot_sh, 
                  viewf, refl, comb, dem, Vf_dem)
  
  names(new_stk) = c("Slope_Asp","Sh_rel","Cast_sh","Shading",
                     "Viewf", "Ref", "Combined", "DEM", 'Vf')
  
  new_stk = crop(new_stk, extent(ex))
  new_stk = mask(new_stk, glacier)
  
  # ### PLOTS >>>
  if (m == 1){ # could do 2 plots -- compare Ib Id Ir
    #png(paste('images/models_g',g,'_',round(dem_res),'.png',sep=''))
    plot(1:length(zenith),rep(NA,length(zenith)),ylim=c(-200,100),
         main='June 21 - change in irradiance',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour')
    legend('topright',c('slope & asp','total_sh','cast_sh','sh_rel','diff_vf','refl', 'comb'),
           pch=c(seq(13,18),3),cex=1.2)
    abline(h=0,lty=3)
    legend('topleft',paste('DEM resolution:',round(dem_res),'m'),bty='n',cex=0.8)
    points(m,mean(values(new_stk[[1]]), na.rm=T),pch=13)
    points(m,mean(values(new_stk[[4]]), na.rm=T),pch=14)
    points(m,mean(values(new_stk[[3]]), na.rm=T),pch=15)
    points(m,mean(values(new_stk[[2]]), na.rm=T),pch=16)
    points(m,mean(values(new_stk[[5]]), na.rm=T),pch=17)
    points(m,mean(values(new_stk[[6]]), na.rm=T),pch=18)
    points(m,mean(values(new_stk[[7]]), na.rm=T),pch=3)
  } else{
    points(m,mean(values(new_stk[[1]]), na.rm=T),pch=13)
    points(m,mean(values(new_stk[[4]]), na.rm=T),pch=14)
    points(m,mean(values(new_stk[[3]]), na.rm=T),pch=15)
    points(m,mean(values(new_stk[[2]]), na.rm=T),pch=16)
    points(m,mean(values(new_stk[[5]]), na.rm=T),pch=17)
    points(m,mean(values(new_stk[[6]]), na.rm=T),pch=18)
    points(m,mean(values(new_stk[[7]]), na.rm=T),pch=3)
  }
  ## END PLOTS >>>
}





#################################################################################
#################################################################################
#################################################################################

### DIFFERENCE MAIN 5

# moment loop
for (m in 1:length(sv[,1])){
  # TOPOGRAPHIC SHADING (cast shadow)
  #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
  sh = doshade(d_mat, sv[m,], dl=dem_res)
  
  # INCIDENT ANGLE
  Z_rad  = radians(zenith)[m]
  Azimuth_rad = radians(azimuth_eq)[m]
  exposures  = a - radians(180)
  cos_inc = acos((cos(Z_rad) * cos(s)) +
                   (sin(Z_rad) * sin(s) * cos(Azimuth_rad - exposures)))
  
  cos_inc = as.matrix(cos_inc)
  # cos_inc[cos_inc < 0] = 0
  cos_inc[cos_inc > radians(90)] = radians(90)
  cos_inc = cos(cos_inc)
  # svv = sv
  # svv[,1:2] = sv[,1:2]*-1
  # hish = hillshading(cgr, svv[m,])
  # cos_inc = cos(hish)
  # INSOLATION
  Idirdif = insolation(zenith[m],jd[m],height,visibility,RH,tempK,0.002,0.45)
  Ib = matrix(Idirdif[,1],nrow=nrow(dem),ncol=ncol(dem))
  Id = matrix(Idirdif[,2],nrow=nrow(dem),ncol=ncol(dem))
  alphaT = 0.6 #0.45
  Iglob = (Ib + Id * VF_mat )*cos_sfc[m] #use cos_inc or cos_sfc[m] ?
  #Ir = Iglob * (1 - VF_mat) * alphaT
  Ir = Iglob * (as.matrix((1 + cos(s))/2) - VF_mat) * alphaT # Dozier (Hetrick 1993)
  #Ir = (Ib*sh + Id * VF_mat) * 0.45 * (1 - VF_mat) * cos_sfc[m]# x albedo
  
  print(paste("Moment",m,"of",length(zenith)," - Ib:",round(mean(Ib,na.rm=T),2),' - Id:',round(mean(Id,na.rm=T),2),
              ' - Ir:',round(mean(Ir,na.rm=T),2)))
  
  
  model_flat =    (Ib)*cos_sfc[m]
  model_flat_base = (Ib + Id)*cos_sfc[m]
  model_flat_sh = (Ib * sh)*cos_sfc[m]
  model_inc_sr =  (Ib) * cos_inc
  model_inc_sh =  (Ib * sh)*cos_inc
  model_vf_base = (Ib * sh + Id)*cos_inc
  model_vf =      (Ib * sh + Id * VF_mat )*cos_inc
  model_ref =     (Ib * sh + Id * VF_mat + Ir)*cos_inc
  
  
  # CREATE MODELS
  slope_asp = model_inc_sh - model_flat_sh
  tot_sh = model_flat_sh - model_flat
  cast_sh = model_inc_sh - model_inc_sr
  sh_rel = (tot_sh - cast_sh)
  comb = model_ref - model_flat_base
  viewf = model_vf - model_vf_base
  refl = model_ref - model_vf #refl = model_ref - model_vf_base
  # 
  # ## CREATE MODELS
  # slope_asp = (model_inc_sh) / model_flat_sh
  # tot_sh = model_flat_sh / model_flat
  # cast_sh = model_inc_sh / model_inc_sr
  # sh_rel = (tot_sh / cast_sh)
  # comb = model_ref / model_flat_base
  # viewf = model_vf / model_vf_base
  # refl = model_ref / model_vf_base
  
  # slope_asp = (model_inc_sh - model_flat_sh)/model_flat_sh
  # tot_sh = (model_flat_sh - model_flat)/model_flat
  # cast_sh = (model_inc_sh - model_inc_sr)/model_flat_sh
  # sh_rel = ((tot_sh - cast_sh) - model_inc_sr)/model_inc_sr
  # comb = (model_ref - model_flat_base)/model_flat_base
  # viewf = (model_vf - model_vf_base)/model_vf_base
  # refl = (model_ref - model_vf_base)/model_vf_base
  
  
  # REMAKE RASTER
  tot_sh = make_raster(tot_sh, dem)
  cast_sh = make_raster(cast_sh, dem)
  sh_rel = make_raster(sh_rel, dem)
  slope_asp = make_raster(slope_asp, dem)
  comb = make_raster(comb, dem)
  viewf = make_raster(viewf, dem)
  refl = make_raster(refl, dem)
  
  # SAVE TO STACK & MASK
  ex = extent(glacier)
  ex@ymin = ex@ymin-0.001
  ex@ymax = ex@ymax+0.001
  new_stk = stack(slope_asp, sh_rel, cast_sh, tot_sh, 
                  viewf, refl, comb, dem, Vf_dem)
  
  names(new_stk) = c("Slope_Asp","Sh_rel","Cast_sh","Shading",
                     "Viewf", "Ref", "Combined", "DEM", 'Vf')
  
  new_stk = crop(new_stk, extent(ex))
  new_stk = mask(new_stk, glacier)
  
  # ### PLOTS >>>
  if (m == 1){ # could do 2 plots -- compare Ib Id Ir
    #png(paste('images/models_g',g,'_',round(dem_res),'.png',sep=''))
    plot(1:length(zenith),rep(NA,length(zenith)),ylim=c(-200,100), #,ylim=c(-1,1),
         main='June 21 - change in irradiance',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour')
    legend('bottomleft',c('slope & asp','total_sh','diff_vf','refl', 'comb'),
           pch=c(13,14,17,18,3),cex=1.2)
    abline(h=0,lty=3)
    legend('topleft',paste('DEM resolution:',round(dem_res),'m'),bty='n',cex=0.8)
    points(m,mean(values(new_stk[[1]]), na.rm=T),pch=13)
    points(m,mean(values(new_stk[[4]]), na.rm=T),pch=14)
    points(m,mean(values(new_stk[[5]]), na.rm=T),pch=17)
    points(m,mean(values(new_stk[[6]]), na.rm=T),pch=18)
    points(m,mean(values(new_stk[[7]]), na.rm=T),pch=3)
  } else{
    points(m,mean(values(new_stk[[1]]), na.rm=T),pch=13)
    points(m,mean(values(new_stk[[4]]), na.rm=T),pch=14)
    points(m,mean(values(new_stk[[5]]), na.rm=T),pch=17)
    points(m,mean(values(new_stk[[6]]), na.rm=T),pch=18)
    points(m,mean(values(new_stk[[7]]), na.rm=T),pch=3)
  }
  ## END PLOTS >>>
}









#################################################################################
#################################################################################
#################################################################################

### DIFFERENCE at a point
#spt3 = SpatialPoints(coordinates(data.frame(86.948,28.04)),proj4string=crs(dem)) # NORTH
spt3 = SpatialPoints(coordinates(data.frame(86.625,28.0415)),proj4string=crs(dem)) # SOUTH

# moment loop
for (m in 1:length(sv[,1])){
  # TOPOGRAPHIC SHADING (cast shadow)
  #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
  sh = doshade(d_mat, sv[m,], dl=dem_res)
  
  # INCIDENT ANGLE
  Z_rad  = radians(zenith)[m]
  Azimuth_rad = radians(azimuth_eq)[m]
  exposures  = a - radians(180)
  cos_inc = acos((cos(Z_rad) * cos(s)) +
                   (sin(Z_rad) * sin(s) * cos(Azimuth_rad - exposures)))
  
  cos_inc = as.matrix(cos_inc)
  # cos_inc[cos_inc < 0] = 0
  cos_inc[cos_inc > radians(90)] = radians(90)
  cos_inc = cos(cos_inc)
  # svv = sv
  # svv[,1:2] = sv[,1:2]*-1
  # hish = hillshading(cgr, svv[m,])
  # cos_inc = cos(hish)
  # INSOLATION
  Idirdif = insolation(zenith[m],jd[m],height,visibility,RH,tempK,0.002,0.45)
  Ib = matrix(Idirdif[,1],nrow=nrow(dem),ncol=ncol(dem))
  Id = matrix(Idirdif[,2],nrow=nrow(dem),ncol=ncol(dem))
  alphaT = 0.6 #0.45
  Iglob = (Ib + Id * VF_mat )*cos_sfc[m] #use cos_inc or cos_sfc[m] ?
  #Ir = Iglob * (1 - VF_mat) * alphaT
  Ir = Iglob * (as.matrix((1 + cos(s))/2) - VF_mat) * alphaT # Dozier (Hetrick 1993)
  #Ir = (Ib*sh + Id * VF_mat) * 0.45 * (1 - VF_mat) * cos_sfc[m]# x albedo
  
  print(paste("Moment",m,"of",length(zenith)," - Ib:",round(mean(Ib,na.rm=T),2),' - Id:',round(mean(Id,na.rm=T),2),
              ' - Ir:',round(mean(Ir,na.rm=T),2)))
  
  
  model_flat =    (Ib)*cos_sfc[m]
  model_flat_base = (Ib + Id)*cos_sfc[m]
  model_flat_sh = (Ib * sh)*cos_sfc[m]
  model_inc_sr =  (Ib) * cos_inc
  model_inc_sh =  (Ib * sh)*cos_inc
  model_vf_base = (Ib * sh + Id)*cos_inc
  model_vf =      (Ib * sh + Id * VF_mat )*cos_inc
  model_ref =     (Ib * sh + Id * VF_mat + Ir)*cos_inc
  
  
  # # CREATE MODELS
  slope_asp = model_inc_sh - model_flat_sh
  tot_sh = model_flat_sh - model_flat
  cast_sh = model_inc_sh - model_inc_sr
  sh_rel = (tot_sh - cast_sh)
  comb = model_ref - model_flat_base
  viewf = model_vf - model_vf_base
  refl = model_ref - model_vf_base

  # CREATE MODELS
  # slope_asp = (model_inc_sh - model_flat_sh) + model_flat
  # tot_sh = (model_flat_sh - model_flat) + model_flat
  # cast_sh = (model_inc_sh - model_inc_sr) + model_flat
  # sh_rel = ((tot_sh - cast_sh)) + model_flat
  # comb = (model_ref - model_flat_base) + model_flat
  # viewf = (model_vf - model_vf_base) + model_flat
  # refl = (model_ref - model_vf_base) + model_flat
  
  
  # REMAKE RASTER
  tot_sh = make_raster(tot_sh, dem)
  cast_sh = make_raster(cast_sh, dem)
  sh_rel = make_raster(sh_rel, dem)
  slope_asp = make_raster(slope_asp, dem)
  comb = make_raster(comb, dem)
  viewf = make_raster(viewf, dem)
  refl = make_raster(refl, dem)
  
  # SAVE TO STACK & MASK
  ex = extent(glacier)
  ex@ymin = ex@ymin-0.001
  ex@ymax = ex@ymax+0.001
  new_stk = stack(slope_asp, sh_rel, cast_sh, tot_sh, 
                  viewf, refl, comb, dem, Vf_dem)
  
  names(new_stk) = c("Slope_Asp","Sh_rel","Cast_sh","Shading",
                     "Viewf", "Ref", "Combined", "DEM", 'Vf')
  
  new_stk = crop(new_stk, extent(ex))
  new_stk = mask(new_stk, glacier)
  
  
  #EXTRACT
  sapt = extract(new_stk[[1]], spt3)
  tspt = extract(new_stk[[4]], spt3)
  dspt = extract(new_stk[[5]], spt3)
  rept = extract(new_stk[[6]], spt3)
  copt = extract(new_stk[[7]], spt3)
  
  #EXTRACT
  sapt = extract(new_stk[[1]], spt3)
  tspt = extract(new_stk[[4]], spt3)
  dspt = extract(new_stk[[5]], spt3)
  rept = extract(new_stk[[6]], spt3)
  copt = extract(new_stk[[7]], spt3)
  
  
  
  # ### PLOTS >>>
  if (m == 1){ # could do 2 plots -- compare Ib Id Ir
    #png(paste('images/models_g',g,'_',round(dem_res),'.png',sep=''))
    plot(1:length(zenith),rep(NA,length(zenith)),ylim=c(-200,200), # ylim=c(-300,100)
         main='June 21 - change in irradiance',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour')
    legend('bottom',c('slope & asp','total_sh','diff_vf','refl', 'comb'),
           pch=c(13,14,17,18,3),cex=1.2)
    abline(h=0,lty=3)
    legend('topleft',paste('DEM resolution:',round(dem_res),'m'),bty='n',cex=0.8)
    points(m,sapt,pch=13)
    points(m,tspt,pch=14)
    points(m,dspt,pch=17)
    points(m,rept,pch=18)
    points(m,copt,pch=3)
  } else{
    points(m,sapt,pch=13)
    points(m,tspt,pch=14)
    points(m,dspt,pch=17)
    points(m,rept,pch=18)
    points(m,copt,pch=3)
  }
  ## END PLOTS >>>
}


































#par(mfrow=c(2,2))

for (m in 1:length(sv[,1])){
  # TOPOGRAPHIC SHADING (cast shadow)
  #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
  sh = doshade(d_mat, sv[m,], dl=dem_res)
  
  # INCIDENT ANGLE
  Z_rad  = radians(zenith)[m]
  Azimuth_rad = radians(azimuth_eq)[m]
  exposures  = a - radians(180)
  cos_inc = acos((cos(Z_rad) * cos(s)) +
                   (sin(Z_rad) * sin(s) * cos(Azimuth_rad - exposures)))
  
  cos_inc = as.matrix(cos_inc)
  # cos_inc[cos_inc < 0] = 0
  cos_inc[cos_inc > radians(90)] = radians(90)
  cos_inc = cos(cos_inc)
  # svv = sv
  # svv[,1:2] = sv[,1:2]*-1
  # hish = hillshading(cgr, svv[m,])
  # cos_inc = cos(hish)
  #################################################################################
  # INSOLATION
  Idirdif = insolation(zenith[m],jd[m],height,visibility,RH,tempK,0.002,0.45)
  Ib = matrix(Idirdif[,1],nrow=nrow(dem),ncol=ncol(dem))
  Id = matrix(Idirdif[,2],nrow=nrow(dem),ncol=ncol(dem))
  alphaT = 0.6 #0.45
  Iglob = (Ib * sh + Id * VF_mat )*cos_sfc[m] #use cos_inc or cos_sfc[m] ?
  #Ir = Iglob * (1 - VF_mat) * alphaT
  Ir = Iglob * (as.matrix((1 + cos(s))/2) - VF_mat) * alphaT # Dozier (Hetrick 1993)
  #Ir = (Ib*sh + Id * VF_mat) * 0.45 * (1 - VF_mat) * cos_sfc[m]# x albedo
  
  print(paste("Moment",m,"of",length(zenith)," - Ib:",round(mean(Ib,na.rm=T),2),' - Id:',round(mean(Id,na.rm=T),2),
              ' - Ir:',round(mean(Ir,na.rm=T),2)))
  
  # ADD TO MODELS
  model_flat =    make_raster((Ib)*cos_sfc[m], dem)
  model_flat_base = make_raster((Ib + Id)*cos_sfc[m], dem)
  model_flat_sh = make_raster((Ib * sh)*cos_sfc[m], dem)
  model_inc_sr =  make_raster((Ib) * cos_inc, dem)
  model_inc_sh =  make_raster((Ib * sh)*cos_inc, dem)
  model_vf_base = make_raster((Ib * sh + Id)*cos_inc, dem)
  model_vf =      make_raster((Ib * sh + Id * VF_mat )*cos_inc, dem)
  model_ref =     make_raster((Ib * sh + Id * VF_mat + Ir)*cos_inc, dem)

  
  # SAVE TO STACK & MASK
  ex = extent(glacier)
  ex@ymin = ex@ymin-0.001
  ex@ymax = ex@ymax+0.001
  new_stk = stack(model_flat, model_flat_sh, model_inc_sr, model_inc_sh,
                  model_vf, model_vf_base, model_ref)

  
  new_stk = crop(new_stk, extent(ex))
  new_stk = mask(new_stk, glacier)
  
  
  #EXTRACT
  mp1 = extract(new_stk[[1]], spt3)
  mp2 = extract(new_stk[[2]], spt3)
  mp3 = extract(new_stk[[3]], spt3)
  mp4 = extract(new_stk[[4]], spt3)
  mp5 = extract(new_stk[[5]], spt3)
  mp6 = extract(new_stk[[6]], spt3)
  mp7 = extract(new_stk[[7]], spt3)
  
  
  ######################################################################################

  # ### PLOTS >>>
  if (m == 1){ # could do 2 plots -- compare Ib Id Ir
    #png(paste('images/models_g',g,'_',round(dem_res),'.png',sep=''))
    plot(1:length(zenith),rep(NA,length(zenith)),ylim=c(0,1100),
         main='June 21',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour')
    legend('topright',c('flat','flat_sh','slope','slope_sh','diffo',"difft",'refl'),
           pch=c(seq(13,17),17:18),cex=1.2)
    legend('topleft',paste('DEM resolution:',round(dem_res),'m'),bty='n',cex=0.8)
    # points(m,mp1,pch=13)
    # points(m,mp2,pch=14)
    #
    # points(m,mp3,pch=15)
    # points(m,mp4,pch=16)
    #
    # points(m,mp5,pch=17)
    # points(m,mp6,pch=17)
    #
    points(m,mp6,pch=17)
    points(m,mp7,pch=18)
  } else{
    # points(m,mp1,pch=13)
    # points(m,mp2,pch=14)
    #
    # points(m,mp3,pch=15)
    # points(m,mp4,pch=16)
    #
    # points(m,mp5,pch=17)
    # points(m,mp6,pch=17)
    #
    points(m,mp6,pch=17)
    points(m,mp7,pch=18)
    
  }
  ## END PLOTS >>>
}






