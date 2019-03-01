#
#
#
#
#

location.variables(demL, shape = glaciers[2,], resampleFactor = 3)


cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')

# RUN
# MAIN FIX
s = s_a[[1]]
a = s_a[[2]]
Vf_dem = make.raster(VF_mat,dem)
jd=JD(seq(ISOdate(2017,3,21,0),ISOdate(2017,3,21,23),by="15 mins"))
## sun position and vector
sv = sunvector(jd,lat_lon[1],lat_lon[2],tmz)
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



#################################################################################
#################################################################################
#################################################################################

### MAIN 4 MODELS

### TOTALS at a point
spt3 = SpatialPoints(coordinates(data.frame(86.948,28.04)),proj4string=crs(dem)) # NORTH
#spt3 = SpatialPoints(coordinates(data.frame(86.625,28.0415)),proj4string=crs(dem)) # SOUTH

# moment loop
for (m in 1:length(sv[,1])){
  # TOPOGRAPHIC SHADING (cast shadow)
  #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
  sh = doshade(d_mat, sv[m,], dl=dem_res)
  
  # INCIDENT ANGLE
  Z_rad  = radians(zenith)[m]
  Azimuth_rad = radians(azimuth_eq)[m]
  exposures  = a - radians(180)
  # cos_inc = acos((cos(Z_rad) * cos(s)) +
  #                  (sin(Z_rad) * sin(s) * cos(Azimuth_rad - exposures)))
  cos_inc = acos((cos(Z_rad) * cos(radians(15))) +
                   (sin(Z_rad) * sin(radians(15)) * cos(Azimuth_rad - exposures)))
  
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
  slope_asp = model_inc_sr
  tot_sh = model_flat_sh
  comb = model_ref
  #viewf = model_vf
  viewf = (model_vf - model_vf_base) + model_flat
  refl = (model_ref - model_vf_base) + model_flat
  
  
  # REMAKE RASTER
  tot_sh = make.raster(tot_sh, dem)
  slope_asp = make.raster(slope_asp, dem)
  comb = make.raster(comb, dem)
  viewf = make.raster(viewf, dem)
  refl = make.raster(refl, dem)
  
  # SAVE TO STACK & MASK
  new_stk = stack(slope_asp, tot_sh, 
                  viewf, refl, comb, dem, Vf_dem)
  
  # names(new_stk) = c("Slope_Asp","Sh_rel","Cast_sh","Shading",
  #                    "Viewf", "Ref", "Combined", "DEM", 'Vf')
  
  new_stk = crop(new_stk, glacier)
  new_stk = mask(new_stk, glacier)
  
  
  #EXTRACT
  sapt = extract(new_stk[[1]], spt3)
  tspt = extract(new_stk[[2]], spt3)
  dspt = extract(new_stk[[3]], spt3)
  rept = extract(new_stk[[4]], spt3)
  copt = extract(new_stk[[5]], spt3)
  
  #EXTRACT
  sapt = extract(new_stk[[1]], spt3)
  tspt = extract(new_stk[[2]], spt3)
  dspt = extract(new_stk[[3]], spt3)
  rept = extract(new_stk[[4]], spt3)
  copt = extract(new_stk[[5]], spt3)

  fbase = make.raster(model_flat,dem)
  fbase = extract(fbase, spt3)
  
  # PLOT ALL VARS
  # # ### PLOTS >>>
  # if (m == 1){ # could do 2 plots -- compare Ib Id Ir
  #   #png(paste('images/models_g',g,'_',round(dem_res),'.png',sep=''))
  #   plot(1:length(zenith),rep(NA,length(zenith)),ylim=c(0,1200), # ylim=c(-300,100)
  #        main='',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour',cex.lab=1.6,cex.axis=1.6)
  #   legend('bottom',c('Incidence angle','Topographic shading','Diffuse sky','Terrain-reflected', 'Combined'),
  #          pch=c(13,14,17,18,3),cex=1.4, col = cols[c(1,4:7)])
  #   abline(h=0,lty=3)
  #   #legend('topleft',paste('DEM resolution:',round(dem_res),'m'),bty='n',cex=0.8)
  #   points(m,fbase,pch=20,cex = 2, col = 'black')
  #   points(m,sapt,pch=13,cex = 2, col = cols[1])
  #   points(m,tspt,pch=14,cex = 2, col = cols[4])
  #   points(m,dspt,pch=17,cex = 1, col = cols[5])
  #   points(m,rept,pch=18,cex = 1.5, col = cols[6])
  #   points(m,copt,pch=3,cex = 3, col = cols[7])
  # } else{
  #   points(m,fbase,pch=20,cex = 2, col = 'black')
  #   points(m,sapt,pch=13,cex = 2, col = cols[1])
  #   points(m,tspt,pch=14,cex = 2, col = cols[4])
  #   points(m,dspt,pch=17,cex = 1, col = cols[5])
  #   points(m,rept,pch=18,cex = 1.5, col = cols[6])
  #   points(m,copt,pch=3,cex = 3, col = cols[7])
  # }
  # ### PLOTS ONLY ONE >>>
  if (m == 1){ # could do 2 plots -- compare Ib Id Ir
    #png(paste('images/models_g',g,'_',round(dem_res),'.png',sep=''))
    plot(1:length(zenith),rep(NA,length(zenith)),ylim=c(0,1100), # ylim=c(-300,100)
         main='',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour',cex.lab=1.6,cex.axis=1.6)
    # legend('topright',c('Flat','Incidence angle','Topographic shading'),
    #        pch=c(20,13,14),cex=1.4, col = c('black',cols[c(1,4)]))
    # legend('topright',c('Flat','Diffuse sky','Terrain-reflected'),
    #        pch=c(20,17,18),cex=1.4, col = c('black',cols[c(5,6)]))
    abline(h=0,lty=3)
    #legend('topleft',paste('DEM resolution:',round(dem_res),'m'),bty='n',cex=0.8)
    points(m,fbase,pch=20,cex = 2, col = 'black')
    # points(m,sapt,pch=13,cex = 2, col = cols[1])
    # points(m,tspt,pch=14,cex = 2, col = cols[4])
    points(m,dspt,pch=17,cex = 1, col = cols[5])
    points(m,rept,pch=18,cex = 1.5, col = cols[6])
    # points(m,copt,pch=3,cex = 3, col = cols[7])
  } else{
    points(m,fbase,pch=20,cex = 2, col = 'black')
    # points(m,sapt,pch=13,cex = 2, col = cols[1])
    # points(m,tspt,pch=14,cex = 2, col = cols[4])
    points(m,dspt,pch=17,cex = 1, col = cols[5])
    points(m,rept,pch=18,cex = 1.5, col = cols[6])
    # points(m,copt,pch=3,cex = 3, col = cols[7])
  }
  ## END PLOTS >>>
}

















#################################################################################
#################################################################################
#################################################################################

### DIFFERENCE at a point
spt3 = SpatialPoints(coordinates(data.frame(86.948,28.04)),proj4string=crs(dem)) # NORTH
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
  # cos_inc = acos((cos(Z_rad) * cos(radians(15))) +
  #                  (sin(Z_rad) * sin(radians(15)) * cos(Azimuth_rad - exposures)))
  
  cos_inc = as.matrix(cos_inc)
  # cos_inc[cos_inc < 0] = 0
  cos_inc[cos_inc > radians(90)] = radians(90)
  cos_inc = cos(cos_inc)

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
  
  # AS PERCENTAGE
  # slope_asp = (model_inc_sh - model_flat_sh) / model_flat_sh
  # tot_sh = (model_flat_sh - model_flat) / model_flat
  # cast_sh = (model_inc_sh - model_inc_sr) / model_inc_sr
  # sh_rel = ((tot_sh - cast_sh)) / cast_sh
  # comb = (model_ref - model_flat_base) / model_flat_base
  # viewf = (model_vf - model_vf_base) / model_vf_base
  # refl = (model_ref - model_vf_base) / model_vf_base
  
  
  # REMAKE RASTER
  tot_sh = make.raster(tot_sh, dem)
  cast_sh = make.raster(cast_sh, dem)
  sh_rel = make.raster(sh_rel, dem)
  slope_asp = make.raster(slope_asp, dem)
  comb = make.raster(comb, dem)
  viewf = make.raster(viewf, dem)
  refl = make.raster(refl, dem)
  
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
  
  
  # #EXTRACT
  # sapt = extract(new_stk[[1]], spt3)
  # tspt = extract(new_stk[[4]], spt3)
  # dspt = extract(new_stk[[5]], spt3)
  # rept = extract(new_stk[[6]], spt3)
  # copt = extract(new_stk[[7]], spt3)
  
  #EXTRACT
  sapt = cellStats(new_stk[[1]], mean)
  tspt = cellStats(new_stk[[4]], mean)
  dspt = cellStats(new_stk[[5]], mean)
  rept = cellStats(new_stk[[6]], mean)
  copt = cellStats(new_stk[[7]], mean)
  
  p_ch = c(13,14,17,18,3)
  p_ch = c(20,20,20,20,20)
  y_lim = c(-150,100) #c(-1,1) #c(-150,100)
  # ### PLOTS >>>
  if (m == 1){ # could do 2 plots -- compare Ib Id Ir
    #png(paste('images/models_g',g,'_',round(dem_res),'.png',sep=''))
    plot(1:length(zenith),rep(NA,length(zenith)),ylim=y_lim, # ylim=c(-300,120)
         main='',ylab=expression(Irradiance~(Wm^-2)),xlab='Solar moment',cex.lab=1.6,cex.axis=1.6)
    legend('bottom',c('Incidence angle','Topographic shading','Diffuse sky','Terrain-reflected', 'Combined'),
           pch=c(13,14,17,18,3),cex=1.4) #, col = cols[c(1,4:7)])
    abline(h=0,lty=3)
    #legend('topleft',paste('DEM resolution:',round(dem_res),'m'),bty='n',cex=0.8)
    points(m,sapt,pch=13,cex = 1.5) #, col = cols[1])
    points(m,tspt,pch=14,cex = 1.5) #, col = cols[4])
    points(m,dspt,pch=17,cex = 1) #, col = cols[5])
    points(m,rept,pch=18,cex = 1.5) #, col = cols[6])
    points(m,copt,pch=3,cex = 2) #, col = cols[7])
  } else{
    points(m,sapt,pch=13,cex = 1.5) #, col = cols[1])
    points(m,tspt,pch=14,cex = 1.5) #, col = cols[4])
    points(m,dspt,pch=17,cex = 1) #, col = cols[5])
    points(m,rept,pch=18,cex = 1.5) #, col = cols[6])
    points(m,copt,pch=3,cex = 2) #, col = cols[7])
  }
  ## END PLOTS >>>
}

