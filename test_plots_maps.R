
# WATCH MODELS FROM MAP VIEW FOR GIVEN DAY
# TESTS TESTS TESTS


require(RColorBrewer)
colsgrad = rev(brewer.pal(11, "RdBu"))

for (m in 1:length(sv[,1])){
  # TOPOGRAPHIC SHADING (cast shadow)
  #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
  sh = doshade(d_mat, sv[m,], dl=dem_res)
  
  # INCIDENT ANGLE
  Z_rad  = radians(zenith)[m]
  #Azimuth_rad = radians(sunpos(sv)[,1])[m]
  Azimuth_rad = radians(azimuth_eq)[m]
  exposures  = a - radians(180)
  cos_inc = acos((cos(Z_rad) * cos(s)) +
                   (sin(Z_rad) * sin(s) * cos(Azimuth_rad - exposures)))
  
  cos_inc = as.matrix(cos_inc)
  #cos_inc[cos_inc < 0] = 0
  cos_inc[cos_inc > radians(90)] = radians(90)
  cos_inc = cos(cos_inc)
  
  #################################################################################
  # INSOLATION
  Idirdif = insolation(zenith[m],jd[m],height,visibility,RH,tempK,0.002,0.45)
  Ib = matrix(Idirdif[,1],nrow=nrow(dem),ncol=ncol(dem))
  Id = matrix(Idirdif[,2],nrow=nrow(dem),ncol=ncol(dem))
  Ir = (Ib + Id) * 0.45 # x albedo
  
  print(paste("Moment",m,"of",length(zenith)," - Ib:",round(mean(Ib,na.rm=T),2),' - Id:',round(mean(Id,na.rm=T),2),
              ' - Ir:',round(mean(Ir,na.rm=T),2)))
  
  # ADD TO MODELS
  # model_flat =    model_flat + (Ib)*cos_sfc[m]
  # model_flat_base = model_flat_base + (Ib + Id)*cos_sfc[m]
  # model_flat_sh = model_flat_sh + (Ib * sh)*cos_sfc[m]
  # model_inc_sr =  model_inc_sr + (Ib) * cos_inc
  # model_inc_sh =  model_inc_sh + (Ib * sh)*cos_inc
  # model_vf_base = model_vf_base + (Ib * sh + Id)*cos_inc
  # model_vf =      model_vf + (Ib * sh + Id * VF_mat )*cos_inc
  # model_ref =     model_ref + (Ib * sh + Id * VF_mat + Ir * (1 - VF_mat))*cos_inc
  
  
  
  par(mfrow=c(2,2))
  plot(make_raster((Ib)*cos_sfc[m], dem), main=paste("model flat", m, "of", length(zenith)), col= colsgrad)
  plot(glacier, lwd=2,add=T)
  plot(make_raster((Ib) * cos_inc, dem), main="model slope", col= colsgrad)
  plot(glacier, lwd=2,add=T)
  plot(make_raster((Ib * sh)*cos_inc, dem), main="model slope and shade", col= colsgrad)
  plot(glacier, lwd=2,add=T)
  plot(make_raster((Ib * sh + Id * VF_mat + Ir * (1 - VF_mat))*cos_inc, dem), main="model combined", col= colsgrad)
  plot(glacier, lwd=2,add=T)
}



# POLAR PLOT OF ZENITH AND AZIMUTH
mo = 3
jd=JD(seq(ISOdate(2017,mo,21,0),ISOdate(2017,mo,21,23),by="mins"))
## sun position and vector
sv = sunvector(jd,lat,lon,tmz)
sp=sunpos(sv)
sp=sp[which(sp[,2]<=90),]
polar.plot(90-sp[,2],sp[,1],start=90,clockwise=TRUE,rp.type='s',
           point.symbols=20,point.col=2,cex=2,radial.lim=c(0,90),
           main='Apparent solar path at Everest on June 21')



# TEST SHADING
## create a pyramid 100 units by side and 50 nunits tall
m=matrix(0,nrow=100,ncol=100)
for (i in 1:100){ for (j in 1:100){
  m[i,j]=50-max(abs(i-50),abs(j-50)) }}
## place it on a large flat expanse
mm=matrix(0,nrow=500,ncol=500)
mm[201:300,201:300]=m
# TEST
for (m in 1:length(sv[,1])){
  # TOPOGRAPHIC SHADING (cast shadow)
  #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
  sh = doshade(mm, sv[m,], dl=1)
  ddd = make_raster(sh, dem)
  plot(make_raster(mm,dem), main=m)
  plot(ddd, col = c('white','black'), add = T)
}






















