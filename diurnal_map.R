#
# Diurnal Variation
#
#
# # # # # # # # # # #

# glacier and resolution
location.variables(demL, shape = glaciers[2,], resampleFactor = 3) 

# generate solar info for day (default month=3)
choose.heat.day()

# plot function
p.heat <- function(diff_vals = FALSE){
  
  # max and min elevation
  dmin = ceiling(dem@data@min)
  dmax = floor(dem@data@max)
  
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
    alphaT = 0.45 #0.45
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
    
    
    # # # # # # #
    # PLOTS
    if (diff_vals){
      comb = make.raster((model_ref - model_flat_base), dem)
    } else{
      comb = make.raster(model_ref, dem)
    }
    
    # CREATE DATA MATRIX
    comb = mask(crop(comb, glacier),glacier)
    c2 = getValues(comb)
    d2 = mask(crop(dem, glacier),glacier)
    d2 = getValues(d2)
    df1 = data.frame(d2,c2)
    df1 = df1[!is.na(df1[,2]),]
    
    # make plots
    if(m==1){
      dfh = df1
    } else{
      dfh = cbind(dfh, df1[,2])
    }
  } 
  print(paste("___FINISHED___zenith:",m))
  return(dfh)
}

#########################
# GENERATE HEAT MAP PLOT
dfh = p.heat()
names(dfh) = c('Elevation',paste0('m_',seq(1:m)))

# order elevation
df <- dfh[order(dfh$Elevation),]
# round to nearest 10th
df$Elevation = round(df$Elevation,-1)
# aggregate cells by mean across 10 m elevation bins
df2 = aggregate(.~Elevation, df, mean)
df3 = df2[,2:ncol(df2)]
df3 = t(df3)
names(df3) = df2$Elevation
row.names(df3) <- 1:m

dfmat = data.matrix(df3)
hm = heatmap(dfmat, Rowv=NA, Colv=NA, col = rev(heat.colors(256)), scale="column", margins=c(5,10))

# only ggplot
mine.heatmap <- ggplot(data = df2,
                       mapping = aes(x = Elevation,y = .,fill = Abundance)) +
  geom_tile() +
  xlab(label = "Elevation")

library(tidyr)
https://jcoliver.github.io/learn-r/006-heatmaps.html





#########################
library(gplots)

heatmap.2(df3, Rowv=NA, Colv=NA, col = rev(heat.colors(256)), scale="none", margins=c(5,10),
          key.xlab=expression(Mean~change~'in'~irradiance~(Wm^-2)), xlab = "Elevation (m)",
          key=TRUE, symkey=FALSE, density.info="none", ylab = "Solar moment",
          key.title="", trace="none")




dfh = p.heat(diff_vals = TRUE)

heatmap.2(df3, Rowv=NA, Colv=NA, col = rev(heat.colors(256)), scale="none", margins=c(5,10),
          key.xlab=expression(Mean~change~'in'~irradiance~(Wm^-2)), xlab = "Elevation (m)",
          key=TRUE, symkey=FALSE, density.info="none", ylab = "Solar moment",
          key.title="", trace="none")



###


##### OTHER FUNCTIONS
choose.heat.day <- function(month = 3){
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
  
  # RUN
  # MAIN FIX
  s = s_a[[1]]
  a = s_a[[2]]
  Vf_dem = make.raster(VF_mat,dem)
  jd=JD(seq(ISOdate(2017,month,21,0),ISOdate(2017,month,21,23),by="15 mins"))
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
}
