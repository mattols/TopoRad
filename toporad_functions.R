# 
# TOPOGRAPHIC SOLAR RADIATION MODEL FUNCTIONS
# 01/28/2019
# 
# Matthew Olson - University of Utah, Department of Geography 
# email: matthew.olson@geog.utah.edu
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

require(insol)
require(raster)
require(rgdal)

###############################################
dpath = "F:/HiMAT/MATTO/DATA/REMOTE_IMAGERY/DEM/WV_DEMS/WORLDVIEW/8m_wgs84/hma_8m_677.tif"
gpath = "F:/HiMAT/MATTO/DATA/SHAPEFILES/RGI_WV_PROJ/wv_glaciers.shp"

###############################################
load.wv.dat <- function(dpath, gpath){
  demL      <<- raster(dpath)
  glaciers  <<- readOGR(gpath) 
  glaciers  <<- glaciers[11:20,] # only select glaciers in everest region (10)
}

###############################################
sw.glacier.res <- function(date = ISOdate(2017, 6, 21, 0), savepath = "F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/", perc = FALSE){
  # iterates through all glaciers indices given and calls sw.res
  # returns anomaly dataframe for all glaciers
  strt = Sys.time()
  # iterate through all glaciers
  g_num     <- length(glaciers@polygons)
  for (g in 1:g_num){
    # run for single glacier
    dfg <- sw.res(demL, shape = glaciers[g,], gn = g, date = date, savepath = savepath, perc=perc) # anomaly for g
    
    # compile dataframe
    if (g == 1){dfga = dfg} else{dfga = rbind(dfga,dfg)}
  }
  # write files
  if (!is.null(savepath)){
    write.csv(dfga, file = paste0(savepath,"dfga.csv"), row.names=FALSE)
  }
  print("__Finished ALL__")
  end = Sys.time() - strt 
  print(end)
  return(dfga)
}

###############################################
sw.res <- function(demL, shape, gn = NA, date = ISOdate(2017, 6, 21, 0), resampleFactor = c(1,3,4,12,20,33,65,130), savepath = "F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/", perc = FALSE, isave = "F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/images_tmp/"){
  # creates a dataframe of daily values at each resolution for a given glacier
  # creates anomaly dataframe
  strt = Sys.time()
  
  # loop through resolutions for single glacier
  for (rf in resampleFactor){
    print(paste("> Starting resolution:", rf, "   ", match(rf,resampleFactor), "of", length(resampleFactor), "   for glacier", gn))
    location.variables(demL, shape, resampleFactor = rf)
    tfstk <- sw.daily(date, perc=perc)
    # save output of each resolution to one dataframe
    dft <- sw.res.dataframe(tfstk)
    
    # # plot values along elevation for given resolution
    # if (!is.null(isave)){
    #   png(paste0(isave,"g",gn,"_r",rf,".png"),width = 970, height = 790)
    #   #png(paste0(savepath,"images_tmp/test1",rf,".png"),width = 970, height = 790)
    #   p.sw.elv(dft, y_lim = c(-250,150))
    #   legend("topright", legend=paste("res:", round(dem_res,-1)),bty='n', cex=1.5)
    #   legend("topleft", legend=paste("glacier:", gn), bty='n', cex=1.5)
    #   dev.off()
    # }
    
    if (length(resampleFactor) > 1){
      if (rf == 1){
        # concat resolution df
        dftf = dft
        tfstk8 = tfstk
      }else{
        dftf = rbind(dftf,dft)
        # create anomaly df
        dftmp = sw.anomaly(tfstk8, tfstk, nr = rf, g = gn)
        if (rf == resampleFactor[2]){
          dfa = dftmp
        } else{
          dfa = rbind(dfa,dftmp)
        }
      }
    }
  }
  # write files
  if (!is.null(savepath)){
    write.csv(dftf, file = paste0(savepath,"dftf_",gn,".csv"), row.names=FALSE)
    write.csv(dfa, file = paste0(savepath,"dfa_",gn,".csv"), row.names=FALSE)
  }
  print(paste0("__Finished Anomalies For Glacier ", gn,"__"))
  end = Sys.time() - strt 
  print(end)
  if (length(resampleFactor) > 1){return(dfa)} else{return(dft)}
}

###############################################
sw.anomaly <- function(tfstk8, tfstk, nr, g){
  # create dataframe of anomalies
  # g_select = c(2,8,7,6,3) #Which glaciers?
  # g_select = seq(1,10) #Which glaciers?
  # r_select = c(4,12,22,30) # Which Resolutions?
  
  # force 8m resolution to new sample size
  tf_forced   <- aggregate(tfstk8,fact=nr,fun=mean)
  ntfstk      <- resample(tf_forced, tfstk, method='bilinear')
  
  # anomalies and dataframe
  astk        <- tfstk - ntfstk
  astk        <- stack(astk, tfstk[[9]])
  names(astk) <- c("Incidence angle","Self shading","Cast shadows","Topographic shading","Diffuse sky","Terrain reflected",
                 "Combined", "Sky view factor", "Elevation", "Elevation1")
  adf         <- as.data.frame(astk)
  
  # add fields
  adf$Distance.Elevation  <- adf$Elevation1 - glacier@data$Zmed
  adf$Resolution          <- rep(round(dem_res,2), length(adf$Incidence.angle))
  adf$Glacier             <- rep(glacier@data$RGIId, length(adf$Incidence.angle))
  adf$Gn                  <- rep(g, length(adf$Incidence.angle))
  adf$Rf                  <- rep(nr, length(adf$Incidence.angle))
  adf                     <- adf[complete.cases(adf$Combined),]
  return(adf)
}

###############################################
sw.res.dataframe <- function(tfstk){
  # new df
  dft = as.data.frame(tfstk)
  # add fields
  dft$Distance.Elevation  <- dft$Elevation - glacier@data$Zmed
  dft$Resolution          <- rep(round(dem_res,2), length(dft$Incidence.angle))
  dft$Glacier             <- rep(glacier@data$RGIId, length(dft$Incidence.angle))
  dtf                     <- dft[complete.cases(dft$Combined),]
  dft                     <- dft[!is.na(dft$Combined),]
  return(dft)
}

###############################################
sw.daily <- function(date = ISOdate(2017, 6, 21, 0), perc = FALSE, sw_totals = FALSE, plot_moment = FALSE){
  # daily sw variables -> call sw.moment()
  year  <- format(date,'%Y')
  month <- format(date,'%m')
  day   <- format(date,'%d')
  print(paste("Calculating shortwave for", ISOdate(year, month, day, 0)))
  ptm <- proc.time()
  
  # julian moment at every 15 mins
  jd=JD(seq(ISOdate(year,month,day,0),ISOdate(year,month,day,23),by="15 mins"))
  
  # sun position and vector
  sv = sunvector(jd,lat_lon[1],lat_lon[2],tmz); sp1=sunpos(sv)
  
  # daylight hours (zenith <= 90)
  sp=sp1[which(sp1[,2]<=90),]
  sv=sv[which(sp1[,2]<=90),]
  
  # zenith and azimuth angles
  zenith=sp[,2]
  az_noon = which.min(abs(180-sunpos(sv))[,1])
  azimuth_eq = c(sunpos(sv)[,1][1:az_noon]-180,sunpos(sv)[,1][(az_noon+1):length(sunpos(sv)[,1])]-180)
  
  ## LOOP
  atmos.vars()
  create.tfmodels()
  print(paste("...cycling through", length(zenith), "moments"))
  for (m in 1:length(zenith)){
    # calculate fluxes at time m
    if (plot_moment){
      sw.moment(sv[m,], zenith[m], azimuth_eq[m], jd[m], plot_moment=m, zlen = length(zenith))
    } else{
      sw.moment(sv[m,], zenith[m], azimuth_eq[m], jd[m])
    }
  }
  # create final raster stack
  if (sw_totals){
    tfstk <- sw.totals.stk(keepAll = FALSE, mask=FALSE, savevar = NULL)
  } else{
    tfstk = sw.change.stk(zenith, average=TRUE, percentage=perc, savevar = NULL, mask_inner = FALSE)
  }
  print(proc.time() - ptm)
  return(tfstk)
}

###############################################
sw.moment <- function(sv, zenith, azimuth_eq, jd, alphaT = 0.45, plot_moment=NULL, zlen= NULL){
  # ! call topo.forcing.model
  # calculates all shortwave fluxes during the given moment of the day
  
  # zenith and incident angles
  cos_inc <- cos.slope(zenith, azimuth_eq, aspect = s_a[[2]], slope = s_a[[1]])
  cos_sfc <- cos(radians(zenith))
  
  # insolation arriving perpendicular to solar beam (direct and diffuse)
  Idirdif = insolation(zenith,jd,height,visibility,RH,tempK,0.002,0.45)
  Ib = matrix(Idirdif[,1],nrow=nrow(dem),ncol=ncol(dem))
  Id = matrix(Idirdif[,2],nrow=nrow(dem),ncol=ncol(dem))
  
  # terrain-reflected
  # Ir = Iglob * (1 - VF_mat) * alphaT) 
  # Ir = (Ib*sh + Id * VF_mat) * 0.45 * (1 - VF_mat) * cos_sfc[m]
  Iglob = (Ib + Id * VF_mat)*cos_sfc
  Ir = Iglob * (as.matrix((1 + cos(s_a[[1]]))/2) - VF_mat) * alphaT # (Hetrick 1993)
  Ir[Ir < 0 ] = 0
  # Ir = Iglob * (1 - VF_mat) * alphaT # Dozier (Hetrick 1993)
  
  # topographic shading
  sh = doshade(d_mat, sv, dl=dem_res)
  
  # plot moment (optional)
  if (!is.null(plot_moment)){
    p.sw.moment(Ib, Id, Ir, sh, cos_inc, cos_sfc, moment = plot_moment, zz = zlen)
  } else{
    # Run toposol models (values continuously saved in memory)
    topo.forcing.models(Ib, Id, Ir, sh, cos_inc, cos_sfc)
  }
}

###############################################
topo.forcing.models <- function(Ib, Id, Ir, sh, cos_inc, cos_sfc){
  # add values for moment 
  
  ## MODELS
  model_flat      <<- model_flat        +   (Ib)*cos_sfc
  model_flat_base <<- model_flat_base   +   (Ib + Id)*cos_sfc
  model_flat_sh   <<- model_flat_sh     +   (Ib * sh)*cos_sfc
  model_inc_sr    <<- model_inc_sr      +   (Ib) * cos_inc
  model_inc_sh    <<- model_inc_sh      +   (Ib * sh)*cos_inc
  model_vf_base   <<- model_vf_base     +   (Ib * sh + Id)*cos_inc
  model_vf        <<- model_vf          +   (Ib * sh + Id * VF_mat )*cos_inc
  model_ref       <<- model_ref         +   (Ib * sh + Id * VF_mat + Ir)*cos_inc 
  
  print(paste("Ib:",round(mean(Ib,na.rm=T),2),' - Id:',round(mean(Id,na.rm=T),2),
              ' - Ir:',round(mean(Ir,na.rm=T),2)))
}


###############################################
sw.totals.stk <- function(keepAll = FALSE, mask=FALSE, savevar = NULL){
  # returns total daily solar radiation (mulitiply by dt)
  # savevar should be path to folder
  # if keepALL==TRUE return:
  # model_flat:       direct sw on flat surface
  # model_flat_base:  direct and diffuse on flat surface
  # model_inc_sr:     direct sw on slope
  # model_inc_sr:     direct sw with shading on slope
  # model_vf:         direct and diffuse sw on a slope with surrounding terrain (only excludes terrain-reflected)
  # model_ref:        complete solar radiation model
  # final layer is always dem in either case
  
  # Intergrate for all hours in day
  # Wm^-2s^-1 -> Wm^-2d^-1 (s -> 15 mins) (60*15 -> 15/(60*24))
  dt = (15*15)/24
  
  if (keepALL){
    m1 <-     make.raster((model_flat * dt), dem)
    m2 <-     make.raster((model_flat_base * dt), dem)
    m3 <-     make.raster((model_inc_sr * dt), dem)
    m4 <-     make.raster((model_inc_sr * dt), dem)
    m5 <-     make.raster((model_vf * dt), dem)
    m6 <-     make.raster((model_ref * dt), dem)
    stk = stack(m1, m2, m3, m4, m5, m6, make.raster(VF_mat,dem), dem)
    names(stk) = c("Sw.dir.flat","Sw.dirdif.flat","Sw.dir.slope","Sw.dirdif.shade.slope",
                   "Sw.noRef","Sw.insolation","Sky.view.factor","Elevation")
  } else{
    m1 <-     make.raster((model_ref * dt), dem)
    stk = stack(m1,dem)
    names(stk) = c("Sw.insolation","Elevation")
  }
  
  if (mask){
    ex = extent(glacier)
    ex@ymin = ex@ymin-0.001
    ex@ymax = ex@ymax+0.001
    stk = crop(stk, extent(ex))
    stk = mask(stk, glacier)
  }
  
  if (!is.null(savevar)){
    # nam <- paste0(glacier@data$RGIId,"_res_", round(dem_res))
    # assign(nam, new_stk)
    # name is the time when model finishes
    nam = Sys.time()
    writeRaster(stk, filename = paste(savevar,'/',nam,".grd",sep=""), format="raster")
    print(paste("Stack variable", nam, "saved to:", savevar))
  }
  
  return(stk)
}

###############################################
sw.change.stk <- function(zenith, average=TRUE, percentage=FALSE, savevar = NULL, mask_inner = FALSE){
  # final values | returns raster stack
  # savevar should be path to folder
  if(percentage){
    slope_asp   = ((model_inc_sh - model_flat_sh)/model_flat_sh)*10000
    tot_sh      = ((model_flat_sh - model_flat)/model_flat)*10000
    cast_sh     = ((model_inc_sh - model_inc_sr)/model_inc_sr)*10000
    sh_rel      = ((tot_sh - cast_sh)/cast_sh)*10000
    diff_t      = ((model_vf - model_vf_base)/model_vf_base)*10000
    refl        = ((model_ref - model_vf)/model_vf)*10000
    comb        = ((model_ref - model_flat_base)/model_flat_base)*10000
    
  } else{
    # final models - change in irradiance (Wm-2)
    slope_asp   = model_inc_sh - model_flat_sh
    tot_sh      = model_flat_sh - model_flat
    cast_sh     = model_inc_sh - model_inc_sr
    sh_rel      = (tot_sh - cast_sh)
    diff_t      = model_vf - model_vf_base
    refl        = model_ref - model_vf
    comb        = model_ref - model_flat_base
  }
  if (average){
    print("...calculating mean irradiance")
    # DAILY MEAN
    I_sa =    make.raster((slope_asp / length(zenith)), dem)
    I_sr =    make.raster((sh_rel / length(zenith)), dem)
    I_cs =   make.raster((cast_sh / length(zenith)), dem)
    I_sh =   make.raster((tot_sh / length(zenith)), dem)
    I_dif =   make.raster((diff_t / length(zenith)), dem)
    I_ref =  make.raster((refl / length(zenith)), dem)
    I_com =  make.raster((comb / length(zenith)), dem)
  } else {
    print("ERROR::use sw.totals.stk() function instead!")
    stop()
    # print("Final values will be daily sums")
    I_sa =    make.raster((slope_asp), dem)
    I_sr =    make.raster((sh_rel), dem)
    I_cs =   make.raster((cast_sh), dem)
    I_sh =   make.raster((tot_sh), dem)
    I_dif =   make.raster((diff_t), dem)
    I_ref =  make.raster((refl), dem)
    I_com =  make.raster((comb), dem)
  }
  stk = stack(I_sa, I_sr, I_cs, I_sh, I_dif, I_ref, I_com, make.raster(VF_mat,dem), dem)
  names(stk) = c("Incidence angle","Self shading","Cast shadows","Topographic shading","Diffuse sky","Terrain reflected",
                 "Combined", "Sky view factor", "Elevation")
  ex = extent(glacier)
  ex@ymin = ex@ymin-0.001
  ex@ymax = ex@ymax+0.001
  stk = crop(stk, extent(ex))
  
  if (mask_inner){
    # mask to inside of shape
    cent_distance <- res(dem)[1]/2 # for added accuracy: (res(dem)[1]/2 + 0.0009 #(0.0007 is good))
    inner_shape <- suppressWarnings(buffer(glacier,width=-cent_distance))
    stk = mask(stk, inner_shape)
  } else{
    stk = mask(stk, glacier)
  }
  
  # save raster stack? (file location)
  if (!is.null(savevar)){
    nam <- paste0(glacier@data$RGIId,"_res_", round(dem_res))
    assign(nam, stk)
    writeRaster(stk, filename = paste(savevar,'/',nam,".grd",sep=""), format="raster")
    print(paste("Stack variable", nam, "saved to:", savevar))
  }
  
  return(stk)
}

###############################################
location.variables <- function(demL, shape, resampleFactor = 1){
  # varibles that remain constant over time
  # crop raster
  dem <<- crop.raster(demL, shape)
  dem <<- void.fill(dem)
  glacier <<- shape
  # resample if necessary
  if (resampleFactor == 1){
    print("Keep native resolution")
  } else{
    dem <<- new.resolution(dem, resampleFactor)
  }
  # make globally available
  d_mat <<- as.matrix(dem)
  lat_lon <<- c(round((dem@extent@ymax + dem@extent@ymin)/2,5), round((dem@extent@xmax + dem@extent@xmin)/2,5))
  dem_res <<- dem.res(dem, lat_lon[1])
  VF_mat <<- view.factor(d_mat, dem, dem_res)
  tmz <<- round(lat_lon[2]/15,1)
  s_a <<- slope.aspect(dem)
  print("Location variables loaded to memory")
}

###############################################
cos.slope <- function(zenith, azimuth_eq, aspect, slope){
  # returns a matrix of the cosine of the incident angle at a given moment
  exposures  = aspect - radians(180)
  cos_inc = acos((cos(radians(zenith)) * cos(slope)) +
                   (sin(radians(zenith)) * sin(slope) * cos(radians(azimuth_eq) - exposures)))
  
  cos_inc = as.matrix(cos_inc)
  # get rid of self shading values
  cos_inc[cos_inc > radians(90)] = radians(90)
  cos_inc = cos(cos_inc)
  return(cos_inc)
}

###############################################
create.tfmodels <- function(){
  # generate empty model matrices
  model_flat      <<- array(0,dim=dim(d_mat))
  model_flat_base <<- array(0,dim=dim(d_mat))
  model_flat_sh   <<- array(0,dim=dim(d_mat))
  model_inc_sr    <<- array(0,dim=dim(d_mat))
  model_inc_sh    <<- array(0,dim=dim(d_mat))
  model_vf_base   <<- array(0,dim=dim(d_mat))
  model_vf        <<- array(0,dim=dim(d_mat))
  model_ref       <<- array(0,dim=dim(d_mat)) 
}

###############################################
view.factor <- function(dem_mat, dem, dem_res, elv_interval = 5, az_interval = 15){
  print("____Generating sky view factor_____")
  ptm <- proc.time()
  ELV = rev(seq(0, 90, elv_interval))
  AZI = seq(0, 345, az_interval)
  AZ = matrix(0,nrow=dim(dem_mat)[1]*dim(dem_mat)[2],ncol=length(AZI))
  for (vv in 1:length(AZI)){
    Z1 = matrix(0,nrow=dim(dem_mat)[1]*dim(dem_mat)[2],ncol=length(ELV))
    for (mm in 1:length(ELV)){
      sv = normalvector(ELV[mm],AZI[vv])
      sh <- doshade(dem_mat, sv, dl=dem_res)
      Z1[,mm] = as.array(sh)
    }
    AZ[,vv] = rowSums(Z1)/length(ELV)
  }
  VF_mat = matrix(rowMeans(AZ), nrow=nrow(dem), ncol=ncol(dem))
  #VF_dem <- make.raster(VF_mat, dem)
  print(proc.time() - ptm)
  return(VF_mat)
}

###############################################
atmos.vars <- function(){
  # atmospheric variables (constant simulating clear-sky)
  height <<- array(d_mat)
  visibility <<- 28 
  RH <<- 60
  tempK <<- 278.15
}

###############################################
make.raster <- function(matrix, dem){
  raster(matrix,
         xmn=dem@extent@xmin, xmx=dem@extent@xmax,
         ymn=dem@extent@ymin, ymx=dem@extent@ymax, 
         crs=crs(dem))
}

###############################################
crop.raster <- function(stk, shp, buffer = 0.03){
  r <- crop(stk,extent(xmin(shp)-buffer,xmax(shp)+buffer,
                       ymin(shp)-buffer,ymax(shp)+buffer))
  return(r)
}

###############################################
gauss.window <- function(sigma=2, n=5) { #(spatialEco)
  m <- matrix(ncol=n, nrow=n)
  mcol <- rep(1:n, n)
  mrow <- rep(1:n, each=n)
  x <- mcol - ceiling(n/2)
  y <- mrow - ceiling(n/2)
  m[cbind(mrow, mcol)] <- 1/(2*pi*sigma^2) * exp(-(x^2+y^2)/(2*sigma^2))
  m / sum(m)
}

###############################################
new.resolution <- function(dem, resample_factor, sigma = 2, wn = 5, funct = mean, methd = 'bilinear'){
  print(paste('Resolution resampled by', resample_factor))
  # low pass gaussian filter to prevent aliasing
  gm <- gauss.window(sigma=sigma, n=wn)
  smooth.dem <- focal(dem, w = gm, fun = funct, na.rm=TRUE, pad=FALSE)
  # resampling method
  agg <- aggregate((smooth.dem*(wn**2)),fact=resample_factor,fun=funct)
  ndem = resample(dem, agg, method=methd)
  return(ndem)
}

###############################################
dem.res <- function(dem, latitude){
  resolution <- res(dem)[1]/(1/(111320*cos(radians(latitude))))
  dem_res <- round(resolution, 2)
  return(dem_res)
}

###############################################
slope.aspect <- function(dem, units = 'radians', neighbor = 8){
  s <- terrain(dem, opt='slope',unit=units,neighbors=neighbor)
  a <- terrain(dem, opt='aspect',unit=units,neighbors=neighbor)
  stk <- stack(s,a)
  return(stk)
}

###############################################
# Function to replace the focal value with the mean of a 3x3 window if NA. 
# If the window size increases the index value [i] needs to change as well 
# (eg., for a 5x5 window the index would be 13, 3x3 the index would be 5). Multiply then divide by two (round up)?
fill.na <- function(x, i=5) {
  if( is.na(x)[i] ) {
    return( round(mean(x, na.rm=TRUE),0) )
  } else {
    return( round(x[i],0) )
  }
}
# Void Fill using Nearest Neighbor (w = window size)
# if window size changes, index in fill.na function should change
void.fill <- function(vdem, win = 3){
  while(any(is.na(values(vdem)))){
    vdem <- focal(vdem, w = matrix(1,win,win), fun = fill.na, 
                  pad = TRUE, na.rm = FALSE, NAonly=FALSE)
  }
  return(vdem)
}
# improved version (no resampling outside of void-filled areas)
# error when used to creat sky view factor?
void.fill2 <- function(vdem, win = 3, stillNA = FALSE){
  if(any(is.na(values(vdem)))){
    for (i in 1:3){ # only 3x
      vdem <- focal(vdem, w = matrix(1,win,win), fun = fill.na, 
                    pad = TRUE, na.rm = FALSE, NAonly=TRUE)
    }
    odem = vdem
    while(any(is.na(values(vdem)))){
      vdem <- focal(vdem, w = matrix(1,win,win), fun = fill.na, 
                    pad = TRUE, na.rm = FALSE, NAonly=FALSE)
      stillNA = TRUE
    }
    if(stillNA){odem[is.na(values(odem))] = vdem[is.na(values(odem))]}
  } else{odem = vdem}
  return(odem)
}
###############################################
##############################################################################################
# END