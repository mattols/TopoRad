#
#
# WV COMPARISON WITH ASTER, SRTM, and ALOS
#
#################################################

############
# OTHER DEMS
aster_dem = raster("F:/HiMAT/MATTO/DATA/REMOTE_IMAGERY/DEM/WV_DEMS/ASTER/aster_dem_wgs.tif")
aster_dem = crop.raster(aster_dem, glaciers, buffer = 0.3)
srtm_dem = raster("F:/HiMAT/MATTO/DATA/REMOTE_IMAGERY/DEM/WV_DEMS/SRTM/srtm_dem_wgs.tif")
srtm_dem = crop.raster(srtm_dem, glaciers, buffer = 0.3)
alos_dem = raster("F:/HiMAT/MATTO/DATA/REMOTE_IMAGERY/DEM/WV_DEMS/ALOS/alos_dem_wgs.tif")
alos_dem = crop.raster(alos_dem, glaciers, buffer = 0.3)
wv_dem = crop.raster(demL, glaciers, buffer=0.3)
# vdem = void.fill(wv_dem)
# wv30_dem = new.resolution(vdem, resample_factor = 4)
# make list of all dems used
dem_ls = list(wv_dem, aster_dem, srtm_dem, alos_dem, wv_dem)

## march
# df_aster <- sw.res(aster_dem, shape = glaciers[2,],)
# df_srtm <- sw.res(srtm_dem, shape = glaciers[2,])
# df_alos <- sw.res(alos_dem, shape = glaciers[2,])

#################################################
sw.dem.comp <- function(dem_ls, shape, gn = NA, date = ISOdate(2017, 3, 21, 0) , savepath = "F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/", isave = "F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/images_tmp/"){
  # creates a dataframe of daily values at each resolution for a given glacier
  # creates anomaly dataframe
  dem_names = c("WV", "ASTER","SRTM", "ALOS", "WV30")
  strt = Sys.time()
  
  # loop through resolutions for single glacier
  for (dd in 1:length(dem_ls)){
    print(paste("> Starting dem:", dem_names[dd], "   for glacier", gn))
    if (dd == 5){
      # create WV 30 m (last)
      location.variables(dem_ls[[dd]], shape, resampleFactor = 4)
    } else{
      location.variables(dem_ls[[dd]], shape, resampleFactor = 1)
    }
    
    tfstk <- sw.daily(date)
    # save output of each resolution to one dataframe
    dft <- sw.res.dataframe(tfstk)
    dft$dem <- dem_names[dd]
    
    # plot values along elevation for given resolution
    if (!is.null(isave)){
      png(paste0(isave,"g",gn,"_",dem_names[dd],".png"),width = 970, height = 790)
      p.sw.elv(dft, y_lim = c(-350,150))
      legend("topright", legend = dem_names[dd], bty='n', cex=1.5)
      legend("topleft", legend=paste("glacier:", gn), bty='n', cex=1.5)
      dev.off()
    }
    
    if (dd == 1){
      # concat resolution df
      dftf = dft
      tfstk8 = tfstk
    }else{
      dftf = rbind(dftf,dft)
      # create anomaly df
      dftmp = sw.anomaly(tfstk8, tfstk, nr = 4, g = gn)
      dftmp$dem <- dem_names[dd]
      if (dd == 2){
        dfa = dftmp
      } else{
        dfa = rbind(dfa,dftmp)
      }
    }
  }
  # write files
  if (!is.null(savepath)){
    write.csv(dftf, file = paste0(savepath,"dftf_",gn,".csv"), row.names=FALSE)
    write.csv(dfa, file = paste0(savepath,"dfa_",gn,".csv"), row.names=FALSE)
  }
  print(paste0("__Finished DEM Comparison Anomalies For Glacier ", gn,"__"))
  end = Sys.time() - strt 
  print(end)
  return(dfa)
}

###############################################
sw.glacier.dems <- function(date = ISOdate(2017, 6, 21, 0), savepath = "F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dem_comp/"){
  # iterates through all glaciers indices given and calls sw.res
  # returns anomaly dataframe for all glaciers
  strt = Sys.time()
  # iterate through all glaciers
  g_num     <- length(glaciers@polygons)
  for (g in 1:g_num){
    # run for single glacier
    dfg <- sw.dem.comp(dem_ls = dem_ls, shape = glaciers[g,], gn = g, date = date) # anomaly for g
    
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

# run code for all DEMs and glaciers
dfga <- sw.glacier.dems(date = ISOdate(2017, 3, 21, 0))

