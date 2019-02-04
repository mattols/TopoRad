# 
# TOPOGRAPHIC SOLAR RADIATION MODEL MAIN
# 01/28/2019
# 
# Matthew Olson - University of Utah, Department of Geography 
# email: matthew.olson@geog.utah.edu
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

###############################################
## load functions and data
source('F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/SRC2/toposol2_functions.R')
source('F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/SRC2/toposol2_plots.R')

###############################################
## load data
load.wv.dat(dpath, gpath)

###############################################
## examples (working)
# location.variables(demL, shape = glaciers[2,], resampleFactor = 3)  #(resample north facing glacier to 20m (factor of 3))
# tfstk = sw.daily(date = ISOdate(2017, 6, 21, 0))                    #(calculate daily sw tf models for loaded variables)
# df <- sw.res(demL, shape = glaciers[2,])                            #(create resolution anomaly df for north facing glacier)
# df <- sw.res(demL, shape = glaciers[5,], gn= 5, resampleFactor = c(1,12), savepath = NULL)

svp = "F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/"
dfga <- sw.glacier.res()

# plotting?
# 

# ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! ! !
# TO DO:
# creat df for all glaciers
# create dfm dataframe to save/plot a certain value at varying resolutions over time
# TO DO PLOTS: 
# varying resolution throughout day for given variable
# plot map view
# 
# g = 2 # incorporate glacier function loop
# resampleFactor = c(1,4,12,20,33,65,130)
dftf <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dftf_2.csv")
dfa <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dfa_2.csv")

# !!!!!!!!!!!!!!!!!!!!!!
# TEST PLOTS (CHECK)
p.sw.anom(dfa,y_lim = c(-50,100))
p.sw.elv(dfa,res=8)
p.sw.anom(dfa,y_lim = c(-50,100), plot4 = FALSE, topovar = 9)












# Plot maps
location.variables(demL, glaciers[5,], resampleFactor = 4)
date = ISOdate(2017, 6, 21, 0)
tfstk <- sw.daily(date)
stkvar = tfstk[[1]]
plot(tfstk[[1]], col=colsgrad[1:8], breaks = c(-350,-200,-150,-100,-60,-10,0,10,60))
plot(glacier,add=T)
legend("bottomleft", legend = names(stkvar),bty='n',cex=1.5)
plot(tfstk[[4]], col=rev(blues9), breaks = c(-350,-250,-150,-100,-50,-25,-10,0))
plot(glacier,add=T) 
plot(tfstk[[4]], col=rev(blues9), breaks = c(-350,-250,-150,-100,-50,-25,-10,0))
plot(glacier,add=T) 
tfstk@data



















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



















