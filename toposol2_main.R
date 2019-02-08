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
dfga <- sw.glacier.res(date = ISOdate(2017, 12, 21, 0))

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


# !!!!!!!!!!!!!!!!!!!!!!
# TEST PLOTS (CHECK)
p.sw.anom(dfa,y_lim = c(-50,100))
p.sw.elv(dfa,res=8)
p.sw.anom(dfa,y_lim = c(-50,100), plot4 = FALSE, topovar = 9)
p.sw.box(dfa, drop_res = c(1,4))
p.sw.box(dftf)


####################
# Plot moments
tfstk = sw.daily(date = ISOdate(2017, 6, 21, 0), plot_moment = TRUE) 
#try2
location.variables(demL, shape = glaciers[2,], resampleFactor = 12)
# tfstk = sw.daily(date = ISOdate(2017, 6, 21, 0), plot_moment = TRUE) # #VERY SLOW! 
#
spt = SpatialPoints(coordinates(data.frame(86.948,28.04)),proj4string=crs(dem))
p.sw.moment2(mods= c(1,4), spt = spt) # ALSO SLOW
# mods SA = c(1,4) | TS = c(1,5) | DIF = c(6,7) | REF = c())









