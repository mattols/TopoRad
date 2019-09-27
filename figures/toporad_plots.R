# 
# TOPOGRAPHIC SOLAR RADIATION MODEL PLOTTING METHODS
# 01/28/2019
# 
# Matthew Olson - University of Utah, Department of Geography 
# email: matthew.olson@geog.utah.edu
#
# # # # # # # # # # # # # # # # # # # # # # # # # # # # #

require(RColorBrewer)
require(reshape2)
require(ggplot2)

colsgrad = rev(brewer.pal(11, "RdBu"))

###############################################
load.df <- function(df_location, all = FALSE){
  # load all saved dataframes in folder or only specific
  # dftf <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dftf.csv", row.names = FALSE)
  # dfa <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dfa.csv", row.names = FALSE)
  if (all){
    dftf  <<- read.csv(paste0(df_location, "dftf.csv"), row.names = FALSE) # values at each resolution for given glacier
    dfa   <<- read.csv(paste0(df_location, "dfa.csv"), row.names = FALSE)    # anomalies for given glacier
    dfga  <<- read.csv(paste0(df_location, "dfga.csv"), row.names = FALSE)  # df of anomalies for all glaciers
  } else{
    df <- read.csv(df_location, row.names = FALSE)
    return(df)
  }
}

###############################################
p.sw.anom <- function(df, y_lim = c(-100,150), plot4 = TRUE, mods_plot4 = c(1,4,5,6,7), drop_res = c(1,4), topovar=7, placeLegend = 'topleft', multi_glacier=FALSE){
  # plot anomaly values from dataframe
  
  # set plot params
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
  col_names = unlist(lapply(names(df), function(x) {gsub("\\.", " ", x)}))
  
  # create resolution list
  if (multi_glacier == FALSE){
    res_ls  <- as.numeric(levels(as.factor(df$Resolution)))
  } else{
    # for all
    res_ls  <- as.numeric(levels(as.factor(round(df$Resolution,-1))))
    df$Resolution = round(df$Resolution,-1)
  }
  
  if (length(res_ls) > 1 & !is.null(drop_res)){
    res_ls <- res_ls[-drop_res]
  }
  l_ty = c(1,5,4,2,3)    #seq(1, length(res_ls))
  l_wd = c(4, rep(2, length(res_ls) - 1))
  
  if(plot4){
    # par(mfrow=c(2,5))
    # par(oma = c(1,3,0,0))
    # par(mar=c(3,1,2,1))
    
    #mod_select = c(1,4,6,7)
    #mod_select = c(1,4:6)
    mod_select = mods_plot4
    
    for (j in mod_select){
      # select data
      dfg = df[df$Resolution == res_ls[1],]
      
      # make plots
      plot(smooth.spline(dfg[,j]~dfg$Distance.Elevation), type = 'l', col='white',
           ylim=y_lim, ylab=expression(SW~anomaly~from~'8m'~(Wm^2)), lwd = l_wd[1],
           lty = l_ty[1],xlab='Distance from mid-elevation (m)',cex.lab=1.3, cex.axis=1.8,yaxt = 'n')
      legend("topright",legend = col_names[j],cex=1.4,bty='n')
      
      if (match(j,mod_select) == 1){
        axis(2,cex.axis=1.8)
      } else{
        axis(2,cex.axis=1.8, labels=F)
      }
      
      for(rr in res_ls){
        # iterate through all resolutions
        dfg = df[df$Resolution == rr,]
        
        lines(smooth.spline(dfg[,j]~dfg$Distance.Elevation), lwd=l_wd[match(rr,res_ls)],
              lty=l_ty[match(rr,res_ls)], col=cols[j])
      }
      abline(h=0,lty=3)
    }
  } else{
    # par(mfrow=c(1,1))
    # par(oma=c(0,0,0,0))
    # par(mar=c(5,5.1,2,1))
    
    # automatically set to Combined if plot4 is FALSE
    mod_select = topovar
    
    # make plots
    dfg = df[df$Resolution == res_ls[1],]
    plot(smooth.spline(dfg[,mod_select]~dfg$Distance.Elevation), type = 'l', col='white',
         ylim=y_lim, ylab=expression(SW~anomaly~from~'8m'~(Wm^2)), lwd = l_wd[1],
         lty = l_ty[1],xlab='Distance from mid-elevation (m)',cex.lab=1.3, cex.axis=1.8)
    legend("topright",legend = col_names[mod_select],cex=1.4,bty='n')
    
    for(rr in res_ls){
      # iterate through all resolutions
      dfg = df[df$Resolution == rr,]
      
      lines(smooth.spline(dfg[,mod_select]~dfg$Distance.Elevation), lwd=l_wd[match(rr,res_ls)],
            lty=l_ty[match(rr,res_ls)], col=cols[mod_select])
    }
    abline(h=0,lty=3)
  }
  if(!is.null(placeLegend)){
    legend(placeLegend,legend = round(res_ls,-1),col=cols[topovar],lwd=l_wd,
           lty=l_ty,cex=1.4,bty='n')
  }
  # par(mfrow=c(1,1))
  # par(mar=c(5,5.1,2,1))
}

###############################################
p.sw.elv <- function(df, res=NULL, y_lim = c(-250,100), placeLegend = 'bottomleft', inc_all_shade = FALSE){
  # output from dfts
  # plot anomaly values from dataframe (define resolution)
  
  # set plot params
  # par(mfrow=c(1,1))
  # par(mar=c(5,5.8,2,2))
  l_ty = c(5,1,1,1,3,4,1)
  l_wd = c(3,4,4,4,3,3,5)
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4')
  
  # include cast shadows and self shading also?
  if (inc_all_shade){
    mod_select = c(1:7)
    mod_names = unlist(lapply(names(df), function(x) {gsub("\\.", " ", x)}))[mod_select]
  } else{
    mod_select = c(1,4:7)
    mod_names = unlist(lapply(names(df), function(x) {gsub("\\.", " ", x)}))[mod_select]
  }
  
  # select resolution
  if (!is.null(res)){
    dfg = df[df$Resolution == res,]
    
    # find closest resolution
    if (nrow(dfg) == 0){
      res_ls  <- as.numeric(levels(as.factor(df$Resolution)))
      new_res <- res_ls[which.min(abs(res_ls-res))]
      dfg = df[df$Resolution == new_res,]
    }
  } else{
    dfg = df
  }
  
  # exclude any NA values
  dfg <- dfg[!is.na(dfg[,7]),]
  dfg <- dfg[!is.na(dfg[,1]),]
  
  # make plots
  plot(smooth.spline(dfg[,1]~dfg[,9]), type = 'l', col='white',
       ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = l_wd[1],
       lty = l_ty[1],xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8)
  for(j in mod_select){
    lines(smooth.spline(dfg[,j]~dfg[,9]), lwd=l_wd[j],
          lty=l_ty[j], col=cols[j])
  }
  abline(h=0,lty=3)
  if(!is.null(placeLegend)){
    legend(placeLegend,mod_names,col=cols[mod_select],lwd=l_wd[mod_select],
           lty=l_ty[mod_select],cex=1.4,bty='n')
  }
}

###############################################
p.sw.box <- function(df, mod_select = c(1,4,6,7), drop_res = c(2,5), n_col = 2, multi_glacier=FALSE){   #(drop_res is 1,4 for anomaly df)
  # plot params
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
  cols2 = cols[mod_select]
  
  # create resolution list
  if (multi_glacier == FALSE){
    res_ls  <- as.numeric(levels(as.factor(df$Resolution)))
  } else{
    # for all
    res_ls  <- as.numeric(levels(as.factor(round(df$Resolution,-1))))
    df$Resolution = round(df$Resolution,-1)
  }
  
  # set resolution and select data
  if (length(res_ls) > 1 & !is.null(drop_res)){
    res_ls <- res_ls[-drop_res]
  }
  # select resolutions
  dfs = df[df$Resolution %in% res_ls,]
  res_names = round(res_ls, -1)
  
  if (drop_res[1]==2){
    res_names[1] = 8
  }
  
  # boxplots
  dmelt <- melt(dfs,id.vars='Resolution', measure.vars=mod_select)
  names(cols2) <- levels(dmelt$variable)
  ggplot(dmelt, aes(x=factor(Resolution), y=value,fill=variable)) +
    geom_hline(yintercept = 0, linetype='dashed') +
    geom_boxplot() + scale_fill_manual(name = "", values = cols2) +
    facet_wrap(.~variable, ncol=n_col, scales = "fixed") + 
    scale_x_discrete(name = "Resolution (m)", labels=res_names) +
    ylab(expression(Mean~irradiance~change~(Wm^-2))) +
    theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1)) + 
    theme_classic() + theme(text = element_text(size = 20))
  
}

###############################################
p.sw.anom.ggplot <- function(df, mod_select = c(1,4,6,7), drop_res = c(1,4), n_col = 2){   #(drop_res is 1,4 for anomaly df)
  # plot params
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
  cols2 = cols[mod_select]
  
  # set resolution and select data
  res_ls  <- as.numeric(levels(as.factor(df$Resolution)))
  if (length(res_ls) > 1 & !is.null(drop_res)){
    res_ls <- res_ls[-drop_res]
  }
  # select resolutions
  dfs = df[df$Resolution %in% res_ls,]
  res_names = round(res_ls, -1)
  
  if (drop_res[1]==2){
    res_names[1] = 8
  }
  
  res = 30
  
  # select resolution
  if (!is.null(res)){
    dfs = df[df$Resolution == res,]
    
    # find closest resolution
    if (nrow(dfs) == 0){
      res_ls  <- as.numeric(levels(as.factor(df$Resolution)))
      new_res <- res_ls[which.min(abs(res_ls-res))]
      dfs = df[df$Resolution == new_res,]
    }
  } else{
    dfs = df
  }
  
  # boxplots
  dmelt <- melt(dfs,id.vars='Distance.Elevation', measure.vars=mod_select)
  names(cols2) <- levels(dmelt$variable)
  ggplot(dmelt, aes(x=factor(Distance.Elevation), y=value,fill=variable)) +
    geom_hline(yintercept = 0, linetype='dashed') +
    geom_line() + scale_fill_manual(name = "", values = cols2) +
    #facet_wrap(.~variable, ncol=n_col, scales = "fixed") + 
    #scale_x_discrete(name = "Resolution (m)", labels=res_names) +
    ylab(expression(Mean~irradiance~change~(Wm^-2))) +
    theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1)) + 
    theme_classic() + theme(text = element_text(size = 20))
  
}

###############################################
p.sw.moment <- function(Ib, Id, Ir, sh, cos_inc, cos_sfc, moment = NA, zz = NULL){
  # plot moment by moment update
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
  print(moment)
  ## MODELS
  m_flat      <- mean((Ib)*cos_sfc,na.rm=T)
  m_flat_base <- mean((Ib + Id)*cos_sfc,na.rm=T)
  m_flat_sh   <- mean((Ib * sh)*cos_sfc,na.rm=T)
  m_inc_sr    <- mean((Ib) * cos_inc,na.rm=T)
  m_inc_sh    <- mean((Ib * sh)*cos_inc,na.rm=T)
  m_vf_base   <- mean((Ib * sh + Id)*cos_inc,na.rm=T)
  m_vf        <- mean((Ib * sh + Id * VF_mat )*cos_inc,na.rm=T)
  m_ref       <- mean((Ib * sh + Id * VF_mat + Ir)*cos_inc ,na.rm=T)
  
  # mods SA = c(1,4) | TS = c(1,5) | DIF = c(6,7) | REF = c())
  mods = c(1,4)
  mod_ls = c(m_flat,m_flat_base,m_flat_sh,m_inc_sr,m_inc_sh,m_vf_base,m_vf,m_ref)
  mod_nam = c("m_flat","m_flat_base","Topographic shading","Incidence angle","Topographic shading","Flat diffuse","Diffuse sky","Terrain-reflected")
  cols2 = cols[mods[2]]
  
  ## PLOT
  if (moment == 1){
    plot(1:zz,rep(NA,zz),ylim=c(0,1200),
         main='',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour',cex.lab=1.6,cex.axis=1.6)
    # legend('topright',legend = c("Base", mod_nam[mods[2]]), pch=c(21,19),
    #        col=c("black",cols2), bg = c('lightgray',NA),cex=1.2,bty='n')
    #legend('topleft',paste('res:',round(dem_res,-1),'m'),bty='n',cex=0.8)
    
    points(moment,mod_ls[mods[1]],pch = 21, bg = "lightgray", col = "black", cex=2.5)
    points(moment,mod_ls[mods[2]],pch=19, col=cols2, cex=2)

  } else{
    points(moment,mod_ls[mods[1]],pch = 21, bg = "lightgray", col = "black", cex=2.5)
    points(moment,mod_ls[mods[2]],pch=19, col=cols2, cex=2)
  }
}

###############################################
p.sw.moment2 <- function(date = ISOdate(2017, 6, 21, 0), mods = c(1,4), change_vals=FALSE, spt = NULL, center_sp = FALSE){
  # ! call shortwave.moments
  # bell curve of variables over the course of the day
  # mods SA = c(1,4) | TS = c(1,5) | DIF = c(6,7) | REF = c())
  # plot moment by moment update
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
  cols2 = cols[mods[2]]
  
  if (center_sp){
    #extract center point of glacier  
    spt = SpatialPoints(coordinates(data.frame(lat_lon[2],lat_lon[1])),proj4string=crs(dem))
  }
  
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
    # TOPOGRAPHIC SHADING (cast shadow)
    #print(paste('...making shade for', m, 'of', length(sv[,1]), 'moments - for day', i))
    sh = doshade(d_mat, sv[m,], dl=dem_res)
    
    # INCIDENT ANGLE
    # cos_inc <- cos.slope(zenith[m], azimuth_eq[m], aspect = s_a[[2]], slope = s_a[[1]])
    # !!!!! FORCE SPECIFIC SLOPE
    asp2 = s_a[[2]]
    asp2@data@values = radians(1)
    cos_inc <- cos.slope(zenith[m], azimuth_eq[m], aspect = asp2, slope = radians(20))
    
    #################################################################################
    # INSOLATION
    Idirdif = insolation(zenith[m],jd[m],height,visibility,RH,tempK,0.002,0.45)
    Ib = matrix(Idirdif[,1],nrow=nrow(dem),ncol=ncol(dem))
    Id = matrix(Idirdif[,2],nrow=nrow(dem),ncol=ncol(dem))
    alphaT = 0.45
    # terrain-reflected
    # Ir = Iglob * (1 - VF_mat) * alphaT) 
    # Ir = (Ib*sh + Id * VF_mat) * 0.45 * (1 - VF_mat) * cos_sfc[m]
    Iglob = (Ib + Id * VF_mat)*cos_sfc[m]
    Ir = Iglob * (as.matrix((1 + cos(s_a[[1]]))/2) - VF_mat) * alphaT # (Hetrick 1993)
    Ir[Ir < 0 ] = 0
    # Ir = Iglob * (1 - VF_mat) * alphaT # Dozier (Hetrick 1993)
    
    ## MODELS
    m1 <-     make.raster((Ib)*cos_sfc[m], dem)
    m2 <-     make.raster((Ib + Id)*cos_sfc[m], dem)
    m3 <-     make.raster((Ib * sh)*cos_sfc[m], dem)
    m4 <-     make.raster((Ib) * cos_inc, dem)
    m5 <-     make.raster((Ib * sh)*cos_inc, dem)
    m6 <-     make.raster((Ib * sh + Id)*cos_inc, dem)
    m7 <-     make.raster((Ib * sh + Id * VF_mat )*cos_inc, dem)
    m8 <-     make.raster((Ib * sh + Id * VF_mat + Ir)*cos_inc, dem)
    
    # display models or change in irradiance?
    if (change_vals){
      print("doesnt exist yet!")
      model_flat      <<- array(0,dim=dim(d_mat))
      model_flat_base <<- array(0,dim=dim(d_mat))
      model_flat_sh   <<- array(0,dim=dim(d_mat))
      model_inc_sr    <<- array(0,dim=dim(d_mat))
      model_inc_sh    <<- array(0,dim=dim(d_mat))
      model_vf_base   <<- array(0,dim=dim(d_mat))
      model_vf        <<- array(0,dim=dim(d_mat))
      model_ref       <<- array(0,dim=dim(d_mat)) 
      
      slope_asp   = model_inc_sh - model_flat_sh
      tot_sh      = model_flat_sh - model_flat
      cast_sh     = model_inc_sh - model_inc_sr
      sh_rel      = (tot_sh - cast_sh)
      diff_t      = model_vf - model_vf_base
      refl        = model_ref - model_vf
      comb        = model_ref - model_flat_base
      
      sa = m5 - m3
      ts = m5 - m4
      df = m7 - m6
      ref = m8 - m7
      com = m7 - m2
      stk = stack(sa, m2, m3, m4, m5, m6, m7, m8)
      stk = mask(crop(stk, glacier), glacier)
      
      y_lim = c(-200,200)
      
    } else{
      stk = stack(m1, m2, m3, m4, m5, m6, m7, m8)
      stk = mask(crop(stk, glacier), glacier)
      
      # FIX! MODEL NAMES ETC
      mod_nam = c("Flat direct","Flat dirdif","Flat shading","Incidence angle","Topographic shading",
                  "Diffuse slope","Diffuse sky","Terrain-reflected")
      y_lim = c(0,1200)
    }
    
    # which values to extract?
    if (!is.null(spt)){
      # extract at a point
      mod_ls = extract(stk,spt)
    } else{
      # take overall mean of glacier
      mod_ls = colMeans(getValues(stk),na.rm=T)
    }
    
    print(paste(m, '-',mod_ls[mods[1]],'|', mod_ls[mods[2]]))
    
    ## PLOT
    if (m == 1){
      plot(1:length(zenith),rep(NA,length(zenith)),ylim=y_lim,
           main='',ylab=expression(Irradiance~(Wm^-2)),xlab='Sun hour',cex.lab=1.6,cex.axis=1.6)
      # legend('topright',legend = c("Base", mod_nam[mods[2]]), pch=c(21,17),
      #        col=c("black",cols2), bg = c('lightgray',NA),cex=1.2,bty='n')
      #legend('topleft',paste('res:',round(dem_res,-1),'m'),bty='n',cex=0.8)
      
      points(m,mod_ls[mods[1]],pch = 21, bg = "slategrey", col = "black", cex=2.5)
      points(m,mod_ls[mods[2]],pch=24, bg=cols2, col='black', cex=1.5)
      
    } else{
      points(m,mod_ls[mods[1]],pch = 21, bg = "slategrey", col = "black", cex=2.5)
      points(m,mod_ls[mods[2]],pch=24, bg=cols2, col='black', cex=1.5)
    }
  }
}

###############################################
plot.shortwave.maps <- function(){
  # ! call shortwave.moments
  
}

###############################################
plot.elevation <- function(stk = NULL){
  if (stk==NULL){
    # ! call shortwave.moments (if doesn't already exist)
    
  } else{
    
  }
}



