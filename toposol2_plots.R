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
p.sw.anom <- function(df, y_lim = c(-100,150), plot4 = TRUE, mods_plot4 = c(1,4,6,7), drop_res = c(1,4), topovar=7, placeLegend = 'topleft'){
  # plot anomaly values from dataframe
  
  # set plot params
  cols = c('steelblue',brewer.pal(6,'Reds')[3:5],'palegreen4','orange2','slategray4') # brewer.pal(2,'Accent')
  col_names = unlist(lapply(names(df), function(x) {gsub("\\.", " ", x)}))
  
  # create resolution list
  res_ls  <- as.numeric(levels(as.factor(df$Resolution)))
  if (length(res_ls) > 1 & !is.null(drop_res)){
    res_ls <- res_ls[-drop_res]
  }
  l_ty = c(1,5,4,2,3)    #seq(1, length(res_ls))
  l_wd = c(4, rep(2, length(res_ls) - 1))
  
  if(plot4){
    par(mfrow=c(2,2))
    par(mar=c(3,3,2,1))
    
    #mod_select = c(1,4,6,7)
    #mod_select = c(1,4:6)
    mod_select = mods_plot4
    
    for (j in mod_select){
      # select data
      dfg = df[df$Resolution == res_ls[1],]
      
      # make plots
      plot(smooth.spline(dfg[,j]~dfg$Distance.Elevation), type = 'l', col='white',
           ylim=y_lim, ylab=expression(SW~anomaly~from~'8m'~(Wm^2)), lwd = l_wd[1],
           lty = l_ty[1],xlab='Distance from mid-elevation (m)',cex.lab=1.3, cex.axis=1.8)
      legend("topright",legend = col_names[j],cex=1.4,bty='n')
      
      for(rr in res_ls){
        # iterate through all resolutions
        dfg = df[df$Resolution == rr,]
        
        lines(smooth.spline(dfg[,j]~dfg$Distance.Elevation), lwd=l_wd[match(rr,res_ls)],
              lty=l_ty[match(rr,res_ls)], col=cols[j])
      }
      abline(h=0,lty=3)
    }
  } else{
    par(mfrow=c(1,1))
    par(mar=c(5,5.1,2,1))
    
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
           lty=l_ty,cex=1.1,bty='n')
  }
  par(mfrow=c(1,1))
  par(mar=c(5,5.1,2,1))
}

###############################################
p.sw.box <- function(df, mod_select = c(1,4,6,7), drop_res = c(1,4)){
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
  
  # boxplots
  dmelt <- melt(dfs,id.vars='Resolution', measure.vars=mod_select)
  names(cols2) <- levels(dmelt$variable)
  ggplot(dmelt, aes(x=factor(Resolution), y=value,fill=variable))+
    geom_boxplot() + scale_color_manual(name = "Topo factor", values = cols2) +
    facet_wrap(.~variable, ncol=2) + scale_x_discrete(name = "Resolution (m)", labels=res_names) +
    scale_y_discrete(name=expression(Mean~irradiance~change~(Wm^-2)), limits = seq(-150,100,25)) +
    theme(axis.text.x=element_text(angle=90, vjust=0.4,hjust=1)) + theme_classic()
  
}

###############################################
p.sw.elv <- function(df, res=NULL, y_lim = c(-250,100), placeLegend = 'bottomleft', inc_all_shade = FALSE){
  # output from dfts
  # plot anomaly values from dataframe (define resolution)
  
  # set plot params
  #par(mfrow=c(1,1))
  par(mar=c(5,5.8,2,2))
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
           lty=l_ty[mod_select],cex=1.6,bty='n')
  }
}


###############################################
plot.sw.moments <- function(){
  # plot moment by moment update
}

###############################################
plot.shortwave.moment <- function(){
  # ! call shortwave.moments
  # bell curve of variables over the course of the day
  
  ## PLOT
  if (m == 1){ # could do 2 plots -- compare Ib Id Ir
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
    points(m,mean((Ib * sh + Id * VF_mat + Ir * (1 - VF_mat))*cos_inc,na.rm=T),pch=18)
  } else{
    points(m,mean((Ib)*cos_sfc[m],na.rm=T),pch=13)
    points(m,mean((Ib * sh)*cos_sfc[m],na.rm=T),pch=14)
    points(m,mean((Ib) * cos_inc,na.rm=T),pch=15)
    points(m,mean((Ib * sh)*cos_inc,na.rm=T),pch=16)
    points(m,mean((Ib * sh + Id * VF_mat )*cos_inc,na.rm=T),pch=17)
    points(m,mean((Ib * sh + Id * VF_mat + Ir * (1 - VF_mat))*cos_inc,na.rm=T),pch=18)
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



