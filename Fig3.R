#
#
# FIGURE 3
#
# # # # # # # # # # # # # # # # # # # # # # # # #

###############################################
p.sw.fig3 <- function(df, res=NULL, y_lim = c(-250,100), placeLegend = 'bottomleft', means=FALSE, inc_all_shade = FALSE, ax = NULL){
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
  
  # make plots
  plot(smooth.spline(dfg[,1]~dfg[,9]), type = 'l', col='white',
       ylim=y_lim, ylab=expression(Mean~change~'in'~irradiance~(Wm^2)), lwd = l_wd[1],
       lty = l_ty[1],xlab='Elevation (m)',cex.lab=1.3, cex.axis=1.8, axes = F)
  if (!is.null(ax)){
    for (i in ax){axis(i, cex.lab=1.3, cex.axis=1.8)}
  }
  box()
  axis(2, labels=NA)
  axis(1, labels=NA)
  # all lines
  for(j in mod_select){
    lines(smooth.spline(dfg[,j]~dfg[,9]), lwd=l_wd[j],
          lty=l_ty[j], col=cols[j])
  }
  abline(h=0,lty=3)
  if(!is.null(placeLegend)){
    legend(placeLegend,mod_names,col=cols[mod_select],lwd=l_wd[mod_select],
           lty=l_ty[mod_select],cex=1.5,bty='n')
  }
  if(means){
    val = round(mean(dfg[,7], na.rm=T))
    legend("bottomleft",legend = bquote(paste(mu == .(val))),cex=1.5,bty='n')
  }
}


# north glacier
par(mfrow=c(3,2))
# par(mar=c(5,5.8,2,2))
par(oma=c(2,3,0.3,0.3))
par(mar=c(1,1,0,0))
dftf_n <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dftf_2.csv")
p.sw.fig3(dftf_n,res=8, y_lim = c(-400,250), placeLegend = NULL, ax = 2)
# south glacier
dftf_s <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/march21_all/dftf_8.csv")
p.sw.fig3(dftf_s,res=8, y_lim = c(-400,250), placeLegend = NULL)
# JUNE
dftf_n <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dftf_2.csv")
p.sw.fig3(dftf_n,res=8, y_lim = c(-400,250), placeLegend = NULL, ax = 2)
# south glacier
dftf_s <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/june21_all/dftf_8.csv")
p.sw.fig3(dftf_s,res=8, y_lim = c(-400,250), placeLegend = NULL)
# DECEMBER
dftf_n <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dec21_all/dftf_2.csv")
p.sw.fig3(dftf_n,res=8, y_lim = c(-400,250), placeLegend = NULL, ax = c(1,2))
# south glacier
dftf_s <- read.csv("F:/HiMAT/MATTO/PROJECTS/WV_RESOLUTION/variables/dec21_all/dftf_8.csv")
p.sw.fig3(dftf_s,res=8, y_lim = c(-400,250), ax = 1)

