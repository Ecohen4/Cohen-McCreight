########################
## Surface Contour Function
#########################
# example variables to be passed into function
z<-"precip.total"
data<-monthly
surfaceplot<-function(data, z, nplots) {
  # create sequential vector of months in data range
  #mons<-seq(range(data$Date)[1], range(data$Date)[2], by="months")
  # or if Date range is discontinuous, use
  mons<-unique(data$Date)
  mon<-mons[order(mons)]
  # PLOT 
  par(mfrow=c(1,1)) 
  par(mar=c(2,2,1.5,1)+0.1) # par(mar=c(5,4,4,2)+0.1)
  par(oma=c( 0,0,0,4))  # 4 space margin width on RHS
  set.panel(3,4)      # 3X4 matrix of plots
  # now draw plots using image() command
  for (i in 1:length(mons) %/% n){
    data1<-subset(data, Date==mons[i])
    zz0<-interp(x=data1$Long, y=data1$Lat, z=data1[,z])
    image(zz0, col=topo.colors(n=64,alpha=1), main=paste(data1$Mon[1], data1$Yr[1], "Total Precip (mm)", sep=" "), lab.breaks=NULL)
    contour(zz0, add=T)
    world(add=TRUE, lwd=4)
  }
  set.panel(1,4) # nXm matrix of plots
  par(oma=c(0,0,0,1))# reset margin to be much smaller.
  par(mar=c(0,0,0,0)+0.0) 
  image.plot(legend.only=TRUE, legend.width=2.5, zlim=c(0,max(data[,z])), col=topo.colors(n=64,alpha=1), cex.axis=1.5) 
  # end plot 1 of n
  
  for(i in length(mons) %% n : length(mons))
    data1<-subset(data, Date==mons[i])
  zz0<-interp(x=data1$Long, y=data1$Lat, z=data1[,z])
  image(zz0, col=topo.colors(n=64,alpha=1), main=paste(data1$Mon[1], data1$Yr[1], "Total Precip (mm)", sep=" "), lab.breaks=NULL)
  contour(zz0, add=T)
  world(add=TRUE, lwd=4)
}
set.panel(1,4) # nXm matrix of plots
par(oma=c(0,0,0,1))# reset margin to be much smaller.
par(mar=c(0,0,0,0)+0.0) 
image.plot(legend.only=TRUE, legend.width=2.5, zlim=c(0,max(data[,z])), col=topo.colors(n=64,alpha=1), cex.axis=1.5) 
# end plot 2 of 2
}

}

# example
surfacefun(monthly, "precip.total")