findClosestISD <- function(stn, knn) {
  ## find station name, id, lat,
  llDist <- rdist.earth(matrix(c(stn$Long,stn$Lat), ncol=2),
                        matrix(c(metdata$Long, metdata$Lat), ncol=2),
                        miles=FALSE, R=6371) ## mean radius in km
  sortDistInds <- sort( llDist, ind=TRUE)$ix
  return( cbind(ID=stn[,1],
                distance.km=llDist[sortDistInds[1:knn]],
                metdata[sortDistInds[1:knn],] ) )
}