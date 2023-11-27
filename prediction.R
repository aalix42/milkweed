library(terra)
library(dplyr)


# Image merge ---------

dirP <- "/media/hkropp/research/milkweed/predictions/run02"

filesL <- list.files(dirP)

Nimg <- length(filesL)


milkImg <- list()

for(i in 1:Nimg){
  milkImg[[i]] <- rast(paste0(dirP,"/",filesL[i]))
  
  
}

milkAll <- do.call(merge, milkImg)
plot(milkAll)


milkMap1 <- ifel(milkAll >= 0.1, 1, 0) 

milkMap2 <- ifel(milkAll >= 0.6, 1, 0) 

writeRaster(milkAll, "/media/hkropp/research/milkweed/predictions/maps/run02/milk_prob.tif", filetype="GTiff" )
writeRaster(milkMap1, "/media/hkropp/research/milkweed/predictions/maps/run02/milk_T1.tif", filetype="GTiff" )
writeRaster(milkMap2, "/media/hkropp/research/milkweed/predictions/maps/run02/milk_T2.tif", filetype="GTiff" )
