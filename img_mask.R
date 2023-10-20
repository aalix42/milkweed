##########################################################
###### convert shapefiles to raster for masks        ####
###### runs r v4.3.0                                 #####
##########################################################


library(terra)
library(dplyr)

dirD <- "/media/hkropp/research/milkweed"
# names of all shapefiles
img <- list.files(paste0(dirD, "/training/img_g22"),pattern=".tif")
imgXML <- grepl(".aux.xml",img)
img <- img[imgXML == FALSE ] 



milkweed <- list.files(paste0(dirD, "/shapefiles"),pattern=".shp")
milkweedXML <- grepl(".xml", milkweed)
milkweed <- milkweed[milkweedXML == FALSE ] 


# extract numbers and naming info
imgNumber <- as.numeric(gsub("\\D","", img))
milkweedNumber <- as.numeric(gsub("\\D","", milkweed))
milkweedName <- gsub(".shp", ".tif",milkweed)

# read in all images
imgL <- list()
for(i in 1:length(img)){
  imgL[[i]] <- rast(paste0(dirD,"/training/img_g22/" ,img[i]))
  
}


#### Water ----
# read in shapefiles
milkL <- list()
for(i in 1:length(milkweed)){
  milkL[[i]] <- vect(paste0(dirD, "/shapefiles/",milkweed[i]))
}  
plot(milkL[[1]])

# pull out the corresponding image and convert shapefile to raster
imgPos <- numeric()
milkR <- list()
for(i in 1:length(milkweed)){
  imgPos <- which(imgNumber == milkweedNumber[i])
 milkR[[i]] <- rasterize(milkL[[i]], imgL[[imgPos]], background=0)
}

plot(milkR[[10]])
