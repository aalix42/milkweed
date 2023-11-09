library(terra)
library(dplyr)



imgG22 <- rast("K:/Environmental_Studies/hkropp/projects/aalix/classification/images/golf_07_27_2022.tif")
extent22 <- vect("K:/Environmental_Studies/hkropp/projects/aalix/classification/extents/extent_07_27_22.shp")

img22c <- mask(imgG22, extent22)



set.seed(15)
rowsi <- sample(1:(nrow(img22c)-255), 500)
set.seed(43)
colsi <- sample(1:(ncol(img22c)-255), 500)

# subset raster
training.samples <- list()
training.check <- numeric()

for(i in 1:500){
  training.samples[[i]] <-  imgG22[rowsi[i]:(rowsi[i]+255), colsi[i]:(colsi[i]+255), drop=FALSE]
  # create a check to remove any training samples with NAs
  training.check[i] <- ifelse(sum(values(training.samples[[i]],mat=FALSE)) == 0,0,1)
}




plotRGB(training.samples[[10]], r=3, g=2, b=1, stretch="lin")
values(training.samples[[10]])



training.valid <- which(training.check == 1)
samples.use <- list()
#first sample looks odd
for(i in 1:344){
  samples.use[[i]] <- training.samples[[training.valid[i]]]
}
plotRGB(samples.use[[3]], stretch="lin")



for(i in 108:200){
  writeRaster(samples.use[[i]], paste0("K:/Environmental_Studies/hkropp/projects/aalix/classification/training/img_g22/img_",i,".tif"))
} 

for(i in 201:340){
  writeRaster(samples.use[[i]], paste0("K:/Environmental_Studies/hkropp/projects/aalix/classification/training/img_g22/img_",i,".tif"))
} 


tileI <- makeTiles(img22c, c(256,256), "K:/Environmental_Studies/hkropp/projects/aalix/classification/image_tiles/golf_07_22_22/img.tif")


# read in stratified sampling boundary

st_bound <- vect("K:/Environmental_Studies/hkropp/projects/aalix/classification/bound/strat_bound.shp")



boundRast <- rasterize(st_bound, img22c)
plot(boundRast)


terra::plot(img22c, stretch="lin")
for(i in 1:66){
  plot(samples.use[[i]], add=TRUE)
}  
set.seed(15)
test <- spatSample(st_bound, 60)
plot(st_bound)
plot(test, add=TRUE)

xycell <- cells(img22c, test)

testxy <- rowColFromCell(img22c, 14382021)

# subset raster
training.samples <- list()
training.check <- numeric()
rowi <- numeric()
coli <- numeric()
#training.check <- numeric()
for(i in 1:60){
  rowi[i] <- rowFromCell(img22c, xycell[i,2])
  coli[i] <- colFromCell(img22c, xycell[i,2])
}

for(i in 1:60){  
  if(rowi[i] <= nrow(img22c)-255 & coli[i] <= ncol(img22c)-255){
  training.samples[[i]] <-  img22c[rowi[i]:(rowi[i]+255), coli[i]:(coli[i]+255), drop=FALSE]
  training.check[i] <- ifelse(is.na(mean(values(training.samples[[i]],mat=FALSE))) == TRUE,0,1)
  }else{
    training.samples[[i]] <- NA
    training.check[i] <- 0
  }
}
training.valid <- which(training.check == 1)

samples.use <- list()
#first sample looks odd
for(i in 1:37){
  samples.use[[i]] <- training.samples[[training.valid[i]]]
}
terra::plot(img22c, stretch="lin")
for(i in 1:37){
  plot( samples.use[[i]]$Band_1, add=TRUE)
}  
plot(test, add=TRUE)
plot(st_bound, add=TRUE)
plot(samples.use[[9]], stretch="lin")

for(i in 1:37){
  writeRaster(samples.use[[i]], paste0("K:/Environmental_Studies/hkropp/projects/aalix/classification/training/img_g22/img_",i+340,".tif"))
} 