library(terra)
library(dplyr)



imgG22 <- rast("K:/Environmental_Studies/hkropp/projects/aalix/classification/images/golf_07_27_2022.tif")
extent22 <- vect("K:/Environmental_Studies/hkropp/projects/aalix/classification/extents/extent_07_27_22.shp")

img22c <- mask(imgG22, extent22)



set.seed(15)
rowsi <- sample(1:(nrow(img22c)-255), 300)
set.seed(43)
colsi <- sample(1:(ncol(img22c)-255), 300)

# subset raster
training.samples <- list()
training.check <- numeric()

for(i in 1:300){
  training.samples[[i]] <-  imgG22[rowsi[i]:(rowsi[i]+255), colsi[i]:(colsi[i]+255), drop=FALSE]
  # create a check to remove any training samples with NAs
  training.check[i] <- ifelse(sum(values(training.samples[[i]],mat=FALSE)) == 0,0,1)
}




plotRGB(training.samples[[10]], r=3, g=2, b=1, stretch="lin")
values(training.samples[[10]])



training.valid <- which(training.check == 1)
samples.use <- list()
#first sample looks odd
for(i in 1:200){
  samples.use[[i]] <- training.samples[[training.valid[i]]]
}
plotRGB(samples.use[[3]], stretch="lin")



for(i in 108:200){
  writeRaster(samples.use[[i]], paste0("K:/Environmental_Studies/hkropp/projects/aalix/classification/training/img_g22/img_",i,".tif"))
} 




tileI <- makeTiles(img22c, c(256,256), "K:/Environmental_Studies/hkropp/projects/aalix/classification/image_tiles/golf_07_22_22/img.tif")

