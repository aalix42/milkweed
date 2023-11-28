library(dplyr)
library(terra)
library(ggplot2)
library(sf)
library(caret)

VP1 <- vect("K:/Environmental_Studies/hkropp/projects/aalix/Valid/valid2.shp")
VP2 <- vect("K:/Environmental_Studies/hkropp/projects/aalix/Valid/valid3.shp")
VP3 <- vect("K:/Environmental_Studies/hkropp/projects/aalix/Valid/valid4.shp")
VP4 <- vect("K:/Environmental_Studies/hkropp/projects/aalix/Valid/valid6.shp")

milk <- rast("K:/Environmental_Studies/hkropp/projects/aalix/classification/maps/run02/milk_T1.tif")

VE1 <- extract(milk, VP1)
VE2 <- extract(milk, VP2)
VE3 <- extract(milk, VP3)
VE4 <- extract(milk, VP4)

##extract values out of table 
VT1 <- values(VP1)
VT2 <- values(VP2)
VT3 <- values(VP3)
VT4 <- values(VP4)

#creating a comparison table for validation. 
validTable <- data.frame(reference = c(VT1$Class, VT2$Class, VT3$Class, VT4$Class), 
                         prediction = c(VE1$predict__img1009.tif, 
                                        VE2$predict__img1009.tif, 
                                        VE3$predict__img1009.tif,
                                        VE4$predict__img1009.tif))
validTable$referenceID <- ifelse(validTable$reference == "o", 0 , 
                                 ifelse(validTable$reference == "m", 1, NA))
validDF <- na.omit(validTable)
conf <- confusionMatrix(as.factor(validTable$prediction), as.factor(validTable$referenceID))
conf

