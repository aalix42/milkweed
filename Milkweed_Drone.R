#### libraries ----
library(terra)
library(sf)
library(dplyr)
library(lubridate)
library(ggplot2)
library(RColorBrewer)

#### read in data ----
# read in drone data 
d07_05 <- rast(c("K:/Environmental_Studies/hkropp/projects/aalix/Flight07_05/flight_07_05_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight07_05/flight_07_05_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight07_05/flight_07_05_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight07_05/flight_07_05_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight07_05/flight_07_05_transparent_reflectance_nir.tif"))
poly07_05 <- vect("K:/Environmental_Studies/hkropp/projects/aalix/forR/Plots07_05.shp")


plotRGB(d07_05, r=3, g = 2, b = 1, stretch = "lin")

d07_26 <- rast(c("K:/Environmental_Studies/hkropp/projects/aalix/flight7_26/flight07_26_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/flight7_26/flight07_26_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/flight7_26/flight07_26_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/flight7_26/flight07_26_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/flight7_26/flight07_26_transparent_reflectance_nir.tif"))
poly7_26 <- vect(c("K:/Environmental_Studies/hkropp/projects/aalix/forR/newPlots7_26.shp"))

plotRGB(d07_26, r=3, g = 2, b = 1, stretch = "lin")

#calculate NDVI
NDVI7_26 <- (d07_26[[5]] - d07_26[[3]])/ (d07_26[[5]] + d07_26[[3]])
plot(NDVI7_26)
plot(poly7_26, add = TRUE)

#calculate NDVI
NDVI07_05 <- (d07_05[[5]] - d07_05[[3]])/ (d07_05[[5]] + d07_05[[3]])
plot(NDVI07_05)
plot(poly07_05,add=TRUE)

##ebi7/26
EBI07_26 <- (d07_26[[1]] + d07_26[[2]] + d07_26[[3]])/ ((d07_26[[2]]/d07_26[[1]]) * (d07_26[[3]]-d07_26[[1]]+1))
plot(EBI07_26)
plot(poly7_26,add=TRUE)

#calculate EBI
EBI07_05 <- (d07_05[[1]] + d07_05[[2]] + d07_05[[3]])/ ((d07_05[[2]]/d07_05[[1]]) * (d07_05[[3]]-d07_05[[1]]+1))
plot(EBI07_05)
plot(poly07_05,add=TRUE)

zonal_07_05 <- zonal(EBI07_05, poly07_05, fun="mean")
table_07_05 <- values(poly07_05, dataframe = TRUE)
table_07_05$EBI<-zonal_07_05[,1]

#zonal stats 
zonal07_26new <- zonal(NDVI7_26, poly7_26, fun = "mean")
EBIZonal7_26 <- zonal(EBI07_26, poly7_26, fun = "mean")
table7_26new <- values(poly7_26, dataframe=TRUE)
table7_26new$NDVI <- zonal07_26new[,1]
table7_26new$EBI <- EBIZonal7_26[,1]

zonal07_26b <- zonal(d07_26, poly7_26, fun="mean")
table_0726new <- cbind(table7_26new, zonal07_26b[,1:5])

#zonalStats
zonal07_05 <- zonal(NDVI07_05, poly07_05, fun="mean")
table_07_05 <- values(poly07_05, dataframe=TRUE)
table_07_05$NDVI<-zonal07_05[,1]
table_07_05$EBI<-zonal_07_05[,1]
 
test <- values(poly7_26, dataframe=TRUE)

zonal07_05b <- zonal(d07_05, poly07_05, fun="mean")
table_07_05f<- cbind(table_07_05, zonal07_05b[,1:5])

#### read in 07/06 ----
d07_06 <- rast(c("K:/Environmental_Studies/hkropp/projects/aalix/Flight_07_06/flight07_06_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight_07_06/flight07_06_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight_07_06/flight07_06_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight_07_06/flight07_06_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight_07_06/flight07_06_transparent_reflectance_nir.tif"))


poly07_06 <- vect("K:/Environmental_Studies/hkropp/projects/aalix/forR/poly_07_06.shp")
plotRGB(d07_06, r=3, g = 2, b = 1, stretch = "lin")

EBI07_06 <- (d07_06[[1]] + d07_06[[2]] + d07_06[[3]])/ ((d07_06[[2]]/d07_06[[1]]) * (d07_06[[3]]-d07_06[[1]]+1))
plot(EBI07_06)
plot(poly07_06,add=TRUE)

zonal_07_06 <- zonal(EBI07_06, poly07_06, fun="mean")
table_07_06 <- values(poly07_06, dataframe = TRUE)
table_07_06$EBI<-zonal_07_06[,1]

#calculateNDVI, reading in the plots and creating the NDVI formula. 
NDVI07_06 <- (d07_06[[5]] - d07_06[[3]])/ (d07_06[[5]] + d07_06[[3]])
plot(NDVI07_06)
plot(poly07_06,add=TRUE)


zonal_07_06n <- zonal(NDVI07_06, poly07_06, fun="mean")
table_07_06n <- values(poly07_06, dataframe = TRUE)
table_07_06n$NDVI<-zonal_07_06n[,1]


zonal_07_06r <- zonal(d07_06, poly07_06, fun="mean")

table_07_06f <- cbind(table_07_06, zonal_07_06r[,1:5])
table_07_06f$NDVI<-table_07_06n$NDVI

#read in old data 
old7_11 <- rast(c("K:/Environmental_Studies/hkropp/projects/aalix/oldData/flight_07_11_2022_transparent_reflectance_blue.tif",
                  "K:/Environmental_Studies/hkropp/projects/aalix/oldData/flight_07_11_2022_transparent_reflectance_red.tif",
                  "K:/Environmental_Studies/hkropp/projects/aalix/oldData/flight_07_11_2022_transparent_reflectance_green.tif",
                  "K:/Environmental_Studies/hkropp/projects/aalix/oldData/flight_07_11_2022_transparent_reflectance_red edge.tif",
                  "K:/Environmental_Studies/hkropp/projects/aalix/oldData/flight_07_11_2022_transparent_reflectance_nir.tif"))
plotRGB(old7_11, r = 2, g = 3, b = 1, stretch = "lin")


####read in 07/14 ----
d07_14phantom <- rast(c("K:/Environmental_Studies/hkropp/projects/aalix/flight_07_14/flight_07_14_transparent_reflectance_blue.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight_07_14/flight_07_14_transparent_reflectance_red.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight_07_14/flight_07_14_transparent_reflectance_green.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight_07_14/flight_07_14_transparent_reflectance_red edge.tif",
                 "K:/Environmental_Studies/hkropp/projects/aalix/Flight_07_14/flight_07_14_transparent_reflectance_nir.tif"))
poly07_14 <- vect("K:/Environmental_Studies/hkropp/projects/aalix/forR/poly07_14.shp")

EBI07_14 <- (d07_14phantom[[1]] + d07_14phantom[[2]]) + (d07_14phantom[[3]]/ (d07_14phantom[[2]]/d07_14phantom[[1]]) * d07_14phantom[[3]]-d07_14phantom[[1]])
plot(EBI07_14)
plot(poly07_14,add=TRUE)

field2023 <- rast
plotRGB(d07_14phantom, r=2, g = 3, b = 1, stretch = "lin")

NDVI07_14 <- (d07_14phantom[[5]] - d07_14phantom[[2]])/ (d07_14phantom[[5]] + d07_14phantom[[2]])
plot(NDVI07_14)
plot(poly07_14,add=TRUE)

zonal_07_14 <- zonal(NDVI07_14, poly07_14, fun="mean")
table_07_14 <- values(poly07_14, dataframe = TRUE)
table_07_14$NDVI<-zonal_07_14[,1]


zonal_07_14b <- zonal(EBI07_14, poly07_14, fun="mean")
table_07_14 <- values(poly07_14, dataframe = TRUE)
table_07_14$EBI<-zonal_07_14b[,1]


zonal_07_014 <- zonal(d07_14phantom, poly07_14, fun="mean")

table_07_14f<- cbind(table_07_14, zonal_07_014)
table_07_14f$NDVI<-zonal_07_14[,1]


# xls files


#d07_14Matrice <- rast(c("K:/Environmental_Studies/hkropp/projects/aalix/mica_07_14/mica_07_14_23_transparent_reflectance_blue.tif",
                       #"K:/Environmental_Studies/hkropp/projects/aalix/mica_07_14/mica_07_14_23_transparent_reflectance_green.tif",
                        #"K:/Environmental_Studies/hkropp/projects/aalix/mica_07_14/mica_07_14_23_transparent_reflectance_red.tif",
                      #"K:/Environmental_Studies/hkropp/projects/aalix/mica_07_14/mica_07_14_23_transparent_reflectance_red edge.tif",
                        #"K:/Environmental_Studies/hkropp/projects/aalix/mica_07_14/mica_07_14_23_transparent_reflectance_nir.tif"))
#poly07_14mica <- vect("K:/Environmental_Studies/hkropp/projects/aalix/forR/poly07_14.shp")
#plotRGB(d07_14Matrice, r=3, g = 2, b = 1, stretch = "lin")

#NDVI07_14_Matrice <- (d07_14Matrice[[5]]-d07_14Matrice[[3]]) / (d07_14Matrice[[5]] + d07_14Matrice[[3]])
#plot(NDVI07_14_Matrice)
#plot(poly07_14mica, add = TRUE)

#zonal_07_14_mica <- zonal(NDVI07_14_Matrice, poly07_14mica, fun="mean")
#table_07_14mica <- values(poly07_14mica, dataframe = TRUE)
#table_07_14mica$NDVI<-zonal_07_14_mica[,1]
####merge data----
df_0705 <- data.frame(Plot = table_07_05f$Plot,
                      Subplot = table_07_05f$Subplot,
                      Date = rep("7/5/2023", nrow(table_07_05f)), 
                      Red = table_07_05f$flight_07_05_transparent_reflectance_red,
                      Red.Edge = table_07_05f$"flight_07_05_transparent_reflectance_red edge",
                      NIR = table_07_05f$flight_07_05_transparent_reflectance_nir,
                      Green = table_07_05f$flight_07_05_transparent_reflectance_green,
                      Blue = table_07_05f$flight_07_05_transparent_reflectance_blue,
                      NDVI = table_07_05f$NDVI, 
                      EBI = table_07_05f$EBI
                  )



df_0706 <- data.frame(Plot = table_07_06f$Plot,
                      Subplot = table_07_06f$SubPlot, 
                      Date = rep("7/6/2023", nrow(table_07_06f)), 
                      Red = table_07_06f$flight07_06_transparent_reflectance_red,
                      Red.Edge = table_07_06f$"flight07_06_transparent_reflectance_red edge",
                      NIR = table_07_06f$flight07_06_transparent_reflectance_nir,
                      Green = table_07_06f$flight07_06_transparent_reflectance_green,
                      Blue = table_07_06f$flight07_06_transparent_reflectance_blue,
                      NDVI = table_07_06f$NDVI,
                      EBI = table_07_06f$EBI
                      )

df_0714 <- data.frame(Plot = table_07_14f$Plot,
                      Subplot = table_07_14f$Subplot, 
                      Date = rep("7/14/2023", nrow(table_07_14f)),
                      Red = table_07_14f$flight_07_14_transparent_reflectance_red,
                      Red.Edge = table_07_14f$"flight_07_14_transparent_reflectance_red edge",
                      NIR = table_07_14f$flight_07_14_transparent_reflectance_nir,
                      Green = table_07_14f$flight_07_14_transparent_reflectance_green,
                      Blue = table_07_14f$flight_07_14_transparent_reflectance_blue,
                      NDVI = table_07_14f$NDVI,
                      EBI = table_07_14f$EBI)

df_726 <- data.frame(Plot = table_0726new$Plot, 
                     Subplot = table_0726new$Subplot, 
                     Date = rep("7/26/2023", nrow(table_0726new)), 
                     Red = table_0726new$flight07_26_transparent_reflectance_red, 
                     Red.Edge = table_0726new$"flight07_26_transparent_reflectance_red edge",
                     NIR = table_0726new$flight07_26_transparent_reflectance_nir, 
                     Green = table_0726new$flight07_26_transparent_reflectance_green, 
                     Blue = table_0726new$flight07_26_transparent_reflectance_blue, 
                     NDVI = table_0726new$NDVI, 
                     EBI = table_0726new$EBI)
                     
allFlights <- rbind(df_0705, df_0706, df_0714, df_726)

#allFlights$dateF <- mdy(allFlights$Date)
####new data ----
field2022 <- read.csv("K:/Environmental_Studies/hkropp/projects/aalix/FP2022.csv")
field2022$EBI<- (field2022$Blue + field2022$Green + field2022$Red)/((field2022$Green/field2022$Blue)*(field2022$Red - field2022$Blue+1))
##(d07_06[[1]] + d07_06[[2]] + d07_06[[3]])/ ((d07_06[[2]]/d07_06[[1]]) + (d07_06[[3]]-d07_06[[1]]+1))
field2023 <- read.csv("K:/Environmental_Studies/hkropp/projects/aalix/field_plotsUpdatedfixed2.csv")
#field2023$dateF<- mdy(field2023$Date)
df2023 <- left_join(field2023, allFlights, by=c("Plot", "Subplot", "Date"))

fieldAll<- rbind(field2022, df2023)


fieldAll$Flowering <- ifelse(fieldAll$Date == "6/29/2022", "F",
                      ifelse(fieldAll$Date == "6/30/2022", "F",
                      ifelse(fieldAll$Date == "7/11/2022", "T",
                      ifelse(fieldAll$Date == "7/16/2022", "S",
                      ifelse(fieldAll$Date == "7/13/2022", "S",
                      ifelse(fieldAll$Date == "7/5/2023", "F",
                      ifelse(fieldAll$Date == "7/6/2023", "F",
                      ifelse(fieldAll$Date == "7/14/2023", "T",
                      ifelse(fieldAll$Date == "7/25/2023", "S",
                      ifelse(fieldAll$Date == "7/26/2023", "S", NA))))))))))



fieldAll$dateF <- mdy(fieldAll$Date)
fieldAll$Year <- year(fieldAll$dateF)

fieldAll$FLD <- fieldAll$Flowers/fieldAll$Individuals
fieldAll$SSD <- fieldAll$Seeds/fieldAll$Individuals
fieldAll$Cover <- as.numeric(fieldAll$Cover)
fieldAll$DD <- fieldAll$DeadFlowers/fieldAll$Individuals
fieldAll$GI <- fieldAll$Green/(fieldAll$Red+fieldAll$Green+fieldAll$Blue)
fieldAll$GNDVI <- (fieldAll$NIR-fieldAll$Green)/(fieldAll$NIR+fieldAll$Green)
fieldAll$Bloom <- fieldAll$Flowers+fieldAll$Buds

#FlowersPerIndividuals, does percent coverage change?
Flowering <- fieldAll %>%
  filter(Flowering == "F")

ggplot(Flowering, aes(x=as.factor(Year), y=FLD, fill = as.factor(Year))) +
  geom_boxplot() +
  xlab("Year") + 
  ylab("Inflorescence/Individual") + 
  theme_classic()+labs(fill = "Year") + 
  theme(text = element_text(size = 36))+
  scale_fill_manual(values = c("red", "yellow"))

ggplot(fieldAll, aes(x=as.factor(Year), y=Individuals, fill= as.factor(Year))) + 
  geom_boxplot()+ 
  xlab("Year") + 
  theme_classic()+labs(fill = "Year") + 
  theme(text = element_text(size = 40))+
  scale_fill_manual(values = c("red", "yellow"))
ggplot(Flowering, aes(x=as.factor(DD), y = NDVI))
geom_boxplot()




#summary stats 
FlowerSummary <- Flowering %>%
  group_by(Year) %>%
  summarise(mean = mean(FLD, na.rm=TRUE),
            sd = sd(FLD, na.rm=TRUE), 
            n = n())
FlowerSummary$SE<- FlowerSummary$sd/sqrt(FlowerSummary$n)
FlowerSummary

2.48-1.432

IndividualSummarry <- fieldAll %>%
  group_by(Year) %>%
  summarise(mean = mean(Individuals, na.rm=TRUE),
            sd = sd(Individuals, na.rm=TRUE), 
            n = n())

IndividualSummarry
t.test(Flowering$FLD ~ as.factor(Flowering$Year))
shapiro.test(Flowering$FLD[Flowering$Year == 2022])
shapiro.test(Flowering$FLD[Flowering$Year == 2023])

NDVISummarry <- fieldAll %>%
  group_by(Flowering) %>%
  summarise(mean = mean(NDVI, na.rm=TRUE),
            sd = sd(NDVI, na.rm=TRUE), 
            n = n())
NDVISummarry

GNDVISummarry <- fieldAll %>%
  group_by(Flowering) %>%
  summarise(mean = mean(GNDVI, na.rm=TRUE),
            sd = sd(GNDVI, na.rm=TRUE), 
            n = n())
GNDVISummarry


Seeding <- fieldAll %>%
  filter(Flowering == "S")
ggplot(Seeding, aes(x=as.factor(Year), y=SSD, fill=(as.factor(Year))+ 
  geom_boxplot()+
  xlab("Year")+
  ylab("Seeds per Individual")))


ggplot(fieldAll%>%filter(Flowering=="F")%>%filter(Individuals>0), aes(x=Flowers, y=EBI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll%>%filter(Flowering=="S"), aes(x=Flowers, y=EBI, color=Flowering)) + 
  geom_point()

SeedSummary <- Seeding %>%
  group_by(Year) %>%
  summarise(mean = mean(SSD, na.rm=TRUE),
            sd = sd(SSD, na.rm=TRUE), 
            n = n())
SeedSummary$SE<- SeedSummary$sd/sqrt(SeedSummary$n)

t.test(Seeding$SSD ~ as.factor(Seeding$Year))
shapiro.test(Seeding$SSD[Seeding$Year == 2022])
shapiro.test(Seeding$SSD[Seeding$Year == 2023])


ggplot(fieldAll%>%filter(Flowering=="F")%>%filter(Individuals>0), aes(x=Flowers, y=EBI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll%>%filter(Individuals>0), aes(x=Flowers, y=GNDVI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll%>%filter(Individuals>0), aes(x=Bloom, y=GNDVI, color=Flowering)) + 
  geom_point()


ggplot(fieldAll%>%filter(Individuals>0), aes(x=FLD, y=GNDVI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll%>%filter(Individuals>0), aes(x=Bloom, y=EBI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll%>%filter(Individuals>0), aes(x=DD, y=NDVI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll%>%filter(Individuals>0, Flowering != "T"), aes(x=Individuals, y=NDVI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll%>%filter(Individuals>0, DD > 0), aes(x=DD, y=GNDVI, color=Flowering)) + 
  geom_point() + 
  geom_smooth()

ggplot(fieldAll%>%filter(Individuals>0, Flowering != "T"), aes(x=Individuals, y=GNDVI, color=Flowering)) + 
  geom_point()
  geom_smooth(method = "lm", se = FALSE) 

ggplot(fieldAll, aes(x= Individuals, y = NDVI))+
  geom_point()

ggplot(fieldAll%>%filter(Individuals>0), aes(x=Individuals, y=NDVI, color=Flowering)) + 
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) 

ggplot(fieldAll%>%filter(Stressed==0), aes(x=FLD, y=NDVI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll%>%filter(Individuals>0), aes(x=as.factor(Stressed), y=NDVI)) + 
  geom_boxplot()

ggplot(fieldAll, aes(x=as.factor(Year), y=Individuals, fill= as.factor(Year))) + 
  geom_boxplot()+ 
  xlab("Year") + 
  theme_classic()+labs(fill = "Year") + 
  theme(text = element_text(size = 25))+
  scale_fill_manual(values = c("red", "yellow"))
                                        



ggplot(fieldAll, aes(x=as.factor(dateF), y=GNDVI, color=Flowering)) + 
  geom_boxplot()

ggplot(fieldAll%>%filter(Individuals>0), aes(x=as.factor(dateF), y=NDVI, color=Flowering)) + 
  geom_boxplot()

ggplot(fieldAll%>%filter(Individuals>0), aes(x=as.factor(dateF), y=FLD, color=Flowering)) + 
  geom_boxplot()


ggplot(fieldAll%>%filter(Cover>0), aes(x=Cover, y=GNDVI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll%>%filter(Cover>0), aes(x=Cover, y=GI, color=Flowering)) + 
  geom_point()


ggplot(fieldAll, aes(x=Red, y=Blue, color=Flowering)) + 
  geom_point()

ggplot(fieldAll, aes(x=Red.Edge, y=Blue, color=Flowering)) + 
  geom_point()


ggplot(fieldAll%>%filter(Flowering=="F"), aes(x=NIR, y=Blue, color=Flowers)) + 
  geom_point()

ggplot(fieldAll%>%filter(Flowering=="F"), aes(x=Flowers, y=NDVI, color=Date)) + 
  geom_point()

ggplot(fieldAll, aes(x=Green, y=Blue, color=Flowering)) + 
  geom_point()

ggplot(fieldAll, aes(x=NIR, y=Red.Edge, color=Flowering)) + 
  geom_point()

ggplot(fieldAll, aes(x=Red, y=Red.Edge, color=Flowering)) + 
  geom_point()

ggplot(fieldAll, aes(x=Green, y=NIR, color=Flowering)) + 
  geom_point()

ggplot(fieldAll, aes(x=Red.Edge, y=Blue, color=Flowering)) + 
  geom_point()

ggplot(fieldAll, aes(x=NIR, y=Blue, color=Flowering)) + 
  geom_point()


deadFlowers <- fieldAll %>% 
  filter(DD>0)

ggplot(deadFlowers, aes(x=DD, y=NDVI)) + 
  geom_point()

####summary stats NDVI vs dead flowers ----
#summary stats ----
DeadSummary <- deadFlowers %>%
  group_by(Year) %>%
  summarise(mean = mean(DD, na.rm=TRUE),
            sd = sd(DD, na.rm=TRUE), 
            n = n())

#####attempt at correlation co
ggplot(fieldAll, aes(x=Individuals, y=NDVI, color=Flowering)) +
  geom_point()

ggplot(fieldAll, aes(x=Cover, y=NDVI)) + 
  geom_point()


ggplot(fieldAll, aes(x=Flowers, y=NDVI, color=Flowering)) + 
  geom_point()

ggplot(fieldAll, aes(x=DeadFlowers, y=(NIR-Red)/Green, color=Flowering)) + 
  geom_point()

ggplot(fieldAll, aes(x=Cover, y = NDVI, color=Flowering)) + 
  geom_point()

####boxplots of flowering and NDVI over both years ----
ggplot(data = field2022, aes(x=Flowering2022, y=NDVI)) + 
  geom_boxplot() +
  labs( x= "Flowering2022", y="NDVI")

ggplot(data = field2023, aes(x=Flowering2023, y=NDVI)) + 
  geom_boxplot() +
  labs( x= "Flowering", y="NDVI")

