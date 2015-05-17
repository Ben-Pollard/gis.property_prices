library(dplyr)
library(data.table)
library(stringr)
library(rgdal)
library(maptools)
library(gpclib)
gpclibPermit()
#gpclibPermitStatus()=TRUE

#get price paid data
pp <- read.csv("~/Projects/data/Mapping/pp-2015.csv", col.names=c('tranID','price','dateOfTransfer','postcode','propType','newBuild','tenure','PAON','SAON','street','locality','town','district','county','recordStatus'))
#drop unwanted fields and rows
pp <- pp %>% subset(county=='GREATER LONDON') %>% select(price, dateOfTransfer,postcode,propType,newBuild,tenure, recordStatus)

#get codepoint
setwd('/home/user/Projects/data/Mapping/codepo_gb/Data/CSV')
getwd()
cphead <- unname(unlist(read.csv("~/Projects/data/Mapping/codepo_gb/Doc/Code-Point_Open_Column_Headers.csv")))
fileList <- list.files("~/Projects/data/Mapping/codepo_)gb/Data/CSV")
#London postcode areas
areas <- unique(str_extract(pp$postcode, '[A-Z]{,2}'))
areas <- areas[areas!=""]
#inititalise create list of dfs, call rbind on list
cp <- lapply(areas,function(x) cp <- read.csv(paste(getwd(),'/',tolower(x),'.csv',sep=""), header=FALSE))
cp <- do.call(rbind,cp)
colnames(cp) <- cphead
cp <- cp %>% select(Postcode,Eastings,Northings)

#join
cp$Postcode <- gsub(' ','',as.character(cp$Postcode))
pp$postcode <- gsub(' ','',as.character(pp$postcode))
colnames(cp)[colnames(cp)=='Postcode'] <- 'postcode'
ldngeo <- inner_join(pp,cp)

View(left_join(pp,cp) %>% subset(is.na(Eastings)))

#geocode
wgs84 = '+proj=longlat +datum=WGS84'
bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'
mrc = '+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs'
coordinates(ldngeo) = cbind(ldngeo$Eastings, ldngeo$Northings)
plot(ldngeo)
ldngeo@proj4string <- CRS(bng)

#export points
writeOGR(ldngeo,'/home/user/Projects/data/Mapping/myLayers','pricePoints','ESRI Shapefile')

#create boroughs shape
wards <- readOGR(dsn='/home/user/Projects/data/Mapping/boundaries/London-wards-2014/London-wards-2014_ESRI',layer='London_Ward_CityMerged')
polys <- unionSpatialPolygons(SpatialPolygons(wards@polygons),wards@data$BOROUGH)
df <- data.frame(getSpPPolygonsIDSlots(polys))
row.names(df) <- df$getSpPPolygonsIDSlots.polys.
boroughs <- SpatialPolygonsDataFrame(polys,df)
writeOGR(boroughs,'/home/user/Projects/data/Mapping/myLayers','boroughs','ESRI Shapefile')

spplot(boroughs)


#to do
#loess infill LLSOAs per month / interpolation
#% change per month
#recurrent nueral network (hierarchical, so neighbours linked to neigbours, tube stops linked to tube stops?)
#or: function of neighbour price change last month, neighbour predicted change this month
