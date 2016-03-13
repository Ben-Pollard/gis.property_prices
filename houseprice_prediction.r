library(dplyr)
library(data.table)
library(stringr)
library(rgdal)
library(maptools)
library(gpclib)
library(gstat)
library(ggplot2)
library(lubridate)

bng = '+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs'

#converts dataframe to spatial points dataframe
makeGeo <- function(df,projectionString){
  geo <- data.frame(filter(df,Eastings>0))
  coordinates(geo) = cbind(geo$Eastings, geo$Northings)
  geo@proj4string <- CRS(projectionString)
  return(geo)
}  

#get price paid data
pp <- fread("~/Projects/data/Mapping/pp-complete.csv",header = FALSE)
setnames(pp,c('tranID','price','dateOfTransfer','postcode','propType','newBuild','tenure','PAON','SAON','street','locality','town','district','county','recordStatus'))
#drop unwanted fields and rows
pp <- pp %>% subset(county=='GREATER LONDON') %>% select(price, dateOfTransfer,postcode,propType,newBuild,tenure, recordStatus)
#parse dates
pp <- pp %>% mutate(dateOfTransfer=as.POSIXct(dateOfTransfer),month=paste(year(dateOfTransfer),str_pad(month(dateOfTransfer),2,'left','0'),sep='-'))
pp$price <- as.numeric(pp$price)

#get codepoint
setwd('/home/user/Projects/data/Mapping/codepo_gb/Data/CSV')
getwd()
cphead <- unname(unlist(read.csv("~/Projects/data/Mapping/codepo_gb/Doc/Code-Point_Open_Column_Headers.csv")))
#fileList <- list.files("~/Projects/data/Mapping/codepo_)gb/Data/CSV")
#London postcode areas
areas <- unique(str_extract(pp$postcode, '[A-Z]{,2}'))
areas <- areas[areas!=""]
#inititalise create list of dfs, call rbind on list
cp <- lapply(areas,function(x) cp <- read.csv(paste(getwd(),'/',tolower(x),'.csv',sep=""), header=FALSE))
cp <- do.call(rbind,cp)
colnames(cp) <- cphead
cp <- cp %>% select(Postcode,Eastings,Northings) %>% mutate(Eastings=Eastings/100000, Northings=Northings/100000) %>% data.table()

#join
cp$Postcode <- gsub(' ','',as.character(cp$Postcode))
pp$postcode <- gsub(' ','',as.character(pp$postcode))
setnames(cp,'Postcode','postcode')
ldngeo <- inner_join(pp,cp)


##Interpolate points - do per month
#create geocoded codepoint view of London
cp_geo <- makeGeo(cp,bng)

months <- unique(pp$month) #get months to apply over

########################################
#Save image
save.image('/home/user/Projects/data/Mapping/houseprice_prediction.r_obj')

#Load image
load('/home/user/Projects/data/Mapping/houseprice_prediction.r_obj')
#######################################

####subset, geocode, krige, replace with real where available
#Actual prices paid this month
ldngeo_subsset <- makeGeo(subset(ldngeo,month=='2012-02',propType='F'),bng)

hist(log(ldngeo_subsset$price))
vgm <- variogram(log(price)~1, ldngeo_subsset)
plot(vgm, plot.numbers = TRUE, pch = "+")
vgmfit <- fit.variogram(vgm,vgm(.4,"Pow",0.2,.5))
plot(vgm, model=vgmfit)
kriged<- krige(log(price)~1, ldngeo_subsset, ldngeo_subsset, model = vgmfit)
spplot(kriged["var1.pred"], main = "Meuse zinc ordinary kriging log predictions")


#Estimate covariates
#cp_subset <- ldngeo %>% select(postcode,dateOfTransfer,propType) %>% group_by(postcode,propType) %>% 
 # summarise(maxDate=max(dateOfTransfer), minDate=min(dateOfTransfer))

#Infill unknowns
ldn_geo_subset_interp <- krige(log(price)~Eastings+Northings,ldngeo_subsset,cp_geo) #krige using actual prices paid and all postcodes
ldn_geo_subset_interp <- krige(log(price)~Eastings+Northings+I(Eastings*Northings)+I(Eastings^2)+I(Northings^2),ldngeo_subsset,cp_geo) #krige

#validate
a=data.frame(ldngeo_subsset)
b=data.frame(ldn_geo_subset_interp)
test <- inner_join(a,b)
test <- select(test,postcode,price,var1.pred) %>% mutate(pred=exp(var1.pred)) %>% select(price,pred)
plot(x=test$price,y=test$pred,xlim=c(100000,500000),ylim=c(100000,500000))
?plot
#not working - see http://geostat-course.org/system/files/lewis_tutortPM.pdf
#https://stats.stackexchange.com/questions/8000/proper-way-of-using-recurrent-neural-network-for-time-series-analysis


#Plotting
plotPoints <- function(spatialdf,colorfield){
  df <- data.frame(spatialdf)
  ggplot(df) + geom_point(aes(x=coords.x1,y=coords.x2,color=colorfield))
}
plotPoints(ldngeo,price)
plotPoints(ldn_geo_interp,var1.pred)


#to do
#loess infill LLSOAs per month / interpolation
#% change per month
#recurrent nueral network (hierarchical, so neighbours linked to neigbours, tube stops linked to tube stops?)
#or: function of neighbour price change last month, neighbour predicted change this month
