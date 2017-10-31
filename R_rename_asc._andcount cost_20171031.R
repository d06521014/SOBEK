library(sp) 	# provides classes for spatial data in R
library(raster)	# provides classes and methods for raster datasets
library(rgdal)	# interface to the Geospatial Data Abstraction Library to read and write different spatial file formats
library(gstat) # Use gstat's idw routine
library(dplyr)
library(tmap)

#projection twd97
projtwd97 <-CRS("+proj=tmerc +ellps=GRS80 +lon_0=121 +x_0=250000 +k=0.9999 +units=m +no_defs")


data.path<-"D:/floodasc/" #read basin outline
setwd(data.path)
zw_area <- readOGR("G:/ZW/02HMS41", "basin_outline")#read ZW drainage boundary
zw_landusefarm <- readOGR("D:/floodasc","zwlandusefarm")# read ZW drainage landuse lcodec1=農田
zw_landuse5 <- readOGR("D:/floodasc","zwlanduse05")# read ZW drainage landuse lcodec1=05建物
zw_landusefish <- readOGR("D:/floodasc","zwlandusefishshp")# read ZW drainage landuse lcodec2=0102漁塭
plot(zw_landusefarm)
plot(zw_landusefish)
plot(zw_landuse5)
proj4string(zw_area) <-projtwd97

raster_file_name<-list.files(data.path,pattern = "asc")
#s<-paste0(data.path,raster_file_name[1])
#flood_area <-raster(s)
#plot(flood_area)

for (i in 1:length(raster_file_name)){
  
  s<-paste0(data.path,raster_file_name[i])
  flood_area <-raster(s)
#  plot(flood_area)
  proj4string(flood_area) <-projtwd97
  flood_area.m<- mask(flood_area, zw_area)
  writeRaster(flood_area.m, filename=paste0(data.path,"e",raster_file_name[i]), format="ascii",overwrite=TRUE)
  
  p<- rasterToPoints(flood_area.m, fun=function(x){x>0})
  pzw<-as.data.frame(p)
  pzw$volume<-pzw[,3]*1600
 
  names(pzw) <- c("x","y","z","v")
  
  pzwt<-pzw%>%
        select(z,v)%>%
        group_by(z<0.3,z>=0.3&z<0.5,z>=0.5&z<1,z>=1&z<1.5,z>=1.5&z<2,z>=2)%>%
        summarise(area_ha=n()*0.16,volume_m3=sum(v))%>%
        mutate(average_depth_m=volume_m3/area_ha/10000)%>%
        arrange(average_depth_m,desc(average_depth_m))
  
  write.csv(pzwt, paste0(data.path,sub(pattern =".asc" , replacement ="" , raster_file_name[i]),"total",".csv"))
  
  
  flood_area.m1<- mask(flood_area, zw_landusefarm)
  flood_area.m5<- mask(flood_area, zw_landuse5)
  flood_area.m6<- mask(flood_area, zw_landusefish)
  
 # plot(flood_area.m1)
 # plot(flood_area.m5)
#  plot(flood_area.m6)
  for (j in 1:3){
    if (j==1){
      p <- rasterToPoints(flood_area.m1, fun=function(x){x>0})
    } 
    if (j==2){
      p <- rasterToPoints(flood_area.m5, fun=function(x){x>0})
    } 
    if (j==3){
      p <- rasterToPoints(flood_area.m6, fun=function(x){x>0})
    }
     p.f<-as.data.frame(p)
     p.f$volume<-p.f[,3]*1600
     names(p.f) <- c("x","y","z","v")
    
     if (j==1){
       p.f$cost<-0.3*(p.f[,3]*0.16)^4 - 1.4778*(p.f[,3]*0.16)^3 + 0.3417*(p.f[,3]*0.16)^2 + 9.4611*(p.f[,3]*0.16) - 0.0083
       
       z1<-p.f%>%
         group_by(z<0.3,z>=0.3&z<0.5,z>=0.5&z<1,z>=1&z<1.5,z>=1.5&z<2,z>=2)%>%
         summarise(area_ha=n()*0.16,volume_m3=sum(v),costv=sum(cost))%>%
         mutate(average_depth_m=volume_m3/area_ha/10000)%>%
         arrange(average_depth_m,desc(average_depth_m))
       z1$landuse="farm"
      
     }
     if (j==2){
         p.f$cost<-53*(-8.6667*(p.f[,3]*0.16)^4 + 36.074*(p.f[,3]*0.16)^3 - 36.611*(p.f[,3]*0.16)^2 + 31.989*(p.f[,3]*0.16)- 0.1349)
         z2<-p.f%>%
         group_by(z<0.3,z>=0.3&z<0.5,z>=0.5&z<1,z>=1&z<1.5,z>=1.5&z<2,z>=2)%>%
           summarise(area_ha=n()*0.16,volume_m3=sum(v),costv=sum(cost))%>%
           mutate(average_depth_m=volume_m3/area_ha/10000)%>%
           arrange(average_depth_m,desc(average_depth_m))
         z2$landuse="urban"
     }
     if (j==3){
       p.f$cost<--1.0519*(p.f[,3]*0.16)^3 + 9.9016*(p.f[,3]*0.16)^2 + 5.8074*(p.f[,3]*0.16) + 0.0302
       z3<-p.f%>%
         group_by(z<0.3,z>=0.3&z<0.5,z>=0.5&z<1,z>=1&z<1.5,z>=1.5&z<2,z>=2)%>%
         summarise(area_ha=n()*0.16,volume_m3=sum(v),costv=sum(cost))%>%
         mutate(average_depth_m=volume_m3/area_ha/10000)%>%
         arrange(average_depth_m,desc(average_depth_m))
       z3$landuse="fish"
     } 
    
  }# end of j
  z4<-full_join(z1, z2)
  z5<-full_join(z4, z3)
  write.csv(z5, paste0(data.path,sub(pattern =".asc" , replacement ="" , raster_file_name[i]),"landuse",".csv"))
 # ss<- sub(pattern =".asc" , replacement ="" , raster_file_name[i])
#  write.csv(z1, paste0(data.path,sub(pattern =".asc" , replacement ="" , raster_file_name[i]),"farm",".csv"))
 # write.csv(z2, paste0(data.path,sub(pattern =".asc" , replacement ="" , raster_file_name[i]),"ubarn",".csv"))
#  write.csv(z3, paste0(data.path,sub(pattern =".asc" , replacement ="" , raster_file_name[i]),"fish",".csv"))
  #z4<-full_join(z1, z2)

}

# p.f<-p.f%>%
#  summarise(count=n())


#v<-as.data.frame(volume)
# write.csv(v,"G:/ZW/05 volume/volume.csv", sep=",")


#z1<-p.f%>%
# select(z,v)%>%
#group_by(z<0.3,z>=0.3&z<0.5,z>=0.5&z<1,z>=1&z<1.5,z>=1.5&z<2,z>=2)%>%
# summarise(count=n(),volume=sum(v))%>%
# mutate(average_depth=volume/count/1600)



