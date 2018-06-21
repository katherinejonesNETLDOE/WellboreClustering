###3D triangulation
install.packages("rgeos",dependecies=TRUE)
install.packages("rgl")
install.packages("deldir")
install.packages("dplyr")
install.packages("plot3D")
install.packages("RColorBrewer")
install.packages("sp")

library(sp)
library(rgeos)
library(rgl)
library(deldir)
library(dplyr)
library(plot3D)
library(RColorBrewer)

setwd("P:\\05_AnalysisProjects_Working\\SIMPA\\WellboreClusteringMethods\\") #double back slash at end so can put abbreviated folder paths throughout scripts

#####

#enter filepath to input data - use abbreviated folder path within working directory (back slashes included above)

input.filepath<-"inputs\\arbuck_anad.csv"
#output.filepath<-"P:\\05_AnalysisProjects_Working\\SIMPA\\WellboreClusteringMethods\\outputs\\gt_3d_nn"

options("scipen"=1000, "digits"=10) ##'scipen': integer. A penalty to be applied when deciding to print numeric values in fixed or exponential notation. 
#Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than 'scipen' digits wider.


#read in CSV data - currently set up to take in IHS data
df<-read.csv(input.filepath)

df$API_Number_str<- as.character(df$API_Number)

df$API_10<-substr(df$API_Number_str,1,10)

#df<-df[unique(df$API_10),]
df<-df[!duplicated(df$API_10),]

df<-df[!is.na(df$Depth_Tota),]
df<-df[df$Depth_Tota!=0,]

write.csv(df,"P:\\05_AnalysisProjects_Working\\SIMPA\\WellboreClusteringMethods\\inputs\\arbuck_anad_clean.csv")



#matrix.xy<-cbind(df$Surface_Longitude,df$Surface_Latitude)
matrix.xy<-cbind(df$Surface_Lo,df$Surface_La)


#uses rgdal to project the data from lat,long to UTM
#need to understand if it loses accuracy during this process

xy.proj<-rgdal::project(matrix.xy, "+proj=utm +zone=14 ellps=WGS84") 

x<-xy.proj[,1]
y<-xy.proj[,2]

duptest<-duplicatedxy(x,y)

dups<-which(duptest==TRUE)

x<-x[-dups]
y<-y[-dups]
z<-as.vector(df$Depth_Tota)
z<-as.vector(z/3.2808)
z<- (-(z[-dups]))

xyz<-as.data.frame(cbind(x,y,z))
colnames(xyz)<-c("x","y","z")


 #2D

# tri.all.points<-tri.mesh(xy.proj$x,xy.proj$y,duplicate="remove")


col <- cm.colors(20)[1 + round(19*(z - min(z))/diff(range(z)))]

dxyz <- deldir::deldir(x, y, z = z, suppressMsge = TRUE) #creates the triangulation
#persp3d(dxyz, col = col) #plots the triangulation

df.tri<-dxyz$dirsgs #make new dataframe from dirsgs dataframe from deldir()
df.tri$z1<-NA
df.tri$z2<-NA
df.tri$xy.dist<-NA
df.tri$euc.dist<-NA #creates columns for desired info
df.tri$z1<-round(z[df.tri$ind1],digits=1) #assigns the z value by the ind1 index (i.e. the index of the first point in the line, using it's position grab the z value from the depth vector)
df.tri$z2<-round(z[df.tri$ind2],digits=1) #same thing but for ind2 (i.e. second point in the line)
df.tri$xy.dist<- sqrt(((df.tri$x1-df.tri$x2)^2 + (df.tri$y1-df.tri$y2)^2)) #using begin and end xy coords, calculate distance 
df.tri$depth.diff<-round(abs(df.tri$z1-df.tri$z2),digits=1)+1 #absolute value of depth difference between begining and end line points
#df.tri$depth.diff[df.tri$depth.diff==0]<-1
df.tri$euc.dist<- sqrt(df.tri$xy.dist^2 + (df.tri$z1-df.tri$z2)^2) #pythagorean theory to get euclidean distance (i.e. hypotenuse of triangle)
df.tri$norm.xy.dist<-(df.tri$xy.dist-min(df.tri$xy.dist))/(max(df.tri$xy.dist)- min(df.tri$xy.dist)) #normalize the xy distance (for weighting purposes)
df.tri$log.norm.xy.dist<-log(df.tri$norm.xy.dist) #take the log since the distribution is heavily left-skewed
log.min<-min( df.tri$log.norm.xy.dist[df.tri$log.norm.xy.dist!=min(df.tri$log.norm.xy.dist)] ) #get the msallest value (aka largest negative value)
df.tri$pos.log.norm.xy.dist<-df.tri$log.norm.xy.dist+abs(log.min) #add the absolute value of the smallest value so all log values are positive, just shift the scale so we don't bounce form positive to negative when sq. and cu.
df.tri$sq.log.norm.xy.dist<-df.tri$pos.log.norm.xy.dist^2 #squared index 
df.tri$cu.log.norm.xy.dist<-df.tri$pos.log.norm.xy.dist^3 #cubed index
df.tri$slope.percent<-df.tri$depth.diff/df.tri$xy.dist #rise over run (use regular xy.dist)
df.tri$slope.degrees <- (atan(df.tri$slope.percent))*57.295779513 #convert from inverse tangent of percentage yields radians, then convert to degrees
df.tri$line.num<-0:(nrow(df.tri)-1) #create a line.num attribute that matches the FID in Arc 


df.tri$index_lin<-df.tri$slope.degrees * df.tri$pos.log.norm.xy.dist *100 # linear weight on log xy distance 
df.tri$index_sq<-df.tri$slope.degrees * df.tri$sq.log.norm.xy.dist *100  #  squared weight on log xy distance 
df.tri$index_cu<-df.tri$slope.degrees * df.tri$cu.log.norm.xy.dist *100 #  cubed weight on log xy distance 

#below line has to be changed depending on what index you want to use 
# orders the index and assigns a color order based on the values 
#this col.order is then fed to the color ramp for drawing
df.tri$col.order = findInterval(df.tri$index, sort(df.tri$index))

#write out csv of dataframe for import into arc
#write.csv(df.tri,"P:\\05_AnalysisProjects_Working\\SIMPA\\WellboreClusteringMethods\\outputs\\df_tri_lines\\df_tri.csv")



cols = brewer.pal(10,"RdYlGn")
#pal = colorRampPalette(c("red","black"))
pal = colorRampPalette(cols) 

open3d(scale=c(1,1,1))
segments3d(x=as.vector(t(df.tri[,c(1,3)])),
           y=as.vector(t(df.tri[,c(2,4)])),
           z=as.vector(t(df.tri[,c(11,12)])),col=pal(nrow(df.tri))[df.tri$col.order])




#for 2D drawing
segments(x0=df.tri[,1],y0=df.tri[,2],x1=df.tri[,3],y1=df.tri[,4],col=pal(nrow(df.tri))[df.tri$col.order]) 

projection="+proj=utm +zone=14 ellps=WGS84"

#get stuff in the right for to export to spatial lines dataframe so it can be drawn in Arc
begin.coords<-as.data.frame(df.tri[,1:2])
colnames(begin.coords)<-c("x","y")
end.coords<-as.data.frame(df.tri[,3:4])
colnames(end.coords)<-c("x","y")

lns <- vector("list", nrow(begin.coords))

#for the elements in the list representing the number of lines
for (i in seq_along(lns)) {
  #
  lns[[i]] <- Lines(list(Line(rbind(begin.coords[i, ], end.coords[i,]))), ID=as.character(i))
}

#didn't right in projection because having trouble getting rgdal to recognize utm zone 15
sp_lns<-SpatialLines(lns, proj4string = CRS(projection))



df <- data.frame(len = sapply(1:length(sp_lns), function(i) gLength(sp_lns[i, ])))
rownames(df) <- sapply(1:length(sp_lns), function(i) sp_lns@lines[[i]]@ID)

sp_lns_df <- SpatialLinesDataFrame(sp_lns, data = df)#, row.names(sp_lns)= df.tri$line.num)

rgdal::writeOGR(sp_lns_df,dsn="P:\\05_AnalysisProjects_Working\\SIMPA\\WellboreClusteringMethods\\outputs",
         layer="seg_2d_anad_ab",driver="ESRI Shapefile",morphToESRI = T)




