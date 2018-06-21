setwd("P:\\05_AnalysisProjects_Working\\SIMPA\\WellboreClusteringMethods\\") #double back slash at end so can put abbreviated folder paths throughout scripts


#####

#install packages, if needed
#call libraries

#install.packages("tripack",dependencies = TRUE)
#install.packages("rgdal", dependencies=TRUE)
#install.packages("dplyr",dependencies = TRUE)
#install.packages("stringr")


library(tripack)
library(rgdal)
library(dplyr)
library(stringr)
library(rgeos)

#####

#enter filepath to input data - use abbreviated folder path within working directory (back slashes included above)

input.filepath<-"inputs\\okwells_AB_AUp\\Oklahoma_allwells_IHS081315.csv"
#output.filepath<-"P:\\05_AnalysisProjects_Working\\SIMPA\\WellboreClusteringMethods\\outputs\\gt_3d_nn"


options("scipen"=1000, "digits"=10) ##'scipen': integer. A penalty to be applied when deciding to print numeric values in fixed or exponential notation. 
#Positive values bias towards fixed and negative towards scientific notation: fixed notation will be preferred unless it is more than 'scipen' digits wider.


#read in CSV data - currently set up to take in IHS data
df<-read.csv(input.filepath)

df$API_Number_str<- as.character(df$API_Number)

df$API_10<-substr(df$API_Number_str,1,10)

df<-df[unique(df$API_10),]
#df<-df[!duplicated(df$API_10),]

df<-df[!is.na(df$Depth_Tota),]


#checking duplicates for new and old IHS data

#oktableold<-as.data.frame(read.csv("R:\\D_Users\\68_Jones\\Projects\\SIMPA\\SIMPA_analysis\\OKwell_csvdata\\oktable_old.csv"))
# df$APIcopy<-as.character(df$API_Number)
# #alloldduplicates<-df[duplicated(df$API_Number),] #doesn't return any duplicates becuase queried on 14-dig API
# df$api10 <-as.character(str_sub(df$APIcopy,1,10))
# #tendig_APIduplicates<-df[duplicated(df$api10),] #know which 10-dig API's are duplicated
# tendig_API_df<-df[!duplicated(df$api10),]
# #tendig_API_duplicate_test<-df[!duplicated(df$api10),]

#df<-df[c(1:10000),]
#split on the period before the filetype, take the first chunk
split.filepath<-strsplit(input.filepath,"[.]")[[1]][1]
#get the last two characters before the period to make the region initials
input.initials<-str_sub(split.filepath,-2,-1)




#creates individual dataframes with each of these columns

lat<-data.frame(df[ ,which(colnames(df)=="Surface_La")])    #change these depending on csv headers
long<-data.frame(df[ ,which(colnames(df)=="Surface_Lo")])   #change these depending on csv headers
depth<-data.frame(df[ ,which(colnames(df)=="Depth_Tota")])   #change these depending on csv headers
spud_date<-data.frame(df[ ,which(colnames(df)=="Date_Spud")]) 
form_produc<-data.frame(df[ ,which(colnames(df)=="Formation_")]) 
api10<-data.frame(df[ ,which(colnames(df)=="API_10")]) 

#puts all of the single column dataframes into one and names the columns
df.lat.long.depth<-as.data.frame(cbind(long,lat,depth,spud_date,api10))
colnames(df.lat.long.depth)<-c("x","y","depth","spud_date","api10")
df.lat.long.depth$spud_year<-as.numeric(format(as.Date(df.lat.long.depth$spud_date, format="%m/%d/%Y"),"%Y")) #get just the year for spuds
df.lat.long.depth$form_produc<-df$Formation_Producing_Name
df.lat.long.depth$spud_date<-NULL #get rid of column year was taken from

#colnames(df.lat.long.depth)<-c("x","y","depth","api10","spud_year","form_produc")
##### NEED ##### 
#to deal more extensively with duplicates, but this will come from searching wellbores with matching 10-dig API's

#looks at the x,y columns in the df.lat.long.depth dataframe and remove any repeated x,y coords
#need to better understand how this function rounds the decimal degrees etc. 
df.1.uni.depth<-df.lat.long.depth[!duplicated(df.lat.long.depth[,1:2]),] #in theory, shouldn't have to search on this after clearing out the duplicate API's

#writes depth to be it's own vector after depths assocaited with duplicate (x.y) coords. are removed
depth.vec<-as.vector(df.lat.long.depth[,3])
spud.vec<-as.vector(df.lat.long.depth[,5])
api10.vec<-as.vector(df.lat.long.depth[,4])
form.vec<-as.vector(df.lat.long.depth[,6])

#creates a 2D matrix to store x,y coords
matrix.xy<-cbind(df.lat.long.depth[,1],df.lat.long.depth[,2])

#uses rgdal to project the data from lat,long to UTM
#need to understand if it loses accuracy during this process
xy.proj<-rgdal::project(matrix.xy, "+proj=utm +zone=15 ellps=WGS84") 

#write out independent x,y vectors for these coordinates
x.proj<-xy.proj[,1]
y.proj<-xy.proj[,2]

#with projected x&y vectors, make triangulation! 
tri.all.points<-tri.mesh(x.proj,y.proj,duplicate="remove")
#get some info about triangles
summary(tri.all.points)

#plots the points and connecting lines
plot.tri(tri.all.points)

#creates dataframe to append all of the i values, the x coords, the y coords, and the depth for each i value
tri.withdepth<-as.data.frame(cbind((1:length(tri.all.points$x)),as.vector(tri.all.points$x),as.vector(tri.all.points$y),depth.vec,spud.vec,api10.vec,form.vec))
colnames(tri.withdepth)<-c("i","x","y","depth","spud","api10","form_produc")
tri.withdepth$i <- as.numeric(as.character(tri.withdepth$i))
tri.withdepth$x <- as.numeric(as.character(tri.withdepth$x))
tri.withdepth$y <- as.numeric(as.character(tri.withdepth$y))


#not clear why this is needed, but defined as list length minus 1 --> list of what? more than just vertices?
k <- seq_len( tri.all.points$tlnew - 1 )

i <- tri.all.points$tlist[k] #get the beginning points of edges  

j <- tri.all.points$tlist[tri.all.points$tlptr[k]] #get the end points of corresponding edges

#filter to get rid of any weird i points?
keep <- i > 0

# if an i value is greater than 0, index all of the i points to only those elements that should be kept, take abs value
i <- abs( i[ keep ] )
#index j to i elements that were kept
j <- abs( j[ keep ] )

#new plot to show points and segments with more control over plotting
plot.new()
plot( tri.withdepth$x, tri.withdepth$y,pch=19,cex=.5,col="black" )
#text(tri.withdepth$x, tri.withdepth$y,labels=tri.withdepth$i,pos = 4)

#gets the ith and jth elements from the tri.all.points$x and tri.all.points$y columns of the dataframe  
##NOT SURE WHAT THIS LINE MEANS #there are as many rows as there are columns
#the beginning point of the sement is plotted indexed x,y[i] and the end point is plotted indexed to x,y[j]
segments( tri.all.points$x[i], tri.all.points$y[i], tri.all.points$x[j], tri.all.points$y[j], col="grey",lwd=1 )

##Need to be redefined for each shapefile before calling export.spatial.lines function##
# begin.coords<-data.frame(x=c(tri.all.points$x[i]),y=c(tri.all.points$y[i])) #define x,y for segment begin
# end.coords<-data.frame(x=c(tri.all.points$x[j]),y=c(tri.all.points$y[j]))   #define x,y for segment end
# layer.name=paste0(input.initials,"_all_segments")  #give a layer name to differentiate all/global/local/att. down selects
# 
# begin.export1<-Sys.time()
# source("scripts\\Export_Polylines.R")
# export.spatial.lines(begin.coords,end.coords,layer.name)
# end.export1<-Sys.time()
# 
# export1.time<-end.export1-begin.export1
# print(export1.time)


#calculates distances of all segments in the total triangulation
#loops through all values in the i,j vectors
distances <- sqrt( ( tri.all.points$x[i] - tri.all.points$x[j] ) ^ 2 + ( tri.all.points$y[i] - tri.all.points$y[j] ) ^ 2 )

#creates a dataframe recording the beginning point, end point, and distance of a segment
df2<-as.data.frame(cbind(i,j,distances),stringsAsFactors=FALSE)
df2$api10<- tri.withdepth$api10[match(df2$i, tri.withdepth$i)]
df2$depth.i<- tri.withdepth$depth[match(df2$i, tri.withdepth$i)]
df2$depth.j<- tri.withdepth$depth[match(df2$j, tri.withdepth$i)]
df2$xi<-tri.withdepth$x[match(df2$i, tri.withdepth$i)]
df2$yi<-tri.withdepth$y[match(df2$i, tri.withdepth$i)]
df2$xj<-tri.withdepth$x[match(df2$j, tri.withdepth$i)]
df2$yj<-tri.withdepth$y[match(df2$j, tri.withdepth$i)]
#df$depth.i<-as.numeric(df$depth.i)
#colnames(df)<-c("i","j","distances","depth.i","depth.j","x","y")
df2$depth.i<-(as.numeric(as.character(df2$depth.i))*0.3048)
df2$depth.j<-(as.numeric(as.character(df2$depth.j))*0.3048)
df2$depth.diff<-abs(df2$depth.i- df2$depth.j)
#df[df$depth.diff == 0, 8]<-1
#df$overlap.frac<-(as.numeric(df$depth.j)/df$depth.i)
#df[df$overlap.frac >= 1, 9]<-1
#df$dist.sq<-df$distances*df$distances
#df<-df[c("i","j","x","y","distances","depth.i","depth.j","depth.diff","overlap.frac")]#"dist.sq")]
#df$index<- df$distances/(df$depth.diff*df$overlap.frac)
#df$index<-round(df$index,digits=5)
df2$D3.diff<-NA
df2$D3.diff<-sqrt((df2$distances)^2 + (df2$depth.diff)^2)
#df$depth.cat<-cut(df$depth.i, seq(0,30000,1000), right=FALSE,labels=c(1:30))

df.test<-df2[unique(df2$i),]

plot.new()
plot(df.test$xi,df.test$yi,pch=.5,col="red")

df3<-df2[!is.na(df2$depth.diff),]
df4<-df3[df3$depth.i!=0,]
df5<-df4[df4$depth.j!=0,]

# df$slope<-as.numeric(df$depth.diff/df$distances)
# df.3D.nn$index<-df.3D.nn$slope*df.3D.nn$distances

df.notri<-as.data.frame(cbind(api10,x.proj,y.proj,depth.vec))

plot.new()
plot( df$x, df$y,pch=19,cex=1,col="black")
#text(df$x,df$y,labels=df$depth.cat,pos=2)
#text(df$x,df$y,labels=df$index,pos=4)



#text(tri.withdepth$x, tri.withdepth$y,labels=tri.withdepth$i,pos = 4)

#gets the ith and jth elements from the tri.all.points$x and tri.all.points$y columns of the dataframe  
##NOT SURE WHAT THIS LINE MEANS #there are as many rows as there are columns
#the beginning point of the sement is plotted indexed x,y[i] and the end point is plotted indexed to x,y[j]
segments( tri.all.points$x[i], tri.all.points$y[i], tri.all.points$x[j], tri.all.points$y[j], col="grey",lwd=1 )


#unique.global.i<-as.vector(good.global.edges.tri[!duplicated(good.global.edges.tri$i),1])

df.i<-df5[unique(df5$i),]


plot.new()
plot( df2$xi, df2$yi,pch=19,cex=1,col="black")

df.notri<-as.data.frame(cbind(api10,x.proj,y.proj,depth.vec))
colnames(df.notri)<-c("api10","x","y","depth")

projection.info<-"+proj=utm +zone=15+datum=WGS84"

spdf <- SpatialPointsDataFrame(df.notri[,c("x", "y")], df.notri[,1:4],proj4string = CRS(projection.info))  

writeOGR(spdf,dsn="P:\\05_AnalysisProjects_Working\\SIMPA\\WellboreClusteringMethods\\TIN_shapefiles\\well_shapefiles\\3D",
         layer="GT_noNADUP_m4",driver="ESRI Shapefile",morphToESRI = T)









#duplicates already removed 
df.3D.nn<-data.frame()


for (i in df.i) {
  neebs<-df[df$i==i,]
  nn<-neebs[neebs$D3.diff==min(neebs$D3.diff,na.rm=TRUE),]
  df.3D.nn<-rbind(df.3D.nn,nn)
}


df.3D.nn$slope<-as.numeric(df.3D.nn$depth.diff/df.3D.nn$distances)
df.3D.nn$index<-df.3D.nn$slope*df.3D.nn$distances


#source("\\Export_Points.R")

projection.info<-"+proj=utm +zone=15+datum=WGS84"

spdf <- SpatialPointsDataFrame(df.3D.nn[,c("xi", "yi")], df.3D.nn[,1:13],proj4string = CRS(projection.info))  

writeOGR(spdf,dsn=output.filepath,
         layer="gt_3d_nn",driver="ESRI Shapefile",morphToESRI = T)

























plot.new()
rbPal <- colorRampPalette(c('red','blue'),method="linear")
df.3D.nn$Col <- rbPal(50)[as.numeric(cut(df.3D.nn$index,breaks = 50))]
plot(df.3D.nn$x,df.3D.nn$y,cex=.25,pch=19,col=df.3D.nn$Col)

df.3D.nn$Col <- rbPal(5)[as.numeric(cut(df.3D.nn$D3.diff,breaks = 5))]

projection.info<-"+proj=utm +zone=15+datum=WGS84"

spdf <- SpatialPointsDataFrame(df.3D.nn[,c("x", "y")], df.3D.nn[,1:9],proj4string = CRS(projection.info))  

rasterize(spdf$x, spdf$y, df.3D.nn$D3.diff, fun='min', background=NA,
          mask=FALSE, update=FALSE, updateValue='all', filename="P:\\05_AnalysisProjects_Working\\SIMPA\\WellboreClusteringMethods\\raster", na.rm=TRUE)

i<-df.3D.nn$i
sub.i<-i[1:50]
j<-df.3D.nn$j
sub.j<-j[1:50]


s <- seq(length(x)-1)# one shorter than data
arrows(x[s], y[s], x[s+1], y[s+1], col= 1:3)


plot.new()
segments(df.3D.nn$x[i], df.3D.nn$y[i], df.3D.nn$x[j], df.3D.nn$y[j], col= df.3D.nn$Col ,lwd=2 )
plot(df.3D.nn$x[sub.i],df.3D.nn$y[sub.i],cex=.5,pch=19,col=df.3D.nn$Col)

hist(df.3D.nn$D3.diff,breaks=50)
