
#capture what time the script started
beginning<-Sys.time()

##Set up##

#parent folder for everything - where project files, inputs, outputs, notes,etc. are stored
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
input.filepath<-"inputs\\okwells_GT.csv"

#read in CSV data - currently set up to take in IHS data
df<-read.csv(input.filepath)


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
lat<-data.frame(df[ ,which(colnames(df)=="Surface_Latitude")])    #change these depending on csv headers
long<-data.frame(df[ ,which(colnames(df)=="Surface_Longitude")])   #change these depending on csv headers
depth<-data.frame(df[ ,which(colnames(df)=="Depth_Total_Driller")])   #change these depending on csv headers
spud_date<-data.frame(df[ ,which(colnames(df)=="Date_Spud")]) 

#puts all of the single column dataframes into one and names the columns
df.lat.long.depth<-as.data.frame(cbind(long,lat,depth,spud_date))
colnames(df.lat.long.depth)<-c("x","y","depth","spud_date")
df.lat.long.depth$spud_year<-as.numeric(format(as.Date(df.lat.long.depth$spud_date, format="%m/%d/%Y"),"%Y")) #get just the year for spuds
df.lat.long.depth$spud_date<-NULL #get rid of column year was taken from

##### NEED ##### 
#to deal more extensively with duplicates, but this will come from searching wellbores with matching 10-dig API's

#looks at the x,y columns in the df.lat.long.depth dataframe and remove any repeated x,y coords
#need to better understand how this function rounds the decimal degrees etc. 
df.1.uni.depth<-df.lat.long.depth[!duplicated(df.lat.long.depth[,1:2]),] #in theory, shouldn't have to search on this after clearing out the duplicate API's

#writes depth to be it's own vector after depths assocaited with duplicate (x.y) coords. are removed
depth.vec<-as.vector(df.lat.long.depth[,3])
spud.vec<-as.vector(df.lat.long.depth[,4])

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
tri.withdepth<-as.data.frame(cbind(as.vector(1:length(tri.all.points$x)),as.vector(tri.all.points$x),as.vector(tri.all.points$y),depth.vec,spud.vec))
colnames(tri.withdepth)<-c("i","x","y","depth","spud")

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
point1.point2.length<-as.data.frame(cbind(i,j,distances))

### BEGIN GLOBAL DOWN SELECT ###

before.global.downselect<-Sys.time()

#gets all of the unique segments (i.e. all i values that are present)
unique.points<-point1.point2.length[!duplicated(point1.point2.length$i),]

#create an empty dataframe to receive information about points that pass the global down select
good.global.edges.tri<-data.frame()

global.mean.tri<-mean(distances)  #mean distance of all global segments
global.var.tri<-sd(distances)     #standard deviation of all global segments


#for all unique i values 
for (n in 1:nrow(unique.points)){
  print (n) #print just for kicks -- n= unique i value
  point1.subset<-point1.point2.length[point1.point2.length$i==n,] #for each i value return the j value and distance
  local.mean.tri<-mean(point1.subset$distances) #get the mean of the distances to i's neighbors
  local.var.tri<-sd(point1.subset$distances) #get the std dev. of the distances to neighbors around i
  alpha<-global.mean.tri/local.mean.tri #create alpha value for each point
  global.dist.const<-global.mean.tri+ (alpha*global.var.tri) #create global.dist.constraint value from paper
  for (row in 1:nrow(point1.subset)){ #for each row in the i value subset showing it's neighbors
    if(isTRUE(point1.subset[row,3]<global.dist.const)){ #check to see if the distance value for the segment is less than the global.dist.const
      good.global.edges.tri<-rbind(good.global.edges.tri,point1.subset[row,]) #if above return true, rbind that row with i,j, distance values to dataframe
    }
  }
}


after.global.downselect<-Sys.time()

global.downselect.time <-after.global.downselect- before.global.downselect

print(global.downselect.time) 

# this function is used when looking for pairs of values 
#indicates which rows (i,j,distance) were included/not included in good.global.edges.tri - Does this by "yes"/ "no" in $check column
point1.point2.length$check <- ifelse(is.na(match(paste0(point1.point2.length$i, point1.point2.length$j), 
                                                 paste0(good.global.edges.tri$i, good.global.edges.tri$j))),"No", "Yes")

#if the i,j combination is not found in good.global.edges = "No", if it is, "Yes" take i value and print it to new i vector
i.good<-point1.point2.length[point1.point2.length$check=="Yes",1]
#do the same with j vector
j.good<-point1.point2.length[point1.point2.length$check=="Yes",2]


#these are the lengths and their point indexes that were removed in the global down select
deleted.distances<-point1.point2.length[point1.point2.length$check=="No",]

#plots all edges that pass the global distance constraint
segments(tri.all.points$x[i.good], tri.all.points$y[i.good], tri.all.points$x[j.good], tri.all.points$y[j.good], lwd = 1 )

## END of global down select ##

##Need to be redefined for each shapefile before calling export.spatial.lines function##
# begin.coords<-data.frame(x=c(tri.all.points$x[i.good]),y=c(tri.all.points$y[i.good])) #define x,y for segment begin
# end.coords<-data.frame(x=c(tri.all.points$x[j.good]),y=c(tri.all.points$y[j.good]))   #define x,y for segment end
# layer.name=paste0(input.initials,"_global_segments")  #give a layer name to differentiate all/global/local/att. down selects
# 
# begin.export2<-Sys.time()
# source("scripts\\Export_Polylines.R")
# export.spatial.lines(begin.coords,end.coords,layer.name)
# end.export2<-Sys.time()
# 
# export2.time<-end.export2-begin.export2
# print(export2.time)

## END of global down select ##

## BEGIN local down select ##

before.local.criteria<-Sys.time()

#get the unique i values from good.global.edges.tri
unique.global.i<-as.vector(good.global.edges.tri[!duplicated(good.global.edges.tri$i),1])


## Generate 2-Order.Mean (Pi) and Mean.Var (Pi)

#empty dataframe to populate with i values, 2-Order.Mean (Pi) AND Mean.Variation(Pi)
order2mean.meanvar<-data.frame()

#used this when trying to run on subset of data to speed up testing
#test.i<-unique.global.i[11217:11532]


#for all unique i values in global.good.edges.tri
for (i.val in unique.global.i) {
  print (i.val) #print i value to keep track of time
  i.val.df<-good.global.edges.tri[good.global.edges.tri$i==i.val,] #create a subset of the dataframe for all i values 
  
  #get distance to each of the first order neighbors of i
  i.neigh.dist<-c(i.val.df$distances)
  
  #put all first order neighbors in a vector (i.e. get the point index, aka j value column)
  j.val.vec<-as.vector(i.val.df[,2])
  
  #create an empty vector to append the distances of the 2nd order neighbors (i.e. the distances between i's 1st and 2nd order neighbors)
  i.2neigh.dist<-c()
  
  #create an empty vector to return the variances for i's 1st neighbors 
  #(i.e.- look at the j values from the global down select, take the variance of all edges connected to those j values)
  i.neighbs.var<-c()
  
  #while looping through the unique i values found in the global down select to get the 2-order mean
  #go ahead and loop through the 1st order neighbors of i (i.e. j values in global down select) to get the edge distances to calculate the 2-order.mean
  for (j.val in j.val.vec) {
    
    #for each 1st order neighbor, get it's corresponding j-val neighbors (i.e. 2nd order neighbors from i.val)  
    jval.neighbs<-subset(good.global.edges.tri, i== j.val)
    
    #while looping for 2-order.mean lengths, go ahead and grab the variance of edge distances around i's first order neighbors 
    jval.var<-sd(as.vector(jval.neighbs$distances),na.rm=TRUE)
    
    ###append the variances for first order neighbors (i.e. global down select jvals)
    i.neighbs.var<-c(i.neighbs.var,jval.var)
    
    #get a vector of all the distances between 1st and 2nd order neighbors 
    i.2neigh.dist<-c(i.2neigh.dist,as.vector(jval.neighbs$distances))
    
    ###create a vector listing the neighbors for that j.val
    neighbors.2.vec<-as.vector(jval.neighbs$j)
    
    ####create an empty vector to return the variances for i's second neighbors (i.e.- variance of j.val nieghbors)
    jval.neighbors.vars<-c()
    
    ###for every neighbor of j.val (i 2nd neighbors)
    for (val in neighbors.2.vec){
      
      ###get all the edges connected to that second order neighbor
      j.val.neighbors.for.var<-subset(good.global.edges.tri, i== val)
      
      ###get variance of edges connected to that second order neighbor
      jval.neighb.var<-sd(as.vector(j.val.neighbors.for.var$distances),na.rm=TRUE)
      
      ###append the variances for the second order neighbors 
      jval.neighbors.vars<-c(jval.neighbors.vars,jval.neighb.var)
      
    }
  }
  
  #combine the first order neighbor distances and the second order neighbor distances
  first.second.neigh.lengths<-c(i.neigh.dist,i.2neigh.dist)
  
  ###combine the 1st order neighbors and 2nd order neighbors variances
  first.second.neigh.vars<-c(i.neighbs.var,jval.neighbors.vars)
  
  #take the mean of the vector of 1st and 2nd neighbor distances
  second.order.mean<-mean(first.second.neigh.lengths,na.rm=TRUE)
  
  ###take the mean of the vector of 1st and 2nd neighbor variances
  mean.var<-mean(first.second.neigh.vars,na.rm=TRUE)
  
  #match the original i.val to it's second order mean AND mean.var in data.frame format
  newrow<-as.data.frame(cbind(i.val,second.order.mean,mean.var))
  
  ###create Local.Dist.Const column
  ###this column has to be created wtih NA's because when looping through and trying to rbind() 
  ###the new rows adding to dataframe have to have the same number of columns
  newrow$Local.Dist.Const<-NA
  
  #create a final data.frame output with all i.vals within global.good.edges.tri and their 2-Order.Mean and Mean.Var
  order2mean.meanvar<-rbind(order2mean.meanvar,newrow)
  
  #populate the Local.Dist.Const column for (Pi)
  ###Local distance constraint for Pi
  order2mean.meanvar$Local.Dist.Const<-order2mean.meanvar$second.order.mean + 1*order2mean.meanvar$mean.var
}

after.local.criteria<-Sys.time()
local.criteria.time<-after.local.criteria-before.local.criteria
print(local.criteria.time)

###With new Local.Dist.Const for each (Pi), go through the global.good.edges.tri dataframe and remove any edges 
###extending from a point that are greater than the local distance constraint established for that i value

## for every i.val in unique.global.i - check to see if the (i,j) pairs in good.global.edges.tri are longer than
## the local.dist.const. for the given i

before.local.downselect<-Sys.time()

#create an empty dataframe that will receive edges that do meet the Local.Dist.Const
good.local.edges.tri<-data.frame()

#for every unique i value in the global down selected points
for (i.val in unique.global.i) {
  #create a dataframe subset containing edges linked to i
  i.val.df<-good.global.edges.tri[good.global.edges.tri$i==i.val,]
  #for each row in that dataframe subset 
  for (row in 1:nrow(i.val.df)){
    #reference it to the row in the order2mean.meanvar dataframe that contains the corresponding i value
    row.to.reference<-order2mean.meanvar[order2mean.meanvar$i.val==i.val,]
    #if the edges distance (column 3 of i.val.df) is less than the local.dist.const (column 4 of order2mean.meanvar dataframe) - i.e. returns TRUE
    if (isTRUE(i.val.df[row,3] < row.to.reference[,4] )) {
      #rbind to dataframe created to hold all edges that meet the local.dist.const.
      good.local.edges.tri<-rbind(good.local.edges.tri, i.val.df[row,])
    }
  }
}

after.local.downselect<-Sys.time()
local.downselect.time<-after.local.downselect-before.local.downselect
print(local.downselect.time)


#now look for (i,j) pairs that exist in good.local.edges.tri but NOT in good.local.edges.tri (i.e. edges that need to be deleted)
good.global.edges.tri$check <- ifelse(is.na(match(paste0(good.global.edges.tri$i, good.global.edges.tri$j), 
                                                  paste0(good.local.edges.tri$i, good.local.edges.tri$j))),"No", "Yes")

#for rows where yes is printed (i.e. the pair passes global downselect)
i.good.local<-good.global.edges.tri[good.global.edges.tri$check=="Yes",1] #return the i value of good pairs
j.good.local<-good.global.edges.tri[good.global.edges.tri$check=="Yes",2] #return the j value of good pairs

#the above vectors should be the same length, and the corresponding elements represent (i,j) pairs

## plotting of final edges that pass the local_dist_const.
segments(tri.all.points$x[i.good.local], tri.all.points$y[i.good.local], tri.all.points$x[j.good.local], tri.all.points$y[j.good.local], lwd = 1,col="blue" )



#the T1 equivalent of local downselect 

## BEGIN T1/attribute-based down select ##

before.local.criteria<-Sys.time()

#get the unique i values from good.local.edges.tri
unique.i.good.local<-as.vector(good.local.edges.tri[!duplicated(good.local.edges.tri$i),1])


## Generate 2-Order.Mean (Pi) and Mean.Var (Pi) - to get a value for T1 instead of a local distance criteria

#empty dataframe to populate with i values, 2-Order.Mean (Pi) AND Mean.Variation(Pi)
T1.order2mean.meanvar<-data.frame()

#used this when trying to run on subset of data to speed up testing
#test.i<-unique.global.i[11217:11532]
good.local.edges.tri$spud<- tri.withdepth$spud[match(good.local.edges.tri$i, tri.withdepth$i)]
good.local.edges.tri$depth.i<- tri.withdepth$depth[match(good.local.edges.tri$i, tri.withdepth$i)]
good.local.edges.tri$depth.j<- tri.withdepth$depth[match(good.local.edges.tri$j, tri.withdepth$i)]
good.local.edges.tri$depth.diff<-abs(good.local.edges.tri$depth.i-good.local.edges.tri$depth.j)

#for all unique i values in global.local.edges.tri
for (i.val in unique.i.good.local) {
  print (i.val) #print i value to keep track of time
  i.val.df<-good.local.edges.tri[good.local.edges.tri$i==i.val,] #create a subset of the dataframe for all i values 
  
  #get attribute distance to each of the first order neighbors of i
  i.neigh.dist<-c(i.val.df$depth.diff)
  
  #put all first order neighbors in a vector (i.e. get the point index, aka j value column)
  j.val.vec<-as.vector(i.val.df[,2])
  
  #create an empty vector to append the attribute differences of the 2nd order neighbors (i.e. the distances between i's 1st and 2nd order neighbors)
  i.2neigh.dist<-c()
  
  #create an empty vector to return the variances for i's 1st neighbors and their attribute differences
  #(i.e.- look at the j values from the local down select, take the variance of all edges connected to those j values)
  i.neighbs.var<-c()
  
  #while looping through the unique i values found in the local down select to get the 2-order mean
  #go ahead and loop through the 1st order neighbors of i (i.e. j values in local down select) to get the attribute differences to calculate the 2-order.mean
  for (j.val in j.val.vec) {
    
    #for each 1st order neighbor, get it's corresponding j-val neighbors (i.e. 2nd order neighbors from i.val)  
    jval.neighbs<-subset(good.local.edges.tri, i== j.val)
    
    #while looping for 2-order.mean att.differences, go ahead and grab the variance of att.differences around i's first order neighbors 
    jval.var<-sd(as.vector(jval.neighbs$depth.diff),na.rm=TRUE)
    
    ###append the variances for first order neighbors (i.e. local down select jvals)
    i.neighbs.var<-c(i.neighbs.var,jval.var)
    
    #get a vector of all the att.differences between 1st and 2nd order neighbors 
    i.2neigh.dist<-c(i.2neigh.dist,as.vector(jval.neighbs$depth.diff))
    
    ###create a vector listing the neighbors for that j.val
    neighbors.2.vec<-as.vector(jval.neighbs$j)
    
    ####create an empty vector to return the variances for i's second neighbors (i.e.- variance of j.val nieghbors)
    jval.neighbors.vars<-c()
    
    ###for every neighbor of j.val (i 2nd neighbors)
    for (val in neighbors.2.vec){
      
      ###get all the edges connected to that second order neighbor
      j.val.neighbors.for.var<-subset(good.global.edges.tri, i== val)
      
      ###get the att.difference variance of edges connected to that second order neighbor
      jval.neighb.var<-sd(as.vector(j.val.neighbors.for.var$depth.diff),na.rm=TRUE)
      
      ###append the variances for the second order neighbors 
      jval.neighbors.vars<-c(jval.neighbors.vars,jval.neighb.var)
      
    }
  }
  
  #combine the first order neighbor distances and the second order neighbor att.differences
  first.second.neigh.lengths<-c(i.neigh.dist,i.2neigh.dist)
  
  ###combine the 1st order neighbors and 2nd order neighbors att.diff. variances
  first.second.neigh.vars<-c(i.neighbs.var,jval.neighbors.vars)
  
  #take the mean of the vector of 1st and 2nd neighbor att.differences
  second.order.mean<-mean(first.second.neigh.lengths,na.rm=TRUE)
  
  ###take the mean of the vector of 1st and 2nd neighbor att.diff. variances
  mean.var<-mean(first.second.neigh.vars,na.rm=TRUE)
  
  #match the original i.val to it's second order mean AND mean.var in data.frame format
  newrow<-as.data.frame(cbind(i.val,second.order.mean,mean.var))
  
  ###create T1.Dist.Const column
  ###this column has to be created wtih NA's because when looping through and trying to rbind() 
  ###the new rows adding to dataframe have to have the same number of columns
  newrow$T1.diff.const<-NA
  
  #create a final data.frame output with all i.vals within global.good.edges.tri and their 2-Order.Mean and Mean.Var
  T1.order2mean.meanvar<-rbind(T1.order2mean.meanvar,newrow)
  
  #populate the Local.Dist.Const column for (Pi)
  ###Local distance constraint for Pi
  T1.order2mean.meanvar$T1.diff.const<-T1.order2mean.meanvar$second.order.mean + 1*T1.order2mean.meanvar$mean.var
}

after.local.criteria<-Sys.time()
local.criteria.time<-after.local.criteria-before.local.criteria
print(local.criteria.time)

###With new Local.Dist.Const for each (Pi), go through the global.good.edges.tri dataframe and remove any edges 
###extending from a point that are greater than the local distance constraint established for that i value

## for every i.val in unique.local.i - check to see if the (i,j) pairs in good.local.edges.tri are more different
## than the T1.diff.const. for the given i

before.T1.downselect<-Sys.time()

#create an empty dataframe that will receive edges that do meet the T1.diff.const
good.local.good.depth<-data.frame()

#for every unique i value in the local down selected points -- same unique vector as used at the beginning of T1 process
for (i.val in unique.i.good.local) {
  #create a dataframe subset containing edges linked to i
  i.val.df<-good.local.edges.tri[good.local.edges.tri$i==i.val,]
  #for each row in that dataframe subset 
  for (row in 1:nrow(i.val.df)){
    #reference it to the row in the order2mean.meanvar dataframe that contains the corresponding i value
    row.to.reference<-T1.order2mean.meanvar[T1.order2mean.meanvar$i.val==i.val,]
    #if the edges distance (column 3 of i.val.df) is less than the local.dist.const (column 4 of order2mean.meanvar dataframe) - i.e. returns TRUE
    if (isTRUE(i.val.df[row,7] < row.to.reference[,4] )) {
      #rbind to dataframe created to hold all edges that meet the local.dist.const.
      good.local.good.depth<-rbind(good.local.good.depth, i.val.df[row,])
    }
  }
}

after.local.downselect<-Sys.time()
local.downselect.time<-after.local.downselect-before.local.downselect
print(local.downselect.time)


#now look for (i,j) pairs that exist in good.local.edges.tri but NOT in good.local.edges.tri (i.e. edges that need to be deleted)
good.local.edges.tri$check <- ifelse(is.na(match(paste0(good.local.edges.tri$i, good.local.edges.tri$j), 
                                                 paste0(good.local.good.depth$i, good.local.good.depth$j))),"No", "Yes")

#for rows where yes is printed (i.e. the pair passes global downselect)
i.good.depth<-good.local.edges.tri[good.local.edges.tri$check=="Yes",1] #return the i value of good pairs
j.good.depth<-good.local.edges.tri[good.local.edges.tri$check=="Yes",2] #return the j value of good pairs

#the above vectors should be the same length, and the corresponding elements represent (i,j) pairs

## plotting of final edges that pass the T1 constraint
segments(tri.all.points$x[i.good.depth], tri.all.points$y[i.good.depth], tri.all.points$x[j.good.depth], tri.all.points$y[j.good.depth], lwd = 1,col="orange" )

###END OF T1 DOWN SELECT###

# begin.coords<-data.frame(x=c(tri.all.points$x[i.good.local]),y=c(tri.all.points$y[i.good.local])) #define x,y for segment begin
# end.coords<-data.frame(x=c(tri.all.points$x[j.good.local]),y=c(tri.all.points$y[j.good.local]))   #define x,y for segment end
# layer.name=paste0(input.initials,"_local_segments")  #give a layer name to differentiate all/global/local/att. down selects

# begin.export3<-Sys.time()
# source("scripts\\Export_Polylines.R")
# export.spatial.lines(begin.coords,end.coords,layer.name)
# end.export3<-Sys.time()
# 
# export3.time<-end.export3-begin.export3
# print(export3.time)

### used to write out the time it takes to perform each of the different operations in the method

# filename=paste0(input.initials,'times.txt') #creates a filename for printing out run times
# write(c("Global Downselect",global.downselect.time,"Local Criteria",local.criteria.time,"Local Downselect",local.downselect.time,"T1 Criteria",t1.criteria.time,"T1 downselect",t1.downselect.time,#"Total Export Time",total.export.time,
#         "Total time",total.time), file=filename,append=FALSE) #prints the different times on lines beneath their process name


#good.local.good.depth<-good.local.depth


### Get the Directly Spatially Reachable and the Spatially Reachable points for each Pi ###
#for each unique i in unique i vector created from good.local.good.depth
unique.i.good.depth<-unique(i.good.depth)

#set an empty vector to receive all of the average attribute differences for each unique i
i.SDR<-c()
num.orig.neighbors<-c()
differences<-c()
avg.att.diff<-c()

all.points.pairs<-as.data.frame(cbind(i,j)) #cbind original (i,j) point pairs to be used for Neighborhood(Pi)*used in density indicator calculation, purity of neighborhood

#for each i, get the average attribute difference between i and its individual neighbors
for (value in unique.i.good.depth){
  i.pnt.neighbors<-good.local.good.depth[good.local.good.depth$i==value,] #get all of the edges from that i value
  j.val.vec<-as.vector(i.pnt.neighbors[,2]) #use to find spatially reachable/2nd order neighbs of i
  sdr<-length(j.val.vec)#how many neighbors are DSR from i? 
  i.SDR<-c(i.SDR,sdr) #vector containing the number of points that are directly spatially reachable for each unique i value
  i.orig.neighborhood<-all.points.pairs[all.points.pairs$i==value,] #from the initial list of point pairs, get the number of times i occurs
  tally.orig.neighbs<-nrow(i.orig.neighborhood) #get the number of rows in that dataframe (i.e. how many times i occurrs)
  num.orig.neighbors<-c(num.orig.neighbors,tally.orig.neighbs) #append the neighborhood tally for that i value to the number of original neighbors vector
  differences<-c() #create an empty differences vector for each i value so differences can be tabulated on a per i value basis
  
  for (row in 1:nrow(i.pnt.neighbors)){ #for each row in the dataframe with i and it's neighbors
    att.diff<-abs(i.pnt.neighbors[row,5]-i.pnt.neighbors[row,6]) #get the absolute value of the att. difference between i and j attributes
    differences<-c(differences,att.diff) #append attribute differences to the running list of differences for that i value
  }
  
  avg.att.diff<-c(avg.att.diff,mean(differences)) #create a master list of the average i value attribute differences
}  


#create dataframe with the information that is necessary for computing spatial reachability while clustering 
#unique.i.good.depth,current number of spatially reachable points, number of original neighbors, i.avg.att.diff
spatial.reachability<-as.data.frame(cbind(unique.i.good.depth,i.SDR,num.orig.neighbors,avg.att.diff))  
colnames(spatial.reachability)<-c("i","sdr","num.orig.neighbs","avg.att.diff") #set easy colnames

#create the DI indicator 
spatial.reachability$DI<- spatial.reachability$sdr + (spatial.reachability$sdr/spatial.reachability$num.orig.neighbs) #create the DI indicator 
spatial.reachability$clust<-NA  #add a column to receive cluster number



clustering.info<-spatial.reachability #make copy for ease of rerunning
cluster.num<-1 #set the start of counter for cluster.num


#good.local.good.depth.reset<-good.local.good.depth
#append DI information to good.local.good.depth out here, so don't have to sort using information from multiple dataframes
good.local.good.depth$DI.i<-clustering.info[match(good.local.good.depth$i, clustering.info$i),"DI"] #all i values should have a match in clustering.info 
good.local.good.depth$DI.j<-clustering.info[match(good.local.good.depth$j, clustering.info$i),"DI"] #some values occur in good.local.good.depth$j that are not i values 
#in clustering.info - therefore, doesn't get a DI calculated for it, so it throws an error in places where it can't assign a DI value to an existing j value
good.local.good.depth$avg.att.diff<-clustering.info[match(good.local.good.depth$i, clustering.info$i),"avg.att.diff"] #append average attribute difference for each i values neighborhood

###### NEED to figure out if we should just remove rows that have j.values without DI? 
good.local.good.depth<-good.local.good.depth[!is.na(good.local.good.depth$DI.j),] #remove where j values doesn't exist in clustering.info - can't evaluate future loops if so

##### NEED to figure out if it is appropriate to filter this early on?

#look only for i values that have spatially directly reachable points and density indicators greater than 0
clustering.info <- clustering.info[with(clustering.info,(!(is.na(sdr)) & DI != 0)),]
#t1<-2500

### BEGIN the iterative clustering process ###

while (sum(is.na(clustering.info$clust))!=0){ #While there are still points unclustered in clustering.info$clust (indicated by NA), create a new cluster
  
  NAs.remaining<-clustering.info[is.na(clustering.info$clust),] #downselect to a new dataframe that has only NA's in $clust
  ordered.DI<-NAs.remaining[order(NAs.remaining$DI,decreasing = TRUE),]#order the dataframe from highest to lowest DensityIndicator ($DI) where $clust == NA's 
  max.di<-max(ordered.DI$DI) #get the max DI
  initial.core.candidates<-ordered.DI[ordered.DI$DI==max.di,] #find all i's with the max DI
  spatial.core.i<-initial.core.candidates[initial.core.candidates$avg.att.diff==min(initial.core.candidates$avg.att.diff),1][1]#the spatial core is then the i with highest DI and lowest avg. att.diff
  
  # if (is.na(spatial.core.i)){
  #   spatial.core.i<-initial.core.candidates[1,1]
  #   
  # }
  
  #if the above line returns multiple spatial.core.i values, take the first one 
  
  ##### NEED to change how this is handled -- instead of just taking the first of multiple i values returned
  ##### thought about making it default to the neighbor with nearest distance, but that doens't work becuase of the necessary i value to j.value association, where this
  ##### sorting is only based on i values
  ######## SOLUTION: start with i values that have the highest number of spatially directly reachable neighbors 
  ##### if equivalent after that, select first in list
  
  ##if there is more than one i value returned to be a spatial core....
  if (spatial.core.i>1){
    i.and.sdr<-as.data.frame(matrix(0,ncol=2,nrow=length(spatial.core.i)))#create a dataframe with 2 columns and nrows= # of potential spatial cores
    counter=1 #start a counter
    for (i.val in spatial.core.i){ #for the values in the spatial.core.i vector
      points.reachable<-initial.core.candidates[initial.core.candidates$i==i.val,2] #get the number of points that are spatially directly reachable from the i val
      i.and.sdr[counter,1]<-i.val #paste the i value in the first column and row = counter
      i.and.sdr[counter,2]<-sdr #paste the # of sdr points from that i value in the second column and row = counter
      counter=counter+1 #increase the counter by one to make sure it pastes in the next row for the next i value
    }
    
    spatial.core.i<-i.and.sdr[i.and.sdr$V2==max(i.and.sdr[,2]),1] #get the i value from the created dataframe that corresponds to highest # of sdr neighbors
  }
  
  
  clustering.info$clust[clustering.info$i == spatial.core.i] <- cluster.num #get spatial.core.i output from beginning of while loop or stepping into if statement
  
  depth.comparison.avg<-good.local.good.depth[good.local.good.depth$i==spatial.core.i,5][1] #returns a list of all times i's depth appears, just want 1 element
  depth.list<-c() #create empty vector to receive depths for new cluster
  counter<-length(depth.list)  #create counter that is the length of depth.list
  first.neighbs<-good.local.good.depth[good.local.good.depth$i==spatial.core.i,] #make mini dataframe of 1st order neighbors
  first.neighbs.order<-first.neighbs[order(first.neighbs$DI.j,decreasing = TRUE),] #order the neighbors in descending order of DI to be systematic about searching
  next.list<-as.vector(first.neighbs.order$j) #get the next list (in DI order) to keep searching
  
  
  while (length(next.list)!=0){ #but when next list does equal zero, then change up cluster...and depth average..and counter...then restart
    #start with the newest next list built on the point with the next highest density indicator (new spatial core)
    new.next.list<-c()
    #start with the newest next list on built on the point with the next highest density indicator (new spatial core)
    #need to sort neighbors by density indicator in here
    
    for(j.val in next.list) {  #for every value that was a neighbor of a previous point that passed the check
      if(j.val %in% good.local.good.depth$i){ #if this neighbor value occurs as an i value in good.local.good.depth 
        neebs<-good.local.good.depth[good.local.good.depth$i==j.val,] #get the neighbors for this point
      }
      if (nrow(neebs)==1) { #if there is only one row in neebs, no need to order it - just set ordered.neebs===neebs
        ordered.neebs<-neebs
      }
      else{ #if there's more than 1 row
        ordered.neebs<-neebs[order(neebs$DI.j,decreasing = TRUE),] #go ahead and order the neebs dataframe by high to low DI value and set equal to ordered.neebs
        print(paste0("point to search for neighbors: ",j.val)) #prints the i value of the point you're ordering the neighbors for
        print(paste0("this is the number of neighbors for point: ", nrow(neebs) )) #prints the number of neighbors being ordered
      }
      
      for (row in 1:nrow(ordered.neebs)){ #get all of the neighbors for the j.value above, go through the rows
        if(ordered.neebs[row,10]!=0){
          
        if (is.na(clustering.info[clustering.info$i==(ordered.neebs[row,2]),6])==TRUE){ #if the point has already been assigned, back to for loop and go to next neighbor
          #######duplication between line above and below? 
          i.val.ordered.neebs<-ordered.neebs[1,1] #get the ivalue of the ordered.neebs so can grab T1 reference value
          if ((ordered.neebs[row,5]- depth.comparison.avg) <= (T1.order2mean.meanvar[T1.order2mean.meanvar$i.val==i.val.ordered.neebs,4])){  # &&  is.na(clustering.info[clustering.info$i==ordered.neebs[row,2],6])  ){  #if the $clust doesnt already have an assignement
            
            depth.list<-c(depth.list, ordered.neebs[row,5]) #add new depths
            print(paste0("Cluster number: ",cluster.num," depth added: ", tail(depth.list, n=1), " from point ", ordered.neebs[row,2]))  #print to console for checking
            counter<-length(depth.list) #make counter length of depth.list
            clustering.info$clust[clustering.info$i==ordered.neebs[row,2]]<-cluster.num #assign the new cluster number to the $clust column 
            depth.comparison.avg<-sum(depth.list)/counter # make new average for comparison from depth.list and counter
            new.next.list<-c(new.next.list,ordered.neebs[row,2]) #add the neighbor that now checks out to the new list so it's neighbors can be evaluated
          }
          
        }
        
        } 
      }
     
   
    }
    next.list<-new.next.list #once you step out of evaluating the neighbors for a single jvalue (in top for loop), pass a new list of neighbors to search through
  }
  cluster.num<-cluster.num+1 #when no more neighbors pass criteria, next.list is empty, steps out of 2nd while loop to top while loop, and begins another cluster
}

#create a dataframe that contains i values, x coords, y coords, and cluster number
df.plotting<-data.frame()
df.plotting<-as.data.frame(cbind(tri.withdepth$i,tri.withdepth$x,tri.withdepth$y,tri.withdepth$depth,tri.withdepth$spud)) #all of the original points, so some have been removed from final clusters, 
#because their connecting edges were removed
##### NEED to figure out how to deal with eliminated points
##### could create polygon boundaries of more or less continuous clusters and go back to reassign points that were dropped from initial data
colnames(df.plotting)<-c("i","long","lat")

#set coordinate columns to x and y
clustering.info$x<-df.plotting[match(clustering.info$i,df.plotting$i),"long"] 
clustering.info$y<-df.plotting[match(clustering.info$i,df.plotting$i),"lat"] 

#figure out how many clusters have only 1 point in them...
clust.tab<-as.data.frame(table(clustering.info$clust)) #tally up number of times each $clust value appears
#set all cluster values with a frequency of 1 to noise points
noise<-as.integer(clust.tab$Var1[clust.tab$Freq==1]) #wherever the $Freq==1, that cluster value is added to a vector of noise clusters




no.noise.clust<-clustering.info[!(clustering.info$i %in% noise),] 


no.noise.clust$depth<-tri.withdepth[match(no.noise.clust$i,tri.withdepth$i),"depth"] 
no.noise.clust$spud<-tri.withdepth[match(no.noise.clust$i,tri.withdepth$i),"spud"] 

plot(no.noise.clust$x, no.noise.clust$y,pch=19,cex=.5,col=no.noise.clust$clust)
plot(no.noise.clust$x, no.noise.clust$y,pch=19,cex=.5,col= "black")


clust1<-no.noise.clust[no.noise.clust$clust==1,] #inspecting shape of individual clusters
clust2<-no.noise.clust[no.noise.clust$clust==2,] #inspecting shape of individual clusters
clust3<-no.noise.clust[no.noise.clust$clust==3,] #inspecting shape of individual clusters
points(clust1$x,clust1$y,pch=19,cex=.5,col="purple")
points(clust2$x,clust2$y,pch=19,cex=.5,col="red")
points(clust3$x,clust3$y,pch=19,cex=.5,col="blue")




dataframe.withxy<-no.noise.clust #coordinates must be fed with column heading "x" and "y"
layer.name=paste0(input.initials,"_all_clusters") 
source("scripts_inputs\\new\\scripts\\Export_Points.R")
export.spatial.points(dataframe.withxy,layer.name) 


clust.nums<-as.data.frame(table(no.noise.clust$clust))
clust.by.FREQ<-clust.nums[order(clust.nums$Freq,decreasing=TRUE),]
freq_required<-clust.by.FREQ[1:25,] #get the formations that have more than 1000 wells categorized in them (this includes API14 (aka spatial duplicates))
most.clusters<-as.vector(freq_required$Var1) #create a vector of the names of popular formations

most.clusters.noblank<-setdiff(most.clusters,"") #removes the blank entry from being a possible formation match

#searches in formation.info for all records that belong to one of the popular formations
top.clusters<-no.noise.clust[no.noise.clust$clust %in% most.clusters.noblank,]

cluster.table<-as.data.frame(table(top.clusters$clust))


depth.mean<-aggregate(top.clusters$depth, by=list(top.clusters$clust), FUN=mean,na.rm=TRUE)
depth.sd<-aggregate(top.clusters$depth, by=list(top.clusters$clust), FUN=sd,na.rm=TRUE)

#top.formations.ok$spud_year<-as.numeric(format(as.Date(top.formations.ok$Date_Spud, format="%m/%d/%Y"),"%Y"))

spud_year.mean<-aggregate(top.clusters$spud, by=list(top.clusters$clust), FUN=mean,na.rm=TRUE)
spud_year.sd<-aggregate(top.clusters$spud, by=list(top.clusters$clust), FUN=sd,na.rm=TRUE)

clust.depth.time<-as.data.frame(cbind(depth.mean[1],round(depth.mean[2], digits = 0),round(depth.sd[2], digits = 0),
                                      round(spud_year.mean[2], digits = 0),round(spud_year.sd[2], digits = 0)))
colnames(clust.depth.time)<-c("clust","avg.depth","sd.depth","avg.spud.year","sd.spud.year")












