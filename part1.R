## ----init,echo=FALSE,result="hide", cache=FALSE--------------------------
# build("./part1/part1.Rhtml", "./templatr/inst/templates/pres1/","index.html",title="Part 1")
knitr::opts_chunk$set(comment=NA, prompt=TRUE, dev="CairoPNG",
out.width="500px", out.height="500px", dpi=72, fig.width=7
)
#, fig.width=20, fig.height=20, 
# out.width="150px", out.height="150px")
knitr::opts_knit$set(root.dir=wd)
setwd(wd)
knitr::opts_chunk$set(fig.full=FALSE)
op = options()$continue
options(continue=" ")
# grDevices::X11.options(type='cairo')
palette("default")
set.seed(310366)
library(rgl)
knit_hooks$set(webgl = hook_webgl)

## ------------------------------------------------------------------------
plot(rnorm(1000), rnorm(1000))
abline(h=2, col="#FF000080", lwd=25)

## ------------------------------------------------------------------------
par(mfrow=c(2,1))
hist(rnorm(1000))
abline(v=2)
m = matrix(runif(24),8, 3)
image(x=-3:4, y=4:6, z=m)
text(-1.5,5.5,"Top Left",cex=4)

## ----fig.full=TRUE-------------------------------------------------------
xy=list(
   x = c(41, 13, 21, 40, 44,
         31, 49, 55, 65, 83,
         67, 63, 87, 100, 93,
         84, 60)/100,
   y = c(76, 49, 42, 56, 31,
         2, 2, 26, 3, 2, 33,
        61, 48, 82, 85, 62, 78)/100)

head = circleGrob(0.5,0.85,.15)
body = pathGrob(xy$x,xy$y)
person = grobTree(head,body)
grid.draw(person)
vp <- viewport(width=0.5, height=0.5, x=0.25, y=0.45, angle=45)
pushViewport(vp)
grid.draw(editGrob(person, gp=gpar(col="red")))

## ----fig.full=TRUE-------------------------------------------------------
p = xyplot(mpg~wt, data=mtcars)
# nothing happens!

## ----fig.full=TRUE-------------------------------------------------------
print(p)

## ----fig.full=TRUE-------------------------------------------------------
xyplot(mpg~wt,data=mtcars, main="Cars",
   panel=function(x,y,...){
      panel.xyplot(x,y);panel.lmline(x,y)
   })

## ----crimes1-------------------------------------------------------------
crime = read.csv("./data/2016-03-cumbria-street.csv")
names(crime)
head(crime)

## ----map1,fig.full=TRUE--------------------------------------------------
plot(crime$Longitude, crime$Latitude)

## ----map2,fig.full=TRUE--------------------------------------------------
plot(crime$Longitude, crime$Latitude, asp=1)

## ----map3,fig.full=TRUE--------------------------------------------------
asp = 1/cos((pi/180)*mean(range(crime$Latitude,na.rm=TRUE)))
plot(crime$Longitude, crime$Latitude, asp=asp)

## ----fig.full=TRUE-------------------------------------------------------
plot(crime$Longitude, crime$Latitude,
   asp=1.726,
   pch=19,
   cex=1,
   axes=FALSE, xlab="", ylab="")
points(crime$Longitude[crime$Crime.type=="Robbery"], 
       crime$Latitude[crime$Crime.type=="Robbery"],
       col="red",pch=17)

## ----fig.full=TRUE-------------------------------------------------------
crimeSP = subset(crime, !is.na(Longitude) & !is.na(Latitude))
coordinates(crimeSP) = ~Longitude + Latitude
plot(crimeSP)

## ----fig.full=TRUE-------------------------------------------------------
projection(crimeSP)="+init=epsg:4326"
plot(crimeSP)

## ----fig.full=TRUE-------------------------------------------------------
plot(crimeSP)
points(crimeSP[crimeSP$Crime.type=="Robbery",],
       col="red",
       pch=17)
legend("topleft", 
     legend=c("Robbery","Other Crime"),
     col=c("red","black"),
     pch=c(17,3),
     pt.cex=2)
title("Robbery")

## ----fig.full=TRUE-------------------------------------------------------
roads = shapefile("./data/mainroads.shp")
table(roads$type)
lut = data.frame(
   row.names=names(table(roads$type)),
   colour = c("blue","blue","red","red",
              "grey50","green","green",
              "yellow","yellow","red","red"),
   width = c(3,3,3,3,1,2,2,1,1,3,3),
   stringsAsFactors=FALSE
)
head(lut)
plot(roads, col=lut[roads$type,"colour"], lwd=lut[roads$type,"width"])

## ----fig.full=TRUE-------------------------------------------------------
lut$order = c(11,10,7,6,1,5,4,3,2,9,8)
plot(SpatialPoints(
       as(extent(roads),
       "SpatialPoints"),
       proj4string=CRS(projection(roads))),
     col=NA)
lut = lut[order(lut$order),]
for(layer in 1:nrow(lut)){
 road_type = row.names(lut)[layer]
 lines(roads[roads$type==road_type,], 
       col=lut[layer,"colour"], 
       lwd=lut[layer,"width"])
}

## ----fig.full=TRUE-------------------------------------------------------
roads$order = lut[roads$type,"order"]
rorder = roads[order(roads$order),]
plot(rorder, col=lut[rorder$type,"colour"], lwd=lut[rorder$type,"width"])

## ------------------------------------------------------------------------
admin = shapefile("./data/cumbriaLSOA.shp")
extent(admin)
adminLL = spTransform(admin,"+init=epsg:4326") 
extent(adminLL)
cumbriaLL = shapefile("./data/cumbria.shp")

## ----fig.full=TRUE-------------------------------------------------------
plot(adminLL, col="#ffeecc", border="grey50")
plot(cumbriaLL, add=TRUE, lwd=2)
lines(roads)
points(crimeSP)

## ----fig.full=TRUE-------------------------------------------------------
library(OpenStreetMap)
wm = list(x=c(-3.08, -2.82), y=c(54.50, 54.32))
wmap = openmap(c(wm$y[1],wm$x[1]),c(wm$y[2],wm$x[2]), type="osm")
plot(wmap)
plot(spTransform(crimeSP,"+init=epsg:3857"), 
     pch=19,
     col=1+(crimeSP$Crime.type=="Anti-social behaviour"),
     add=TRUE)
legend("top", 
     legend=c("ASBOs","Other Crime"),
     col=c("red","black"),
     pch=c(19),
     pt.cex=2,
     bg="white")

## ----fig.full=TRUE-------------------------------------------------------
require(RColorBrewer)
library(raster)
dem = raster("./data/cumbriaDem.tif")
par(mfrow=c(2,2))
plot(dem)
plot(dem,zlim=c(0,1200))
plot(dem,col=rainbow(10))
plot(dem,col=gray(seq(1,0,len=10)))
plot(rorder,col=lut[rorder$type,"colour"],lwd=1,add=TRUE)

## ----fig.full=TRUE-------------------------------------------------------
lu = raster("./data/cumbriaLandUse.tif")
lu@legend@colortable[1]=NA

cumbriaP = spTransform(cumbriaLL,projection(lu))
plot(cumbriaP)
plot(lu, add=TRUE)
plot(cumbriaP, add=TRUE)

codes =read.csv("./data/lucodes.csv",as.is=TRUE)
head(codes[,c("GRID_CODE","LABEL3","colour")])

row.names(codes)=codes$GRID_CODE # for lookup
topUsage = codes[names(sort(table(lu[]),dec=TRUE))[1:10],] # get top ten most common categories
topUsage = topUsage[!topUsage$LABEL1=="Missing",] # remove missing
legend("topleft", 
    legend=topUsage$LABEL3, 
    col=topUsage$colour, 
    pch=15, pt.cex=1, bg="#FFFFFF80",
    title="Top Land Cover by Area")

## ----fig.full=TRUE-------------------------------------------------------
library(rgeos)
# compute 4*pi*area/perimeter^2 as a simple compactness measure
admin$Area = gArea(admin, byid=TRUE)
admin$Length = gLength(admin, byid=TRUE)
admin$Compact = 4*pi*admin$Area/admin$Length^2
ordering = order(-admin$Compact)
par(mfrow=c(10,10))
par(mar=c(0,0,0,0))
# plot the 50 least compact, the 50 most compact
for(i in 1:50){
 j = ordering[i]
 plot(admin[j,], col="#ffeecc")
 text(coordinates(admin[j,]),
    sprintf(admin$Compact[j], fmt="%4.2f"))
}
for(i in 50:1){
 j = rev(ordering)[i]
 plot(admin[j,], col="#cceeff")
 text(coordinates(admin[j,]),
    sprintf(admin$Compact[j], fmt="%4.2f"))
}

## ----fig.full=TRUE-------------------------------------------------------
classIntervals(admin$Compact,n=5, style="pretty")
plot(classIntervals(admin$Compact,n=5, style="pretty"),
    pal=brewer.pal(5,"Spectral"))

## ------------------------------------------------------------------------
plot(classIntervals(admin$Compact,n=5, style="sd"),
    pal=brewer.pal(5,"Spectral"))
plot(classIntervals(admin$Compact,n=5, style="equal"),
    pal=brewer.pal(5,"Spectral"))
plot(classIntervals(admin$Compact,n=5, style="quantile"),
    pal=brewer.pal(5,"Spectral"))


## ----fig.full=TRUE-------------------------------------------------------
ci = classIntervals(admin$Compact, n=5, style="pretty")

# use class intervals as lookup into palette
plot(admin, 
     col=brewer.pal(
         length(ci$brks)-1,
         "Spectral")[findCols(ci)]
    )

# get the labels
pt = print(ci)
legend("topleft",legend=names(pt),
    col=brewer.pal(length(ci$brks)-1,"Spectral"),
    pch=15,title="Shape")

## ------------------------------------------------------------------------
fscale = function(input, output){
### return a function that rescales 
### from the limits of `input` to the limits of `output`
   f = function(x){
     s = (x-min(input))/(max(input)-min(input))
     p = min(output) + (max(output)-min(output))*s
     return(p)
     } 
   return(f)
}

# this scales our 0-1 values to 1-10 
f10 = fscale(c(0,1),c(1,10))
f10(c(0,.5,1))

## ----fig.full=TRUE-------------------------------------------------------
plot(SpatialPoints(admin),cex=f10(admin$Compact),pch=1)

## ----fig.full=TRUE-------------------------------------------------------

cscale = function(data, colours){
  scaler = fscale(data, c(0,1))
  ramp = colorRamp(colours)
  f = function(d){
     values = scaler(d)
     colours = rgb(ramp(values),max=255)
     return(colours)
   }
  return(f)
}

pal = brewer.pal(5,"RdYlBu")
fullscale = cscale(c(0,1),pal)
datascale = cscale(admin$Compact, pal)

plot(admin, col=fullscale(admin$Compact), 
     main="scale 0-1")
plot(admin, col=datascale(admin$Compact),
     main="scale to data range")

## ----fig.full=TRUE-------------------------------------------------------
par(mfrow=c(5,5))
par(mar=c(0,0,0,0))
ii = sample(nrow(admin),25)
for(i in ii){
 plot(admin[i,], col=datascale(admin$Compact[i]))
}

## ----fig.full=TRUE-------------------------------------------------------
spplot(admin,"Compact")

## ----fig.full=TRUE-------------------------------------------------------
spplot(admin,"Compact",col.regions=gray(seq(0,1,len=10)))

## ----fig.full=TRUE-------------------------------------------------------
admin$Council = substr(admin$name,1, nchar(admin$name)-5)
head(admin@data)
ggplot(admin@data, aes(x=Area, y=Length^2, col=Council)) + 
  geom_point(pch=17, cex=3)

## ----fig.full=TRUE-------------------------------------------------------
g = ggplot(admin@data, aes(x=Area, y=Length, col=Council)) + 
    geom_point(cex=4)
g + scale_colour_brewer(palette="Set2")
g + scale_colour_brewer(palette="Dark2")

## ----fig.full=TRUE-------------------------------------------------------
ggplot(crime, aes(x=Longitude, y=Latitude)) + geom_point() + coord_map()


## ----fig.full=TRUE-------------------------------------------------------
admin_fort = fortify(admin, region="name")
head(admin_fort)

## ----fig.full=TRUE-------------------------------------------------------
ggplot(admin@data, aes(map_id=name) ) +
   geom_map(
     aes(map_id=name, fill=Council),
         map = admin_fort, colour="black") + 
   expand_limits(x=admin_fort$long ,y=admin_fort$lat) + 
   coord_fixed()

## ----fig.full=TRUE-------------------------------------------------------
library(maps)
map('world')

## ----fig.full=TRUE-------------------------------------------------------
map('world',projection="sinusoidal")

## ----fig.full=TRUE-------------------------------------------------------
map('world','UK')
plot(dem,add=TRUE, legend=FALSE)
map('world',add=TRUE)

## ----fig.full=TRUE, out.width="500px", out.height="500px", dpi=144, fig.width=12----
library(rworldmap)
mapCountryData(nameColumnToPlot='GEO3major',
   colourPalette=brewer.pal(7,"Set2"))

## ----fig.full=TRUE,  out.width="500px", out.height="500px", dpi=144, fig.width=12----
mapBubbles(dF=getMap(), 
      nameZSize="POP_EST",
      nameZColour="GEO3",
      colourPalette='rainbow', 
      oceanCol='lightblue', landCol='wheat') 

## ----fig.full=TRUE-------------------------------------------------------
library(choroplethr)
library(choroplethrMaps)
county_choropleth(df_pop_county,state_zoom    = "california",reference_map = TRUE)

## ----eval=FALSE----------------------------------------------------------
## library(quickmapr)
## q = qmap(crimeSP, adminLL) # same proj needed
## zi(q) # one click and zoom in
## zo(q) # click zoom out
## i(q, "adminLL") # click an admin for info
## i(q, "crimeSP") # click on a crime

## ----fig.full=TRUE-------------------------------------------------------
qtm(admin, fill="Compact")

## ----fig.full=TRUE-------------------------------------------------------
qtm(admin, fill="Compact", style="classic")

## ----fig.full=TRUE-------------------------------------------------------
osmap = read_osm(bb(admin, ext=1.1, projection="longlat"))
tm_shape(osmap) + tm_raster()

## ----fig.full=TRUE-------------------------------------------------------
tm_shape(osmap) + tm_raster() + 
   tm_shape(admin) + tm_polygons("Compact", alpha=0.5)

## ----fig.full=TRUE-------------------------------------------------------
tm_shape(admin) + tm_borders() + 
  tm_fill("Compact") + 
  tm_facets(by="Council", free.coords=TRUE, drop.shapes=FALSE)

## ----fig.full=TRUE-------------------------------------------------------
tm_shape(dem) + tm_raster() + 
   tm_shape(admin) + tm_borders() + 
   tm_shape(crimeSP) + tm_dots()

## ----eval=FALSE----------------------------------------------------------
## # this
## admin$logCompact = log(admin$Compact)
## tm_shape(admin) + tm_fill("logCompact")
## # or this
## tm_shape(admin) + tm_fill(~log(Compact))
## # this
## admin$random = runif(nrow(admin))
## tm_shape(admin) + tm_fill(random)
## # or this
## tm_shape(admin) + tm_fill(runif(nrow(admin)))

## ----fig.full=TRUE-------------------------------------------------------
lusp = as(lu, "SpatialGridDataFrame")
lusp$cumbriaLandUse = as.factor(lusp$cumbriaLandUse)
lupalette = codes[levels(lusp$cumbriaLandUse),"colour"]
levels(lusp$cumbriaLandUse) = codes[levels(lusp$cumbriaLandUse),"LABEL3"]
tm_shape(lusp) + tm_raster("cumbriaLandUse", palette=lupalette, max.categories=29)

## ----eval=FALSE----------------------------------------------------------
## library(rgl)
## 
## demG = projectRaster(dem,crs=CRS("+init=epsg:27700"))
## 
## terrain3d(x=xFromCol(demG),
##           y=yFromRow(demG),
##           z=t(raster::as.matrix(demG)*10),
##           col=terrain.colors(870)[demG[]+1],
##           back="lines")

## ----exit,cache=FALSE,echo=FALSE,result="hide"---------------------------
options(continue=op)

