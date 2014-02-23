#+ setup, include=FALSE,results="hide",messages=FALSE
#  ##########################################################################
#' MEGS Index calculation
#' =====================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' mpa@aqua.dtu.dk
#'
#' Tue Feb 18 17:47:54 2014
#'
#  Calculates the MEGS Indices using the standard estimation procedure
#
#  To do:
#
#  Notes:
#   - This script contains RMarkdown. For instructions on
#     how to build it into HTML, please see the end of the file 
# */
#' <small>*This work by Mark R Payne is licensed under a  Creative Commons
#' Attribution-NonCommercial-ShareAlike 3.0 Unported License. 
#' For details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
#' Basically, this means that you are free to "share" and "remix" for 
#' non-commerical purposes as you see fit, so long as you "attribute" me for my
#' contribution. Derivatives can be distributed under the same or 
#' similar license.*</small>
#'
#' <small>*This work comes with ABSOLUTELY NO WARRANTY or support.*</small>
#'
#' <small>*This work is also subject to the BEER-WARE License. For details, see
#' http://en.wikipedia.org/wiki/Beerware*</small>
#'
#/*##########################################################################*/
#+ include=FALSE

# ========================================================================
# Initialise system
# ========================================================================
#Configure markdown stlye
require(knitr);opts_chunk$set(echo=FALSE,results="hide",messages=FALSE,
                              fig.path="mdfigures/")

# House cleaning
rm(list = ls(all.names=TRUE));  graphics.off()
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)
cat(sprintf("\n%s\n","MEGS TAEP Calculation"))
cat(sprintf("Analysis performed %s\n\n",date()))

log.msg <- function(fmt,...) {cat(sprintf(fmt,...));flush.console();
                              return(invisible(NULL))}

library(raster)
library(reshape)
library(maps);library(mapdata);library(maptools)

#Start recording from here
opts_chunk$set(results="markup",echo=FALSE)

### ========================================================================
### Setup data
### ========================================================================
#Configuration
agg.factor <- 5   #Resolution for testing on land
onland.threshold <- 0.10  #A pixel is land if it is more than 10% land

#Load externals
log.msg("Loading data...\n")
load("objects//EPR_data_QA.RData")
load("objects//isobaths.RData")  #For later use in plotting

#Import data
log.msg("Setting up data...\n")
dat <- subset(dat,Component=="W" & Year==2013)

### ========================================================================
### Setup calculation grid
### ========================================================================
log.msg("Preparing grids...\n")
#Classical calculation of the index is based on half-ICESrectangles
#ie 0.5 deg lon x 0.5 deg lat, each of which has a five-letter code
#together with an E/W designation. However, this is not a particularly
#easy system to convert to/from lat/long and there is no explicit
#reason to use this system. Here, we create our own analogue of it
#using the sp package library - conceptually it is the same grid
#but we simply use our own indexing system.
#ICES rectangles are centered at latitudes 0.25 and 0.75, and longitudes
#0.5. The half-rectangle grid used here is therefore centered at
#latitudes of 0.25 and 0.75, and longitudes of 0.25 and 0.75. We first
#create such a grid.
dat$halfrect.lon <- round(dat$declon*2-0.5)/2+0.25
dat$halfrect.lat <- round(dat$declat*2-0.5)/2+0.25
halfrect.grd <- GridTopology(c(lon=min(dat$halfrect.lon), 
                               lat=min(dat$halfrect.lat)),
                             c(0.5,0.5),
                             c(diff(range(dat$halfrect.lon))/0.5+1,
                               diff(range(dat$halfrect.lat))/0.5+1))
grd.sp <- SpatialGrid(halfrect.grd,proj4string=CRS("+proj=longlat"))
grd.r <- raster(grd.sp)

#Assign samples to a grid point
dat.sp <- dat
coordinates(dat.sp) <- ~ declon + declat
proj4string(dat.sp) <- proj4string(grd.sp)
dat$grid.idx <- over(dat.sp,grd.sp)

#Identify grid points where the centre is on land
#Convert grid to a much higher res version - we use
#this to be more robust about land detection
grd.hires <- disaggregate(grd.r,fact=agg.factor)
#Setup a map object
map.dat <- map("worldHires",
               xlim=bbox(dat.sp)["declon",],
               ylim=bbox(dat.sp)["declat",],
               plot=FALSE,fill=TRUE,res=0)
map.sp <- map2SpatialPolygons(map.dat,map.dat$names)
proj4string(map.sp) <- proj4string(dat.sp)
#Then identify onland
iswet.hires <- rasterize(map.sp,grd.hires)
#Reaggregate back to default resolution
wet.frac <- aggregate(is.na(iswet.hires),fact=agg.factor,fun=mean)
#And create the onland raster
iswet   <- wet.frac > (1-onland.threshold)

#Setup cell areas using the prescribed formula
pxl.lat <- grd.r
pxl.lat[] <- coordinates(pxl.lat)[,2]
pxl.area <- cos(pxl.lat/180*pi)*(30 * 1853.2)^2

### ========================================================================
### Ready to do the calcuations
### ========================================================================
#Setup plotting
pdf("plots/EPR_by_period_and_year.pdf",width=210/25.4,height=297/25.4)

#Split into sampling periods and process
dat.l <- split(dat,dat[,c("Period","Year")],drop=TRUE)
idxs.l <- lapply(dat.l,function(d) {
  #Get period /year
  p.yr <- unique(d[,c("Period","Year")])
  log.msg("Processing %i, period %i...\n",p.yr$Year,p.yr$Period)
  
  #Calculate average EPR per subrectangle
  EPR.rect <- melt(tapply(d$EPR.est,d$grid.idx,mean))
  
  #Convert to raster via spdf
  EPR.r <- grd.r
  EPR.r[] <- as.numeric(NA)
  EPR.r[EPR.rect$indices] <- EPR.rect$value

  #Do the interpolations... Interpolation takes place in a "queen"
  #pattern ie the 8 surrounding pixels
  queen.wts <- matrix(1,3,3)
  queen.wts[2,2] <- 0
  interp.EPR <- focal(EPR.r,w=queen.wts,fun=mean,pad=TRUE,NAonly=TRUE,na.rm=TRUE)
  
  #Neighbourhood patter. Interpolation is allowed
  #when a cell has two or more neighbours in a "rook" pattern
  #ie N, W, E, S of the centre
  rook.wts <- matrix(c(0,1,0,1,0,1,0,1,0),3,3)
  n.rook.neighbours <- focal(!is.na(EPR.r),w=rook.wts,
                             pad=TRUE,padValue=FALSE,Sfun=sum)
    
  #Interpolation is allowed under three conditions
  # i. The cell has two or more neighbours
  # ii. The value of the cell is empty
  # iii. We are not interpolating onto land
  interp.ok <- n.rook.neighbours >=2 & is.na(EPR.r) & iswet
  interp.EPR[ !interp.ok] <- NA
  
  #Create a brick
  EPR.b <- brick(EPR.r,interp.EPR)
  names(EPR.b) <- c("EPR.obs","EPR.intp")
  
  #Shift to SpatialPoints df, as this is well suited for plotting
  spdf <- as(EPR.b,"SpatialPointsDataFrame")
  spdf$intp <- ifelse(!is.na(spdf$EPR.intp),TRUE,
                      ifelse(!is.na(spdf$EPR.obs),FALSE,NA))
  spdf$EPR <- ifelse(spdf$intp,spdf$EPR.intp,spdf$EPR.obs)
  
  #Bin following scheme in WGMEGS reports
  spdf$EPR.bins <- cut(spdf$EPR,breaks=c(-Inf,0,10,100,500,1000,2000,Inf),right=TRUE,
                      labels=c("0","0-10","10-100","100-500",
                               "500-1000","1000-2000","2000-"))
  
  #Generate plots
  bubble.cexs <- c(15,27,47,74,106,144,180)/180*5
  bubble.pchs <- c(1,rep(16,6))
  plot(spdf[,"intp"],pch=NA,
       xlim=bbox(grd.sp)["lon",],
       ylim=bbox(grd.sp)["lat",])
  title(main=sprintf("%i, period %i",p.yr$Year,p.yr$Period))
  box()
  map("worldHires",add=TRUE,col="grey",fill=TRUE)
  plot(isobaths.sp,add=TRUE,col="darkgrey")
  plot(spdf[,"EPR.bins"],add=TRUE,
       col=ifelse(spdf$intp,"red","blue"),
       pch=bubble.pchs[spdf$EPR.bins],
       cex=bubble.cexs[spdf$EPR.bins])
  legend("bottomleft",bg="white",
         legend=levels(spdf$EPR.bins),
         col="blue",
         pch=bubble.pchs,
         pt.cex=bubble.cexs)

  #Calculate EPR per pixel
  EPR.pxl <- EPR.b*pxl.area
  names(EPR.pxl) <- names(EPR.b)
  #Summary statistics
  return(data.frame(year=p.yr$Year,
                    period=p.yr$Period,
                    nrect.obs=cellStats(!is.na(EPR.b[["EPR.obs"]]),sum),
                    nrect.interp=cellStats(!is.na(EPR.b[["EPR.intp"]]),sum),
                    observed=cellStats(EPR.pxl[["EPR.obs"]],sum),
                    interp=cellStats(EPR.pxl[["EPR.intp"]],sum)))
  
})
dev.off()

### ========================================================================
### Calculate indices
### ========================================================================
idxs <- do.call(rbind,idxs.l)
idxs$period.EPR <- idxs$observed + idxs$interp
idxs$frac.interp <- idxs$interp/idxs$period.EPR


### ========================================================================
### Visualise
### ========================================================================
p <- xyplot(period.EPR/1e12 ~ period|factor(year),idxs,type="b",groups=year,
            as.table=TRUE,
            xlab="Period",ylab="Egg production [10¹²]",
            ylim=c(0,45),
            scales=list(alternating=FALSE))
print(p)

#/* ========================================================================*/
#   Complete
#/* ========================================================================*/
#'
#+ echo=FALSE,results='asis'
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("\nAnalysis complete in %.1fs at %s.\n",proc.time()[3]-start.time,date())

#Useage notes
#   - This script contains and supports RMarkdown. To build it to HTML
#     use the following commands
#       > library(knitr)
#       > opts_knit$set(root.dir=getwd())
#       > spin("this_file.r")
#     and open the corresponding html file e.g. in Firefox
#   - Add markdown directly using #'
#   - Add code chunk options directly using #+
#
