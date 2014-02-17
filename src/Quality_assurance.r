# ##########################################################################
# Prepare MEGS survey data 
#
# by Mark R Payne
# DTU-Aqua, Charlottenlund, DK
# mpa@aqua.dtu.dk
# Fri Nov  1 09:32:18 2013
# 
# Loads and prepares the MEGS survey data for analysis
#
# To do:
#
# Notes:
#
# --------------------------------------------------------------------------
# "THE MODIFIED BEER-WARE LICENSE" 
# This work by Mark R Payne is licensed under a 
# Creative Commons Attribution-NonCommercial -ShareAlike 3.0 Unported License 
# for details, see http://creativecommons.org/licenses/by-nc-sa/3.0/deed.en_US
# Basically, this means that you are free to "share" and "remix" for non-
# commerical purposes as you see fit, so long as you "attribute" me for my
# contribution. Derivatives can be distributed under the same or similar license.
#
# This work comes with ABSOLUTELY NO WARRANTY or support.
#
# If we meet some day, and you think still this stuff is worth it, you
# can buy me a beer in return. 
# --------------------------------------------------------------------------
# ##########################################################################

### ========================================================================
### Initialise system
### ========================================================================
# Start with house cleaning
rm(list = ls(all.names=TRUE));  graphics.off()
cat(sprintf("\n%s\n%s\n\n","Prepare MEGS survey data",date()))
start.time <- proc.time()[3]
options(stringsAsFactors=FALSE)

log.msg <- function(fmt,...) {cat(sprintf(fmt,...));flush.console();return(invisible(NULL))}
library(sp); library(rgdal)
library(maptools);library(maps);library(mapdata)
library(MASS)
library(mgcv)

source("src//common_elements.r")

### ========================================================================
### Load and prepare data object
### ========================================================================
log.msg("Importing raw data...\n")

#Data set
fnames <- dir("raw_data",pattern="[0-9]{4}.csv$",full.names=TRUE)
obj.dir <- "data"

#Load data
dat.l <- lapply(fnames,read.csv2,
                strip.white=TRUE,blank.lines.skip=TRUE,
                na.strings="")

#Tidy up some inconsistent column names
dat.l <- lapply(dat.l,function(d) {
     n <- colnames(d)
     n[n=="TempSur.5m."] <- "Temp5m"
     n[n=="decLat"] <- "declat"
     colnames(d) <- n
     return(d)
})

#Subset columns - DateTime values are handle separately later
cols.to.keep <- c("Year","Month","Day","Hour","Minutes","Period","Country","Vessel","Gear","declon",
                  "declat","Sdepth","Bdepth","VolFilt","Temp5m",
                  "Temp20m","Mac1","MacStage1",
                  "MacFactor")
dat.kept <- do.call(rbind,lapply(dat.l,"[",cols.to.keep))
colnames(dat.kept) <- gsub("^dec(.{3})$","\\1",colnames(dat.kept))

#Drop rows where remainder is NA
missing.vals <- rowSums(as.matrix(sapply(dat.kept,is.na)))
dat.all <- subset(dat.kept,missing.vals!=length(cols.to.keep))
dat <- dat.all

### ========================================================================
### Manual parsing of columns
### Not all columns parse successfully, so we need to handle 
### this explicitly
### ========================================================================
#Identify problematic columns
allowed.char.cols <- c("Country","Vessel","Gear")
parsed.as.char <- names(which(sapply(dat,class)=="character"))
wrong.parsing <- parsed.as.char[!parsed.as.char %in% allowed.char.cols]

#Parse manually
pe.db <- data.frame(column="NA", missing=0,did.not.parse=0)
pe.l <- list()
for(i in seq(wrong.parsing)) {
  n <- wrong.parsing[i]
  log.msg("Manually parsing %s...\n",n)
  dat[,n] <- suppressWarnings(as.numeric(gsub(",|\\.+",".",dat[,n])))  
  
  #Identify parsing errors
  orig.colname <- sprintf("%s.orig",n)
  parse.colname <- sprintf("%s.parsed",n)
  d <- dat[,-which(colnames(dat)==n)]
  d[[orig.colname]] <- dat.all[,n] 
  d[[parse.colname]]   <- dat[,n]
  d.parse.err <- subset(d,is.na(d[,parse.colname]) & ! is.na(d[,orig.colname]))
  
  #Store results
  pe.db[i,] <- list(n,
                      nrow(subset(dat.all,is.na(dat.all[,n]))),
                      nrow(d.parse.err))
  pe.l[[n]] <- d.parse.err
}

#Missing columns
log.msg("Checking for missing entries...\n")
miss.db <- sapply(dat,function(d) {sum(is.na(d))})
miss.l <- lapply(dat,function(d) subset(dat,is.na(d)))

### ========================================================================
### Error detection algorithms
### Here we check for and correct various common errors that pop up in the
### the dataset
### ========================================================================
log.msg("Error dectection analysis...\n")
#Error dection suite
err.add <- function(l,name,desc,rws) {
  l[[name]] <- rws
  attr(l[[name]],"code") <- name
  attr(l[[name]],"desc") <- desc
  return(l)
}

err.l <- list()

#Raising factors that are less than one (RFLT1) 
err.l <- err.add(err.l,
                 "RFLT1","Raising factor less than 1",
                 subset(dat,MacFactor<1))

#Missing both Mac1 and Stage1 (MissBo)
err.l <- err.add(err.l,
                 "MissBoMac","Missing both Mac1 and MacStage1",
                 subset(dat,is.na(Mac1) & is.na(MacStage1)))

#Temperature at both 5m and 20m missing
err.l <- err.add(err.l,
                 "MissBoTemp","Missing temperatures at both 5m and 20m",
                 subset(dat,is.na(Temp5m)&is.na(Temp20m)))

#Years wrongly entered
err.l <- err.add(err.l,
                 "invalid.yr","No MEGS survey in given year",
                 subset(dat,!(Year %in% seq(1977,2013,by=3))))

#Hours or minutes are nonsensical
err.l <- err.add(err.l,
                 "invalid.hour","Hour field does not make sense",
                 subset(dat,!(Hour %in% 0:24)))
err.l <- err.add(err.l,
                 "invalid.min","Minute field does not make sense",
                 subset(dat,!(Minutes %in% 0:60)))

### ========================================================================
### Spatial integrity checks - esp points on land
### ========================================================================
log.msg("Checking for points on land...\n")
#Create spatial object
dat.sp <- dat
coordinates(dat.sp) <- ~ lon + lat
proj4string(dat.sp) <- CRS("+proj=longlat")

#Extract coastlines
map.dat <- map("worldHires",
               xlim=bbox(dat.sp)["lon",],
               ylim=bbox(dat.sp)["lat",],
               plot=FALSE,fill=TRUE,res=0)
map.sp <- map2SpatialPolygons(map.dat,map.dat$names)
proj4string(map.sp) <- proj4string(dat.sp)

#Test for points on land
on.land <- over(dat.sp,map.sp)

#Record points on land 
on.land.sp <- subset(dat.sp,!is.na(on.land))
err.l <- err.add(err.l,
                 "onland","Points are on land",
                 as.data.frame(on.land.sp))

#Assign to Western or Southern component
Scomp.hauls <- !is.na(over(dat.sp,Scomp.ll))
dat$comp <- ifelse(Scomp.hauls,"S","W")

#Plot
pdf("plots/spat_distribution.pdf")
if(nrow(on.land.sp)>0) {
  plot(dat.sp,pch=NA)
  plot(map.sp,add=TRUE,col="grey")
  plot(on.land.sp,add=TRUE,col="red")
  title(main="Points reported on land")

}
plot(dat.sp,pch=NA,main="Western / Southern allocation")
plot(map.sp,add=TRUE,col="grey")
plot(dat.sp,col=ifelse(dat.sp$comp=="S","red","green"))

### ========================================================================
### Error correction algorithms
### ========================================================================
log.msg("Doing error corrections...\n")

#Make corrections to Mac1
dat$raising.factor <- ifelse(is.na(dat$MacFactor),1,dat$MacFactor) #If missing, set to 1
dat$raising.factor <- ifelse(dat$raising.factor<1,    #If less than 1, its been inverted
                             1/dat$raising.factor,dat$raising.factor)

dat$n.counted <- ifelse(is.na(dat$Mac1) & is.na(dat$MacStage1), #If both Mac1 and MacStage1 missing, set to zero
                        0,dat$Mac1)  
dat$n.counted <- ifelse(is.na(dat$Mac1) & !is.na(dat$MacStage1),  #Data has been entered as raised instead
                        dat$MacStage1/dat$raising.factor,  dat$n.counted)

if(any(is.na(dat$n.counted),is.na(dat$raising.factor))) {#Final check that we got all the errors in Egg counts
  stop("Egg count errors remain. Please check.")
}

#Correct temperatures
dat$Temp <- ifelse(!is.na(dat$Temp20m),dat$Temp20m,dat$Temp5m)
if(any(is.na(dat$Temp))) {#Final check that we got all the errors in Egg counts
  warning("Temperature errors remain. Please check.")
}

#Correct specific lines
dat[dat$Year==2003,"Year"] <- 2013
dat[dat$Year==2210,"Year"] <- 2010
dat[dat$Year==2077,"Year"] <- 2007
dat[dat$Country=="SP(AZTI)" & dat$Vessel=="COSA" & dat$Year==1992 & dat$Month==6 & dat$Day==8,"Period"] <- 3

### ========================================================================
### Derived and additional variables
### ========================================================================
log.msg("Derived variables...\n")

#Add time variables
dat$POSIX <- with(dat,
                  ISOdate(Year,Month,Day,tz="GMT"))
#                               ifelse(is.na(Hour),12,Hour),
#                               ifelse(is.na(Minutes),30,Minutes),
#                               00,tz="GMT"))
dat$doy <- as.POSIXlt(dat$POSIX)$yday+1
dat$Period <- factor(dat$Period)

#Tidy up gear
dat$Gear <- toupper(dat$Gear)

#Calculate solar angle - currently disabled due to poor
#quality of time data
#azel.mat <- solarpos(dat.sp,dat.sp$POSIX)
#dat$sol.el <- azel.mat[,2]
#dat$Daytime <- dat$sol.el > -6   #We use civil dawn as the definition of day night

#Calculate UTM coordinates
pts.utm <- spTransform(dat.sp,CRSobj=utm.crs)
utm.coords <- coordinates(pts.utm)
dat$eastings <- utm.coords[,"lon"]
dat$northings <- utm.coords[,"lat"]

#Calcualte the Offset factor
dat$dev.time <- exp(-1.31*log(dat$Temp)+6.90) #Based on Mendiola et al. (2006)
dat <- transform(dat,offset.factor=VolFilt*dev.time/raising.factor/Sdepth,
                 PA.offset=VolFilt*dev.time)   #We won't split if there are no-eggs
dat$log.offset <- log(dat$offset.factor)

#Presence Absence of Stage 1 eggs
dat$EggsPresent <- dat$n.counted!=0

#Tidy up Vessels (remove spanish ñ from Alvariño)
dat$Vessel <- factor(dat$Vessel)
levels(dat$Vessel)[4] <- "ALVARINO"

### ========================================================================
### Period estimation
### We use a logistic regresion on an ordered factor to estimate the periods
### properly and robustly
### ========================================================================
dat$Period.est <- NA
periods <- list()
yr.combs <- c(as.list(unique(dat$Year)),list(unique(dat$Year)))

for(y in yr.combs) {
  #Xtract data
  y.idxs <- which(dat$Year%in%y)
  d <- dat[y.idxs,]
  d$Period <- as.numeric(as.character(d$Period))
  #Fit a high order gam
  m <- gam(Period ~ s(doy,k=30)+doy,data=d)
  #Predict what the period should be
  pred.x <- data.frame(doy=seq(min(d$doy),max(d$doy)))
  pred.x$Period <- round(predict(m,pred.x))
  dat[y.idxs,]$Period.est <- round(predict(m,d))
  #Store
  periods[[paste(y,collapse=",")]] <- pred.x
} 
plt.dat <- dat[order(dat$doy,dat$Year),]
p <- xyplot(Period.est + as.numeric(as.character(Period)) ~doy | factor(Year),
       data=plt.dat,as.table=TRUE,
       type=c("l","p"),distribute.type=TRUE,
       ylab="Period",xlab="Day of Year",
       main="Assigned and estimated period boundaries")
print(p)

### ========================================================================
### Save results
### ========================================================================
log.msg("Saving results...\n")

#Save two versions of the objections - one as a normal data frame, the other
#as an sp object
dat.sp <- dat
coordinates(dat.sp) <- ~eastings + northings
proj4string(dat.sp) <- utm.crs

#Save
save(dat,dat.sp,periods,file=dat.src)
save(periods,file="data/spawning_periods.RData")

### ========================================================================
### Final error checks
### ========================================================================
#Check for Offset factors
err.l <- err.add(err.l,
                 "offsetNA","offset factor is undefined",
                 subset(dat,is.na(offset.factor)))

#Print error tables
err.db <- lapply(err.l,function(l) {
  data.frame(code=attr(l,"code"),
             problem=attr(l,"desc"),
             cases=nrow(l))
})
err.db <- do.call(rbind,err.db)

log.msg("Parse errors...\n")
print(pe.db)

log.msg("Missing values..\n")
print(miss.db)

log.msg("Other errors...\n")
print(err.db)

### ========================================================================
### Complete
### ========================================================================
if(grepl("pdf|png|wmf",names(dev.cur()))) {dmp <- dev.off()}
log.msg("\nComplete in %.1fs at %s.\n",proc.time()[3]-start.time,date())
