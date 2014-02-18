#+ setup, include=FALSE,results="hide",messages=FALSE
#  ##########################################################################
#' MEGS Quality Assurance
#' =====================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' mpa@aqua.dtu.dk
#'
#/* Mon Feb 17 10:02:58 2014
#
# Performs quality assurance checks on the MEGS database
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
require(knitr)
opts_chunk$set(echo=FALSE,results="hide",messages=FALSE,fig.path="mdfigures/")

# House cleaning
rm(list = ls(all.names=TRUE));  graphics.off()
start.time <- proc.time()[3]; options(stringsAsFactors=FALSE)
cat(sprintf("\n%s\n","MEGS Quality Assurance"))
cat(sprintf("Analysis performed %s\n\n",date()))

log.msg <- function(fmt,...) {cat(sprintf(fmt,...));
                              flush.console();return(invisible(NULL))}
library(sp)
library(lattice)
library(tools)
library(reshape)
library(maps);library(mapdata);library(maptools)

#Start recording from here
opts_chunk$set(results="markup",echo=FALSE,messages=FALSE)
#+results="asis"
cat(sprintf("Analysis performed %s\n\n",date()))
options("width"=120)
#/* ========================================================================*/
#'# Introduction
#'  This work documents the results of quality assurance checks on the the
#'  MEGS database. The details of the analysed file are as follows:
#/* ========================================================================*/
#Load data file
load("objects//EP_data_raw.RData")

# File details
f.details <- attr(dat,"source.details")
print(t(f.details))

#'<small>The md5 checksum is used as an indicator of the contents of the file. Files
#'that have identical contents will have identical md5 checksums, irrespective of file
#'name, modification date etc. Files have different even by a single bit will
#'have different checksums </small>
#'
#' ### Data table size
#' Number of rows, Number of columns
dim(dat)

#' ### Data fields available
colnames(dat)

#/* ========================================================================*/
#'# Data Summaries
#' The following tables explore the various contributions to the database
#' from various key values - the numbers in the matrices represent
#' the number of samples in the database. These tables can be used as
#' a quick overview and to check the quality of data entry, particularly for
#' the text fields contained
#/* ========================================================================*/
#'#### Data by Year
print(table(Year=dat$Year),zero.print=".")

#'#### Gear by Year
print(table(Gear=dat$Gear,Year=dat$Year),zero.print=".")

#'#### Vessel by Year
print(table(Vessel=dat$Vessel,Year=dat$Year),zero.print=".")

#'#### Country by Year
print(table(Country=dat$Country,Year=dat$Year),zero.print=".")

#'#### Period by Year
print(table(Year=dat$Year,Period=dat$Period),zero.print=".")

#'#### Year - Country - Vessel - Gear table
prt.dat <- melt(table(Year=dat$Year,Country=dat$Country,Vessel=dat$Vessel,Gear=dat$Gear))
prt.dat <- prt.dat[order(prt.dat[,1:4]),]
print(subset(prt.dat,value!=0),row.names=FALSE)

#/* ========================================================================*/
#'# Data Parsing and Missing Values
#' The first check is of the ability of R to "parse" the data - parsing here
#'  means that the data is converted from its native storage format (e.g. as
#'  characters, integers, real numbers) to an internal representation in R.
#'  R requires that all elements in a "column" have the same data type- Excel
#'  does not and allows characters and numbers to mixed together in the same
#'  column. A failure to parse properly therefore indicates a value 
#'  that is not in agreement with the expected format
#/* ========================================================================*/
#First, expect that we have all the column names that we expect to retain
#as characters
allowed.char.cols <- c("Unique.ID","Component","Country","Vessel","Gear","E.W",
                       "HALFST")
if(any(!allowed.char.cols %in% colnames(dat))) {
  stop("Expected character columns are missing")
}

#Now convert the other columns to numerics
other.cols <- colnames(dat)[!colnames(dat) %in% allowed.char.cols]
parsed.cols <- lapply(dat[other.cols],function(x) {
                  x.clean <- gsub(",",".",x)
                  return( suppressWarnings(as.numeric(x.clean)))})

#Estimate the parsing failures vs the missing values
dat.missing <- colSums(sapply(dat,is.na))
parsed.failures <- colSums(sapply(parsed.cols,is.na))
parsing.sum <- merge(melt(dat.missing),melt(parsed.failures),
                     by="row.names",sort=FALSE,all=TRUE)
colnames(parsing.sum) <- c("col.name","missing.in.src",
                           "missing.after.parsing")
parsing.sum$parse.errors <- parsing.sum$missing.after.parsing - 
                              parsing.sum$missing.in.src

#only print the values where there is a non zero
par.sum.print <- subset(parsing.sum[,-3],
                        rowSums(sapply(parsing.sum[2:3],function(x)x!=0))!=0)
print(par.sum.print)

#Add parsed values back into the data
dat.raw <- dat
dat[names(parsed.cols)] <- parsed.cols

#/* ========================================================================*/
#'# Error detection Algorithms
#' The following tests check for specific types of errors that may, or may not
#' occur in the database. Where an error is detected, some of the key details
#' of the database row that contain that value are given, unless there are more
#' than 250 such values. Note that the values of the columns of concern are
#' pre-parsing, which generally makes it easier to interpret.
#/* ========================================================================*/
#First, setup a display function
disp.err <- function(test,colnames,from=dat.raw) {
  idxs <- which(test)
  if(length(idxs) ==0) {
    cat("No errors detected\n") 
  } else if(length(idxs)>250) {
    cat(sprintf("Errors detected in %i rows. \n",length(idxs)))
    #print(d$Unique.ID)
  } else {
    print(from[idxs,c("Unique.ID","Year","Country","Vessel",colnames)],
          row.names=TRUE)
  }
}

#'#### Raising factor (MacFactor) Missing / Failed to Parse
disp.err(is.na(dat$MacFactor),c("MacFactor"))

#'#### Raising factor is less than 1
disp.err(dat$MacFactor<1,"MacFactor")
dat$raising.factor <- ifelse(is.na(dat$MacFactor),1,
                             dat$MacFactor) #If missing, set to 1
dat$raising.factor <- ifelse(dat$raising.factor<1,    #If less than 1, its been inverted
                             1/dat$raising.factor,dat$raising.factor)


#'#### Sampling depth (Sdepth) Missing / Failed to Parse
disp.err(is.na(dat$Sdepth),c("Sdepth"))

#'####  Mac1 missing / failed to parse
disp.err(is.na(dat$Mac1) , "Mac1")

#'#### Both Mac1 and MacStage1 data are missing
#'If two out of Mac1, MacStage1 and MacFactor are present, it is possible to calculate
#'the missing value. But if there are two missing, there is a problem
disp.err(is.na(dat$Mac1) & is.na(dat$MacStage1), 
         c("Mac1","MacStage1"))

#'#### Temperature at both 5m and 20m missing
disp.err(is.na(dat$TempSur.5m.)&is.na(dat$Temp20m),
         c("TempSur.5m.","Temp20m"))

#'#### No MEGS survey in given year
disp.err(!(dat$Year %in% seq(1977,2013,by=3)), NULL)

#'#### Hours are nonsensical
disp.err(!(dat$Hour %in% c(NA,0:24)), "Hour")

#'#### Minutes are nonsensical
disp.err(!(dat$Minutes %in% c(NA,0:60)), "Minutes")

#/* ========================================================================*/
#'# Spatial integrity of the data
#' The following tests check for errors in the spatial coordinates (decLon, 
#' decLat). 
#/* ========================================================================*/
#' #### Missing spatial coordinates
sp.missing <-  is.na(dat$declon) | is.na(dat$declat)
disp.err(sp.missing,c("declon","declat"))

#Create spatial object
dat.sp <- subset(dat,!sp.missing)
coordinates(dat.sp) <- ~ declon + declat
proj4string(dat.sp) <- CRS("+proj=longlat")

#Extract coastlines
map.dat <- map("worldHires",
               xlim=bbox(dat.sp)["declon",],
               ylim=bbox(dat.sp)["declat",],
               plot=FALSE,fill=TRUE,res=0)
map.sp <- map2SpatialPolygons(map.dat,map.dat$names)
proj4string(map.sp) <- proj4string(dat.sp)

#Test for points on land
onland <- !is.na(over(dat.sp,map.sp))
onland.ids <- dat.sp$Unique.ID[onland]

#'#### Points on land
disp.err(dat$Unique.ID %in% onland.ids,c("declon","declat"))

#'#### Plot spatial distribution
#'Check here that all of the points appear within the expected domain. Points
#'identified as being on land above are plotted in red - all other wet points
#'are plotted in blue.
plot(dat.sp,pch=16,cex=0.5,col="blue")
plot(map.sp,add=TRUE,col="grey")
box()
plot(dat.sp,add=TRUE,
     pch=16,col=ifelse(onland,"red",NA),)

#/* ========================================================================*/
#'# Volume Filtered
#' The volume filtered is key to the successful calculation of egg production.
#' Errors here can easily be problematic
#/* ========================================================================*/
#'#### Volume Filtered Missing Values
disp.err(is.na(dat$VolFilt),"VolFilt")

#'#### Volume Filtered by Gear / Year
#'Severe outliers here can be indicative of calibration errors or data entry
#'problems. The data is grouped acording to gear, to give an idea of what is 
#'"normal" for that gear
bwplot(VolFilt~factor(Year) | Gear,data=dat,
       scales=list(x=list(rot=90,alternating=FALSE),y=list(log=10)),
       as.table=TRUE,
       xlab="Year",ylab="Volume Filtered")

#/* ========================================================================*/
#'# Derived Values
#' The following tests calculate values that are key for subsequent modelling of
#' the Egg production. These problems will be identified when, for example, one
#' or more of these values is miissing.
#/* ========================================================================*/
#'#### Specification of time
#'In cases where the Hour and Minutes are missing, we set the value to 12
#'and 30 accordingly.
dat$POSIX <- with(dat,
                  ISOdate(Year,Month,Day,
                               ifelse(is.na(Hour),12,Hour),
                               ifelse(is.na(Minutes),30,Minutes),
                               00,tz="GMT"))
disp.err(is.na(dat$POSIX),c("Year","Month","Day","Hour","Minutes"))

#'#### Calculation of the Egg Development time
#'Errors here can arise due to missing temperature data. Temperature data is 
#'preferentially used at 20 m - if it is absent, then 5m is used.
dat$Temp <- ifelse(!is.na(dat$Temp20m),dat$Temp20m,dat$TempSur.5m.)
disp.err(is.na(dat$Temp),c("Temp","Temp20m","TempSur.5m."),from=dat)

#'These errors can also propigate through to the estimated egg development time
dat$dev.time <- exp(-1.31*log(dat$Temp)+6.90) #Based on Mendiola et al. (2006)
disp.err(is.na(dat$dev.time),c("Temp","dev.time"),from=dat)

#'#### Offset factor
#'The offset factor is the relationship between the local egg production and
#'the number of eggs that are actually counted. Problems here can arise from
#'any one of these factors.
dat <- transform(dat,offset.factor=VolFilt*dev.time/raising.factor/Sdepth)
disp.err(is.na(dat$offset.factor),
         c("VolFilt","Sdepth","raising.factor","dev.time"),from=dat)

#/* ========================================================================*/
#   Complete
#/* ========================================================================*/
#'
#'-----------
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
#   - If you don't like the format that spin() generates directly, you can
#     use the following approach
#       > library(markdown)
#       > options("markdown.HTML.stylesheet"="/usr/lib/rstudio/resources/markdown.css")
#       > spin("R_template.r")
#       > markdownToHTML("R_template.md","R_template.html",
#               stylesheet="/usr/lib/rstudio/resources/markdown.css"
#     if you're really keen. Alternatively, open the corresponding Rmd and compile
#     it using Rstudio instead.
#   - Add markdown directly using #'
#   - Add code chunk options directly using #+
#


