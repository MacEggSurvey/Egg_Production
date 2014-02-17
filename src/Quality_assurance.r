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
library(xlsx)
library(tools)
library(reshape)
library(maps);library(mapdata);library(maptools)

#Start recording from here
opts_chunk$set(results="markup",echo=FALSE)
#+results="asis"
cat(sprintf("Analysis performed %s\n\n",date()))

#/* ========================================================================*/
#'# Introduction
#'  This work documents the results of quality assurance checks on the the
#'  MEGS database. The details of the analysed file are as follows:
#/* ========================================================================*/
#Load data file
load("objects//EP_data.RData")

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
#'#### Gear by Year
print(table(Gear=dat$Gear,Year=dat$Year),zero.print=".")

#'#### Vessel by Year
print(table(Vessel=dat$Vessel,Year=dat$Year),zero.print=".")

#'#### Country by Year
print(table(Country=dat$Country,Year=dat$Year),zero.print=".")

#'#### Period by Year
print(table(Year=dat$Year,Period=dat$Period),zero.print=".")

#'#### Gear used by each vessel in each year
print(table(Vessel=dat$Vessel,Gear=dat$Gear,Year=dat$Year),zero.print=".")

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
allowed.char.cols <- c("UniqueID","Component","Country","Vessel","Gear","E.W",
                       "half_rect","Notes")

if(any(!allowed.char.cols %in% colnames(dat))) {
  stop("Expected character columns are missing")
}

#Now convert the other columns to numerics
other.cols <- colnames(dat)[!colnames(dat) %in% allowed.char.cols]
parsed.cols <- suppressWarnings(lapply(dat[other.cols],as.numeric))

#Estimate the parsing failures vs the missing values
dat.missing <- colSums(sapply(dat,is.na))
parsed.failures <- colSums(sapply(dat,is.na))
parsing.sum <- merge(melt(dat.missing),melt(parsed.failures),
                     by="row.names",sort=FALSE,all=TRUE)
colnames(parsing.sum) <- c("col.name","missing.in.src",
                           "missing.after.parsing")
parsing.sum$parse.errors <- parsing.sum$missing.after.parsing - 
                              parsing.sum$missing.in.src

#only print the values where there is a non zero
par.sum.print <- subset(parsing.sum,
                        rowSums(sapply(parsing.sum[2:4],function(x)x!=0))!=0)
print(par.sum.print)

#Add parsed values back into the data
dat[names(parsed.cols)] <- parsed.cols

#/* ========================================================================*/
#'# Error detection Algorithms
#' The following tests check for specific types of errors that may, or may not
#' occur in the database
#/* ========================================================================*/
#First, setup a display function
disp.err <- function(d,colnames) {
  if(nrow(d) ==0) {
    cat("No errors detected\n") 
  } else {
    print(d[,c("UniqueID","Country","Vessel",colnames)])
  }
}

#'#### Raising factor is less than 1
disp.err(subset(dat,MacFactor<1),"MacFactor")
dat$raising.factor <- ifelse(is.na(dat$MacFactor),1,
                             dat$MacFactor) #If missing, set to 1
dat$raising.factor <- ifelse(dat$raising.factor<1,    #If less than 1, its been inverted
                             1/dat$raising.factor,dat$raising.factor)

#'#### Both Mac1 and Stage1 data are missing
disp.err(subset(dat,is.na(Mac1) & is.na(MacStage1)), 
         c("Mac1","MacStage1"))

#'#### Temperature at both 5m and 20m missing
disp.err(subset(dat,is.na(Temp5m)&is.na(Temp20m)),
         c("Temp5m","Temp20m"))

#'#### No MEGS survey in give year
disp.err(subset(dat,!(Year %in% seq(1977,2013,by=3))),
         NULL)

#'#### Hours or minutes are nonsensical
disp.err(subset(dat,!(Hour %in% 0:24) | !(Minutes %in% 0:60)),
         c("Hour","Minutes"))

#/* ========================================================================*/
#'# Distribution of samples
#' The following tests check for errors in the spatial coordinates (decLon, 
#' decLat). Points that are on land are marked in red.
#/* ========================================================================*/
#Create spatial object
dat.sp <- dat
coordinates(dat.sp) <- ~ declon + decLat
proj4string(dat.sp) <- CRS("+proj=longlat")

#Extract coastlines
map.dat <- map("worldHires",
               xlim=bbox(dat.sp)["declon",],
               ylim=bbox(dat.sp)["decLat",],
               plot=FALSE,fill=TRUE,res=0)
map.sp <- map2SpatialPolygons(map.dat,map.dat$names)
proj4string(map.sp) <- proj4string(dat.sp)

#Test for points on land
on.land <- over(dat.sp,map.sp)

#Plot spatial distribution
plot(dat.sp,pch=NA)
plot(map.sp,add=TRUE,col="grey")
box()
plot(dat.sp,add=TRUE,
     pch=16,col=ifelse(is.na(on.land),"black","red"),
     cex=ifelse(is.na(on.land),0.5,1))

#' Points on land
disp.err(subset(dat,!is.na(on.land)),c("declon","decLat"))

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
disp.err(subset(dat,is.na(dat$POSIX)),c("Year","Month","Day","Hour","Minutes"))

#'#### Calculation of the Egg Development time
#'Errors here can arise due to missing temperature data. Temperature data is 
#'preferentially used at 20 m - if it is absent, then 5m is used.
dat$Temp <- ifelse(!is.na(dat$Temp20m),dat$Temp20m,dat$Temp5m)
disp.err(subset(dat,is.na(dat$Temp)),c("Temp","Temp20m","Temp5m"))

#'These errors can also propigate through to the estimated egg development time
dat$dev.time <- exp(-1.31*log(dat$Temp)+6.90) #Based on Mendiola et al. (2006)
disp.err(subset(dat,is.na(dat$dev.time)),c("Temp","dev.time"))

#'#### Offset factor
#'The offset factor is the relationship between the local egg production and
#'the number of eggs that are actually 
dat <- transform(dat,offset.factor=VolFilt*dev.time/raising.factor/Sdepth)
disp.err(subset(dat,is.na(dat$dev.time)),
         c("VolFilt","Sdepth","raising.factor","dev.time"))

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


