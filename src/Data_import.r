#+ setup, include=FALSE,results="hide",messages=FALSE
#  ##########################################################################
#' MEGS Data Import
#' =====================================================================
#'
#' by Mark R Payne
#' DTU-Aqua, Charlottenlund, Denmark
#' mpa@aqua.dtu.dk
#'
#' Mon Feb 17 10:02:58 2014
#'
#/* Imports the MEGS data directly from the source excel spreadsheet into
#   an R data object, which we can run further with
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
cat(sprintf("\n%s\n","MEGS Data Import"))
cat(sprintf("Analysis performed %s\n\n",date()))

log.msg <- function(fmt,...) {cat(sprintf(fmt,...));flush.console();
                              return(invisible(NULL))}
library(sp)
library(tools)

#Start recording from here
opts_chunk$set(results="markup",echo=FALSE)

#/* ========================================================================*/
#  Load data
#/* ========================================================================*/
#Identify input data source
fname <- dir("data",pattern=".*csv$",full.names=TRUE)
if(length(fname)!=1) {
  stop("Problem with data source definition. Please ensure that the data directory ",
       "contains one and only one .csv data file")
}

# File details
log.msg("Calculating MD5 Checksum...\n")
f.details <- data.frame(filesize=file.info(fname)$size,
             "last modification time"=file.info(fname)$mtime,
             "md5 Checksum"=md5sum(fname))

#Read in data from the CSV.
log.msg("Loading data...")
dat <- read.csv2(fname,colClasses="character") #Read all as character

#Set rownames. We use the rownumbers from the csv, assuming the header to
#be row 1 and the data starting on row 2 - hopefully these
#should help find the problem quickly
rownames(dat) <- seq(nrow(dat))+1

#Strip out padding rows, defined here as all elements
#being NA
NA.mat <- sapply(dat,is.na)
n.NAs <- rowSums(NA.mat)
mt.row <- n.NAs==ncol(NA.mat)
dat <- subset(dat,!mt.row)

#Attach metadata to object
attr(dat,"source.details") <- f.details

#Save data
save(dat,file="objects/EP_data_raw.RData")

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


