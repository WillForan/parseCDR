library(dplyr)
library(readxl)
library(lubridate)

# WF 20171031 -- inital effort to read in phone data with toy examples

### define some functions

# make "(444) 555-1234" 14445551234
mknum <- function(n) {
 # remove any parens (), spaces, or hypens - 
 n<-gsub('[-() ]','',n)
 # add a 1 if number is too short
 if(nchar(n) == 10 ) n<-paste0('1',n)
 # send back a number
 return(as.numeric(n))
}

# function to read ATT "from file" 
# looks like normal data, but has 13 lines of header
read.forATT <- function(f) {
  ## tricky part, read preamble
  # initilaze the file connection, line count, and first line
  con <- file(f,'r')
  nlines.preamble <- 0
  l<-readLines(con,n=1)
  allinfo <- list()
  # read lines until we hit the end of the file or more likely 
  # we match the header line (any line with at least 3 commas anywhere on it)
  while( !is.null(l) && !grepl(',.*,.*,',l) ) { 
    # read the next line
    # increment the number of lines we've seen (for skipping later)
    nlines.preamble <- nlines.preamble + 1
    # split the line into parts deliminted by :
    # subsitute all the white space away
    info <- sapply(strsplit(l,":"),function(x) gsub('^ *| *$','',gsub(' +',' ',x) ))
    # if have : demited ata, info will have more than 1 item in it
    if(length(info)>1) {
     allinfo[ info[1] ] <- paste(info[-1],collapse=":")
    }
    # read the next line
    l<-readLines(con,n=1)
  }
  close(con)
  
  # read in rest of data:
  # ATT files have 13 lines of preamble (skip=13)
  # and some line at the bottom (fill=T to ignore)
  d <- read.table(f,sep=",",skip=nlines.preamble,header=T,fill=T)

  # take info and make a repeated column in the data table
  d$MatterID      <- allinfo$`Matter ID`
  d$AccountNumber <- allinfo$`Account Number`
  d$FromNumber    <- mknum(allinfo$`Voice Usage For`)
  return(d)
}

read.forTmoble <-function(f) {
  d <- read_xlsx(f,skip=2)
  return(d)
}

read.CDR_Tmoble<-function(f) {
  d <- read_xlsx(f,skip=12)
  return(d)
}

###### 

# get file, Sys.glob  adn "*" instad of writting out the phone number
file.att.for.2 <- Sys.glob('data/ATT raw calls data for * 2nd file.txt')
attfor1 <- read.forATT(file.att.for.2)

file.tmobile.for <- Sys.glob('data/TMobile raw calls data for *.xlsx')
tmobilefor <- read.forTmoble(file.tmobile.for)


