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
    # we are looking for lines like
    #    Account Number: xxxxxxxxx
    # from which will will create
    #   allinfo['Acount Number'] = xxxxxxxxx

    # split the line into parts deliminted by :
    # subsitute all the white space away
    info <- sapply(strsplit(l,":"),function(x) gsub('^ *| *$','',gsub(' +',' ',x) ))
    # if this line has ':' demited data, info will have more than 1 item in it
    # in this case, we want to keep the info, so we save it in "allinfo"
    if(length(info)>1) {
     #info[1] (e.g. Account Number) is the variable name
     # everything else is the value, combine back with : 
     # (incase we had a time like 00:00:00)
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

  # baseless assumption:
  #  forward and dialed mean mostly the same thing 
  #  or at least neither exist at the same time
  unexpectedidx <- which( !is.na(d$FORWARDED) & !is.na(d$DIALED) )
  if(length(unexpectedidx) > 0L  ) warning('ATT from: bad assumption about DIAL and FORWARDED never conflicting! For indexes:',unexpectedidx)

  # make common ConnectedTo column from FORWARDED and DIALED
  noForward <- is.na(attfor1$FORWARDED)
  d$ConnectedTo <- attfor1$FORWARDED
  d$ConnectedTo[noForward] <- attfor1$DIALED[noForward]

  return(d)
}

read.forTmoble <-function(f) {
  # use dplyr "pipeline" style to 
  # rename and "mutate" (change) connected to
  d <-
    read_xlsx(f,skip=2) %>%
    rename(ConnectedTo ='Connected To') %>% 
    mutate(ConnectedTo = mknum(ConnectedTo)) # make phone number a number

  return(d)
}

read.CDR_Tmoble<-function(f) {
  d <- read_xlsx(f,skip=12)
  return(d)
}

###### 

# get file, Sys.glob to use  "*" instad of writting out the phone number
file.att.for.2 <- Sys.glob('data/ATT raw calls data for * 2nd file.txt')
attfor1 <- read.forATT(file.att.for.2)

file.tmobile.for <- Sys.glob('data/TMobile raw calls data for *.xlsx')
tmobilefor <- read.forTmoble(file.tmobile.for)

## some stats

# what numbers do these phones call in common
calledsame <- which(tmobilefor$ConnectedTo %in% unique(na.omit(attfor1$ConnectedTo)))
# none

## summary stats
tmbl.withcnt <- 
    # from tmobile data
    tmobilefor %>%  
    # trim columns and rename them to easier no space version
    select(Event=`Event Type`,ConnectedTo,Direction,Dur=`Duration(Seconds)`)  %>%
    # remove where there is no direction (only keep when not NA)
    filter(!is.na(Direction)) %>% 
    # run stats within each number (each number will have it's own count in column n)
    group_by(ConnectedTo) %>%  
    # generate counts (but don't collapse data -- mutate instead of summarise)
    # so still one row per contact instance but each number has it's n repeated for each instance
    mutate(n=n())  

## visualzie
# plot the summary, but only for call+sms
tmbl.withcnt %>% 
  filter(nchar(ConnectedTo)==11) %>% 
  # send the filter data to plot
  # noteice we use + instead of %>% to 'add' plot features (instead of pipeing)
  ggplot() + 
  # aesthetics: x is event, so is our color fill
  aes(x=Event,fill=Event) +
  # generate a histogram
  geom_histogram(stat='count') +
  # default theme is hard sometimes, so use black and white one
  theme_bw() +  
  ggtitle('frequence of event type for numbers contacted more than 11 times')

# actually summarize. 
# now we only have one row for each number + direction combo
tmbl.smry <- 
    tmbl.withcnt %>%
    group_by(Direction,ConnectedTo) %>%
    summarise( sumdur= sum(Dur), n=n() )

ggplot(tmbl.smry) + 
    # set x, y, and other aesthetics
    aes(x=Direction,y=sumdur,color=n,label=ConnectedTo) + 
    # and points to the graph
    geom_point()  +
    # use a line to connect numbers (group by the phone number)
    geom_line(aes(group=ConnectedTo)) +
    # label numbers with more than 15 contact events
    geom_label(data=tmbl.smry %>% filter(n>15),
               size=3, alpha=.3,
               vjust=-1,hjust=-.1) +
    theme_bw() +
    ggtitle('incoming vs outgoing duration + number of contacts (inc SMS)')
# save it
ggsave('inVsOut.png')
