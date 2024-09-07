# FOR ANALYZING A .out FILE
# First: read in an outputs file: 
rm(list=ls()) # get rid of prior variables # I need to make this more efficient... 
require(tidyverse)
require(ggplot2)
require(beepr)
options(scipen=100) # turns off scientific notation so that reading in file names goes smoothly. 
save.all.outputs <- TRUE # if this is true, saves a csv for each prime dur - otherwise, ONLY saves thresh.df with RTs for examples that cross threshold. 
thresh.forticks <- 0.5
num.updates.filename <- 17200 # For 8t6i6o language: 15,000 and 23,000 and 32,900 are options. For 14t4i2o: 5,000 and 10,000 and 17,200
net.name <- "dynamorph"
targ.time <- 6.0
ticks.perI <- 5 # 5 ticks per interval for this network - ** CHECK THIS ** 
example.name <- "2021-01-19_14t4i2oexamples" # "2021-01-18_8t6i6oexamples"
ex.name.short <- "14t" # "8t"
prime.df.filename <- paste("primetestpairsdf_",example.name,".txt",sep="") # use primetestpairsdf_encbestunrel_ for 20t. 
language <- strsplit(strsplit(example.name,"examples")[[1]][1],"_")[[1]][2]
prime.durs <- c("0.2prime0.0ISI4.0start",# "0.4prime0.0ISI4.0start",
                "0.6prime0.0ISI4.0start", # "0.8prime0.0ISI4.0start",
                "1.0prime0.0ISI4.0start", # "1.2prime0.0ISI4.0start",
                "1.4prime0.0ISI4.0start",  # "1.6prime0.0ISI4.0start",
                "1.8prime0.0ISI4.0start") # , "2.0prime0.0ISI4.0start")
tag <- "" # could be oneperword
path <- "~/Users/username/Desktop/recurnet_morphproc/2_testing/14t4i2o_language"
setwd(path)

take.first <- function(x){as.numeric(strsplit(x," ")[[1]][1])}
take.second <- function(x){suppressWarnings(as.numeric(strsplit(x," ")[[1]][2]))}#suppress warnings means it'll turn "-" to NA without generating warning. 

for (prime.dur in prime.durs) {
  print(prime.dur)
  isi <- as.numeric(strsplit(strsplit(prime.dur,"ISI")[[1]][1],"prime")[[1]][2])
  prime.time <- as.numeric(strsplit(strsplit(prime.dur,"ISI")[[1]][1],"prime")[[1]][1])
  start.time <- as.numeric(strsplit(strsplit(prime.dur,"ISI")[[1]][2],"start")[[1]][1])
  
  print("reading in file")
  if (tag == "oneperword") {
    outs.raw <- read.table(paste(net.name,".",num.updates.filename,"primetest_",tag,"_",prime.dur,"_",example.name,".out",sep=""), header=FALSE,na.strings="-")
    diffpos <- FALSE
  } else {outs.raw <- read.table(paste(net.name,".",num.updates.filename,"primetest_",prime.dur,"_",example.name,".out",sep=""), header=FALSE,na.strings="-")
  diffpos <- TRUE # denotes that there are (or arent) more than one example per word - three positions?
  }

  num.updates <- as.integer(outs.raw[1,1])
  if (num.updates != num.updates.filename) {print("Issue: filename num_updates don't match recorded num_updates")}
  
  # initial read-through to initialize variables
  print("initializing variables")
  ex.start.inds <- which(outs.raw$V1 == num.updates)
  example.nums <- outs.raw$V2[ex.start.inds]
  ticks.on.example <- outs.raw$V1[ex.start.inds+1]
  num.units <- outs.raw$V1[ex.start.inds+3]
  num.groups <- outs.raw$V2[ex.start.inds+1]
  if (sum(num.groups > 1)) {print("there's more than one output group!")}
  
  # example.nums = c()
  # ticks.on.example = c()
  # num.units = c()
  # num.groups = c()
  # ex.start.inds = c()
  # for (i in 1:length(outs.raw)) {
  #   if (take.first(outs.raw[i]) == num.updates) {
  #     example.nums <- c(example.nums,take.second(outs.raw[i]))
  #     ticks.on.example <- c(ticks.on.example, take.first(outs.raw[i+1]))
  #     num.units <- c(num.units, take.first(outs.raw[i+3]))
  #     num.groups <- c(num.groups, take.second(outs.raw[i+1]))
  #     ex.start.inds <- c(ex.start.inds,i)
  #     if (num.groups[length(num.groups)] > 1) {
  #       for (ni in 2:num.groups[length(num.groups)]) {
  #         # I need to figure out how many units are in the other groups 
  #         num.units <- c(num.units, take.first(outs.raw[i+4+num.units[length(num.units)]]))
  #       }
  #     }
  #   }
  # }
  
  num.examples = max(example.nums)+1
  if (sum(num.groups == max(num.groups)) != length(num.groups)) {print("inconsistent num groups")} else {num.groups = num.groups[1]}
  if (num.groups != length(unique(num.units))) {print("inconsistent num units")} else {num.units = unique(num.units)}
  if (sum(ticks.on.example == max(ticks.on.example)) != length(ticks.on.example)) {print("inconsistent num ticks on example")} else {num.ticks = ticks.on.example[1]}
  # check that for ex.start.inds, all element differences are same and they equal what they should 
  ex.start.diffs <- ex.start.inds[2:length(ex.start.inds)] - ex.start.inds[1:(length(ex.start.inds)-1)]
  if (sum(ex.start.diffs==ex.start.diffs[1])!=length(ex.start.diffs)){print("Diff example lengths")}
  if (ex.start.diffs[1] != (((num.units+2)*num.ticks)+2)){print("unexpected ex.start.diff given num ticks and num units")}
  
  
  print(paste("num examples", num.examples))
  print(paste("num groups", num.groups))
  print(paste("num units", paste(num.units,collapse=", ")))
  print(paste("num ticks", num.ticks))
  
  # make a version of outsraw with no headings, just output/target lines
  skip.rows.per.ex<-c(0,1,2+(num.units+2)*((1:num.ticks)-1),3+(num.units+2)*((1:num.ticks)-1))
  ex.track <- rep(ex.start.inds,each=length(skip.rows.per.ex))
  skip.rows <- ex.track + rep(skip.rows.per.ex,num.examples)
  outs.raw.noh <- outs.raw[-skip.rows,] # outs.raw without the headings 
  # split.line <- function(x){suppressWarnings(as.numeric(strsplit(x," ")[[1]]))}#suppress warnings means it'll turn "-" to NA without generating warning. 
  
  # now make the rows of primetest.out.df 
  # ptm <- proc.time()
  # outsmat <- sapply(outs.raw.noh,split.line)
  # proc.time()-ptm
  print("constructing data frame")
  unit <- rep(rep(1:num.units,num.ticks),num.examples)
  tick <- rep(rep(1:num.ticks,each=num.units),num.examples)
  group <- rep(1,length(outs.raw.noh))
  example <- rep(example.nums,each=num.units*num.ticks)
  primetest.out.df = data.frame(example,tick,group,unit,output=outs.raw.noh$V1,target=outs.raw.noh$V2)
  
  # sorting out timing - chop off empty beginning, adjust all ticks and prime / target onsets accordingly
  ## NOTE - commented line below allows you to chop off activations before the prime turns on
  
  # save it before R crashes again.
  if (save.all.outputs == TRUE) {
    write.csv(primetest.out.df,paste(paste("outdf_train",paste(num.updates,"epochs",sep=""),example.name,prime.dur,sep="_"),".csv",sep=""),row.names = FALSE)
  }
  
  
  # print("calculating reaction times")
  # # now calculate thresholds such that no outputs before target onset are counted - eliminate those already above.
  # # start.tick <- 1 # don't drop early parts of example.
  # start.tick <- (start.time+isi+prime.time)*ticks.perI-ticks.perI # leave one time interval before target turns on
  # 
  # require(tidyverse)
  # to.plot.df <- filter(primetest.out.df,tick>=start.tick)
  # to.plot.df$tick <- to.plot.df$tick - (start.tick-1)
  # targ.onset <- ticks.on.example[1]-targ.time*ticks.perI - (start.tick-1)
  # prime.onset <- targ.onset-(isi+prime.time)*ticks.perI
  # 
  # # If I want to read this in again and start analysis from there:
  # # to.plot.df <- read.csv(paste("outdf_train",paste(num.updates.filename,"epochs.csv",sep=""),sep="_"))
  # # ticks.on.example <- max(to.plot.df$tick)
  # 
  # se <- function(v) {sd(v[which(!is.na(v))])/sqrt(length(which(!is.na(v))))}
  # pd = position_dodge(0.4)
  # 
  # # find the point for each example where mean of output activations surpasses thresh.forticks
  # thresh.df <- to.plot.df %>%
  #   filter(target==1) %>% # get rid of ticks before targ turns on
  #   group_by(example, tick) %>%
  #   summarise(mn.out=mean(output))%>% # calculate mean output
  #   filter(mn.out>thresh.forticks) %>% # filter(mn.out>thresh.forticks)
  #   summarise(RT = ifelse(min(tick)==targ.onset,NA,min(tick))) # if it's already past thresh when targ turns on, don't count it.
  # write.table(thresh.df,paste("RTs_",num.updates,"_",example.name,"_",prime.dur,"_",thresh.forticks,"thresh.txt",sep=""),quote=FALSE,row.names=FALSE)
}
beep()
options(scipen=0)






