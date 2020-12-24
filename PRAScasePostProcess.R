rm(list=ls())
install.packages("readxl")

require(ggplot2)
require(readxl)
require(openxlsx)
require(plyr)
require(lubridate)
library(reshape2)
library(skimr)
library(dplyr)
#library(sjPlot)
library(table1)
library(xtable)

baseWD <- "C:/Users/llavin/Desktop/cases12.23"
setwd(paste(baseWD, "", sep="/"))

readFile <- function(filename,filetype,WD,directory){
  csvname <- paste0(filename,"_",filetype,".csv")
  setwd(paste(WD,directory,sep="/"))
  df <- read.csv(csvname,header=FALSE)
  #print(rownames(df))
  return(df)
}

loadResults <- function(WD,casename){
  resultsWD <-paste(WD, "results", sep="/")
  setwd(WD)
  #load xlsx files
  MISO_transmission <- read_excel("NREL-Seams Model (MISO).xlsx", sheet = "Transmission")
  MISO_generation <- read_excel("NREL-Seams Model (MISO).xlsx", sheet = "Generation")
  MISO_load <- read_excel("NREL-Seams Model (MISO).xlsx", sheet = "Load")
  MISO_mapping <- read_excel("NREL-Seams Model (MISO).xlsx", sheet = "Mapping")
  
  #load results-related csvs
  regionEUE <- readFile(casename,"regioneue",resultsWD,"metricresults")
  regionLOLE <- readFile(casename,"regionlole",resultsWD,"metricresults")
  periodEUE <- readFile(casename,"periodeue",resultsWD,"metricresults")
  periodLOLP <- readFile(casename,"periodlolp",resultsWD,"metricresults")
  regionperiodEUE <- readFile(casename,"regionperiodeue",resultsWD,"metricresults")
  regionperiodLOLP <- readFile(casename,"regionperiodlolp",resultsWD,"metricresults")
  utilizations <- readFile(casename,"utilizations",resultsWD,"metricresults")
  flows <- readFile(casename,"flows",resultsWD,"metricresults")
  
  #clean and format to concatenate
  #modelLMP <- AddDatetime(modelLMP)
  
  # return results
  results <- list(regionEUE,regionLOLE,periodEUE,periodLOLP,regionperiodEUE,regionperiodLOLP,utilizations,flows)
  names(results) <- c("regionEUE","regionLOLE","periodEUE","periodLOLP","regionperiodEUE","regionperiodLOLP","utilizations","flows")
  return(results)
}

loadAllCases <- function(dates,folder="303.301SS_Wind303"){
  results <- loadResults(dates,folder)
  return(results)
}

plotLOLPtimeseries <- function(rlist){
  periodLOLP <- rlist$periodLOLP
  periodLOLP$time <- seq(from = 1, to = length(periodLOLP$V1), by = 1)
  periodLOLP$percentLOLP <- periodLOLP$V1*100 #scales frac to %
  
  ggplot(data=periodLOLP, aes(x=time,y=percentLOLP)) +
    geom_line() + theme_classic() + ylab("LOLP(%)") + xlab("Hour") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_blank(),
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("LOLPs are fun"))
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("test",".png"), width=12, height=6)
}

plotzonalIRMs

plotTXuse <- function(rlist){
  utilizations <- rlist$utilizations
  #print(utilizations)
  utilizations <- as.data.frame(t(utilizations)) #transpose
  utilizations$time <- seq(from = 1, to = length(utilizations$V1), by = 1)
  utilizations$percentuse <- utilizations$V1*100 #scales frac to %
  
  ggplot(data=utilizations, aes(x=time,y=percentuse)) +
    geom_line() + theme_classic() + ylab("Utilization(%)") + xlab("Hour") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_blank(),
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("LOLPs are fun"))
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("txtest",".png"), width=12, height=6)
}

results1 <- loadResults(baseWD,"VRE0.4_wind_2012base100%_8760_100%tx_18%IRM_0GWstorage_LAonly_addgulfsolar")

plotLOLPtimeseries(results1)
plotTXuse(results1)
