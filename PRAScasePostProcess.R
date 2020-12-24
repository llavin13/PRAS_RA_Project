rm(list=ls())
install.packages("readxl")
install.packages("BiocManager")
BiocManager::install("rhdf5")

require(rhdf5)
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
  
  setwd(paste(WD,"PRAS_files",sep="/"))
  #load HDF5 file
  loadDF <- t(as.data.frame(h5read(file = paste0(casename,".pras"), 
                        name = "regions/load")))
  genDF <- t(as.data.frame(h5read(file = paste0(casename,".pras"), 
                                  name = "generators/capacity")))
  
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
  results <- list(regionEUE,regionLOLE,periodEUE,periodLOLP,regionperiodEUE,regionperiodLOLP,utilizations,flows,loadDF,genDF)
  names(results) <- c("regionEUE","regionLOLE","periodEUE","periodLOLP","regionperiodEUE","regionperiodLOLP","utilizations","flows","load","gens")
  return(results)
}

loadAllCases <- function(dates,folder="303.301SS_Wind303"){
  results <- loadResults(dates,folder)
  return(results)
}

plotLOLPtimeseries <- function(rlist){
  
  for (i in 1:length(rlist)){
    rlist[[i]] <- rlist[[i]]$periodLOLP
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  periodLOLP <- do.call("rbind", rlist)
  
  periodLOLP$percentLOLP <- periodLOLP$V1*100 #scales frac to %
  
  ggplot(data=periodLOLP, aes(x=time,y=percentLOLP,color=label)) +
    geom_line(lwd=3) + theme_classic() + ylab("LOLP(%)") + xlab("Hour") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("LOLPs are fun"))
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("test",".png"), width=12, height=6)
}

#regionandperiod
plotregionalLOLPtimeseries <- function(rlist){
  
  for (i in 1:length(rlist)){
    rlist[[i]] <- rlist[[i]]$regionperiodLOLP
    rlist[[i]] <- as.data.frame(t(rlist[[i]]))
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  regionperiodLOLP <- do.call("rbind", rlist)
  #now run your melt
  regionperiodLOLP <- melt(regionperiodLOLP, id.vars = c("label","time"), measure.vars = c("V1","V2"))
  regionperiodLOLP$percentLOLP <- regionperiodLOLP$value*100 #scales frac to %
  
  ggplot(data=regionperiodLOLP, aes(x=time,y=percentLOLP,linetype=label,color=variable)) +
    geom_line(lwd=2) + theme_classic() + ylab("LOLP(%)") + xlab("Hour") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("regional LOLPs are fun"))
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("regionLOLPseries",".png"), width=12, height=6)
  return(regionperiodLOLP)
}

#stack EUEs? #prefer to stack and group zone capacities

plotzonalIRMs <- function(rlist){
  return(rlist)
}

plotTXuse <- function(rlist){
  for (i in 1:length(rlist)){
    rlist[[i]] <- rlist[[i]]$utilizations
    rlist[[i]] <- as.data.frame(t(rlist[[i]])) #transpose
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  utilizations <- do.call("rbind", rlist)
  #utilizations <- rlist$utilizations
  #print(utilizations)
  #utilizations <- as.data.frame(t(utilizations)) #transpose
  #utilizations$time <- seq(from = 1, to = length(utilizations$V1), by = 1)
  utilizations$percentuse <- utilizations$V1*100 #scales frac to %
  
  ggplot(data=utilizations, aes(x=time,y=percentuse,color=label)) +
    geom_line(lwd=2) + theme_classic() + ylab("Utilization(%)") + xlab("Hour") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          plot.title = element_text(size = 40, face = "bold", hjust = 0.5),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20)) +
    ggtitle(paste("LOLPs are fun"))
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("txtest",".png"), width=12, height=6)
}

plotCAPstack <- function(rlist){
  for (i in 1:length(rlist)){
    rlist[[i]] <- as.data.frame(rlist[[i]]$load)
    #rlist[[i]] <- as.data.frame(t(rlist[[i]])) #transpose
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  loads <- do.call("rbind", rlist)
  loads <- melt(loads, id.vars = c("label","time"), measure.vars = c("V1","V2"))
  peakloads <- loads %>% 
    group_by(variable) %>% 
    summarise(peakLoad = max(value))
  #now make barplot of peak loads, and should eventually include gens
  
}
plotCAPstack(resultList)

results1 <- loadResults(baseWD,"VRE0.4_wind_2012base100%_48_100%tx_18%IRM_0GWstorage_LAonly_addgulfsolar")
results2 <- loadResults(baseWD,"VRE0.4_wind_2012base100%_48_0%tx_18%IRM_0GWstorage_LAonly_addgulfsolar")

resultList <- list(results1,results2)
names(resultList) <- c("100%Tx","0%Tx")

plotLOLPtimeseries(resultList)
a <- plotregionalLOLPtimeseries(resultList)
plotTXuse(resultList)
