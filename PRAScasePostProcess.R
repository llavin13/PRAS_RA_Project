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
require(cowplot)
#library(sjPlot)
library(table1)
library(xtable)

baseWD <- "C:/Users/llavin/Desktop/cases12.23"
setwd(paste(baseWD, "", sep="/"))

readFile <- function(filename,filetype,WD,directory,header=FALSE){
  csvname <- paste0(filename,"_",filetype,".csv")
  setwd(paste(WD,directory,sep="/"))
  df <- read.csv(csvname,header=header)
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
  geninfo <- as.data.frame(h5read(file = paste0(casename,".pras"), 
                                    name = "generators/_core"))
  
  geninfo$capacity <- as.vector(apply(genDF, 2, FUN=max))
  #colnames(geninfo) <- c("Generator.Name")
  genCSV <- readFile(casename,"gens",resultsWD,"metricresults",header=TRUE)
  genCSV <- merge(genCSV, geninfo, by.x = "Generator.Name", 
                     by.y = "name", all.x = FALSE, all.y = TRUE)
  
  #get geninfo into gencsv by merging
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
  results <- list(regionEUE,regionLOLE,periodEUE,periodLOLP,regionperiodEUE,regionperiodLOLP,utilizations,flows,loadDF,genDF,genCSV,MISO_mapping)
  names(results) <- c("regionEUE","regionLOLE","periodEUE","periodLOLP","regionperiodEUE","regionperiodLOLP","utilizations","flows","load","gens","gensdata","mapper")
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
          plot.title = element_blank(),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20))
    
  
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
  
  g<-ggplot(data=regionperiodLOLP, aes(x=time,y=percentLOLP,linetype=label,color=variable)) +
    geom_line(lwd=2) + theme_classic() + ylab("LOLP(%)") + xlab("Hour") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_blank(),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20))
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("regionLOLPseries",".png"), width=12, height=6)
  return(g)
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
  
  g<-ggplot(data=utilizations, aes(x=time,y=percentuse,color=label)) +
    geom_line(lwd=2) + theme_classic() + ylab("Utilization(%)") + xlab("Hour") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          plot.title = element_blank(),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20))
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("txtest",".png"), width=12, height=6)
  return(g)
}

plotCAPstack <- function(rlist){
  mapDF <- rlist[[1]]$mapper
  #load data
  len <- length(rlist)
  nlist <- list()
  for (i in 1:len){
    nlist[[i]] <- as.data.frame(rlist[[i]]$gensdata)
    rlist[[i]] <- as.data.frame(rlist[[i]]$load)
    
    #rlist[[i]] <- as.data.frame(t(rlist[[i]])) #transpose
    nlist[[i]]$label <- names(rlist[i])
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  
  loads <- do.call("rbind", rlist)
  
  loads <- melt(loads, id.vars = c("label","time"), measure.vars = c("V1","V2"))
  peakloads <- loads %>% 
    group_by(variable,label) %>% 
    summarise(peakLoad = max(value))
  #gen data
  gensdata <- do.call("rbind",nlist)
  gencap <- gensdata %>% group_by(category.y,region,label) %>% summarise(capacity=sum(capacity))
  gencap$type <- rep("Gen",length(gencap$region))
  
  #aggregate labels?
  gencap <- gencap[gencap$label==gencap$label[1],]
  peakloads <- peakloads[peakloads$label==peakloads$label[1],]
  #add rows with loads
  for (i in 1:length(peakloads$variable)){
    gencap[nrow(gencap) + 1,] = list("Load",unique(gencap$region)[i],peakloads$label[i],peakloads$peakLoad[i],"Load")
  }
  
  #filter for mapping
  colnames(mapDF) <- c("ID","ISO","Bus") 
  mapNames <- mapDF %>% filter(
    ID %in% as.numeric(unique(gencap$region))
  )
  
  gencap$Bus <- mapvalues(gencap$region, 
                                   from=unique(gencap$region), 
                                   to=mapNames$Bus)
  
  
  gencap$category.y <- factor(gencap$category.y, levels = c("Load", "Distributed_Solar", "Utility_Solar",
                                                            "Utility_Wind", "Gas_ST","Gas_GT","CC","Coal_ST",
                                                            "Waste HT_ST","Biomass","Nuclear","Hydro"))
  fillcolors <- c("grey","red","yellow","cyan","orange","orange","orange",
                  "black","brown","green","purple","blue")
  #plotting grouped stacked bars
  g <- ggplot(gencap, aes(x = type, y = capacity, fill = category.y)) + 
    geom_bar(stat = 'identity', position = 'stack') + facet_grid(~ Bus) +
    theme_classic() + xlab("") +
    scale_fill_manual(values=fillcolors) +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          plot.title = element_blank(),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          strip.text.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20))
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("stackgentest",".png"), width=12, height=6)
  return(g)
}
g<-plotCAPstack(resultList)

results1 <- loadResults(baseWD,"VRE0.4_wind_2012base100%_48_100%tx_18%IRM_0GWstorage_LAonly_addgulfsolar")
results2 <- loadResults(baseWD,"VRE0.4_wind_2012base100%_48_0%tx_18%IRM_0GWstorage_LAonly_addgulfsolar")
resultList <- list(results1,results2)
names(resultList) <- c("100%Tx","0%Tx")

a <- plotregionalLOLPtimeseries(resultList)
b<-plotCAPstack(resultList)
c<-plotTXuse(resultList)

#make a multi-panel plot
grid <- plot_grid(a, b, labels = c('A', 'B'), label_size = 32)

# first align the top-row plot (p3) with the left-most plot of the
# bottom row (p1)
plots <- align_plots(a, c, align = 'v', axis = 'l')
# then build the top row
top_row <- plot_grid(plots[[1]], b, labels = c('A', 'B'),
                     rel_widths = c(1, .5),label_size = 32)
# then combine with the top row for final plot
grid <- plot_grid(top_row, plots[[2]], labels = c('', 'C'), label_size = 32, ncol = 1)

setwd(paste(baseWD, "results", "Rfigures", sep="/"))
ggsave(paste0("gridplot",".png"), width=24, height=12)
