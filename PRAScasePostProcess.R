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
  
  #ELCC stuff
  solarELCC <- readFile("solarELCC",casename,resultsWD,"ELCCresults",header=TRUE)
  storageELCC <- readFile("storageELCC",casename,resultsWD,"ELCCresults",header=TRUE)
  windELCC<- readFile("windELCC",casename,resultsWD,"ELCCresults",header=TRUE)
  perfectELCC <- readFile("perfectELCC",casename,resultsWD,"ELCCresults",header=TRUE)
  #clean and format to concatenate
  #modelLMP <- AddDatetime(modelLMP)
  
  #grab the utilization to get naming convention based on MW
  linecapacity <- as.numeric(flows[1,1]/utilizations[1,1])
  linelabel <- ceiling(linecapacity)
  #linelabel <- paste0(ceiling(linecapacity),"MW")

  # return results
  results <- list(regionEUE,regionLOLE,periodEUE,periodLOLP,regionperiodEUE,regionperiodLOLP,utilizations,flows,loadDF,genDF,genCSV,MISO_mapping,storageELCC,solarELCC,windELCC,perfectELCC,linelabel)
  names(results) <- c("regionEUE","regionLOLE","periodEUE","periodLOLP","regionperiodEUE","regionperiodLOLP","utilizations","flows","load","gens","gensdata",
                      "mapper","storageELCC","solarELCC","windELCC","perfectELCC","linelabel")
  return(results)
}

loadAllCases <- function(dates,folder="303.301SS_Wind303"){
  results <- loadResults(dates,folder)
  return(results)
}

plotLOLPbars <- function(rlist,caselabel){
  for (i in 1:length(rlist)){
    rlist[[i]] <- rlist[[i]]$periodLOLP
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  #convert time to day
  periodLOLP <- do.call("rbind", rlist)
  periodLOLP$percentLOLP <- periodLOLP$V1*100 #scales frac to %
  periodLOLP$day <- ceiling(periodLOLP$time/24.)
  periodLOLP$pctTx <- as.numeric(gsub("%.*", "", periodLOLP$label))
  #aggregate to counts
  
  aggperiodLOLP <- periodLOLP %>% 
    group_by(pctTx,day) %>% 
    summarise(count= sum(percentLOLP<.0001))
  aggperiodLOLP$day <- as.character(aggperiodLOLP$day)
  
  g<- ggplot(data=aggperiodLOLP, aes(x=pctTx,y=count,color=day)) +
    geom_line(lwd=3) + theme_classic() + ylab("Daily Zero LOLP count") + xlab("Tx%") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_blank(),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20))
  #do a bar plot by day
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("LOLPlines_",caselabel,"%IRM",".png"), width=12, height=6)
  
  aggperiodLOLP$pctTx <- as.factor(aggperiodLOLP$pctTx)
  gg <- ggplot(aggperiodLOLP, aes(pctTx, day, fill = count)) + 
    geom_tile(colour = "white") +
    scale_fill_gradient2(low="white",mid="red", high="white",
                         midpoint=12,limits=c(0,24)) +
    labs(x="%Tx",y="Day") + theme_classic()
  return(gg)
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
  colnames(regionperiodLOLP) <- c("LA-GULF","LA-N","label","time")
  #now run your melt
  regionperiodLOLP <- melt(regionperiodLOLP, id.vars = c("label","time"), measure.vars = c("LA-GULF","LA-N"))
  regionperiodLOLP$percentLOLP <- regionperiodLOLP$value*100 #scales frac to %
  manualtypes <- c("dotted","solid")
  regionperiodLOLP$label <- as.factor(as.numeric(regionperiodLOLP$label))
  
  g<-ggplot(data=regionperiodLOLP, aes(x=time,y=percentLOLP,linetype=variable,color=label)) +
    geom_line(lwd=2) + theme_classic() + ylab("LOLP(%)") + xlab("Hour") +
    scale_linetype_manual(values=manualtypes)+
    guides(color=guide_legend(title="Tx MW:"),
           linetype=guide_legend(title="Zone:",nrow=2)) +
    theme(legend.text = element_text(size=18),
          legend.title = element_text(size=24),
          legend.position = "bottom",
          plot.title = element_blank(),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20),
          plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
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
  utilizations$label <- as.factor(as.numeric(utilizations$label))
  
  g<-ggplot(data=utilizations, aes(x=time,y=percentuse,color=label)) +
    geom_line(lwd=2) + theme_classic() + ylab("Draws with constrained transmission (%)") + xlab("Hour") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_blank(),
          legend.title = element_blank(),
          legend.position ="none",
          plot.title = element_blank(),
          axis.title.y = element_text(size=18),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20),
          plot.margin=unit(c(1,1,1.5,1.2),"cm"))
  
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
    ylab("Capacity (MW)")+
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

plotELCCs <- function(rlist,caselabel){
  #load data
  len <- length(rlist)
  nlist <- list()
  windlist <- list()
  perfectlist <- list()
  for (i in 1:len){
    nlist[[i]] <- as.data.frame(rlist[[i]]$solarELCC)
    windlist[[i]] <- as.data.frame(rlist[[i]]$windELCC)
    perfectlist[[i]] <- as.data.frame(rlist[[i]]$perfectELCC)
    rlist[[i]] <- as.data.frame(rlist[[i]]$storageELCC)
    
    #rlist[[i]] <- as.data.frame(t(rlist[[i]])) #transpose
    nlist[[i]]$label <- names(rlist[i])
    windlist[[i]]$label <- names(rlist[i])
    perfectlist[[i]]$label <- names(rlist[i])
    rlist[[i]]$label <- names(rlist[i]) #may want better label
  }
  storage <- do.call("rbind", rlist)
  solar <- do.call("rbind",nlist)
  wind <- do.call("rbind",windlist)
  perfect <- do.call("rbind",perfectlist)
  #avg elcc column creation
  #eventually want some elif action for the 0/100s
  storage$avgelcc <- ifelse(storage$minelcc == 0, 0, (storage$minelcc+storage$maxelcc)/2.)
  storage$pctTx <- as.numeric(gsub("%.*", "", storage$label))
  storage$type <- rep("Storage (6h)",length(storage$label))
  
  solar$avgelcc <- ifelse(solar$minelcc == 0, 0, (solar$minelcc+solar$maxelcc)/2.)
  solar$pctTx <- as.numeric(gsub("%.*", "", solar$label))
  solar$type <- rep("Solar",length(solar$label))
  
  wind$avgelcc <- ifelse(wind$minelcc == 0, 0, (wind$minelcc+wind$maxelcc)/2.)
  wind$pctTx <- as.numeric(gsub("%.*", "", wind$label))
  wind$type <- rep("Wind",length(wind$label))
  
  perfect$avgelcc <- ifelse(perfect$minelcc == 0, 0, (perfect$minelcc+perfect$maxelcc)/2.)
  perfect$pctTx <- as.numeric(gsub("%.*", "", perfect$label))
  perfect$type <- rep("Perfect Generator",length(perfect$label))
  #print(solar)
  #print(storage)
  labs <- c("resourcename", "capacity","avgelcc","pctTx","type","minelcc","maxelcc")
  resources <- do.call("rbind",list(storage[labs],solar[labs],wind[labs],perfect[labs]))
  #creates the frame you want
  resources$zone <- gsub("Utility.*", "", resources$resourcename)
  resources$zone <- gsub("Distributed.*", "", resources$zone)
  resources$zone <- gsub("\\d.*", "", resources$zone)
  
  #plot
  g <- ggplot(resources, aes(x = pctTx, y = avgelcc, color = zone)) + 
    geom_line(lwd=2) + facet_wrap(~ type,nrow = 2) +
    theme_classic() +
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
  ggsave(paste0("ELCClines_",caselabel,".png"), width=12, height=6)
  
  #return
  return(resources)
}

IRM<-"18"
results1 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_100%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results2 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_90%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results3 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_80%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results4 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_70%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results5 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_60%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results6 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_50%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results7 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_40%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results8 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_30%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results9 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_20%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results10 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_10%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results11 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_1%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))
results12 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_0%tx_",IRM,"%IRM_0GWstorage_LAonly_addgulfsolar"))

resultList <- list(results1,results2,results3,results4,
                   results5,results6,results7,results8,
                   results9,results10)
names(resultList) <- c("100%Tx","90%Tx","80%Tx",
                       "70%Tx","60%Tx","50%Tx",
                       "40%Tx","30%Tx","20%Tx",
                       "10%Tx")

plotLOLPbars(resultList,IRM)
z<-plotELCCs(resultList,IRM)

names(resultList) <- c(results1$linelabel,results2$linelabel,
                       results3$linelabel,results4$linelabel,
                       results5$linelabel,results6$linelabel,
                       results7$linelabel,results8$linelabel,
                       results9$linelabel,results10$linelabel)

a <- plotregionalLOLPtimeseries(resultList)
b<-plotCAPstack(resultList)
c<-plotTXuse(resultList)

#make a multi-panel plot
grid <- plot_grid(b, a, labels = c('A', 'B'), label_size = 32)

# first align the top-row plot (p3) with the left-most plot of the
# bottom row (p1)
plots <- align_plots(b, c, align = 'v', axis = 'l')
# then build the top row
top_row <- plot_grid(plots[[1]], a, labels = c('A', 'B'),
                     rel_widths = c(.75, 1),label_size = 32)
# then combine with the top row for final plot
grid <- plot_grid(top_row, plots[[2]], labels = c('', 'C'), label_size = 32, ncol = 1)

setwd(paste(baseWD, "results", "Rfigures", sep="/"))
ggsave(paste0("gridplot",".png"), width=18, height=12)

