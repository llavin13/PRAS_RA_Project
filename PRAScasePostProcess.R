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
library(readr)
library(drc)
library(nlme)
#library(aomisc)

baseWD <- "C:/Users/Luke/Desktop/PRAS3.12.21"
setwd(paste(baseWD, "", sep="/"))

readFile <- function(filename,filetype,WD,directory,header=FALSE){
  csvname <- paste0(filename,"_",filetype,".csv")
  setwd(paste(WD,directory,sep="/"))
  df <- read.csv(csvname,header=header)
  #print(rownames(df))
  
  #add hour label for storage dfs
  if (grepl("storageELCC", filename, fixed=TRUE)){
    N<-parse_number(filename)
    df$hours <- rep(N,length(df$capacity))
  }
  
  #
  return(df)
}

loadResults <- function(WD,casename,ELCChourlist=list(6),f=0,basecapacity=2749){

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
  
  #generator ELCC stuff
  solarELCC <- readFile("windELCC",casename,resultsWD,"ELCCresults",header=TRUE)
  windELCC<- readFile("windELCC",casename,resultsWD,"ELCCresults",header=TRUE)
  perfectELCC <- readFile("windELCC",casename,resultsWD,"ELCCresults",header=TRUE)
  
  #storage ELCC stuff

  for (i in seq_along(ELCChourlist)){
    filestring <- paste0("storageELCC_",as.character(ELCChourlist[i]),"h")
    if (i==1){
      storageELCC <- readFile(filestring,casename,resultsWD,"ELCCresults",header=T)
    }
    else{
      storageELCC <- rbind(storageELCC,readFile(filestring,casename,resultsWD,"ELCCresults",header=T))
    }
  }
  #storageELCC6 <- readFile("storageELCC_6h",casename,resultsWD,"ELCCresults",header=TRUE)
  #storageELCC12 <- readFile("storageELCC_12h",casename,resultsWD,"ELCCresults",header=TRUE)
  #storageELCC1 <- readFile("storageELCC_1h",casename,resultsWD,"ELCCresults",header=TRUE)
  #storageELCC3 <- readFile("storageELCC_3h",casename,resultsWD,"ELCCresults",header=TRUE)
  #storageELCC <- rbind(storageELCC1,storageELCC3,storageELCC6,storageELCC12)
  #print(storageELCC)
  #clean and format to concatenate
  #modelLMP <- AddDatetime(modelLMP)
  
  #grab the utilization to get naming convention based on MW
  linecapacity <- as.numeric(flows[1,1]/utilizations[1,1])
  linelabel <- ceiling(linecapacity)
  if (is.na(linelabel)){
    linelabel <- ceiling(as.numeric(f*basecapacity))
  }
  #linelabel <- paste0(ceiling(linecapacity),"MW")

  # return results
  results <- list(regionEUE,regionLOLE,periodEUE,periodLOLP,regionperiodEUE,regionperiodLOLP,utilizations,flows,loadDF,genDF,genCSV,MISO_mapping,storageELCC,solarELCC,windELCC,perfectELCC,linelabel)
  names(results) <- c("regionEUE","regionLOLE","periodEUE","periodLOLP","regionperiodEUE","regionperiodLOLP","utilizations","flows","load","gens","gensdata",
                      "mapper","storageELCC","solarELCC","windELCC","perfectELCC","linelabel")
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
  ggsave(paste0("LOLPlines_",caselabel,".png"), dpi=500,width=12, height=6)
  
  aggperiodLOLP$pctTx <- as.factor(aggperiodLOLP$pctTx)
  gg <- ggplot(aggperiodLOLP, aes(pctTx, day, fill = count)) + 
    geom_tile(colour = "white") +
    scale_fill_gradient2(low="white",mid="red", high="white",
                         midpoint=12,limits=c(0,24)) +
    labs(x="%Tx",y="Day") + theme_classic()
  return(gg)
}

plotLOLPheatmap <- function(rlist,caselabel){
  for (i in 1:length(rlist)){
    rlist[[i]] <- rlist[[i]]$regionperiodLOLP
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  #convert time to day
  regionperiodLOLP <- do.call("rbind", rlist)
  
  regionperiodLOLP$"0" <- 1-regionperiodLOLP$V2
  regionperiodLOLP$"1" <- regionperiodLOLP$V2-regionperiodLOLP$V1
  regionperiodLOLP$"2" <- regionperiodLOLP$V1
  
  #format for tiling, add additional cols
  #melt
  a <- melt(regionperiodLOLP, id.vars = c("label","time"), measure.vars = c("0","1","2"))
  a$label <- paste(a$label,"MW Transmission Capacity", sep=" ")
  a$time <- as.factor(a$time)
  colnames(a) <- c("label","time","variable","Probability")
  a$textcol <- ""
  a$textcol[(a$time=="2")&(a$variable=="0")] <- "No loss of load \n in Zone 2"
  a$textcol[(a$time=="1")&(a$variable=="2")&(a$label=="0 MW Transmission Capacity")] <- "Storage cannot\nserve 2-hour events"
  a$textcol[(a$time=="1")&(a$variable=="1")&(a$label=="2750 MW Transmission Capacity")] <- "Storage can\n serve 1-hour events"
  a$textcol[(a$time=="1")&(a$variable=="1")&(a$label=="5499 MW Transmission Capacity")] <- "Storage can\n serve 1-hour events"
  a$textcol[(a$time=="1")&(a$variable=="0")&(a$label=="8247 MW Transmission Capacity")] <- "No loss of load \n with high \n transmission"
  a$Probability <- a$Probability*100
  g <- ggplot(a, aes(variable, time, fill = Probability)) + 
    geom_tile(colour = "white") +
    facet_wrap(.~label)+
    geom_text(data=a,aes(label=textcol),size=5)+
    scale_fill_gradient2(low="white",high="indianred1") +
    labs(x="Loss of Load Event Duration (h)",y="Zone",fill="Probability (%)") +
    theme_classic()+
    theme(legend.key.size = unit(3,"line"),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20),
          plot.title = element_blank(),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20),
          strip.text = element_text(size=20))
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("regionLOLPheatmap_",caselabel,".png"), dpi=500,width=12, height=6)
  #and plot
  return(g)
}
plotLOLPheatmap(resultList,IRM)

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
  ggsave(paste0("test",".png"),dpi=500, width=12, height=6)
}

plotcaseEUE<- function(rlist,caselabel){
  
  for (i in 1:length(rlist)){
    rlist[[i]] <- rlist[[i]]$periodEUE
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  periodEUEdf <- do.call("rbind", rlist)
  
  periodEUEdf$EUE <- periodEUEdf$V1 #scales frac to %
  
  #group sum by tx case
  caseEUE <- periodEUEdf %>% 
    group_by(label) %>% 
    summarise(EUE = sum(EUE))
  
  caseEUE$label <- as.numeric(caseEUE$label)
  print(caseEUE)
  ggplot(data=caseEUE, aes(x=label,y=EUE)) +
    geom_line(lwd=3) + theme_classic() + ylab("EUE (MWh)") + xlab("Tx Cap (MW)") +
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
  ggsave(paste0("EUElines_",caselabel,".png"), dpi=500,width=12, height=6)
}

plotregionalLOLPtimeseries <- function(rlist){
  
  for (i in 1:length(rlist)){
    rlist[[i]] <- rlist[[i]]$regionperiodLOLP
    rlist[[i]] <- as.data.frame(t(rlist[[i]]))
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  regionperiodLOLP <- do.call("rbind", rlist)
  colnames(regionperiodLOLP) <- c("Zone 1","Zone 2","label","time")
  #now run your melt
  regionperiodLOLP <- melt(regionperiodLOLP, id.vars = c("label","time"), measure.vars = c("Zone 1","Zone 2"))
  regionperiodLOLP$percentLOLP <- regionperiodLOLP$value*100 #scales frac to %
  manualtypes <- c("dotted","solid")
  regionperiodLOLP$label <- as.factor(as.numeric(regionperiodLOLP$label))
  
  g<-ggplot(data=regionperiodLOLP, aes(x=time,y=percentLOLP,linetype=variable,color=label)) +
    geom_line(lwd=2) + theme_classic() + ylab("LOLP(%)") + xlab("Hour") +
    scale_linetype_manual(values=manualtypes)+
    guides(color=guide_legend(title="Tx MW:"),
           linetype=guide_legend(title="",nrow=2)) +
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
  ggsave(paste0("regionLOLPseries",".png"), dpi=500,width=12, height=6)
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
  ggsave(paste0("txtest",".png"), dpi=500,width=12, height=6)
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
  #print(loads)
  print(loads %>% 
          group_by(label) %>% 
          summarise(sumLoad = sum(value)))
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
  
  gencap$Bus[gencap$Bus=="LA-GULF"]<- "Zone 1"
  gencap$Bus[gencap$Bus=="LA-N"]<- "Zone 2"
  
   
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
  ggsave(paste0("stackgentest",".png"), dpi=500, width=12, height=6)
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
  storage$avgelcc <- ifelse(storage$maxelcc-storage$minelcc >= 50, 0, (storage$minelcc+storage$maxelcc)/2.)
  storage$pctTx <- as.numeric(gsub("%.*", "", storage$label))
  
  storage$type <- rep("Storage (",length(storage$label))
  storage$type <- paste0(storage$type,storage$hours)
  storage$type <- paste0(storage$type,"h)")
  
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
  resources <- do.call("rbind",list(storage[labs],perfect[labs]))
  #solar[labs],wind[labs],
  #creates the frame you want
  resources$zone <- gsub("Utility.*", "", resources$resourcename)
  resources$zone <- gsub("Distributed.*", "", resources$zone)
  resources$zone <- gsub("\\d.*", "", resources$zone)
  
  resources$zone[resources$zone=="LA-GULF"] <- "Zone 1"
  resources$zone[resources$zone=="LA-N"] <- "Zone 2"
  
  
  g <- ggplot() +
    geom_line(data=resources, aes(x = pctTx, y = avgelcc, color = type),lwd=2) + facet_wrap(~ zone,nrow = 1) +
    theme_classic() +
    ylab("ELCC (%)") + xlab("Transmission Capacity (MW)") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          legend.position = "top",
          plot.title = element_blank(),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          strip.text.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20))
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("ELCClines_",caselabel,".png"), dpi=500,width=12, height=6)
  
  #return
  return(g)
}

plotELCCsLabs <- function(rlist,caselabel){
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
  storage$avgelcc <- ifelse(storage$maxelcc-storage$minelcc >= 50, 0, (storage$minelcc+storage$maxelcc)/2.)
  storage$pctTx <- as.numeric(gsub("%.*", "", storage$label))
  
  storage$type <- rep("Storage (",length(storage$label))
  storage$type <- paste0(storage$type,storage$hours)
  storage$type <- paste0(storage$type,"h)")
  
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
  resources <- do.call("rbind",list(storage[labs],perfect[labs]))
  #solar[labs],wind[labs],
  #creates the frame you want
  resources$zone <- gsub("Utility.*", "", resources$resourcename)
  resources$zone <- gsub("Distributed.*", "", resources$zone)
  resources$zone <- gsub("\\d.*", "", resources$zone)
  
  resources$zone[resources$zone=="LA-GULF"] <- "Zone 1"
  resources$zone[resources$zone=="LA-N"] <- "Zone 2"
  
  rects <- data.frame(xstart = seq(0,5498,2749), xend = seq(2749,8427,2749), 
                      col = c("1","2","3"))
  print(rects)
  #geom_rect(data=rects, aes(ymin=0, ymax=80, xmin=xstart,
  #                           xmax=xend, fill=col), alpha =0.5) + coord_flip()
  #plot
  #facet_wrap(~ zone,nrow = 1) +
  fillcols <- c("gray","black","grey50")
  g <- ggplot() + 
    geom_rect(data=rects, aes(ymin=0, ymax=100, xmin=xstart,xmax=xend, fill=col), alpha =0.3)+
    geom_line(data=resources, aes(x = pctTx, y = avgelcc, color = type),lwd=2) + facet_wrap(~ zone,nrow = 1) +
    scale_fill_manual(values=fillcols)+
    theme_classic() +
    ylab("ELCC (%)") + xlab("Transmission Capacity (MW)") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          legend.position = "top",
          plot.title = element_blank(),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          strip.text.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20))
  #label annotations
  dat_text <- data.frame(
    label = c("ELCC limited by\nZone 1 Charging\nin Hour 1",
              "ELCC limited by\nTransmission"),
    zone   = c("Zone 1", "Zone 2"),
    x     = c(1375, 1375),
    y     = c(50, 50)
  )
  g<-g + geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label),
    size=5
  )
  
  dat_text <- data.frame(
    label = c("Complementarity\nBenefit",
              ""),
    zone   = c("Zone 1", "Zone 2"),
    x     = c(4125, 4125),
    y     = c(50, 50)
  )
  g<-g + geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label),
    color="red",
    size=5
  )
  
  dat_text <- data.frame(
    label = c("",
              "ELCC limited by\nTransmission"),
    zone   = c("Zone 1", "Zone 2"),
    x     = c(4125, 4125),
    y     = c(50, 50)
  )
  g<-g + geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label),
    size=5
  )
  dat_text <- data.frame(
    label = c("Transmission does \n not constrain",
              "Transmission does \n not constrain"),
    zone   = c("Zone 1", "Zone 2"),
    x     = c(6875, 6875),
    y     = c(10, 90)
  )
  g<-g + geom_text(
    data    = dat_text,
    mapping = aes(x = x, y = y, label = label),
    size=5
  )
  #g <- g + annotate("text", label = "ELCC limited by\nZone 1 Charging\nin Hour 1", size = 4, x = 2750/2, y = 50)
  #g <- g + geom_text(data = ann_text,aes(x=x,y=y),label = "ELCC limited by\nZone 1 Charging\nin Hour 1")
  
  setwd(paste(baseWD, "results", "Rfigures", sep="/"))
  ggsave(paste0("ELCClines_",caselabel,".png"), dpi=500,width=12, height=6)
  
  #return
  return(g)
}
IRM <- "lab"
plotELCCsLabs(resultList,IRM)
#fxn to compare RA metrics for case selection

compareMetrics <- function(rlist,caselabel){
  #load data
  len <- length(rlist)
  euelist <- list()
  lolelist <- list()
  loadlist <- list()
  for (i in 1:len){
    euelist[[i]] <- as.data.frame(rlist[[i]]$regionEUE)
    lolelist[[i]] <- as.data.frame(rlist[[i]]$regionLOLE)
    loadlist[[i]] <- as.data.frame(rlist[[i]]$load)
    
    #rlist[[i]] <- as.data.frame(t(rlist[[i]])) #transpose
    euelist[[i]]$label <- names(rlist[i])
    lolelist[[i]]$label <- names(rlist[i])
    loadlist[[i]]$label <- names(rlist[i])
  }
  LOLE <- do.call("rbind", lolelist)
  EUE <- do.call("rbind",euelist)
  Load <- do.call("rbind",loadlist)
  LOLEgroup <- LOLE %>% group_by(label) %>% summarise(LOLE=sum(V1)) #total LOLE
  EUEgroup <- EUE %>% group_by(label) %>% summarise(EUE=sum(V1))
  Load$total <- Load$V1+Load$V2
  df <- Load %>% 
          group_by(label) %>% 
          summarise(TotalLoad = sum(total))
  df$LOLE <- LOLEgroup$LOLE
  df$EUE <- EUEgroup$EUE
  df$normEUE <- df$EUE/df$TotalLoad
  df$label <- as.numeric(df$label)
  df <- df[order(df$label),]
  #return(df[order(df$label),])
  #real goal is to return target case
  #fit tx as fxn of metric
  #g<- ggplot(data=df,aes(x=EUE,y=log(label)))+geom_line(lwd=2)+
  #  theme_classic()
  #clip where the target metric is too low
  tgt <- .005*df$TotalLoad#.00002*df$TotalLoad
  df <- df[df$EUE>.001*tgt,]
  print(tgt)
  print(df)
  #plot
  plot(df$EUE, df$label)
  df$label[df$label==0]<-1
  #data(degradation)
  theta.0 <- min(df$label) * 0.5  
  
  # Estimate the rest parameters using a linear model
  model.0 <- lm(log(label - theta.0) ~ EUE, data=df)  
  alpha.0 <- exp(coef(model.0)[1])
  beta.0 <- coef(model.0)[2]
  
  # Starting parameters
  start <- list(alpha = alpha.0, beta = beta.0, theta = theta.0)
  model <- nls(label ~ alpha * exp(beta * EUE) + theta , data = df, start = start)
  #print(summary(model))
  lines(df$EUE, predict(model, list(EUE = df$EUE)))
  txval<-predict(model, list(EUE = tgt))[1] #transmission capacity for target EUE
  return(txval)
  
}
#for grid search
IRM<-"3"
MWlab <- "0GW"
storageELCCs <- list(1)
results1 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_0%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results2 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_20%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),f=.2,basecapacity=2749,ELCChourlist = storageELCCs)
results3 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_40%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),f=.4,basecapacity=2749,ELCChourlist = storageELCCs)
results4 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_60%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),f=.6,basecapacity=2749,ELCChourlist = storageELCCs)
results5 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_80%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),f=.8,basecapacity=2749,ELCChourlist = storageELCCs)
results6 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_100%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),f=1.,basecapacity=2749,ELCChourlist = storageELCCs)
results7 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_200%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results8 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_300%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results9 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_140%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results10 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_240%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
r11 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_180%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
r12 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_220%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
r13<- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_240%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
r14<- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_260%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
r15<- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_160%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
r16<- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_300%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
r17<- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_2_400%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
resultList <- list(results1,results6,results7,
                   r16)
names(resultList) <- c("0%Tx","100%Tx","200%Tx","300%Tx")
plotLOLPbars(resultList,IRM)
z<-plotELCCs(resultList,IRM)
names(resultList) <- c(results1$linelabel,results6$linelabel,
                       results7$linelabel,r16$linelabel)
compareMetrics(resultList,IRM)
z<-plotELCCs(resultList,IRM)
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
bottom_row <- plot_grid(z,plots[[2]], labels = c("C","D"),
                        rel_widths = c(.5,.5),label_size=32)
# then combine with the top row for final plot
grid <- plot_grid(top_row, bottom_row, labels = c('', ''), label_size = 32, ncol = 1)

setwd(paste(baseWD, "results", "Rfigures", sep="/"))
ggsave(paste0("gridplot_",IRM,".png"),dpi=500, width=18, height=12)

IRM<-"18"
MWlab <- "0GW"
storageELCCs <- list(1,3,6,12)
results1 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_100%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results2 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_90%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results3 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_80%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results4 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_70%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results5 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_60%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results6 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_50%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results7 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_40%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results8 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_30%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results9 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_20%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results10 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_10%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
#results11 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_1%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results12 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_0%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)

results13 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_120%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results14 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_140%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results15 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_160%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results16 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_180%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
results17 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_200%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)


resultList <- list(results1,results2,results3,results4,
                   results5,results6,results7,results8,
                   results9,results10,results13,results14,
                   results15,results16,results17)
names(resultList) <- c("100%Tx","90%Tx","80%Tx",
                       "70%Tx","60%Tx","50%Tx",
                       "40%Tx","30%Tx","20%Tx",
                       "10%Tx","120%Tx","140%Tx",
                       "160%Tx","180%Tx","200%Tx")


plotLOLPbars(resultList,IRM)
z<-plotELCCs(resultList,IRM)

names(resultList) <- c(results1$linelabel,results2$linelabel,
                       results3$linelabel,results4$linelabel,
                       results5$linelabel,results6$linelabel,
                       results7$linelabel,results8$linelabel,
                       results9$linelabel,results10$linelabel,
                       results13$linelabel,results14$linelabel,
                       results15$linelabel,results16$linelabel,
                       results17$linelabel)
compareMetrics(resultList,IRM)
plotcaseEUE(resultList,IRM)
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
ggsave(paste0("gridplot_",IRM,".png"), width=18, height=12)


#same EUE comparison

MWlab <- "0GW"
storageELCCs <- list(1,3,6,12)

IRM<-"25"
results25 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_37%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM<-"18"
results18 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_45%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM<-"10"
results10 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_53%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM <- "0"
results0 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_63%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM <- "-5"
resultsn5 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_77%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM <- "40"
results40 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_26%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM <- "-10"
resultsn10 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_120%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM <- "-20"
resultsn20 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_200%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM <- "50"
results50 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_19%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM <- "60"
results60 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_10%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
IRM <- "70"
results70 <- loadResults(baseWD,paste0("VRE0.1_wind_2012base100%_48_2%tx_",IRM,"%IRM_",MWlab,"storage_LAonly_addgulfsolar"),ELCChourlist = storageELCCs)
resultList <- list(results25,results18,results10,results0,resultsn10,
                   results40,resultsn5,results60,results70,results50,
                   resultsn20)

names(resultList) <- c("37%Tx","45%Tx","53%Tx","63%Tx","120%Tx",
                       "26%Tx","77%Tx","10%Tx","2%Tx","19%Tx",
                       "200%Tx")

plotLOLPbars(resultList,"EUE")
z<-plotELCCs(resultList,"EUE")

names(resultList) <- c(results25$linelabel,results18$linelabel,
                       results10$linelabel,results0$linelabel,
                       resultsn10$linelabel,results40$linelabel,
                       resultsn5$linelabel,results60$linelabel,
                       results70$linelabel,results50$linelabel,
                       resultsn20$linelabel)

plotcaseEUE(resultList,"EUE")
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
ggsave(paste0("gridplot_EUE",".png"), width=18, height=12)

