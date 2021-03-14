rm(list=ls())
install.packages("tibbletime")
install.packages("BiocManager")
BiocManager::install("rhdf5")

require(rhdf5)
require(ggplot2)
require(readxl)
require(openxlsx)
require(dplyr)
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
library(tibbletime)
#library(aomisc)
library(tibbletime)
require(directlabels)

# Create data

baseWD <- "C:/Users/llavi/Desktop/test11.16"
setwd(paste(baseWD, "", sep="/"))

readFile <- function(filename,filetype,WD,directory,header=FALSE){
  csvname <- paste0(filename,"_",filetype,".csv")
  setwd(paste(WD,directory,sep="/"))
  df <- read.csv(csvname,header=header)
  
  #add hour label for storage dfs
  #if (grepl("storageELCC", filename, fixed=TRUE)){
  #  N<-parse_number(filename)
  #  df$hours <- rep(N,length(df$capacity))
  #}
  
  #
  return(df)
}

loadResults <- function(WD,casename,allResults=T){
  
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
  genCSV <- geninfo
  #genCSV <- readFile(casename,"gens",resultsWD,"metricresults",header=TRUE)
  #genCSV <- merge(genCSV, geninfo, by.x = "Generator.Name", 
  #                by.y = "name", all.x = FALSE, all.y = TRUE)
  
  #get geninfo into gencsv by merging
  setwd(WD)
  #load xlsx files
  MISO_transmission <- read_excel("NREL-Seams Model (MISO).xlsx", sheet = "Transmission")
  MISO_generation <- read_excel("NREL-Seams Model (MISO).xlsx", sheet = "Generation")
  MISO_load <- read_excel("NREL-Seams Model (MISO).xlsx", sheet = "Load")
  MISO_mapping <- read_excel("NREL-Seams Model (MISO).xlsx", sheet = "Mapping")
  
  if (allResults){
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
    #solarELCC <- readFile("windELCC",casename,resultsWD,"ELCCresults",header=TRUE)
    #windELCC<- readFile("windELCC",casename,resultsWD,"ELCCresults",header=TRUE)
    #perfectELCC <- readFile("windELCC",casename,resultsWD,"ELCCresults",header=TRUE)
    
    #storage ELCC stuff
    #linelabel <- paste0(ceiling(linecapacity),"MW")
    
    # return results
    results <- list(regionEUE,regionLOLE,periodEUE,periodLOLP,regionperiodEUE,regionperiodLOLP,utilizations,flows,loadDF,genDF,genCSV,MISO_mapping)
    names(results) <- c("regionEUE","regionLOLE","periodEUE","periodLOLP","regionperiodEUE","regionperiodLOLP","utilizations","flows","load","gens","gensdata",
                        "mapper")
  }
  else{
    storageELCC <- readFile("storageELCC",casename,resultsWD,"ELCCresults",header=T)
    solarELCC <- readFile("solarELCC",casename,resultsWD,"ELCCresults",header=TRUE)
    storage1hELCC <- readFile("storageELCC_1hour",casename,resultsWD,"ELCCresults",header=T)
    storage4hELCC <- readFile("storageELCC_4hour",casename,resultsWD,"ELCCresults",header=T)
    results <- list(loadDF,genDF,genCSV,MISO_mapping,storageELCC,storage1hELCC,storage4hELCC,solarELCC)
    names(results) <- c("load","gens","gensdata","mapper","storage","1hstorage","4hstorage","solar")
      
  }
  return(results)
}

createLOLPdf <- function(rlist,caselabel){
  mapping <- rlist[[1]]$mapper %>% pull('CEP Bus Name')
  for (i in 1:length(rlist)){
    rlist[[i]] <- as.data.frame(t(rlist[[i]]$regionperiodLOLP))
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  #convert time to day
  regionperiodLOLP <- do.call("rbind", rlist)
  colnames(regionperiodLOLP)[c(1:length(mapping))] <- mapping
  
  #melt
  a <- melt(regionperiodLOLP, id.vars = c("label","time"), measure.vars = mapping)
  a$Event <- ifelse(a$value>0,1,0)
  v<-c()
  e<-c()
  counter <- 0
  EventID <- 0
  for (i in 1:length(a$Event)){
    if (i%%50000==0){
      print(i)
    }
    if (a$Event[i]==0){
      counter <- 0
      v<-c(v,counter)
      e<-c(e,0)
    }
    else{
      counter <- counter+1
      if(counter==1){
        EventID <- EventID+1
      }
      v<-c(v,counter)
      e<-c(e,EventID)
    }
  }
  a$counter <- v
  a$EventID <- e
  print("end")
  return(a)
}
plotLOLPheatmap<- function(df,caselabel){
  df <- df[df$counter!=0,]
  df$EventID <- as.factor(df$EventID)
  df <- df %>%
    group_by(EventID) %>%
    mutate(ranks = order(order(value, decreasing=TRUE)))
  df <-df[order(df$EventID, -df$ranks),]
  df<- df %>%
    group_by(EventID) %>%
    mutate(deltaValue = value - lag(value, default = value[1]))
  df$deltaValue[df$deltaValue==0] <- df$value[df$deltaValue==0]
  #finally, get the zone and case-wise total LOLPs so %s can be obtained
  df <- df %>% group_by(label,variable) %>% mutate(caseSum = sum(deltaValue))
  df$Probability <- df$deltaValue/df$caseSum
  
  df$counter <- sprintf('%02d', df$counter)
  df$counter[df$counter>10] <- "11+"
  df$counter <- as.character(df$counter)
  df<-df %>% group_by(counter,label,variable) %>% 
    summarise(across(where(is.numeric), sum))
  #then get the fraction of zonal LOLP of each event magnitude
  df$Probability <- df$Probability*100 #to percent
  
  df$label[df$label=="100%tx"] <- "100% of Base Transmission Capacity"
  df$label[df$label=="25%tx"] <- "25% of Base Transmission Capacity"
  
  #grab the totals
  total <- df %>% 
    group_by(label,variable) %>% 
    summarise(Probability = sum(deltaValue)) %>% 
    mutate(counter = 'Total')

  #df$counter <- factor(df$counter, levels = c("1", "2", "3","4","5","6","7","8","9","10","10+","Total"))
  
  
  g <- ggplot(df, aes(counter,variable)) + 
    geom_tile(aes(fill = Probability),colour = "white") +
    scale_y_discrete(limits = rev(levels(df$variable)))+
    facet_wrap(.~label,nrow=2)+
    scale_fill_gradient2(low="white",high="indianred1") +
    labs(x="Loss of Load Event Duration (h)",y="Zone") +
    theme_classic()+
    theme(legend.key.size = unit(3,"line"),
          legend.text = element_text(size=20),
          legend.title = element_text(size=20),
          plot.title = element_blank(),
          axis.title.y = element_text(size=28),
          axis.title.x = element_text(size=28),
          axis.text.x= element_text(size=20),
          axis.text.y= element_text(size=16),
          strip.text = element_text(size=24))
  g <- g + geom_point(data = total, 
                      aes(color = Probability), 
                      size = 8, 
                      shape = 19) +
    scale_color_gradient2(low = "white",high = "green") +
    geom_text(data = total, size = 6, aes(label = round(Probability,1))) +
    labs(fill="Probability (%)", color ="Zonal LOLH (h/y)")
  
  setwd(paste(baseWD, "results", sep="/"))
  ggsave(paste0("regionLOLPheatmap_",caselabel,".png"), dpi=500,width=15, height=11)
  #and plot
  return(g)
}

plotLOLPCDF<- function(df,caselabel){
  df <- df[df$counter!=0,]
  df$EventID <- as.factor(df$EventID)
  df <- df %>%
    group_by(EventID) %>%
    mutate(ranks = order(order(value, decreasing=TRUE)))
  df <-df[order(df$EventID, -df$ranks),]
  df<- df %>%
    group_by(EventID) %>%
    mutate(deltaValue = value - lag(value, default = value[1]))
  df$deltaValue[df$deltaValue==0] <- df$value[df$deltaValue==0]
  #finally, get the zone and case-wise total LOLPs so %s can be obtained
  df <- df %>% group_by(label,variable) %>% mutate(caseSum = sum(deltaValue))
  df$Probability <- df$deltaValue/df$caseSum
  
  df$counter <- sprintf('%02d', df$counter)
  df$counter[df$counter>10] <- "11+"
  df$counter <- as.character(df$counter)
  df<-df %>% group_by(counter,label,variable) %>% 
    summarise(across(where(is.numeric), sum))
  #then get the fraction of zonal LOLP of each event magnitude
  df$Probability <- df$Probability*100 #to percent
  
  #try the cumulative sum
  df <- df %>% group_by(label, variable) %>% 
    mutate(Cum_Prob = cumsum(Probability))
  
  #grab the totals
  total <- df %>% 
    group_by(label,variable) %>% 
    summarise(Probability = sum(deltaValue)) %>% 
    mutate(counter = 'Total')
  #print(typeof(df$counter))
  #df$counter <- factor(df$counter, levels = c("01", "02", "03","04","05","06","07","08","09","10","11+"))
  df$counter[df$counter=="11+"] <- "11"
  df$counter <- as.numeric(df$counter)
  df$label <- gsub("tx"," Base Tx",df$label)

  df$label <- factor(df$label, levels = c("0% Base Tx", "25% Base Tx",
                                          "50% Base Tx","100% Base Tx",
                                          "150% Base Tx","200% Base Tx"))
  
  include_zones <- c("LA-GULF","MEC","EES-TX","CBPC-NIPCO","MISO-MS")
  mycolors <- c("red","blue","orange","green","purple")
  #then, we plot the CDFs faceted by case
  g <- ggplot(df[df$variable%in%include_zones,], aes(counter,Cum_Prob,color=variable)) +
    geom_line(lwd=2)+scale_color_manual(values=mycolors)+
    facet_wrap(.~label,nrow=1)+
    geom_vline(data=df[df$counter==6,],aes(xintercept = counter), colour = "black",linetype="dashed",lwd=1)+
    labs(x="Loss of Load Event Duration (h)",y="Cumulative Probability (%)") +
    theme_classic()+
    theme(legend.key.size = unit(3,"line"),
          legend.text = element_text(size=24),
          legend.title = element_blank(),
          legend.position = "bottom",
          plot.title = element_blank(),
          axis.title.y = element_text(size=28),
          axis.title.x = element_text(size=28),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20),
          strip.text = element_text(size=24))
  setwd(paste(baseWD, "results", sep="/"))
  ggsave(paste0("LOLPCDFs",caselabel,".png"), dpi=500,width=15, height=8)
  
  return(g)
}

createStorageDF<- function(rlist,MWlab){
  fourh <- list()
  oneh <- list()
  solars <- list()
  for (i in 1:length(rlist)){
    solars[[i]] <- as.data.frame(rlist[[i]]$solar)
    solars[[i]]$hours <- "Solar PV"
    
    fourh[[i]] <- as.data.frame(rlist[[i]]$'4hstorage')
    fourh[[i]] <- subset(fourh[[i]], select = -c(energy))#remove the duration column
    fourh[[i]]$hours <- "4hour ESR"
    
    
    oneh[[i]] <- as.data.frame(rlist[[i]]$'1hstorage')
    oneh[[i]] <- subset(oneh[[i]], select = -c(energy))#remove the duration column
    oneh[[i]]$hours <- "1hour ESR"
    
    
    rlist[[i]] <- as.data.frame(rlist[[i]]$storage)
    rlist[[i]] <- subset(rlist[[i]], select = -c(energy))#remove the duration column
    rlist[[i]]$hours <- "6hour ESR"
    
    solars[[i]]$label <- names(rlist[i])
    fourh[[i]]$label <- names(rlist[i]) 
    oneh[[i]]$label <- names(rlist[i]) 
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    #rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  #convert time to day
  #print(rlist)
  #print(colnames(solars[[6]]))
  #print(colnames(oneh[[6]]))
  #print(colnames(fourh[[6]]))
  #print(colnames(rlist[[6]]))
  mylist <- c(rlist,oneh,fourh)
  storagedf <- do.call("rbind", mylist)
  solardf <- do.call("rbind",c(solars))
  print(colnames(solardf))
  print(colnames(storagedf))
  colnames(solardf) <- colnames(storagedf)
  storagedf <- rbind(storagedf,solardf)
  #storagedf ifelse for avg elcc
  #also grab 1h and 4h storages
  
  #ok, add average ELCCs for plotting
  storagedf$avgelcc <- ifelse(storagedf$maxelcc-storagedf$minelcc>100,storagedf$minelcc,(storagedf$maxelcc+storagedf$minelcc)/2)
  storagedf$avgelcc <- storagedf$avgelcc/5 #converst to %
  storagedf$Zone <- gsub("\\d.*","",storagedf$resourcename)
  storagedf$Zone <- gsub("Utility.*","",storagedf$Zone)
  storagedf$pctTx <- as.numeric(gsub("%.*","",storagedf$label))

  #manually determine line colors
  mycolors <- rep("grey",22)
  #direct label lines at their edges
  #30GW plot needs some labels moved
  if (MWlab=="30GW"){
    print("moving labels...")
    ELCClabelheight <- 95
    orangevjust <- -1.
    greenvjust <- 2.
    purplevjust <- 0.25
    bluevjust <- -2.
    redvjust <- 2.
  }
  else{
    ELCClabelheight <- 5
    orangevjust <- -0.75
    greenvjust <- 0.5
    purplevjust <- 1.
    bluevjust <- -1.
    redvjust <- 1.5
  }
  g <- ggplot() +
    geom_line(data=storagedf, aes(x = pctTx, y = avgelcc, color = Zone),lwd=2,alpha=.3) + facet_wrap(~ hours,nrow = 2) +
    theme_classic() +
    ylab("ELCC (%)") + xlab("Transmission Capacity (%base)") +
    scale_color_manual(values=mycolors)+
    xlim(c(-38,200))+
    ggtitle(paste0(MWlab," Storage ICAP"))+
    geom_vline(data=storagedf[storagedf$pctTx==100,],aes(xintercept = pctTx), colour = "black",linetype="dashed",lwd=2)+
    annotate("text", label = "Base ELCCs", size = 8, x = 130, y = ELCClabelheight) +
    geom_line(data=storagedf[storagedf$Zone=="LA-GULF",], aes(x = pctTx, y = avgelcc),color="red",lwd=2)+
    geom_dl(data=storagedf[storagedf$Zone=="LA-GULF",],aes(x=pctTx,y=avgelcc,label=Zone), method = list(dl.combine("first.points"),cex = 1.2,hjust=1.,vjust=redvjust),color="red") +
    geom_line(data=storagedf[storagedf$Zone=="MEC",], aes(x = pctTx, y = avgelcc),color="blue",lwd=2)+
    geom_dl(data=storagedf[storagedf$Zone=="MEC",],aes(x=pctTx,y=avgelcc,label=Zone), method = list(dl.combine("first.points"),cex = 1.2,hjust=1.,vjust=bluevjust),color="blue") +
    geom_line(data=storagedf[storagedf$Zone=="EES-TX",], aes(x = pctTx, y = avgelcc),color="orange",lwd=2)+
    geom_dl(data=storagedf[storagedf$Zone=="EES-TX",],aes(x=pctTx,y=avgelcc,label=Zone), method = list(dl.combine("first.points"),cex = 1.2,hjust=1.,vjust=orangevjust),color="orange") +
    geom_line(data=storagedf[storagedf$Zone=="CBPC-NIPCO",], aes(x = pctTx, y = avgelcc),color="green",lwd=2)+
    geom_dl(data=storagedf[storagedf$Zone=="CBPC-NIPCO",],aes(x=pctTx,y=avgelcc,label=Zone), method = list(dl.combine("first.points"),cex = 1.,hjust=1.,vjust=greenvjust),color="green") +
    geom_line(data=storagedf[storagedf$Zone=="MISO-MS",], aes(x = pctTx, y = avgelcc),color="purple",lwd=2)+
    geom_dl(data=storagedf[storagedf$Zone=="MISO-MS",],aes(x=pctTx,y=avgelcc,label=Zone), method = list(dl.combine("first.points"),cex = 1.2,hjust=1.,vjust=purplevjust),color="purple") +
    guides(fill=guide_legend(title="")) +
    theme(legend.text = element_text(size=24),
          legend.title = element_blank(),
          legend.position = "none",
          plot.title = element_text(size=28,hjust=0.5),
          axis.title.y = element_text(size=24),
          axis.title.x = element_text(size=24),
          strip.text.x = element_text(size=24),
          axis.text.x= element_text(size=24),
          axis.text.y= element_text(size=20))
  
  setwd(paste(baseWD, "results", sep="/"))
  ggsave(paste0("ELCClines_storage_",MWlab,".png"), dpi=500,width=12, height=10)
  return(g)
}

createSolarDF<- function(rlist){
  for (i in 1:length(rlist)){
    rlist[[i]] <- as.data.frame(rlist[[i]]$solar)
    rlist[[i]]$hours <- "solar"
    rlist[[i]]$label <- names(rlist[i]) #may want better label
    #rlist[[i]]$time <- seq(from = 1, to = length(rlist[[i]]$V1), by = 1)
  }
  solarDF <- do.call("rbind", rlist)
  return(solarDF)
}
#for looking at storage
IRM<-"18"
MWlab <- "30GW"

s1 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_0%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"),allResults=F)
s2 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_25%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"),allResults=F)
s3 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_50%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"),allResults=F)
s4 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_100%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"),allResults=F)
s5 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_150%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"),allResults=F)
s6 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_200%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"),allResults=F)
slist <- list(s1,s2,s3,s4,s5,s6)
names(slist) <- c("0%tx","25%tx","50%tx","100%tx","150%tx","200%tx")
s1$solar
s1$storage
storageDF <- createStorageDF(slist,MWlab)
solarDF <- createSolarDF(slist)
write.csv(storageDF,"mystorages.csv")
write.csv(solarDF,"mysolars.csv")

IRM<-"18"
MWlab <- "30GW"
r1 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_0%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"))
r2 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_25%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"))
r3 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_50%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"))
r4 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_100%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"))
r5 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_150%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"))
r6 <- loadResults(baseWD,paste0("VRE0.4_wind_2012base100%_8760_200%tx_",IRM,"%IRM_",MWlab,"storage_addgulfsolar"))

#"VRE0.4_wind_2012base100%_8760_100%tx_18%IRM_0GWstorage_addgulfsolar_"
rlist <- list(r1,r2,r3,r4,r5,r6)
names(rlist) <- c("0%tx","25%tx","50%tx","100%tx","150%tx","200%tx")
dfALL <- createLOLPdf(rlist,"hi") #this takes a long time, so be careful
dfALL30 <- createLOLPdf(rlist,"hi") #this takes a long time, so be careful
dfALL_gathered <- plotLOLPCDF(dfALL30,"hi")

#cowplot of main results figures
require(cowplot)

plots <- align_plots(storageDF, dfALL_gathered, align = 'v', axis = 'l')
grid <- plot_grid(plots[[1]], plots[[2]], labels = c('A', 'B'), label_size = 32, ncol = 1)

setwd(paste(baseWD, "results", sep="/"))
ggsave(paste0("gridplot_LOLP_",MWlab,".png"), width=18, height=14)

require(scales)
#make my own NREL-ish figure for the paper
ycats <- c("01","02","03","04","05","06","07")
ycats <- factor(ycats,levels = ycats)
xcats <- 10^(-3:7)
#logscale for x

ylabs <- c("Distribution","","Nodal","","Zone/BA"," ","Region")

pdf <- data.frame("x" = rep(xcats,length(ycats)), "y" = rep(ycats, each=length(xcats)))


g<-ggplot() + geom_blank(data=pdf,aes(x=x,y=y))+
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic() + ylab("Geographic Resolution") + xlab("Temporal Resolution (sec)") +
  geom_rect(aes(xmin = 1, xmax = 3600, ymin = "02", ymax = "07"),
            alpha = .1,fill = "purple") +
  geom_rect(aes(xmin = 1, xmax = 3600, ymin = "02", ymax = "07"),
            alpha = 1,color = "purple",linetype="dashed",fill=NA,lwd=3) +
  geom_text(aes(x=8,y="03"),label="Production\nCost\nModel",color="purple",size=7)
g<-g+geom_rect(aes(xmin = .001, xmax = 60, ymin = "01", ymax = "07"),
               alpha = .1,fill = "green") +
  geom_rect(aes(xmin = .001, xmax = 60, ymin = "01", ymax = "07"),
            alpha = 1,color = "green",linetype="dashed",fill=NA,lwd=3)+
  geom_text(aes(x=.05,y="04"),label="Dynamic\nReliability\nAnalysis",color="green",size=7)
g<-g+geom_rect(aes(xmin = 300, xmax = 1000000, ymin = "04", ymax = "07"),
               alpha = .1,fill = "blue") +
  geom_rect(aes(xmin = 300, xmax = 1000000, ymin = "04", ymax = "07"),
            alpha = 1,color = "blue",linetype="dashed",fill=NA,lwd=3)+
  geom_text(aes(x=5000000,y="04"),label="Capacity\nExpansion\nModel",color="blue",size=7)
g<-g+geom_rect(aes(xmin = 3600, xmax = 100000, ymin = "05", ymax = "07"),
               alpha = .1,fill = "orange") +
  geom_rect(aes(xmin = 3600, xmax = 100000, ymin = "05", ymax = "07"),
            alpha = 1,color = "orange",linetype="dashed",fill=NA,lwd=3)+
  geom_text(aes(x=20000,y="06"),label="Resource\nAdequacy\nModel",color="orange",size=6)
g<-g+geom_rect(aes(xmin = 100, xmax = 100000, ymin = "03", ymax = "07"),
               alpha = .2,fill = "red") +
  geom_rect(aes(xmin = 100, xmax = 100000, ymin = "03", ymax = "07"),
            alpha = 1,color = "red",linetype="dashed",fill=NA,lwd=3)+
  geom_text(aes(x=500000,y="03"),label="PRAS",color="red",size=10)#face=bold



g<- g+scale_y_discrete(labels=ylabs) +
  theme(axis.line.y = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.line.x = element_line(arrow = grid::arrow(length = unit(0.3, "cm"), 
                                                       ends = "last")),
        axis.title.y = element_text(size=28),
        axis.title.x = element_text(size=28),
        axis.text.x= element_text(size=20),
        axis.text.y= element_text(size=24),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size=24))

setwd(paste(baseWD, "results", sep="/"))
ggsave(paste0("modelsmap_2",".png"), dpi=500,width=12, height=10)


#capacity price file
paperWD <- "C:/Users/llavi/Desktop/Rose_MISO"
setwd(paperWD)
capDF <- read.csv("historical_capacity_prices.csv",header=T)
capDF$Capacity.Commitment.Period.Start.Date<-as.POSIXct(as.character(capDF$Capacity.Commitment.Period.Start.Date), format="%m/%d/%Y %H:%M")
capDF$year <- as.numeric(gsub("-.*","",as.character(capDF$Capacity.Commitment.Period.Start.Date)))
capDF$month <- month(capDF$Capacity.Commitment.Period.Start.Date)
capDF$Capacity.Market.Zone[capDF$Capacity.Market.Zone=="NYCA"] <- "ROS"
#run a quick aggregation to find the most expensive zones
compareDF <- capDF %>%  group_by(Capacity.Market.Zone) %>% 
  summarise(AvgPrice = mean(Capacity.Price...KW.Year))

countDF<-capDF[capDF$ISO.Name=="New England ISO",] %>% count(Capacity.Market.Zone)


include_labs <- c("NEMA-BOSTON","REST OF POOL","NYC","ROS","PSEG","AEP")
#labels <- c("HIGH","LOW","HIGH","LOW","HIGH","LOW")
subDF <- capDF[capDF$Capacity.Market.Zone%in%include_labs,]
#names(labels) <- include_labs
#subDF$lab <- labels[as.character(subDF$Capacity.Market.Zone)]
#for NYC, include only summer
subDF <- subDF[subDF$Capacity.Commitment.Period.Length!="ONE MONTH",]
subDF <- subDF[subDF$month!=11,]

#subDF<-subDF[order(subDF$ISO.Name,subDF$year),]
subDF$Capacity.Market.Zone <- factor(subDF$Capacity.Market.Zone,levels = include_labs)

#re-label the lines
#put label next to lines in ggplot?

linetypes <- rep(c("dashed","solid"),3)
mycolors <- c("coral","lightcoral","blue","lightblue","green","lightgreen")
myalphas <- rep(c(.9,.6),3)
myfills <- c("coral","blue","green")

ggplot(data=subDF, aes(x=year,y=Capacity.Price...KW.Year,
                       color=Capacity.Market.Zone,linetype=Capacity.Market.Zone,
                       alpha=Capacity.Market.Zone))+
  facet_wrap(~ISO.Name,nrow=1)+
  geom_line(lwd=3.5) + theme_classic() + xlab("Year")+ylab("Capacity Price ($/kW-y)") +
  scale_linetype_manual(values=linetypes)+
  scale_color_manual(values=mycolors)+
  scale_alpha_manual(values=myalphas)+
  geom_ribbon(data=subDF,aes(ymin=-1,ymax=0,x=year,fill=ISO.Name))+
  scale_fill_manual(values=myfills)+
  ylim(c(0,200))+
  coord_cartesian(clip = 'off') +
  labs(color  = "", linetype = "",alpha="")+
  geom_dl(aes(label=Capacity.Market.Zone), method = list(dl.combine("last.points"),cex = 2,hjust=-.09)) +
  guides(fill=guide_legend(title=""),color = F,alpha=F,linetype=F)+
  theme(legend.position ="bottom",
        legend.text = element_text(size=20),
        plot.margin = margin(0.1, 6., 0.1, 0.1, "cm"),
        axis.title.y = element_text(size=28),
        axis.title.x = element_text(size=28),
        axis.text.x= element_text(size=20),
        axis.text.y= element_text(size=24),
        axis.ticks.y = element_blank(),
        strip.text = element_text(size=24))


setwd(paste(baseWD, "results", sep="/"))
ggsave(paste0("capprices3.12_",".png"), dpi=500,width=12, height=6)

