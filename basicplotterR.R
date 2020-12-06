rm(list=ls())
require(ggplot2)
baseWD <- "C:/Users/llavi/Desktop/test11.16/results"
setwd(paste(baseWD, "caseICAPs", sep="/"))

labs <- c("prorataVRE","prorataload","equal")

lab1 <- paste0(labs[1],"VRE0.4_wind_2012base100%_8760_25%tx_18%IRM_30GWstorage_addgulfsolar.csv")
df1 <- read.csv(lab1)
df1$alloc <- rep(labs[1],length(df1$X))

lab2 <- paste0(labs[2],"VRE0.4_wind_2012base100%_8760_25%tx_18%IRM_30GWstorage_addgulfsolar.csv")
df2 <- read.csv(lab2)
df2$alloc <- rep(labs[2],length(df2$X))

lab3 <- paste0(labs[3],"VRE0.4_wind_2012base100%_8760_25%tx_18%IRM_30GWstorage_addgulfsolar.csv")
df3 <- read.csv(lab3)
df3$alloc <- rep(labs[3],length(df3$X))
#add column based on name

df <- rbind(df1,df2,df3)


ggplot(df, aes(x = type, y = capacity)) +
  geom_col() +
  facet_grid(~zone) +
  theme_classic()

mycolors <- c("orange","darkgreen","yellow","cyan")
ggplot(df,aes(x=zone,y=capacity,fill=type))+geom_col()+theme_classic() +
  facet_wrap(~alloc,ncol=3) +
  ggtitle("40% VRE, 30 GW 6-hour storage \n scenario zonal ICAPs") + ylab("Capacity (MW)") +
  scale_fill_manual(values=mycolors) +
  theme(legend.text = element_text(size=24),
        legend.title=element_blank(),
        legend.position = "bottom",
        plot.title = element_text(size=28,hjust=0.5),
        axis.title.y = element_text(size=24),
        axis.title.x = element_blank(),
        strip.text.x = element_text(size=28),
        axis.text.x= element_text(size=8),
        axis.text.y= element_text(size=16))

plotTitle <- "ICAP"
#setwd(baseWD)
ggsave(paste0(plotTitle, ".png"), width=22, height=6)
