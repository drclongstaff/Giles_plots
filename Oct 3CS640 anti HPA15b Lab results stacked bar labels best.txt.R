rm(list=ls())
library(dplyr)
library(ggplot2)
library(tidyverse)
library(patternplot)
#setwd("//ad/net/user/clongsta/My Documents/R/DPlyr")

Plate.DF<-read_csv("CS640 anti-HPA15b titres.csv")
Res.DF<-Plate.DF %>% arrange(Titre) %>% mutate(label_y=cumsum(Titre)) %>% mutate(nn=1:nrow(Plate.DF)) %>% drop_na()

g_ghis<-ggplot(Res.DF, aes(Titre, fill = as.factor(Assay_number), group = nn))+
  scale_x_continuous(trans = 'log2')+
  scale_fill_brewer(palette = "OrRd", name = "Assay number")+
  geom_histogram(binwidth = 1, colour="black")
g_ghis #check the starting histogram


#Get label coordinates from inside ggplot
g_g1.bld<-layer_data(g_ghis, 1) # digs into the structure of the ggplot, get the 1st element in the list, a dataframe
nreal<-g_g1.bld$count!=0  #there are a lot of zeros in the dataframe, find the useful rows
g_g1.bld<-g_g1.bld[nreal,]#make a new dataframe without the 0 rows

Titre_range<-c(2,4,8,16,32,64,128)
#Res.DF$text.x<-g_g1.bld$x
Res.DF$text.x<-Titre_range[g_g1.bld$x] # make a new column for x coordinates incorporating the actual titre range
Res.DF$text.y<-g_g1.bld$y-0.5 #find the y coordinates, nudge them into the middle of the box
Res.DFc<-Res.DF %>% group_by(Titre) %>% mutate(text.yc = rev(text.y)) # it is neccessary to reverse the order of y titres in each group

#Write these files for checking
#Check the data if necessary
write.table(Res.DFc, "clipboard", sep="\t", col.names=T, row.names=F) 
write.csv(Res.DFc, "Res.DF_plot.csv", row.names=F) 

g_ghisc<-ggplot(Res.DFc, aes(Titre, fill = as.factor(Assay_number), group = nn))+
  scale_x_continuous(trans = 'log2', limits = c(1, 256), breaks = c(1,2,4,8,16,32,64,128, 256), 
                     labels = c("neat", "1 in 2", "1 in 4", "1 in 8", "1 in 16", "1 in 32", "1 in 64", "1 in 128", "1 in 256"))+ #x axis settings
  scale_y_continuous(limits = c(0, 16), breaks = c(0, 4, 8, 12, 16), expand = c(0,0))+ #y axis settings
  geom_histogram(binwidth = 1, colour="black")+ #histogram settings
  scale_fill_grey(start = 1, end = 0.7, name = "Assay number")+ # fill colour settings with adjustable grey scale
  #scale_fill_brewer(palette = "OrRd", name = "Assay number")+ #colour settings for photocopying graphs
  theme_classic()+                                               # simple theme
  theme(axis.title=element_text(size = 13), axis.text.x = element_text(size = 12, angle = 45, hjust=1), axis.text.y = element_text(size = 12))+ #adjustments for axis text
  labs(x = "Maximum dilution giving positive result", y = "Number of labs")+ #x and y axis labels
  annotate("text",x=Res.DFc$text.x,y=Res.DFc$text.yc,label=Res.DFc$Lab_number,size=4.0) #annotate with labels inside the boxes, eg lab number
g_ghisc

#Make some tiffs
tiff("CS640 HPA15b 3.tif", units="in",width=11,height=9.0,res=300,compression='lzw')
g_ghisc
dev.off()