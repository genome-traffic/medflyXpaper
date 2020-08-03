if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)
if (!require(data.table)) install.packages('data.table')
library(data.table)
if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(ggrepel)) install.packages('ggrepel')
library(ggrepel)

##Set wd based on source
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir.create("output_data/")

# Data_wrangling ----------------------------------------------------------

Cas9_srd<-read.table("input_data/Cas9_srd.txt", header=T, sep="\t")
group.colors3 <- c(F = "orangered", M = "dodgerblue2",None="grey")
Cas9_srd$cross <- factor(Cas9_srd$cross,levels = c("Cas12a.multiplex","Cas9.1", "Cas9.2","double", "WT"))
Cas9_srd$strain <- factor(Cas9_srd$strain,levels = c("1","1a","1b","1c","1d","1e","1f","1g","2a","2b","2c","2d","1e+2a","1e+2c","2a+2c","WT"))
levels(Cas9_srd$strain)


Cas9_srd$sel<-1
Cas9_srd$sel[Cas9_srd$cross == "double"]<-2
Cas9_srd$sel[Cas9_srd$cross == "WT"]<-2

Cas9_srd1<-subset(Cas9_srd, Cas9_srd$sel==1)
Cas9_srd2<-subset(Cas9_srd, Cas9_srd$sel==2)
# Plotting ----------------------------------------------------------------


ggplot()+
  geom_hline(yintercept=50)+
  geom_boxplot(data=Cas9_srd1, aes(x=strain, y=pct_male, fill=Transgenic_Parent),alpha=0.7, position = position_dodge2(preserve = "single"))+
  geom_point(data=Cas9_srd1, aes(x=strain, y=pct_male,fill=Transgenic_Parent),alpha=0.7,shape=21,color="black",size=3,position = position_dodge(width = 0.8))+
  #facet_grid(~cross, scales = "free_x",space = "free")+
  theme_bw(base_size=15)+
  xlab("")+ylab("")+
  theme(legend.position = "none")+
  scale_fill_manual(values=group.colors3)+
  ylim(35,80)
ggsave("output_data/SRD3.png",width=8, height=3)

ggplot()+
  geom_hline(yintercept=50)+
  geom_boxplot(data=Cas9_srd2, aes(x=strain, y=pct_male, fill=Transgenic_Parent),alpha=0.7, position = position_dodge2(preserve = "single"))+
  geom_point(data=Cas9_srd2, aes(x=strain, y=pct_male,fill=Transgenic_Parent),alpha=0.7,shape=21,color="black",size=3,position = position_dodge(width = 0.8))+
  #facet_grid(~cross, scales = "free_x",space = "free")+
  theme_bw(base_size=15)+
  xlab("")+ylab("")+
  theme(legend.position = "none")+
  scale_fill_manual(values=group.colors3)+
  ylim(35,90)
ggsave("output_data/SRD4.png",width=2.5, height=3)





ggplot()+
  geom_hline(yintercept=50)+
  geom_boxplot(data=Cas9_srd, aes(x=strain, y=pct_male, fill=Transgenic_Parent),alpha=0.7, position = position_dodge2(preserve = "single"))+
  geom_point(data=Cas9_srd, aes(x=strain, y=pct_male,fill=Transgenic_Parent),alpha=0.7,shape=21,color="black",size=3,position = position_dodge(width = 0.8))+
  #facet_grid(~cross, scales = "free_x",space = "free")+
  theme_bw(base_size=15)+
  xlab("")+ylab("")+
  theme(legend.position = "none")+
  scale_fill_manual(values=group.colors3)+
  ylim(35,90)
ggsave("output_data/SRD2.png",width=10, height=3)


ggplot()+
  geom_hline(yintercept=50)+
  geom_boxplot(data=Cas9_srd, aes(x=strain, y=pct_male, fill=Transgenic_Parent),alpha=0.7, position = position_dodge2(preserve = "single"))+
  geom_point(data=Cas9_srd, aes(x=strain, y=pct_male,fill=Transgenic_Parent),alpha=0.7,shape=21,color="black",size=3,position = position_dodge(width = 0.8))+
  facet_grid(~cross, scales = "free_x",space = "free")+
  theme_bw(base_size=15)+
  xlab("")+ylab("")+
  theme(legend.position = "none")+
  scale_fill_manual(values=group.colors3)+
  ylim(35,90)
ggsave("output_data/SRD.png",width=10, height=3)

