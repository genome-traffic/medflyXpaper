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

df_wide_85<-read.table("input_data/df_wide_85.txt", header=T, sep="", na.strings = "NA")
df_wide_270<-read.table("input_data/df_wide_270.txt", header=T, sep="", na.strings = "NA")

#How i calculate the value of kmer specificity for esp_array 
# Data_270 ----------------------------------------------------------
# df_wide_270<-mutate(df_wide_270, esp_array = ((j.counts+genome+sc3)/(j.counts*3)))
# Data_85 ----------------------------------------------------------
#df_wide_85<-mutate(df_wide_85, esp_array = ((j.counts+genome+sc3)/(j.counts*3)))



# Plotting ----------------------------------------------------------------
ggplot()+
  geom_point(data=df_wide_270, aes(x=sc3, y=genome,color=per))+
  geom_point(data=subset(df_wide_270,df_wide_270$esp_array==1),aes(x=sc3, y=j.counts), size=0.8)+
  geom_point(data=subset(df_wide_270,df_wide_270$kmer.id=="35821_50"),aes(x=sc3, y=genome), color = "forestgreen", size=2)+
  scale_color_gradient(low="blue", high="red")+
  annotate("text", 
         80, 10, 
           label = " 270kb-array kmer specifics", 
           color = "black", 
           size=3)+
  annotate("text", 
           80,50, 
           label = "kmer_2", 
           color = "forestgreen", 
           size=3)+
#  ylim(0,70)+xlim(0,100)+
  theme_classic()
  ggsave("output_data/esp_270kb_mers.png",width=9, height=5)


ggplot()+
  geom_point(data=df_wide_85, aes(x=sc3, y=genome,color=per))+
  geom_point(data=subset(df_wide_85,df_wide_85$esp_array==1),aes(x=sc3, y=j.counts))+
  geom_point(data=subset(df_wide_270,df_wide_270$kmer.id=="35821_50"),aes(x=sc3, y=genome), color = "forestgreen", size=3)+
  scale_color_gradient(low="blue", high="red")+
  annotate("text", 
           80, 10, 
           label = " ~85 kb-cluster kmer specifics", 
           color = "black", 
           size=3)+
  annotate("text", 
           70, 40, 
           label = "kmer_1", 
           color = "forestgreen", 
           size=3)+
  ylim(0,700)+xlim(0,400)+
  theme_classic()
  ggsave("output_data/esp_85kb_mers.png",width=9, height=5)

