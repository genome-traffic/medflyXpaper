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

white<-read.table("input_data/white.assay.txt", header=T, sep="\t")
white$Strain <- factor(white$Strain,levels = c("Cas9", "Cas12a", "Control"))
levels(white$Cross2)

# Plotting ----------------------------------------------------------------

ggplot()+
  geom_boxplot(data=white, aes(x=Cross2, y=efficiency),fill="grey",alpha=0.7)+
  geom_point(data=white, aes(x=Cross2, y=efficiency),size=3)+
  facet_grid(Strain~., scales="free")+
  theme_bw(base_size=15)+
  xlab("")+ylab("")+
  coord_flip()
ggsave("output_data/white.png",width=6, height=4)

