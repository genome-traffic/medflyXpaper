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
if (!require(ggrepel)) install.packages('scales')
library(scales)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir.create("output_data/")

# Data_input ----------------------------------------------------------

coordinates <- read.delim("input_data/top6_scaffolds.bed", header=T, stringsAsFactors=FALSE)
inverse<-read.table("input_data/inverse.txt", header=T, sep="\t")
group.colors2 <- c(we = "black", Cas9_we = "grey", Cas9.1 = "green4", Cas9.2 = "magenta4", Cas12a_we = "red",Cas12a_1_2_we = "royalblue3")



# plotting ----------------------------------------------------------------

ggplot()+
  geom_rect(data=coordinates,aes(xmin=start, xmax=end, ymin=0, ymax=3),color="black",fill="white",size=0.4)+
  geom_point(data=inverse,aes(x=start,y=1,fill=name_sel),shape=21,color="black",alpha=1, size=3)+
  geom_text(data=subset(inverse,inverse$name_sel=="Cas9.1" | inverse$name_sel=="Cas9.2"| inverse$name_sel=="we") ,aes(x=start,y=2.2, label=new_name),size=2)+
  geom_text(data=subset(inverse,inverse$name_sel=="Cas9_we") ,aes(x=start,y=2.2, label=new_name),size=2,hjust=+0.8)+
  facet_grid(chromosome~.)+
  guides(color = guide_legend(override.aes = list(size=5,alpha=1)))+
  theme_classic(base_size=15)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position="top",
        panel.spacing.y =unit(.05, "lines"),
        strip.background = element_rect(size=0.2),
        strip.text.y = element_text(size=5),
        axis.line = element_blank())+
  scale_fill_manual(values=group.colors2)
ggsave("output_data/plot_inverse.png",width=5, height=4)
