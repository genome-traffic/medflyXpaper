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

kmers_fl_blast <- read.delim("input_data/2kmers_genomeassembly.blast", header=T, stringsAsFactors=FALSE)
repeats_fl_blast <- read.delim("input_data/repeats_genomeassembly.blast", header=T)
coordinates <- read.delim("input_data/top6_scaffolds.bed", header=T, stringsAsFactors=FALSE)
inverse<-read.table("input_data/inverse.txt", header=T, sep="\t")
inverse2<-read.table("input_data/inverse2.txt", header=T, sep="\t")

# Data_wrangling ----------------------------------------------------------

#select hits only on the first 6 scaffolds
kmers_fl_blast<-subset(kmers_fl_blast, kmers_fl_blast$chromosome<7)
repeats_fl_blast<-subset(repeats_fl_blast, repeats_fl_blast$chromosome<7)
kmers_fl_blast$chromosome<-as.factor(kmers_fl_blast$chromosome)
repeats_fl_blast$chromosome<-as.factor(repeats_fl_blast$chromosome)

#selecting cutoffs for blast
#for kmers
kmers_fl_blast<-subset(kmers_fl_blast, kmers_fl_blast$alignmentlength>23)
kmers_fl_blast<-subset(kmers_fl_blast, kmers_fl_blast$mismatches<2)
#for repeats
repeats_fl_blast<-subset(repeats_fl_blast, repeats_fl_blast$identity>95)
repeats_fl_blast<-subset(repeats_fl_blast, repeats_fl_blast$alignmentlength>250)

#renaming name of repeats for plotting. Also combining gypsy hits into a single hit. Tandem left
repeats_fl_blast$select_kmer[repeats_fl_blast$kmer_id=="rnd-1_family-99"]<-"gypsy"
repeats_fl_blast$select_kmer[repeats_fl_blast$kmer_id=="rnd-6_family-1433"]<-"gypsy"
repeats_fl_blast$select_kmer[repeats_fl_blast$kmer_id=="rnd-4_family-524"]<-"gypsy"
repeats_fl_blast$select_kmer[repeats_fl_blast$kmer_id=="rnd-6_family-6261"]<-"tandem"
#renaming name of kmers of plotting.
kmers_fl_blast$select_kmer[kmers_fl_blast$kmer_id=="kmer_26033678.0"]<-"Cas9.2"
kmers_fl_blast$select_kmer[kmers_fl_blast$kmer_id=="kmer_343044347.0"]<-"Cas9.1"
all<-rbind(kmers_fl_blast, repeats_fl_blast)


# plotting ----------------------------------------------------------------



ggplot()+
  geom_rect(data=coordinates,aes(xmin=start, xmax=end, ymin=0, ymax=3),color="black",fill="white",size=0.4)+
  geom_point(data=kmers_fl_blast,aes(x=s.start,y=1,fill=select_kmer),shape=21,color="black",alpha=1, size=3)+
  geom_point(data=repeats_fl_blast,aes(x=s.start,y=2,fill=select_kmer),shape=22,color="black",alpha=1, size=3)+
  #geom_jitter(data=subset(kmersc5,kmersc5$name_sel=="top"),aes(x=s.start,y=2),shape=21, fill="pink",color="black",alpha=0.8, size=3)+
  #geom_jitter(data=subset(kmersc5,kmersc5$name_sel!="all_others" & kmersc5$name_sel!="top"),aes(x=s.start,y=5,fill=name_sel),shape=21,color="black",alpha=0.8, size=3)+
  #geom_jitter(data=subset(kmersc6,kmersc6$name_sel!="all_others" & kmersc5$name_sel!="top"),aes(x=s.start,y=5,fill=name_sel),shape=22,color="black",alpha=0.7, size=2)+
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
  scale_fill_brewer(palette="Set1")
ggsave("output_data/plot.png",width=5, height=4)


ggplot()+
  geom_bar(data=all,aes(x=chromosome, y=..count.., fill=select_kmer),color="black")+
  scale_fill_brewer(palette="Set1")+
  theme_bw(base_size=15)
ggsave("output_data/plot2.png",width=4, height=5)

group.colors2 <- c(we = "black", Cas9_we = "grey", Cas9.1 = "green4", Cas9.2 = "magenta4", Cas12a_we = "red",Cas12a_1_2_we = "royalblue3")


ggplot()+
  geom_rect(data=coordinates,aes(xmin=start, xmax=end, ymin=0, ymax=3),color="black",fill="white",size=0.4)+
  geom_point(data=inverse2,aes(x=start,y=1,fill=name_sel),shape=21,color="black",alpha=1, size=3)+
  geom_text(data=subset(inverse2,inverse2$name_sel=="Cas9.1" | inverse2$name_sel=="Cas9.2"| inverse2$name_sel=="we") ,aes(x=start,y=2, label=new_name),size=2)+
  geom_text(data=subset(inverse2,inverse2$name_sel=="Cas9_we") ,aes(x=start,y=2, label=new_name),size=2,hjust=+0.8)+
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
