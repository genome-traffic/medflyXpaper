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
    repeats_fl_blast<-subset(repeats_fl_blast, repeats_fl_blast$alignmentlength>100)
    
#renaming name of repeats for plotting. Also combining gypsy hits into a single hit. Tandem left
    repeats_fl_blast$select_kmer[repeats_fl_blast$kmer_id=="rnd-1_family-99"]<-"gypsy"
    repeats_fl_blast$select_kmer[repeats_fl_blast$kmer_id=="rnd-6_family-1433"]<-"gypsy"
    repeats_fl_blast$select_kmer[repeats_fl_blast$kmer_id=="rnd-4_family-524"]<-"gypsy"
    repeats_fl_blast$select_kmer[repeats_fl_blast$kmer_id=="rnd-6_family-6261"]<-"tandem"
#renaming name of kmers of plotting.
    kmers_fl_blast$select_kmer[kmers_fl_blast$kmer_id=="kmer_26033678.0"]<-"Cas9.2"
    kmers_fl_blast$select_kmer[kmers_fl_blast$kmer_id=="kmer_343044347.0"]<-"Cas9.1"
    all<-rbind(kmers_fl_blast, repeats_fl_blast)

    repeats_fl_blast$pos[repeats_fl_blast$kmer_id=="rnd-1_family-99"]<-1
    repeats_fl_blast$pos[repeats_fl_blast$kmer_id=="rnd-6_family-1433"]<-2
    repeats_fl_blast$pos[repeats_fl_blast$kmer_id=="rnd-4_family-524"]<-3
    repeats_fl_blast$pos[repeats_fl_blast$kmer_id=="rnd-6_family-6261"]<-4
    
    
# plotting ----------------------------------------------------------------

ggplot()+
  geom_rect(data=coordinates,aes(xmin=start, xmax=end, ymin=0, ymax=3),color="black",fill="white",size=0.4)+
  geom_point(data=kmers_fl_blast,aes(x=s.start,y=1,fill=select_kmer),shape=21,color="black",alpha=1, size=3)+
  geom_point(data=repeats_fl_blast,aes(x=s.start,y=2,fill=select_kmer),shape=22,color="black",alpha=1, size=3)+
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
ggsave("output_data/plot_1.png",width=5, height=4)


ggplot()+
  geom_bar(data=all,aes(x=chromosome, y=..count.., fill=select_kmer),color="black")+
  scale_fill_brewer(palette="Set1")+
  theme_bw(base_size=15)
ggsave("output_data/plot_2.png",width=4, height=5)


k<-subset(kmers_fl_blast, kmers_fl_blast$chromosome==3)
r<-subset(repeats_fl_blast, repeats_fl_blast$chromosome==3)

levels(repeats_fl_blast$kmer_id)
levels(r$kmer_id)


ggplot()+
  #geom_rect(data=coordinates,aes(xmin=start, xmax=end, ymin=0, ymax=3),color="black",fill="white",size=0.4)+
  geom_point(data=k,aes(x=s.start,y=-1,fill=kmer_id),shape=21,color="black",alpha=1, size=3)+
  geom_point(data=r,aes(x=s.start,y=pos,fill=kmer_id),shape=22,color="black",alpha=1, size=3)+
 # geom_rect(data=r,aes(xmin=s.start, xmax=s.end, ymin=1.5, ymax=2,fill=kmer_id),color="black",fill="white",size=0.4)+
  #facet_grid(chromosome~.)+
  guides(color = guide_legend(override.aes = list(size=5,alpha=1)))+
  theme_classic(base_size=15)+
  # theme(axis.title.y=element_blank(),
  #       axis.text.y=element_blank(),
  #       axis.ticks.y=element_blank(),
  #       legend.position="top",
  #       panel.spacing.y =unit(.05, "lines"),
  #       strip.background = element_rect(size=0.2),
  #       strip.text.y = element_text(size=5),
  #       axis.line = element_blank())+
  scale_fill_brewer(palette="Set1")+
  coord_cartesian(xlim=c(9950000,10000000))

                  