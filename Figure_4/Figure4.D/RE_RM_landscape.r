if (!require(data.table)) install.packages('data.table')
library(data.table)
if (!require(RColorBrewer)) install.packages('RColorBrewer')
library(RColorBrewer)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(ggrepel)) install.packages('ggrepel')
library(ggrepel)
if (!require(treemap)) install.packages('treemap')
library(treemap)
if (!require(treemapify)) install.packages('treemapify')
 library(treemapify)
if (!require(colorspace)) install.packages('colorspace')


##Set wd based on source
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir.create("output_data/")

#Loading the data files ----------------------------------------------------------------

dfam<-read.table("input_file/dfam.txt", header=T, sep="")

# Plotting ----------------------------------------------------------------

group.colors <- c(kmer_1 = "green4", kmer_2 = "magenta4", not_kmer = "grey98")

#Landscape
ggplot(dfam, aes(area = fam_gen_perc_RM, fill = colors, label = repeatmasker_name,
                   subgroup = repeatmasker_subtype)) +
  geom_treemap() +
  geom_treemap_subgroup_border() +
  geom_treemap_subgroup_text(place = "left", grow = T, alpha = .3, colour =
                              "Black", fontface = "italic", min.size = 0) +
geom_treemap_text(colour = "black", place = "topleft", reflow = F)+
  scale_colour_manual(values=group.colors) +
  scale_fill_manual(values=group.colors) +
  theme(legend.position = "bottom")
  ggsave("output_data/kmer_res_lansdcape.png",width=8, height=6)
