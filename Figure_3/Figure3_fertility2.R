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


#----- data -------
fer<-read.table("input_data/Cas9_fertility_Niki.txt", header=T, sep="\t")
fer <- fer[,0:7]
fer$replicate <- as.factor(fer$replicate)
fer <- within(fer, strain <- relevel(strain, ref = 12))
fer <- within(fer, sex <- relevel(sex, ref = 3))
#fer <- subset(fer, cross != "WT")
fer <- subset(fer, sex != "female")

fer <- aggregate(.~strain+cross+sex, data=fer, median)
fer$replicate <- NULL
fer$survival <- fer$adults/fer$embryos
fer$crsex <- with(fer, interaction(cross, sex))
fer <- droplevels(fer)
#fer$crsex = factor(fer$crsex,levels(fer$crsex)[c(1,3,2,4)])
#fer$crsex = factor(fer$crsex,levels(fer$crsex)[c(1,2,4,3,5)])

#----- glm
out <- glm(cbind(as.integer(adults),(as.integer(embryos)-as.integer(adults))) ~ crsex, family=binomial, data=fer)
print(summary(outs))

group.colors3 <- c(Cas9.1 = "dodgerblue2", Cas9.2 = "dodgerblue2",WT="grey")
group.colors2 <- c(Cas9.1 = "dodgerblue2", Cas9.2 = "dodgerblue2",WT="grey")


ggplot(data = fer, mapping = aes(x = crsex, y = survival,fill=cross))+
  geom_boxplot(alpha=0.7)+
  geom_point(alpha=0.7, shape=21, color="black", size=3)+
  theme_bw(base_size=15)+
  xlab("")+ylab("")+
  theme(legend.position = "none")+
  scale_fill_manual(values=group.colors3)+
  ylim(0.1,0.4)
ggsave("output_data/fertility2.png",width=2.5, height=3)



