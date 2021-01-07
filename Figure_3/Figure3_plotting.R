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
if (!require(ggrepel)) install.packages('pcr')
library(pcr)

##Set wd based on source
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
dir.create("output_data/")

# Panel A ----------------------------------------------------------

Cas9_srd<-read.table("input_data/Cas9_srd_pooled_cross.txt", header=T, sep="\t")
group.colors3 <- c(F = "orangered", M = "dodgerblue2",None="grey")

ggplot()+
  geom_hline(yintercept=50)+
  geom_boxplot(data=Cas9_srd, aes(x=strain, y=pct_male, fill=Transgenic_Parent),alpha=0.7, position = position_dodge2(preserve = "single"))+
  geom_point(data=Cas9_srd, aes(x=strain, y=pct_male,fill=Transgenic_Parent),alpha=0.7,shape=21,color="black",size=3,position = position_dodge(width = 0.8))+
  #facet_grid(~cross, scales = "free_x",space = "free")+
  theme_bw(base_size=15)+
  xlab("")+ylab("")+
  theme(legend.position = "none")+
  scale_fill_manual(values=group.colors3)+
  ylim(35,80)
ggsave("output_data/Panel_A.png",width=8, height=3)


# Panel B ----------------------------------------------------------

fer<-read.table("input_data/Cas9_fertility.txt", header=T, sep="\t")
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


ggplot(data = fer, mapping = aes(x = crsex, y = survival,fill=cross))+
  geom_boxplot(alpha=0.7)+
  geom_point(alpha=0.7, shape=21, color="black", size=3)+
  theme_bw(base_size=15)+
  xlab("")+ylab("")+
  theme(legend.position = "none")+
  scale_fill_manual(values=group.colors3)+
  ylim(0.1,0.4)
ggsave("output_data/Panel_B.png",width=2.5, height=3)

# Panel C ----------------------------------------------------------

testes<-read.table("input_data/qRT_testes.txt", header=T, sep="\t")
ovary<-read.table("input_data/qRT_ovary.txt", header=T, sep="\t")
maleC<-read.table("input_data/qRT_maleC.txt", header=T, sep="\t")
femaleC<-read.table("input_data/qRT_femaleC.txt", header=T, sep="\t")

group_var <- rep(c('WT', 'Cas9.1e', 'Cas9.2a', 'Cas9.2b', 'Cas9.2c', 'Cas9.2d', 'Cas9.w'), each = 3)

te <- pcr_analyze(testes,
                  group_var = group_var,
                  reference_gene = 'RpL19',
                  reference_group = 'WT')
te$tissue<-"testes"

ov <- pcr_analyze(ovary,
                  group_var = group_var,
                  reference_gene = 'RpL19',
                  reference_group = 'WT')
ov$tissue<-"ovary"

mc <- pcr_analyze(maleC,
                  group_var = group_var,
                  reference_gene = 'RpL19',
                  reference_group = 'WT')
mc$tissue<-"maleC"

fc <- pcr_analyze(femaleC,
                  group_var = group_var,
                  reference_gene = 'RpL19',
                  reference_group = 'WT')
fc$tissue<-"femaleC"

data<-rbind(te,ov,mc,fc)
data1<-subset(data,data$group!="Cas9.w")
data2<-subset(data1,data1$group!="WT")

group.colors2 <- c(Cas9.w = "grey", Cas9.1e = "green4", Cas9.2a = "magenta4",Cas9.2b = "magenta3",Cas9.2c = "magenta2",Cas9.2d = "magenta")


ggplot(data2, aes(x = tissue, y = relative_expression,fill=group)) +
  geom_bar(stat="identity",position="dodge",color="black")+
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.2, position=position_dodge(.9))+
  scale_fill_manual(values=group.colors2)+
  theme_bw(base_size=15)+
  coord_flip()
ggsave("output_data/Panel_C.png",width=5, height=3)

##Stats

testes_t <- pcr_test(testes,
                     group_var = group_var,
                     reference_gene = 'RpL19',
                     reference_group = 'WT',
                     test = 'lm')
testes_t$tissue<-"testes"


ovary_t <- pcr_test(ovary,
                    group_var = group_var,
                    reference_gene = 'RpL19',
                    reference_group = 'WT',
                    test = 'lm')
ovary_t$tissue<-"ovary"


maleC_t <- pcr_test(maleC,
                    group_var = group_var,
                    reference_gene = 'RpL19',
                    reference_group = 'WT',
                    test = 'lm')
maleC_t$tissue<-"maleC"

femaleC_t <- pcr_test(femaleC,
                      group_var = group_var,
                      reference_gene = 'RpL19',
                      reference_group = 'WT',
                      test = 'lm')
femaleC_t$tissue<-"femaleC"

stats_data<-rbind(testes_t,ovary_t,maleC_t,femaleC_t)
stats_data$sig<-"no"
stats_data$sig[stats_data$p_value <= 0.05]<-"yes"


# Panel E ----------------------------------------------------------

Cas9_srd<-read.table("input_data/Cas9_individual_cross.txt", header=T, sep="\t")
group.colors3 <- c(F = "orangered", M = "dodgerblue2",None="grey")
Cas9_srd$strain <- factor(Cas9_srd$strain,levels = c("1","1a","1b","1c","1d","1e","1f","1g","2b","2c","2d","WT","2a","2a+2a","2a+2c","2c+2c","1e+1e","1e+2a","1e+2c"))

ggplot()+
  geom_hline(yintercept=50)+
  geom_boxplot(data=Cas9_srd, aes(x=strain, y=pct_male, fill=Transgenic_Parent),alpha=0.7, position = position_dodge2(preserve = "single"))+
  geom_point(data=Cas9_srd, aes(x=strain, y=pct_male,fill=Transgenic_Parent),alpha=0.7,shape=21,color="black",size=3,position = position_dodge(width = 0.8))+
  #facet_grid(~cross, scales = "free_x",space = "free")+
  theme_bw(base_size=15)+
  xlab("")+ylab("")+
  theme(legend.position = "none")+
  scale_fill_manual(values=group.colors3)+
  scale_y_continuous(breaks = c(40, 50, 60,70,80,90),limits = c(35, 95))
ggsave("output_data/Panel_E.png",width=4.5, height=3)





