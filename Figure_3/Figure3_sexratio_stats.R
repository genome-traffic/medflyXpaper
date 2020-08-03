setwd("/Users/nikolai/Desktop/Papers/Angela/CCstats")
#dir.create("output_data/")

# --- data input
srd<-read.table("input_data/Cas9_srd.txt", header=T, sep="\t")
srd$cross <- factor(srd$cross,levels = c("Cas9.1", "Cas9.2","double","Cas12a.multiplex", "WT"))
srd$Generation <- as.factor(srd$Generation)
srd <- within(srd, strain <- relevel(strain, ref = 16))

# -- Stats PANEL A
#srd <- subset(srd, strain == "1a")
srd <- subset(srd, cross == "Cas9.2")
srd <- subset(srd, Transgenic_Parent == "M")
srd <- droplevels(srd)
y<-cbind(srd$Male_progeny,srd$Female_progeny)
model<-glm(y~Transgenic_Parent + as.factor(Generation), family=binomial, data=srd)
print(summary(model))

#OR

# # # -- Stats PANEL D
# srd <- subset(srd, cross == "double" | cross == "WT")
# srd <- droplevels(srd)
# y<-cbind(srd$Male_progeny,srd$Female_progeny)
# model<-glm(y~strain, family=binomial, data=srd)
# print(summary(model))

# # #plot(model2)