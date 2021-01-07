setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()

# -- Stats PANEL A ----------------------------------------

# --- data input
srd<-read.table("input_data/Cas9_srd_pooled_cross.txt", header=T, sep="\t")
srd$cross <- factor(srd$cross,levels = c("Cas9.1", "Cas9.2","double","Cas12a.multiplex", "WT"))
srd$Generation <- as.factor(srd$Generation)
#srd <- within(srd, strain <- relevel(strain, ref = 16))

# choose strain
srd <- subset(srd, strain == "1a")
srd <- droplevels(srd)
y<-cbind(srd$Male_progeny,srd$Female_progeny)
model<-glm(y~Transgenic_Parent + as.factor(Generation), family=binomial, data=srd)
print(summary(model))

# -- Stats PANEL E ----------------------------------------

# --- data input
ssrd<-read.table("input_data/Cas9_individual_cross.txt", header=T, sep="\t")
ssrd$cross <- factor(ssrd$cross,levels = c("Cas9.1", "Cas9.2","double","Cas12a.multiplex", "WT"))
ssrd$Generation <- as.factor(ssrd$Generation)
ssrd <- within(ssrd, strain <- relevel(strain, ref = "2a+2a"))
ssrd <- droplevels(ssrd)

y<-cbind(ssrd$Male_progeny,ssrd$Female_progeny)
model<-glm(y~strain, family=binomial, data=ssrd)
print(summary(model))

