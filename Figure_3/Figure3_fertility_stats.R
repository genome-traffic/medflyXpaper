
#----- data -------
fer<-read.table("input_data/Cas9_fertility_Niki.txt", header=T, sep="\t")
fer <- fer[,0:7]
fer$replicate <- as.factor(fer$replicate)
fer <- within(fer, strain <- relevel(strain, ref = 12))
fer <- within(fer, sex <- relevel(sex, ref = 3))
fer <- subset(fer, sex != "female")
fer <- aggregate(.~strain+cross+sex, data=fer, median)
fer$replicate <- NULL
fer$survival <- fer$adults/fer$embryos
fer$crsex <- with(fer, interaction(cross, sex))
fer <- droplevels(fer)

#----- glm
model <- glm(cbind(as.integer(adults),(as.integer(embryos)-as.integer(adults))) ~ crsex, family=binomial, data=fer)
print(summary(model))
