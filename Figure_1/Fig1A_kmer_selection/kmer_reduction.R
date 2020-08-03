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



a<-rep(c("all"), times = 1000)
b<-rep(c("A"), times = 1000)
c<-rep(c(5), times = 1000)
d <- data.frame(a, b,c)

a<-rep(c("red"), times = 500)
b<-rep(c("A"), times = 500)
c<-rep(c(4), times = 500)
e <- data.frame(a, b,c)

a<-rep(c("tar1"), times = 50)
b<-rep(c("A"), times = 50)
c<-rep(c(3), times = 50)
f <- data.frame(a, b,c)

a<-rep(c("tar2"), times = 50)
b<-rep(c("A"), times = 50)
c<-rep(c(3), times = 50)
g <- data.frame(a, b,c)

a<-rep(c("top"), times = 25)
b<-rep(c("A"), times = 25)
c<-rep(c(2), times = 25)
h <- data.frame(a, b,c)

a<-rep(c("sel"), times = 4)
b<-rep(c("A"), times = 4)
c<-rep(c(1), times = 4)
i <- data.frame(a, b,c)

j<-rbind(d,e,f,g,h,i)
                
i<-j

group.colors3 <- c(all = "grey", red = "red",tar1="grey",tar2="grey",top="pink",sel="gold")

ggplot()+
geom_jitter(data=subset(i, i$a=="all"),aes(x=b,c),shape=21,fill="grey",size=2,width = 0.3, height = 0.4,alpha=0.7)+
  geom_jitter(data=subset(i, i$a=="red"),aes(x=b,c,fill=a),shape=21,color="black",size=2,width = 0.3, height = 0.4,alpha=0.7)+  
  geom_jitter(data=subset(i, i$a=="tar1"),aes(x=b,c,fill=a),shape=22,color="black",size=2,width = 0.3, height = 0.4,alpha=0.7)+
  geom_jitter(data=subset(i, i$a=="tar2"),aes(x=b,c,fill=a),shape=23,color="black",size=2,width = 0.3, height = 0.4,alpha=0.7)+
  geom_jitter(data=subset(i, i$a=="top"),aes(x=b,c,fill=a),shape=21,color="black",size=2,width = 0.3, height = 0.3,alpha=1)+
  geom_jitter(data=subset(i, i$a=="sel"),aes(x=b,c,fill=a),shape=21,color="black",size=2,width = 0.1, height = 0.1,alpha=1)+
  theme_void()+
  scale_fill_manual(values=group.colors3)+
  coord_flip()

ggsave("output_data/plot_1.png",width=6, height=4)


