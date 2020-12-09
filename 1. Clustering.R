library(diffusionMap)
library(randomForest)
library(VSURF)
library(cluster)
library(reshape2)
library(ggplot2)
library(mice)
library(neuralnet)
library(keras)
library(devtools)
library(tensorflow)
library(caret)
library(e1071)
library(pROC)
library(ROCR)
library(kohonen)
library(tempR)
library(mclust)

data=read.csv("H:/apps/xp/desktop/R Code/phenotyping_new/data/final_with_biomarker_new.csv")
data_pt=data[,c(1,375:410,418:492,494:807)]

id=data_pt$ID
data_pt=data_pt[,-1]

###Random Forest Model
set.seed(19900315)
fit = randomForest(PTSD_status~., data=data_pt, ntree=4000, proximity=TRUE) 

print(fit)
D = 1-fit$proximity
rn=which(data_pt$PTSD_status == "Positive")
DP=D[rn,rn]

##Use PAM
pamRF <- pam(DP, k = 2)

ct=as.data.frame(cbind(pamRF$clustering,rn))
data_pt$row=1:nrow(data_pt)
data_pt$ct1=ct$V1[match(data_pt$row,ct$rn)]
data_pt$ct1=ifelse(is.na(data_pt$ct1),0,data_pt$ct1)
data_pt=data_pt[,-426]
data_pt$ct=ifelse(data_pt$ct1==1,2,ifelse(data_pt$ct1==2,1,0))
data_pt=data_pt[,-426]

####Do silhouette Score
dmap = diffuse(D,eps.val=30, t=0, neigen=2)

ct1=data_pt$ct
#ct1=ct1+2
ct1=ifelse(ct1==0,"green",ifelse(ct1==1,"blue","red"))

plot(dmap$X[,1],dmap$X[,2],col=ct1, pch=19,
     xlab="Diffusion Map Coordinate 1", 
     ylab="Diffusion Map Coordinate 2")

legend(-0.0025, 0.0012,title="Group",c("PTSD- (71)","PTSD S1 (26)","PTSD S2 (48)"),fill=c("green","blue","red")  ,cex=0.8)



###Silhouette plot
ct2=data_pt$ct
ct2=ct2+1
sil=silhouette(ct2,D)
windows()
plot(sil,main="Silhouette Plot of PTSD -, PTSD S1 and S2",col=c("green","blue","red"))
legend(-0.375, 140,title="Group",c("PTSD- (71)","PTSD S1 (26)","PTSD S2 (48)"),fill=c("green","blue","red")  ,cex=0.8)


###Save the data

data$ct=data_pt$ct

#write.csv(data,"H:/apps/xp/desktop/R Code/Carole phenotyping paper/data/data_with_cluster.csv",row.names = F)




