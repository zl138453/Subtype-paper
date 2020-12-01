SURFB=function(data,seeds=19900315){
  rs=matrix(NA,ncol(data)-1,2)
  d1=data
  g=1:ncol(d1)
  for(i in 1:(ncol(d1)-1)){
    d_new= d1[,g]
    name=names(d_new)
    rs[i,1]=paste(name[1:(ncol(d_new)-1)],collapse = ",")
    rf=randomForest(ct~.,data=d_new,ntree=4000, proximity=TRUE,importance=TRUE)
    pred=rf$votes[,2]
    p=cbind(pred,d1$ct)
    pp <- prediction(p[,1],p[,2])
    roc.perf = performance(pp, measure = "tpr", x.measure = "fpr")
    auc.perf = performance(pp, measure = "auc")
    rs[i,2]=as.numeric(auc.perf@y.values)
    a=rf$importance[order(rf$importance[,4],decreasing = T),]
    g=g[-which(colnames(d_new)==rownames(a)[length(rownames(a))])]
  }
  rs=rs[order(rs[,2],decreasing=TRUE),]
  best_index=which(colnames(data)%in%unlist(strsplit(rs[1,1], ",")))
  mylist=list("auc"=rs,"Best_index"=best_index)
  mylist
  
}
