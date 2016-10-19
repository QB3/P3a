#ozone <- read.table("D:/P3A/ozone.txt", quote="\"", comment.char="")
ozone <- read.table("~/R/P3A/ozone.txt", quote="\"", comment.char="")

library(dummy)
library(randomForest)

ozone_dummy=dummy(ozone, int=TRUE)
ozone_dummy=cbind(ozone[,-c(12,13)], ozone_dummy)

ozone_dummy=data.frame(ozone_dummy)
n=dim(ozone_dummy)[1]
bool=runif(n)<0.30
test=ozone_dummy[bool,]
train=ozone_dummy[!bool,]


generate_distance=function(nTree){
  liste_arbres=list()
  mse=matrix(0, 1 , nTree)
  sum=matrix(0, 1,dim(test)[1])
  
  for (i in 1:nTree){
    fit <- randomForest(maxO3 ~ T9 + T12 + T15 + Ne9 + Ne12 + Ne15 +Wx9 + Wx12 + Wx15 + maxO3y + wind_East + wind_North + 
                          wind_South + wind_West + rain_Dry+rain_Rainy,
                        data=train, importance=TRUE,  ntree=i, mtry=4)
    
    liste_arbres=c(liste_arbres, list(fit))
    sum=sum+predict(fit, test)
    pred=sum/i
    mse_arbre=mean((pred-test$maxO3)^2)
    mse[1,i]=mse_arbre
  }
  plot(1:nTree, mse)
  
  distance_matrix=matrix(0, nTree, nTree)
  l=1;
  m=1;
  for (i in liste_arbres){
    l=1
    for (j in liste_arbres){
      pred_1=predict(i, test)
      pred_2=predict(j, test)
      dist=mean((pred_2-pred_1)^2)
      distance_matrix[l,m]=dist
      l=l+1;
    }
    m=m+1
  }
  return (list(distance_matrix, liste_arbres));
}


plot_forest=function(liste_arbres, test_set, nTree){
  liste_arbres=foret
  test_set=test;
  mse=matrix(0, 1 , nTree)
  sum=matrix(0, 1, dim(test)[1])
  
  for (i in 1:nTree){
    
    sum=sum+predict(liste_arbres[[i]], test_set)
    pred=sum/i
    mse_arbre=mean((pred-test_set$maxO3)^2)
    mse[1,i]=mse_arbre
  }
  plot(1:nTree, mse)
  
}

