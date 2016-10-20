# <<<<<<< HEAD

library(randomForest)
library(ggplot2)


generate_forest=function(nTree, train){
  liste_arbres=list()
  mse=matrix(0, 1 , nTree)
  sum=matrix(0, 1,dim(test)[1])

  for (i in 1:nTree){
    fit <- randomForest(feature_to_predict ~ .,
                        data=train, importance=TRUE,  ntree=i, mtry=4)

    liste_arbres=c(liste_arbres, list(fit))
    sum=sum+predict(fit, test)
    pred=sum/i
    mse_arbre=mean((pred-test$feature_to_predict)^2)
    mse[1,i]=mse_arbre
  }
  plot(1:nTree, mse)

  return (liste_arbres);
}


distance_matrix=function(liste_arbres){
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
  return (distance_matrix)
}


plot_forest=function(liste_arbres, test_set, nTree){
  test_set=test
  mse=matrix(0, 1 , nTree)
  sum=matrix(0, 1, dim(test)[1])

  for (i in 1:nTree){

    sum=sum+predict(liste_arbres[[i]], test_set)
    pred=sum/i
    mse_arbre=mean((pred-test_set$feature_to_predict)^2)
    mse[1,i]=mse_arbre
  }
  data=data.frame(cbind(t(t(1:nTree)), t(mse)))
  return(data)
  # return(ggplot(data, aes(x=X1, y=X2)))

}