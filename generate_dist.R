library(randomForest)
library(ggplot2)


#la fonction generate forest génère une forêt de taille nTree à partir du jeu de données train
generate_forest=function(nTree, train){
  liste_arbres=list()
  mse=matrix(0, 1 , nTree)
  sum=matrix(0, 1,dim(test)[1])

  for (i in 1:nTree){
    fit <- randomForest(feature_to_predict ~ .,
                        data=train, importance=TRUE,  ntree=1, mtry=2)

    liste_arbres=c(liste_arbres, list(fit))
    sum=sum+predict(fit, test)
    pred=sum/i
    mse_arbre=mean((pred-test$feature_to_predict)^2)
    mse[1,i]=mse_arbre
  }
  #plot(1:nTree, mse)

  return (liste_arbres);
}


#à partir d'une liste d'arbres (ie une forêt), la fonction distance_matrix génère la matrice des distance entre les arbres
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


#plot_forest calcule à partir d'une forêt le risque quadratique sur successivement 1,2,3, ..., nTree arbres
#la méthode d'aggrégation dans la forêt est ici une moyenne
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

# plot_forest_poids=function(liste_arbres, tab_poids,test_set, nTree){
#   test_set=test
#   n=dim(test_set)[1]
#   pred=matrix(0, n , nTree)
#   mse=matrix(0, 1, n)
#   renormaliseur
# 
#   for (i in 1:nTree){
#     pred[,i]=predict(liste_arbres[[i]], test_set)
#   }  
#   
  
  
#}