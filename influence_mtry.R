nTree=500
liste_mtry=seq(from=1, to =dim(test)[2], by=2)

final_data=function(nTree, train, liste_mtry, test){
  choix_K=NULL  
  for( mtry in liste_mtry){
    # mtry=1
    liste_arbres=generate_forest(nTree, train, mtry) 
    data=plot_forest(liste_arbres, test, nTree)
    data$mtry=mtry
    choix_K=rbind(choix_K, data)
  }
  choix_K$mtry=as.factor(choix_K$mtry)
  return(choix_K)
}

plot_mse_K=function(nTree, train, liste_mtry, test){
  choix_K=final_data(nTree, train, liste_mtry, test)
  ggplot(choix_K, aes(x=X1, y=X2, color=mtry))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
         labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")
}

plot_mse_K(nTree, train, liste_mtry, test)
