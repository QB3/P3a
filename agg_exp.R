#pondération avec des poids exponentiels
agg_exp=function(liste_arbres_distincts, alpha,test){
  pred=0;
  S=0
  k=length(liste_arbres_distincts)
  for (i in 1:k){
    poids_exp=exp(-alpha*liste_arbres_distincts[[i]]$mse)
    pred=pred+predict(liste_arbres_distincts[[i]], test)*poids_exp
    #print(poids_exp)
    S=S+poids_exp
  }
  pred=pred/S
  return(pred)
}

mse_agg_exp=function(liste_arbres_distincts, alpha, test){
  pred=agg_exp(liste_arbres_distincts, alpha,test)
  return(mean((pred-test$feature_to_predict)^2))
}

agg_exp_liste_alpha=function(liste_arbres_distincts, test, liste_alpha){
  l=length(liste_alpha)
  i=1
  tab=matrix(0,1,l)
  for(alpha in liste_alpha){
    tab[i]=mse_agg_exp(liste_arbres_distincts, alpha, test)
    i=i+1
  }
  return(tab)
}

# liste_alpha=seq(from=0, to = 0.003, by=0.00005)
# res=agg_exp_liste_alpha(liste_arbres_distincts, test, liste_alpha)
# plot(liste_alpha, res)
# 
# pred=agg_exp(liste_arbres_distincts, 0.00175)

#il faudra rajouter un jeu de données de cross validation
alpha_opt=function(liste_arbres_distincts, test, liste_alpha){
  tab=agg_exp_liste_alpha(liste_arbres_distincts, test, liste_alpha)
  m=which.min(tab)
  return(liste_alpha[m])
}

# res=alpha_opt(liste_arbres_distincts, test, liste_alpha)


#crer une finction poids 
poids_exp = function(liste_arbres_distincts, test){
  liste_alpha = seq(from=0, to = 0.0003, by=0.000005)
  res=agg_exp_liste_alpha(liste_arbres_distincts, test, liste_alpha)
  plot(liste_alpha, res)
  alpha_opt = alpha_opt(liste_arbres_distincts, test,liste_alpha)
  print(alpha_opt)
  pred=0;
  S=0
  poids = rep(0,k)
  k=length(liste_arbres_distincts)
  for (i in 1:k){
    poid_exp=exp(-alpha_opt*liste_arbres_distincts[[i]]$mse)
    poids[[i]] = poid_exp
    pred=pred+predict(liste_arbres_distincts[[i]], test)*poid_exp
    #print(poid_exp)
    S=S+poid_exp
  }
  
  poids = poids/S
  return(poids)
  
}