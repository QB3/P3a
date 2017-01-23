

brute_force = function(liste_arbres,test,nTree,k){
  test_dim = dim(test)[1]
  B = matrix(0,test_dim,nTree)
  for(i in 1:nTree){
    B[,i] = predict(liste_arbres[[i]], test)
  }
  w = test[,"feature_to_predict"]
  m=1
  for(i in 1:nTree){
    m = 2*m
  }
  m = m
  liste_opt = rep(0,k)
  erreur_opt = -1
  u = 1
  while(u<m){
    print(u)
    l = 0
    v = u
    while(v>0){
      if(v %% 2==1){
        l=l+1
      }
      v = v%/%2
    }
    if(l==k){
      p = 0
      l1=0
      liste_indices = rep(0,k)
      v = u
      while(v>0){
        p = p+1
        if(v %% 2==1){
          l1 = l1+1
          
          liste_indices[l1]= p
        }
        v = v%/%2
      }
      
     
      A = matrix(0,train2_dim,k)
      for(i in 1:k){
        A[,i] = B[,liste_indices[[i]]]
      }
      
      fit = nnls(A,w)
      poids = coef(fit)
      s = sum(poids)
      poids = poids/s
      pred=0;
      for (i in 1:k){
        pred=pred+A[,i]*poids[i]
      }
      
      #on calcule l'erreur quadratique pour notre forêt final
      erreur = mean((pred-test$feature_to_predict)^2)
      if((erreur_opt == -1)||(erreur<erreur_opt)){
        erreur_opt = erreur
        for(j in 1:k){
          liste_opt[j] = liste_indices[j]
        }
      }
    }
    u = u+1
  }
  return (liste_opt)
}
