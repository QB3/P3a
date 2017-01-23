#source("D:/P3a/P3a_2.R")
#source("D:/P3A/P3a-master/generate_dist.R")

#la fonction max_dist est l'implémentation de l'algorithme qui à partir de la matrice des distances 
#renvoie les éléments les plsu éloignés

max_dist=function(d, N,k){
  index <- -1 
  max_dis = -1
  res <- rep(0, k)
  for (p in 1:N){   
      num_chosen <- p
      
      chosen <- rep(0,N)
      chosen[num_chosen] <- 1
      
      a <- rep(0, k)
      a[1] <- num_chosen
      
      dis_to_set <-rep(0,N)
      
      for(i in 1:N){
          dis_to_set[i] <- d[i,num_chosen]
      }
      
      for( i in 2:k){
          far <- 1
          dist <- -1
          for( j in 1:N){
              if(chosen[j]==0 & dist<dis_to_set[j]){
                  dist <- dis_to_set[j]
                  far <- j
              }
          }
          a[i] <- far
          chosen[far]<-1
          for( j in 1:N){
            if(chosen[j]==0 ){
                dis_to_set[j] <- min(dis_to_set[j],d[j,far])
            }
          }
          
      }
      current_dist <- -1
      for ( i in 1:k){
          for(j in 1:k){
              if(d[a[i],a[j]]>current_dist){
                current_dist <- d[a[i],a[j]]
              }  
          }
      }
      if(current_dist>max_dis){
        max_dist <- current_dist
        index <- p
        for(m in 1:k){
          res[m] = a[m]
        }
      }
  }
  
  
  w =  rep(0, k)
  for (i in 1:N){
    k_min = 1
    dist_min = d[i,res[1]]
    for(j in 1:k){
      if(d[i,res[j]]<dist_min){
        k_min= j
        dist_min = d[i,res[j]]
      }
    }
    w[k_min] = w[k_min]+1;
  }
  for(i in 1:k){
    w[i] = w[i]/ N
  }
  return(list(res,w))
}

