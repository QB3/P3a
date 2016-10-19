#source("D:/P3a/P3a_2.R")
source("/home/qbe/R/P3A/generate_dist.R")

nTree=50
res=generate_distance(nTree)
d=res[[1]]
index <- -1 
max_dis = -1
k = 6
N = nTree
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
    }
}



num_chosen <- index

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

