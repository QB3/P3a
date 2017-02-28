library(randomForest)
library(ggplot2)
library(gtools)

#la fonction generate forest génère une forêt de taille nTree à partir du jeu de données train
generate_forest=function(nTree, train,test){
  liste_arbres=list()
  mse=matrix(0, 1 , nTree)
  sum=matrix(0, 1,dim(test)[1])
  
  for (i in 1:nTree){
    fit <- randomForest(feature_to_predict ~ .,
                        data=train, importance=TRUE,  ntree=1)
    
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
distance_matrix=function(liste_arbres,test){
  distance_matrix=matrix(0, nTree, nTree)
  l=1;
  m=1;
  H = NULL
  for (i in liste_arbres){
    pred=predict(i, test)
    H = c(H,pred)
  }
  for (i in liste_arbres){
    l=1
    for (j in liste_arbres){
      pred_1=H[[m]]
      pred_2=H[[l]]
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
  #test_set=test
  mse=matrix(0, 1 , nTree)
  sum=matrix(0, 1, dim(test_set)[1])
  
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


#determine a confidance level by choosing i  trees randomly and compute
#max and min values we can get
confidence_intervall=function(liste_arbres, test_set, nTree,iteration_num){
  #test_set=test
  low_curve = rep(-1,nTree)
  up_curve = rep(-1,nTree)
  
  rd_index=1:nTree
  mse=rep(0 , nTree)
  sum=rep(0, train2_dim)
  
  
  for(p in 1:iteration_num){
    rd_index = permute(rd_index)
    for(i in 1:nTree){
      sum=sum+predict(liste_arbres[[i]], test_set)
      pred=sum/i
      mse_arbre=mean((pred-test_set$feature_to_predict)^2)
      if(low_curve[i]==-1 | mse_arbre<low_curve[i]){
        low_curve[i]=mse_arbre
      }
      if(mse_arbre>up_curve[i]){
        up_curve[i]=mse_arbre
      }
    }
    
  }
  
  return(c(low_curve,up_curve))
  # return(ggplot(data, aes(x=X1, y=X2)))
  
}



# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}