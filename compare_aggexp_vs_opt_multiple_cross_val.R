
# choix les plus differents
# ponderation : optimisation mydata = ( [train1,train2], test)
mydata <- read.table("D:/P3A/P3a-master/data2.csv",sep=";",header=TRUE)
confidence_int = read.table("D:/P3A/P3a-master/confidence_int_data2.csv",sep=";",header=TRUE)

source("D:/P3A/P3a-master/generate_dist.R")
source("D:/P3A/P3a-master/max_dist.R")
source("D:/P3A/P3a-master/agg_exp.R")
library(gridExtra)
library(nnls)
library(randomForest)
library(ggplot2)

nTree=200 #on choisit le nombre d'arbres dans la forêt
library(dummy)




#on sépare le dataframe en un jeu d'entraînement et un jeu de test
n=dim(mydata)[1]
set.seed(5)
bool=runif(n)<0.30

#on divise mydata en 2 : train et test, et train en 2 pour pouvoir optimiser 
# les coeifficients

cross_test=mydata[bool,]
train=mydata[!bool,]
m=dim(train)[1]
bool=runif(m)<0.30
train2=train[bool,]
train1=train[!bool,]
train2_dim = dim(train2)[1]


#on génère la forêt
liste_arbres=generate_forest(nTree,train1,cross_test) 
#on trace le résultats, erreur quadratique en fonction du nombre d'arbres
g1=ggplot(plot_forest(liste_arbres, cross_test, nTree), aes(X1, X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")#on stocke le graphe

g1

#on calcule la matrice des distances (cette étape est celle qui prend le plus de temps)
d=distance_matrix(liste_arbres,train2)

k = 200 #nmobre d'arbres que l'on souhaite garder dans la foret


best_tree = 0
best_error =-1
for(i in 1:nTree){
  prediction = predict(liste_arbres[[i]], train2)
  m = mean((prediction-train2$feature_to_predict)^2)
  if(best_error==-1 | m<best_error){
    best_tree = i
    best_error = m
  }
}



res=max_dist(d, nTree, k,best_tree) #indice des arbres à garder dans la foret initiale
tab_indices=res[[1]] #on récupère le tableau des indices des arbres les plus distincts
#poids=res[[2]] #ainsi que leur poids

#on récupère les arbres à garder
liste_arbres_distincts=NULL 
for (i in tab_indices){
  #append(liste_arbres_distincts,liste_arbres[[i]])
  liste_arbres_distincts=c(liste_arbres_distincts, list(liste_arbres[[i]]))
}
v = train2[,"feature_to_predict"]
A = NULL
means = NULL
meansexp = NULL
id = NULL
for(selec_num in 1:k){
  id = c(id,selec_num)
  
  A = cbind(A, predict(liste_arbres_distincts[[selec_num]], train2))
  
  
  fit = nnls(A,v)
  poid = coef(fit)
  s = sum(poid)
  poid = poid/s
  
  
  predexp =agg_exp(liste_arbres_distincts, 0.0015,cross_test,selec_num)
  pred=0;
  for (i in 1:selec_num){
    yy = predict(liste_arbres_distincts[[i]], cross_test)
    pred=pred+yy*poid[i]
    
    
  }
  m = mean((pred-cross_test$feature_to_predict)^2)
  mexp = mean((predexp-cross_test$feature_to_predict)^2)
  means = c(means,m)
  meansexp = c(meansexp,mexp)
  
  
}

#on trace la diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés
g2=ggplot(plot_forest(liste_arbres_distincts, cross_test, k), aes(x = X1, y=X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés")
g2
grid.arrange(g1,g2)


#on veut voir le résultat avec pondération


#on calcule l'erreur quadratique pour notre forêt final
mean((pred-cross_test$feature_to_predict)^2)


#printing the intervall of confidence for initial trees (upper curve is the
#maximum mse we can get if we select trees differently and lower is the minimal
#library(gtools)
#iteration_num = 400

#low_curve = rep(-1,nTree)
#up_curve = rep(-1,nTree)

#rd_index=1:nTree



#for(p in 1:iteration_num){
#  sum=rep(0, dim(cross_test)[1])
#  rd_index = permute(rd_index)
#  
#  for(i in 1:nTree){
#    sum=sum+predict(liste_arbres[[rd_index[i]]], cross_test)
#    pred=sum/i
#    mse_arbre=mean((pred-cross_test$feature_to_predict)^2)
#    if(low_curve[i]==-1 | mse_arbre<low_curve[i]){
#      low_curve[i]=mse_arbre
#    }
#    if(mse_arbre>up_curve[i]){
#      up_curve[i]=mse_arbre
#    }
#  }

#}
#confidence_int = data.frame(cbind(low_curve,up_curve))
confidence_int["x"]=1:nTree




#on trace tout sur un même graphique


res_foret_selectionnee=plot_forest(liste_arbres_distincts, cross_test, k)
res_foret_selectionnee$fill="foret arbres diff�rents, non pond�r�s"

pondere = data.frame(cbind(id,means))
pondere$fill = "foret arbre differents ponderation optimale"
names(pondere) <- names(res_foret_selectionnee) 

pondexp = data.frame(cbind(id,meansexp))
pondexp$fill = "foret arbre differents ponderation exponontielle"
names(pondexp) <- names(res_foret_selectionnee) 

lower_curve = data.frame(cbind(id,confidence_int["low_curve"]))
lower_curve$fill = "meilleure performance sans pond�ration"
names(lower_curve) <- names(res_foret_selectionnee) 

upper_curve = data.frame(cbind(id,confidence_int["up_curve"]))
upper_curve$fill = "pire performance sans pond�ration"
names(upper_curve) <- names(res_foret_selectionnee) 

data_to_plot=data.frame(rbind(res_foret_selectionnee,pondexp,pondere,lower_curve,upper_curve))
names(data_to_plot)= c("x","y","fill")
#data_to_plot=rbind( res_foret_selectionnee,pondere)

#data_to_plot$X1=as.numeric(data_to_plot$X1)
#data_to_plot$X2=as.numeric(data_to_plot$X2)



#prepare the plot of the confifdance area:
a = 1:nTree
df = data.frame(cbind(a,confidence_int["low_curve"],confidence_int["up_curve"]))
names(df)= c("x","ymin","ymax")
g1 <- ggplot(df) + 
  stat_smooth(aes(x = x, y = ymin, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(aes(x = x, y = ymax, colour = "max"), method = "loess", se = FALSE)+ylab(label="mse")+xlab(label="nombre d'arbres")+
  labs(title="intervalle de confiance � 90%")
g1

# build plot object for rendering 
gg1 <- ggplot_build(g1)

# extract data for the loess lines from the 'data' slot
df2 <- data.frame(x = gg1$data[[1]]$x,
                  ymin = gg1$data[[1]]$y,
                  ymax = gg1$data[[2]]$y) 

# use the loess data to add the 'ribbon' to plot 
g1+
  geom_ribbon(data = df2, aes(x = x, ymin = ymin, ymax = ymax),
              fill = "grey", alpha = 0.4)


#le graphique final
p1 = ggplot(data_to_plot[1:600,], aes(x=x, y=y, color=fill))+
  geom_point()+stat_smooth(data=data_to_plot[601:800,],aes(x = x, y = y, colour = "min"), method = "loess", se = FALSE) +
  stat_smooth(data=data_to_plot[801:1000,],aes(x = x, y = y, colour = "max"), method = "loess", se = FALSE)
p1+geom_ribbon(data =df2, aes(x = x, ymin = ymin, ymax =ymax), fill="grey", alpha = 0.4,inherit.aes=FALSE)+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")
