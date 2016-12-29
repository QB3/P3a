source("/home/qbe/P3a/generate_dist.R")
source("/home/qbe/P3a/max_dist.R")
source("~/P3a/agg_exp.R")
library(gridExtra)
library(ggplot2)

train <- read.csv("~/P3a/ozone_train.csv")
test <- read.csv("~/P3a/ozone_test.csv")

#on choisit le nombre d'arbres dans la forêt
nTree=100 
mtry=3
#on génère la forêt
liste_arbres=generate_forest(nTree, train, mtry) 

#on trace le résultats, erreur quadratique en fonction du nombre d'arbres
g1=ggplot(plot_forest(liste_arbres, test, nTree), aes(X1, X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")#on stocke le graphe
g1

#on calcule la matrice des distances (cette étape est celle qui prend le plus de temps)
d=distance_matrix(liste_arbres)

k = 10 #nmobre d'arbres que l'on souhaite garder dans la foret
res=max_dist(d, nTree, k) #indice des arbres à garder dans la foret initiale
tab_indices=res[[1]] #on récupère le tableau des indices des arbres les plus distincts
poids=res[[2]] #ainsi que leur poids

#on récupère les arbres à garder
liste_arbres_distincts=NULL 
for (i in tab_indices){
  liste_arbres_distincts=c(liste_arbres_distincts, list(liste_arbres[[i]]))
}

#on trace la diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés
g2=ggplot(plot_forest(liste_arbres_distincts, test, k), aes(x = X1, y=X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés")
g2
grid.arrange(g1,g2)


#on veut voir le résultat avec pondération
#pondération par le nombre d'arbre le plus proches 
pred=0;
for (i in 1:k){
  pred=pred+predict(liste_arbres_distincts[[i]], test)*poids[i]
}

liste_alpha=seq(from=0, to = 0.003, by=0.00005)
alpha=alpha_opt(liste_arbres_distincts, test, liste_alpha)
mse_arbres_ponderés=mse_agg_exp(liste_arbres_distincts, alpha, test)

#on calcule l'erreur quadratique pour notre forêt final
mean((pred-test$feature_to_predict)^2)


#on trace tout sur un même graphique
res_foret_initiale=plot_forest(liste_arbres, test, nTree)
res_foret_initiale$fill="forêt initiale"

res_foret_selectionnee=plot_forest(liste_arbres_distincts, test, k)
res_foret_selectionnee$fill="forêt arbres différents, non pondérés"
data_to_plot=rbind(res_foret_initiale, res_foret_selectionnee)
data_to_plot=rbind(data_to_plot, c(k, mse_arbres_ponderés, "forêt arbre différents pondérés"))
data_to_plot$X1=as.numeric(data_to_plot$X1)
data_to_plot$X2=as.numeric(data_to_plot$X2)

#le graphique final
ggplot(data_to_plot, aes(x=X1, y=X2, color=fill))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")

