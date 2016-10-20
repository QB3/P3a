#ozone <- read.table("D:/P3A/ozone.txt", quote="\"", comment.char="")
ozone <- read.table("~/R/P3A/ozone.txt", quote="\"", comment.char="")
source("/home/qbe/R/P3A/generate_dist.R")
source("/home/qbe/R/P3A/max_dist.R")
library(gridExtra)

nTree=200
library(dummy)
ozone_dummy=dummy(ozone, int=TRUE)
ozone_dummy=cbind(ozone[,-c(12,13)], ozone_dummy)

ozone_dummy=data.frame(ozone_dummy)
n=dim(ozone_dummy)[1]
set.seed(5)
bool=runif(n)<0.30

n=which(names(ozone_dummy)=="maxO3")
names(ozone_dummy)[n]="feature_to_predict"
test=ozone_dummy[bool,]
train=ozone_dummy[!bool,]


liste_arbres=generate_forest(nTree, train)
g1=ggplot(plot_forest(liste_arbres, test, nTree), aes(X1, X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")#on stocke le graphe

g1

d=distance_matrix(liste_arbres)

k = 20 #nmobre d'arbres que l'on souhaite garder dans la foret
res=max_dist(d, nTree, k) #indice des arbres à garder dans la foret initiale
tab_indices=res[[1]]
poids=res[[2]]

liste_arbres_distincts=NULL #on récupère les arbres à garder
for (i in tab_indices){
  liste_arbres_distincts=c(liste_arbres_distincts, list(liste_arbres[[i]]))
}

g2=ggplot(plot_forest(liste_arbres_distincts, test, k), aes(x = X1, y=X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés")
g2
grid.arrange(g1,g2)


#on veut voir le résultat avec pondération
pred=0;
for (i in 1:k){
  pred=pred+predict(liste_arbres_distincts[[i]], test)*poids[i]
}

#on voit que l'erreur quadratique est plus petite que dans la forêt classique
mean((pred-test$feature_to_predict)^2)


#ploter le tout sur un même graphe
res_foret_initiale=plot_forest(liste_arbres, test, nTree)
res_foret_initiale$fill="forêt initiale"

res_foret_selectionnee=plot_forest(liste_arbres_distincts, test, k)
res_foret_selectionnee$fill="forêt arbres différents, non pondérés"
data_to_plot=rbind(res_foret_initiale, res_foret_selectionnee)
data_to_plot=rbind(data_to_plot, c(k, mean((pred-test$feature_to_predict)^2), "forêt arbre différents pondérés"))
data_to_plot$X1=as.numeric(data_to_plot$X1)
data_to_plot$X2=as.numeric(data_to_plot$X2)

ggplot(data_to_plot, aes(x=X1, y=X2, color=fill))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")

