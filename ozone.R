#ozone <- read.table("D:/P3A/ozone.txt", quote="\"", comment.char="")
ozone <- read.table("~/R/P3A/ozone.txt", quote="\"", comment.char="")
source("/home/qbe/R/P3A/generate_dist.R")
source("/home/qbe/R/P3A/max_dist.R")
library(gridExtra)

nTree=50
library(dummy)
ozone_dummy=dummy(ozone, int=TRUE)
ozone_dummy=cbind(ozone[,-c(12,13)], ozone_dummy)

ozone_dummy=data.frame(ozone_dummy)
n=dim(ozone_dummy)[1]
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
  print(i)
  liste_arbres_distincts=c(liste_arbres_distincts, list(liste_arbres[[i]]))
}

g2=ggplot(plot_forest(liste_arbres_distincts, test, k), aes(x = X1, y=X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés")
grid.arrange(g1,g2)

g2
