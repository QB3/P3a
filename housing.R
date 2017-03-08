source("D:/P3A/P3a-master/generate_dist.R")
source("D:/P3A/P3a-master/max_dist.R")
source("D:/P3A/P3a-masteragg_exp.R")
source("D:/P3A/P3a-master/influence_mtry.R")
library(gridExtra)
housing <- read.table("D:/P3A/P3a-master/LL.csv", quote="\"",sep=";", comment.char="")
names(housing)[7]="feature_to_predict"
n=dim(housing)[1]

set.seed(2)
rand=runif(n)
bool_1=rand<=1/3
bool_2=rand>1/3 & rand<=2/3 
bool_3=rand>2/3
train=housing[bool_1,]
test=housing[bool_2,]
CV=housing[bool_3,]
test_dim = dim(test)[1]

nTree=500
mtry=5

liste_mtry=seq(from=1, to=(dim(test)[2]-1), by=2)
plot_mse_mtry(nTree, train, liste_mtry, test)

liste_arbres=generate_forest(nTree, train, mtry) 
g1=ggplot(plot_forest(liste_arbres, test, nTree), aes(X1, X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")#on stocke le graphe
g1




d=distance_matrix(liste_arbres)

k = 10 #nmobre d'arbres que l'on souhaite garder dans la foret
res=max_dist(d, nTree, k) #indice des arbres à garder dans la foret initiale
tab_indices=res[[1]] #on récupère le tableau des indices des arbres les plus distincts
#poids=res[[2]] #ainsi que leur poids

#on récupère les arbres à garder
liste_arbres_distincts=NULL 
for (i in tab_indices){
  liste_arbres_distincts=c(liste_arbres_distincts, list(liste_arbres[[i]]))
}

A = matrix(0,test_dim,k)
for(i in 1:k){
  A[,i] = predict(liste_arbres_distincts[[i]], cross_test)
}
v = predict(liste_arbres_distincts[[i]], cross_test)
b = rep(0,test_dim)
for(i in 1:test_dim){
  b[i] = v[i]
}
fit = nnls(A,b)
poids = coef(fit)
s = sum(poids)
poids = poids/s
liste_arbres_distincts=liste_clust
k=length(liste_clust)
#on trace la diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés
g2=ggplot(plot_forest(liste_arbres_distincts, test, k), aes(x = X1, y=X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés")
g2
grid.arrange(g1,g2)

alpha=-0.1
pred=0;
S=0
for (i in 1:k){
  # i=3
  poids[i]=exp(alpha*liste_arbres_distincts[[i]]$mse[1])
  S=S+poids[i]
  pred=pred+poids[i]*predict(liste_arbres_distincts[[i]], test)
}
pred=pred/S
#on calcule l'erreur quadratique pour notre forêt final
mean((pred-test$feature_to_predict)^2)
poids

liste_alpha=seq(from=0, to = 0.2, by=0.001)
res=agg_exp_liste_alpha(liste_arbres_distincts, test, liste_alpha)
plot(liste_alpha, res)
res=alpha_opt(liste_arbres_distincts, test, liste_alpha)
pred=agg_exp(liste_arbres_distincts, res)

#on trace tout sur un même graphique
res_foret_initiale=plot_forest(liste_arbres, test, nTree)
res_foret_initiale$fill="forêt initiale"

res_foret_selectionnee=plot_forest(liste_arbres_distincts, test, k)
res_foret_selectionnee$fill="forêt arbres différents, non pondérés"
data_to_plot=rbind(res_foret_initiale, res_foret_selectionnee)
data_to_plot=rbind(data_to_plot, c(k, mean((pred-test$feature_to_predict)^2), "forêt arbre différents pondérés"))
data_to_plot$X1=as.numeric(data_to_plot$X1)
data_to_plot$X2=as.numeric(data_to_plot$X2)
#le graphique final
ggplot(data_to_plot, aes(x=X1, y=X2, color=fill))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")


# 
# res_2=NULL
# for( i in liste_arbres){
#   res_2=rbind(res_2, predict(i, test))
# }

# library(FactoMineR)
# #PCA
# res_2=as.data.frame(res_2)
# library(FactoMineR)
# res.pca <- PCA(res_2, scale=T)
# 
# summary(res.pca)
# barplot(res.pca$eig[,1], main = "Eigenvalues", names.arg = 1:nrow(res.pca$eig))
# plot.PCA(res.pca)
# 
# res.pca$ind$coord; 
# res.pca$ind$cos2; res.pca$ind$contrib
# plot.PCA(res.pca, axes = c(3, 4), habillage = 1)
# dimdesc(res.pca)
# plotellipses(res.pca, 1)
# 
# aa=HCPC(res.pca)
# names(aa)
# names(aa$desc.ind)


