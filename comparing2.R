
# parangon
# ponderation : optimisation mydata = ( [train1,train2], test)
mydata <- read.table("D:/P3A/P3a-master/data3.csv",sep=";",header=TRUE)

source("D:/P3A/P3a-master/generate_dist.R")
source("D:/P3A/P3a-master/max_dist.R")
source("D:/P3A/P3a-master/parangon.R")
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


k = 20 #nmobre d'arbres que l'on souhaite garder dans la foret
res=parangon(liste_arbres,train2) #indice des arbres à garder dans la foret initiale
tab_indices=res #on récupère le tableau des indices des arbres les plus distincts
#poids=res[[2]] #ainsi que leur poids

#on récupère les arbres à garder
liste_arbres_distincts=NULL 
for (i in tab_indices){
  #append(liste_arbres_distincts,liste_arbres[[i]])
  liste_arbres_distincts=c(liste_arbres_distincts, list(liste_arbres[[i]]))
}
k = length(tab_indices)
v = train2[,"feature_to_predict"]
A = matrix(0,train2_dim,k)
for(i in 1:k){
  A[,i] = predict(liste_arbres_distincts[[i]], train2)
}

fit = nnls(A,v)
poids = coef(fit)
s = sum(poids)
poids = poids/s

#on trace la diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés
g2=ggplot(plot_forest(liste_arbres_distincts, cross_test, k), aes(x = X1, y=X2))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés, non pondérés")
g2
grid.arrange(g1,g2)


#on veut voir le résultat avec pondération
pred=0;
for (i in 1:k){
  pred=pred+predict(liste_arbres_distincts[[i]], cross_test)*poids[i]
}

#on calcule l'erreur quadratique pour notre forêt final
mean((pred-cross_test$feature_to_predict)^2)


#on trace tout sur un même graphique
res_foret_initiale=plot_forest(liste_arbres, cross_test, nTree)
res_foret_initiale$fill="foret initiale"

res_foret_selectionnee=plot_forest(liste_arbres_distincts, cross_test, k)
res_foret_selectionnee$fill="foret arbres diff�rents, non pond�r�s"
data_to_plot=rbind(res_foret_initiale, res_foret_selectionnee)
data_to_plot=rbind(data_to_plot, c(k, mean((pred-cross_test$feature_to_predict)^2), "foret arbre differents ponderes"))
data_to_plot$X1=as.numeric(data_to_plot$X1)
data_to_plot$X2=as.numeric(data_to_plot$X2)

#le graphique final
ggplot(data_to_plot, aes(x=X1, y=X2, color=fill))+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")
print(k)
