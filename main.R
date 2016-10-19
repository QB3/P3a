source("/home/qbe/R/P3A/max_dist.R")
library(gridExtra)

nTree=50 #nombre d'arbres dans la forêt initiale

#on génère une foret d'arbres de taille nTree, ainsi que la matrice des "distances" (distance à définir plus précisément)
foret=generate_forest(nTree)#on stocke la foret
d=distance_matrix(foret)#on stocke la matrice des dstances
g1=plot_forest(foret, 1, nTree)+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres")#on stocke le graphe


k = 10 #nmobre d'arbres que l'on souhaite garder dans la foret
tab=max_dist(d, nTree, k) #indice des arbres à garder dans la foret initiale

liste_arbres_distincts=NULL #on récupère les arbres à garder
for (i in tab){
  print(i)
  liste_arbres_distincts=c(liste_arbres_distincts, list(foret[[i]]))
}

g2=plot_forest(liste_arbres_distincts, 1, k)+geom_point()+xlab(label="nombre d'arbres")+ylab(label="mse")+
  labs(title="diminution de l'erreur quadratique en fonction du nombre d'arbres, arbres sélectionnés")
grid.arrange(g1,g2)

