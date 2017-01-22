library(FactoMineR)

cross_validation=test
data=NULL
for( i in liste_arbres){
  data=rbind(data, predict(i, cross_validation))
}
data=as.data.frame(data)


# res.pca <- PCA(data, scale = T)
# summary(res.pca)
# barplot(res.pca$eig[,1], main = "Eigenvalues", names.arg = 1:nrow(res.pca$eig))
# plot.PCA(res.pca)
# 
# 
# res.pca$ind$coord; res.pca$ind$cos2; res.pca$ind$contrib
# plot.PCA(res.pca, axes = c(3, 4))
# dimdesc(res.pca)
# plotellipses(res.pca, 1)
# write.infile(res.pca, file = "my_FactoMineR_results.csv")
# 
# 
# res.pca$ind
# # res.hcpc = HCPC(res.pca)

res.hcpc=HCPC(data)



nb_clust=length(res.hcpc$desc.ind$para)

liste_indice=NULL
for( i in 1:nb_clust){
  nom=as.character(i)
  val_parang=res.hcpc$desc.ind$para[[nom]][1]
  parang=as.numeric(names(val_parang))
  liste_indice=c(liste_indice, parang)
}

liste_clust=list()
for(indice in liste_indice){
  # indice=23
  new_element=liste_arbres[[indice]]
  liste_clust=c(liste_clust, list(new_element))
}


