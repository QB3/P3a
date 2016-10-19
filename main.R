source("/home/qbe/R/P3A/max_dist.R")

nTree=50

res=generate_distance(nTree)
d=res[[1]]
foret=res[[2]]
k = 10

tab=max_dist(d, nTree, k)

plot_forest(foret, 1, nTree)
