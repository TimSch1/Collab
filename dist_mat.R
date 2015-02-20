dist_mat = function(df){
  library(tidyr)
  library(zoo)
  library(dtw)
  n = matrix(ncol=3)
  for(i in 1:length(unique(df$trip))){
    j=dist(df[df$trip==i,1:2])
    k = as.matrix(j)
    k = k[,1]
    k = cbind(k, rep(i, length(k)), seq(1, length(k), 1))
    n = rbind(n, k)
  } 
  n = n[-which(is.na(n)),]
  n = as.data.frame(n)
  colnames(n) = c('dist', 'trip', 'time')
  n = spread(n, trip, dist)
  n = na.locf(n)
  return(n)
}
  #distMatrix = dist(t(n[,-1]), method='DTW')
  #hc = hclust(distMatrix, method='single')
  #return(list(n, distMatrix, hc)
#} speeds:
 #21s for dist
 #25725 for DTW dist
#zz = cutree(hc, k=15)
#df$clust = rep(zz, ddply(df, .(trip), count)$n)
#ggplot(df, aes(x=x, y=y, group = clust, color=factor(clust)))+geom_path()

