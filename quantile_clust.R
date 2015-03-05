quantile_clust = function(){
  library(plyr); library(dplyr); library(dtw); library(tidyr); library(zoo)
  df2 = read_trips(getwd())
  df2$dist = sqrt(df2$x^2+df2$y^2)
  df3 = ddply(df2, .(trip), summarize, max.dist = max(dist), max.time = max(time))
  df3$quantiles = cut(df3$max.dist, as.vector(summary(df3$max.dist)))
  df2 = left_join(df2, df3, by='trip')
  df3 = split(df2, df2$quantiles)
  names(df3) = 1:5
  df3 = lapply(df3, select, trip, time, dist)
  df3 = lapply(df3, spread, trip, dist)
  df3 = lapply(df3, na.locf)
  df3 = lapply(df3, t)
  df3 = lapply(df3, dist, method='DTW')
  return(df3)
}

dist.mat = matrix(nrow=nrow(df3), ncol=nrow(df3))

for(i in 1:nrow(df3)){
  for(j in (i+1):nrow(df3)){
    dist.mat[i,j] = ifelse(df3$max.dist[i] >= 0.8*df3$max.dist[j] & df3$max.dist[i] <= 1.2*df3$max.dist[j] &
                           df3$max.time[i] >= 0.8*df3$max.time[j] & df3$max.time[i] <= 1.2*df3$max.time[j],
                           dist(df2[df2$trip==i,5], df2[df2$trip==j,5], method='DTW'), 0)
  }
}
#2:13pm  