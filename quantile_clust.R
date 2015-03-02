quantile_clust = function(){
  library(plyr); library(dplyr); library(dtw); library(tidyr)
  df2 = read_trips(getwd())
  df2$dist = sqrt(df2$x^2+df2$y^2)
  df3 = ddply(df2, .(trip), summarize, max.dist = max(dist))
  df3$quantiles = cut(df3$max.dist, as.vector(summary(df3$max.dist)))
  df2 = left_join(df2, df3, by='trip')
  df3 = split(df2, df2$quantiles)
  names(df3) = 1:5
  df3 = lapply(df3, select, trip, time, dist)
  df3 = lapply(df3, spread, trip, dist)
  df3 = lapply(df3, na.locf)
  df3 = lapply(df3, dist, method='DTW')
  return(df3)
}