rot_all = function(df){
  n = matrix(ncol=2)
  for(i in 1:length(unique(df$trip))){
    j=dist(df[df$trip==i,1:2])
    k = as.matrix(j)
    k = k[,1]
    k = k[max.col(t(k),'last')]
    mp = df[names(k),1:2]
    rot.mat = matrix(c(mp$x,mp$y,-mp$y,mp$x),2,2)/sqrt(mp$x^2+mp$y^2) 
    l = as.matrix(df[df$trip==i,1:2])%*%rot.mat
    if (sum(l[,2] <0)/nrow(l) > 0.5) l[,2] = - l[,2]
    n = rbind(n, l)
  } 
  n = n[-which(is.na(n)),]
  n = as.data.frame(n)
  n = cbind(n, df$trip, df$time)
  colnames(n) = c('x','y','trip','time')
  return(n)
}