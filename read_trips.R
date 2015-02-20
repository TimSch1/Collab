read_trips = function(x){
  setwd(x)
  dir_list = list.files(x)
  num_files = length(dir_list)
  files = lapply(dir_list, read.csv)
  idx = unlist(lapply(files, nrow))
  trip = rep(1:num_files, idx)
  files = do.call(rbind, files)
  time = unlist(sapply(idx, function(x) seq(from=1, to=x, by=1)))
  files = cbind(files, trip, time)
  }
  
#ggplot(p, aes(x=x,y=y, group=trip, color=trip, label=trip))+geom_path()+geom_text(data= p %>% group_by(trip) %>% filter(y == max(y)), color='black', size=5, position = position_jitter())+scale_color_continuous(low='red', high='orange')
