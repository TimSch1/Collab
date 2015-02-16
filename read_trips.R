read_trips = function(x){
  dir_list = list.files(x)
  num_files = length(dir_list)
  files = lapply(dir_list, read.csv)
  idx = unlist(lapply(files, nrow))
  trip = rep(1:num_files, idx)
  files = do.call(rbind, files)
  time = unlist(sapply(idx, function(x) seq(from=1, to=x, by=1)))
  files = cbind(files, trip, time)
  }
  
