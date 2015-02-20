read_all_drivers = function(){
  files = list.files(getwd())
  dir.list = sapply(files, function(x) paste(getwd(),'/',x, sep=''))
  k = lapply(dir.list, read_trips)
}
