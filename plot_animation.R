library(animation)
library(ggplot2)
oopt <- ani.options(interval = 0.1)
trip_animation <- function() {
  lapply(1:max(files$time), function(i) {
    print(ggplot(files[files$time <= i,], aes(x=x,y=y,group=trip))+
            ylim(range(files$y))+xlim(range(files$x))+
            theme(axis.text.y=element_blank(), axis.title.x=element_blank(), 
                axis.title.y=element_blank(), axis.text.x=element_blank(), 
                panel.border=element_blank(), panel.background=element_blank(), 
                axis.ticks = element_blank(), legend.position='none')+
            geom_path(aes(color=trip))) #+facet_wrap(~trip)
    animation::ani.pause()
  })
}
#saveHTML(trip_animation(), autoplay = FALSE, loop = FALSE, verbose = TRUE, outdir = "images", single.opts = "'controls': ['first', 'previous', 'play', 'next', 'last', 'loop', 'speed'], 'delayMin': 0")
