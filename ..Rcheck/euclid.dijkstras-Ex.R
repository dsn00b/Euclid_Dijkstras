pkgname <- "euclid.dijkstras"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('euclid.dijkstras')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("dijkstra")
### * dijkstra

flush(stderr()); flush(stdout())

### Name: dijkstra
### Title: Distance Computation Algorithm
### Aliases: dijkstra

### ** Examples

wiki_graph <- data.frame(v1=c(1,1,1,2,2,2,3,3,3,3,4,4,4,5,5,6,6,6),v2=c(2,3,6,1,3,4,1,2,4,6,2,3,5,4,6,1,3,5),w=c(7,9,14,7,10,15,9,10,11,2,15,11,6,6,9,14,2,9))
dijkstra(wiki_graph,1)
dijkstra(wiki_graph,3)




cleanEx()
nameEx("euclidean")
### * euclidean

flush(stderr()); flush(stdout())

### Name: euclidean
### Title: Get GCD of two numbers
### Aliases: euclidean

### ** Examples

euclidean(123612, 13892347912)
euclidean(100, 1000)



### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
