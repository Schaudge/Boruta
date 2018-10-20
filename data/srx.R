# Small XOR problem data
# documentation in R/Boruta.R

stats::setNames(
 do.call(expand.grid,rep(list(c(TRUE,FALSE)),5)),
 c("A","B","N1","N2","N3")
)->srx
data.frame(
 srx,
 AoB=with(srx,A|B),
 AnB=with(srx,A&B),
 nA=!srx$A
)->srx
srx$Y<-with(srx,A!=B)
srx<-as.data.frame(lapply(srx,factor))

