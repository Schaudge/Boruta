context("Importance source tests")

#Set, as in https://mbq.me/blog/relevance-and-redundancy
setNames(
 do.call(expand.grid,rep(list(c(T,F)),5)),
 c("A","B","N1","N2","N3")
)->X
cbind(X,AoB=with(X,A|B),AnB=with(X,A&B),nA=!X$A)->X
factor(with(X,A!=B))->Y
data.frame(lapply(X,factor))->X
#Numeric value, for more fun
set.seed(777)
X$N4<-runif(nrow(X))

impSources<-c(
 "getImpExtraGini","getImpExtraRaw","getImpExtraZ","getImpFerns",
 "getImpLegacyRfGini","getImpLegacyRfRaw","getImpLegacyRfZ",
 "getImpRfGini","getImpRfRaw","getImpRfZ") #getImpXgboost is special

for(e in impSources)
 test_that(sprintf("Importance source %s works",e),{
  set.seed(777)
  #Run Boruta on this data
  Boruta(X,Y)->ans
  #Nonsense attributes should be rejected
  expect_equal(
   sort(getSelectedAttributes(ans)),
   sort(c('A','B','AnB','AoB','nA'))
  )
 })

test_that("Importance source getImpXgboost works",{
 set.seed(777)
 Boruta(Species~.,data=iris)->ans
})
