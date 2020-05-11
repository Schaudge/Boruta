context("Importance source tests")

set.seed(777)

data(srx)
X<-srx[,-ncol(srx)]
Y<-srx$Y
#Also a numeric nonsense feature, for more fun
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
 if(!suppressWarnings(require('xgboost',quietly=TRUE))) skip("No xgboost available")
 set.seed(777)
 #Xgboost importance is generally poor, lower expectations
 expect_silent(Boruta(Species~.,data=iris,getImp=getImpXgboost)->a)

 #Check if the parameters are passed to Xgboost
 set.seed(777)
 expect_silent(Boruta(Species~.,data=iris,getImp=getImpXgboost,eta=1)->b)

 expect_true(!identical(a$finalDecision,b$finalDecision))
})
