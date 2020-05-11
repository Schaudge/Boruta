context("Importance source tests")

data(srx)
X<-srx[,-ncol(srx)]
Y<-srx$Y

impSources<-c(
 "getImpExtraGini","getImpExtraRaw","getImpExtraZ","getImpFerns",
 "getImpLegacyRfGini","getImpLegacyRfRaw","getImpLegacyRfZ",
 "getImpRfGini","getImpRfRaw","getImpRfZ","getImpXgboost")

for(e in impSources)
 test_that(sprintf("Importance source %s works",e),{
  set.seed(777)
  #Run Boruta on this data
  Boruta(X,Y,getImp=get(e))->ans
  
  if(e!="getImpXgboost"){
   #Nonsense attributes should be rejected
   expect_equal(
    sort(getSelectedAttributes(ans)),
    sort(c('A','B','AnB','AoB','nA'))
   )
  }else{
   #Xgboost is minimal optimal
   expect_equal(
    sort(getSelectedAttributes(ans)),
    sort(c('AnB','AoB'))
   )
  }
 })

test_that("Invalid source is caught",{
 expect_error(Boruta(Species~.,data=iris,getImp=function(...) 1:10),"getImp result has a wrong length")
 expect_error(Boruta(Species~.,data=iris,getImp=function(...) "x"),"getImp result is not a numeric vector")
 expect_warning(Boruta(Species~.,data=iris,getImp=function(x,...) c(0,rep(NA,ncol(x)-1))),"getImp result contains NA")
})
