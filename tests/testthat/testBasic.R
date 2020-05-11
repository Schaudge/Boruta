context("Basic tests")

test_that("Selection works on an extended iris",{
 set.seed(777)
 #Add some nonsense attributes to iris dataset by shuffling original attributes
 iris.extended<-data.frame(iris,apply(iris[,-5],2,sample))
 names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="")
 #Run Boruta on this data
 expect_message(Boruta(Species~.,data=iris.extended,doTrace=10)->Boruta.iris.extended)
 #Nonsense attributes should be rejected
 expect_equal(
  getSelectedAttributes(Boruta.iris.extended),
  names(iris)[-5]
 )

 attStats(Boruta.iris.extended)->a
 expect_equal(sort(rownames(a)),sort(names(iris.extended)[-5]))
 expect_equal(names(a),c("meanImp","medianImp","minImp","maxImp","normHits","decision"))

 rownames(a)[a$decision!="Rejected"]->conf_tent
 
 getSelectedAttributes(Boruta.iris.extended,withTentative=TRUE)->a
 expect_equal(sort(a),sort(conf_tent))

 expect_output(print(Boruta.iris.extended)," 4 attributes confirmed important:")

 tempfile()->canvf
 on.exit(unlink(canvf))
 pdf(canvf)
 on.exit(dev.off())
 expect_silent(plot(Boruta.iris.extended))
 expect_silent(plotImpHistory(Boruta.iris.extended))
})

test_that("No-Boruta input is rejected",{
 expect_error(getSelectedAttributes(iris))
 expect_error(attStats(iris))
 expect_error(TentativeRoughFix(iris))
 expect_error(plot.Boruta(iris))
 expect_error(print.Boruta(iris))
 expect_error(plotImpHistory(iris))
 expect_error(getConfirmedFormula(iris))
 expect_error(getNonRejectedFormula(iris))
})

test_that("Misc errors are caught",{
 expect_silent(Boruta(Species~.,data=iris))->B
 expect_error(plot.Boruta(B,colCode=c("green")))
 expect_error(plotImpHistory(B,colCode=c("green")))
})

test_that("Models without history are handled correctly",{
 Boruta(Species~.,data=iris,holdHistory=FALSE)->B
 expect_null(B$ImpHistory)
 expect_output(print(B)," 4 attributes confirmed important")
 expect_error(plot.Boruta(B))
 expect_error(plotImpHistory(B))
 expect_error(attStats(B))
})
