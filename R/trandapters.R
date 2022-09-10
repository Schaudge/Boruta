# Input transformations which compose with importance adapters

fixna<-function(x){
 nm<-is.na(x)
 if(!any(nm)) return(x)
 if(all(nm)) return(rep(0,length(x)))
 x[nm]<-sample(x[!nm],sum(nm),replace=TRUE)
 x
}

#' Imputation transdapter
#'
#' Wraps the importance adapter to accept NAs in input.
#' 
#' @param adapter importance adapter to transform.
#' @return transformed importance adapter which can be fed into \code{getImp} argument of the \code{\link{Boruta}} function.
#' @note An all-NA feature will be converted to all zeroes, which should be ok as a totally non-informative value with most methods, but it is not universally correct.
#' Ideally, one should avoid having such features in input altogether.
#' @examples
#' set.seed(777)
#' data(srx)
#' srx_na<-srx
#' # Randomly punch 25 holes in the SRX data
#' holes<-25
#' holes<-cbind(
#'  sample(nrow(srx),holes,replace=TRUE),
#'  sample(ncol(srx),holes,replace=TRUE)
#' )
#' srx_na[holes]<-NA
#' # Use impute transdapter to mitigate them with internal imputation
#' Boruta(Y~.,data=srx_na,getImp=imputeTransdapter(getImpRfZ))
#' @export
imputeTransdapter<-function(adapter=getImpRfZ){
 composition<-function(x,y,...)
  adapter(
   data.frame(lapply(x,fixna)),
   fixna(y),
   ...
  )
 comment(composition)<-sprintf("%s, wrapped into imputation transdapter",comment(adapter))
 composition
}
 
#' Conditional transdapter
#'
#' Applies downstream importance source on a given object strata and averages their outputs.
#' 
#' @param groups groups.
#' @param adapter importance adapter to transform.
#' @return transformed importance adapter which can be fed into \code{getImp} argument of the \code{\link{Boruta}} function.
#' @export
conditionalTransdapter<-function(groups,adapter=getImpRfZ){
 as.numeric(table(groups))/length(groups)->w
 stopifnot(is.factor(groups))
 composition<-function(x,y,...)
  colSums(w*t(sapply(levels(groups),function(l) adapter(x[groups==l,],y[groups==l],...))))
 comment(composition)<-sprintf("%s, wrapped into conditional transdapter",comment(adapter))
 composition
}
 
