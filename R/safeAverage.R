#'
#'  Safely compute an average and it's confidence interval
#'  
#' @param x A numeric vector
#' @param average Either "mean" or "median"
#' @param conf.level Confidence level for confidence interval
#' @param na.rm If TRUE, remove NA values
#' @return A 3-element vector: estimate, lowerCI, and upperCI
#' @export
#'  
safeAverage = function(x, average="mean", conf.level=0.95,
                       na.rm=TRUE) {
  decl(x, is.numeric)
  decl(average, is.character)
  decl(conf.level, is.numeric)
  decl(na.rm, is.logical)
  
  if (na.rm) {
    x = x[!is.na(x)]
  }
  if (length(x) == 0) {
    return(c(NA, NA, NA))
  }
  if (all(is.na(x))) {
    return(c(NA, NA, NA))
  }
  if (all(x == x[[1]])) {
    return(c(x[[1]], NA, NA))
  }
  
  switch(average,
         mean = {
           ht <- t.test(x, conf.level=conf.level)
           cbind(estimate=ht$estimate,
                 lowerCI=ht$conf.int[1],
                 upperCI=ht$conf.int[2] )
         },
         median = {
           med <- median(x)
           ci <- wilcox.test(x, conf.int=TRUE, conf.level=conf.level)$conf.int
           cbind(estimate=med,
                 lowerCI=ci[1],
                 upperCI=ci[2] )
         },
         stop("Invalid 'average' parameter: ", average) )
}
