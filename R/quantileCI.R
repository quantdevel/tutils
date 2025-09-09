#'
#'  Estimate and confidence intervals of a quantile
#'
#' @param y A vector of sample data
#' @param prob Numeric probability in closed interval \[0, 1\]
#' @param alpha Tail probability
#'
#' @return Three-element,named vector: estimate, lowerCI, upperCI
#'
#' @export
#'
quantileCI = function(y, prob, conf.level=0.95,
                      method="binomial", na.rm=FALSE, ...) {
  decl(y, is.numeric)
  decl(prob, is.numeric)
  decl(conf.level, is.numeric)

  if (na.rm) {
    y = y[!is.na(y)]
  }
  if (length(y) == 0) {
    return(c(estimate=NA, lowerCI=NA, upperCI=NA))
  }
  if (length(y) == 1) {
    return(c(estimate=y, lowerCI=NA, upperCI=NA))
  }

  est <- as.numeric(quantile(y, probs=prob))

  if (all(y == y[1])) {
    ci = c(NA, NA)    # CI is meaningless
  } else {
    ci = switch(method,
                binomial = quantileCI.binomial(y, prob, conf.level),
                boot = quantileCI.boot(y, prob, conf.level, ...),
                jack = quantileCI.jack(y, prob, conf.level),
                stop("quantileCI: Invalid method: ", method)
    )
  }

  return(c(estimate=est, lowerCI=ci[1], upperCI=ci[2]))
}

quantileCI.boot = function(y, prob, conf.level, R=999) {
  stat = function(x, idx) quantile(x[idx], probs=prob)
  b = boot::boot(y, statistic=stat, R=R)
  bci = boot::boot.ci(b, conf.level, type="perc")
  interval = bci$percent
  c(interval[4], interval[5])
}

quantileCI.binomial = function(y, prob, conf.level) {
  alpha = 1 - conf.level
  sort(y)[qbinom(c(alpha/2, 1-alpha/2), length(y), prob)]
}

quantileCI.jack = function(y, prob, conf.level) {
  stop("Not implemented")    # Needs jackknife() function
}

#'
#'  Estimate and confidence interval of the median
#'
#' @param y Vector of sample data
#' @param alpha Tail probability
#' @param method Method
#' @param na.rm If TRUE, remove NA values from data
#'
#' @return A named, three-value vector: estimate, lowerCI, upperCI
#'
#' @export
#'
medianCI = function(y, conf.level=0.95, method="binomial", na.rm=FALSE) {
  decl(y, is.numeric)
  decl(conf.level, is.numeric)

  if (na.rm) {
    y = y[!is.na(y)]
  }
  if (length(y) == 0) {
    return(c(estimate=NA, lowerCI=NA, upperCI=NA))
  }
  if (length(y) == 1) {
    return(c(estimate=y, lowerCI=NA, upperCI=NA))
  }

  switch(method,
         wilcox = medianCI.wilcox(y=y, conf.level=conf.level),
         olive = medianCI.olive(y=y, conf.level=conf.level),
         quantileCI(y=y, prob=0.50, conf.level=conf.level, method=method, na.rm=FALSE)
  )
}

#
# Median conf. int. method of Dave Olive.
#
# In my brief experience, this method gives odd results
# and sometimes very wide intervals.
#
# See:
#   http://www.math.siu.edu/olive/ppmedci.pdf
# and:
#   http://exploringdatablog.blogspot.sk/2012/04/david-olives-median-confidence-interval.html
#
medianCI.olive = function(y, conf.level) {

    alpha = 1 - conf.level

    #
    #  First, compute the median
    #
    n = length(y)
    ysort = sort(y)
    nhalf = floor(n/2)
    if (2*nhalf < n){
      med = ysort[nhalf + 1]    # n odd
    } else{
      med = (ysort[nhalf] + ysort[nhalf+1])/2    # n even
    }

    #
    #  Next, compute Olives standard error for the median
    #
    Ln = nhalf - ceiling(sqrt(n/4))
    Un = n - Ln
    SE = 0.5*(ysort[Un] - ysort[Ln+1])

    #
    #  Compute the confidence interval based on Students t-distribution
    #  The degrees of freedom parameter p is discussed in Olives paper
    #
    p = Un - Ln - 1
    t = qt(p = 1 - alpha/2, df = p)
    medLCI = med - t * SE
    medUCI = med + t * SE

    c(estimate=med, lowerCI=medLCI, upperCI=medUCI)
}

medianCI.wilcox = function(y, conf.level) {
  med = median(y)
  ci = wilcox.test(y, conf.int=TRUE, conf.level=conf.level)$conf.int
  c(estimate=med, lowerCI=ci[1], upperCI=ci[2])
}
