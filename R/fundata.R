#' Compute Control Limits for Proportion Data
#'
#' This function will return a single data frame consisting of two sets of control limits, which can then be overlaid in a funnel plot.
#' The incoming data frame (input) should have one observation per row. It must have a column labeled 'n' which represents the number of events (numerator) and a column labeled 'd' which represents the total (denominator).
#' Other by variables are permitted (e.g. sex, or age).
#'
#' @param input A data frame of your sample data, in the format outlined above.
#' @param benchmark A number between 0 and 1 representing the benchmark (e.g. null) estimate for which confidence limits are calculated for. If not specified, the overall proportion of events is used.
#' @param alpha A number between 0 and 1 representing the desired confidence limit (e.g. 0.95)
#' @param alpha2 A number between 0 and 1 representing the desired confidence limit (e.g. 0.998)
#' @param method Choose between approximate or exact binomial control limits.
#' @param step Minor ticks between 1 and the maximum denominator size of the raw data for which the control limits are calculated for. Must be integer for method=exact.
#'
#' @examples #My sample data
#' @examples my_data  <- data.frame(id=c('A','B','C','D','E'), n=c(2,5,10,15,18), d=c(20,20,20,20,20))
#'
#' @examples #Compute approximate control limits
#' @examples my_fpdata <- fundata(my_data, alpha=0.95, alpha2=0.998, method='approximate', step=0.5)
#' @import stats
#' @export


fundata <- function(input, benchmark, alpha=0.95, alpha2=0.998, method='approximate', step=0.5) {

  #Check if method = exact was specified and if so, an integer was supplied for the step parameter.
  if (method=="exact" & round(step) != step) {
    warning("Step must be a whole number for method='exact'")
  }

  #Check if a custom benchmark was supplied. If not, use the overall sample proportion.
  if(missing(benchmark)) {
   theta  <- sum(input$n)/sum(input$d)
   } else {
   theta <- benchmark
   }

  #Obtain the largest denominator (total) in the sample data.
   n <- max(input$d)


  #Get 2 sided areas for supplied alphas
   p  <- (1-alpha)/2
   p2 <- (1-alpha2)/2

  #Calculate Exact Control Limits
  if (method=='exact') {

    upper  <- qbinom(1-p, seq(1,n, step), theta)/seq(1,n, step)
    lower  <- qbinom(p, seq(1,n, step), theta)/seq(1,n, step)
    upper2 <- qbinom(1-p2, seq(1,n, step), theta)/seq(1,n, step)
    lower2 <- qbinom(p2, seq(1,n, step), theta)/seq(1,n, step)
  }

  #Calculate Approximate Control Limits
  else if (method=='approximate') {

    upper  <- theta + qnorm(1-p) * sqrt((theta*(1-theta))/seq(1,n, step))
    lower  <- theta + qnorm(p) * sqrt((theta*(1-theta))/seq(1,n, step))
    upper2 <- theta + qnorm(1-p2) * sqrt((theta*(1-theta))/seq(1,n, step))
    lower2 <- theta + qnorm(p2) * sqrt((theta*(1-theta))/seq(1,n, step))
  }

  #Return a data frame of Control Limits
  return(data.frame(benchmark=theta, d=seq(1,n, step), up=upper, lo=lower, up2=upper2, lo2=lower2))

}
