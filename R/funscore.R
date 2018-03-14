#' Score Proportion Data
#'
#' This function will return a single data frame consisting of (1) the original sample data and (2) two categorical variables which record whether each sample data point is In Control or Extreme, relative to the two sets of alphas specified. These two categorical variables might be useful for additional analyses or coloring the funnel plot.
#'
#' @param input A data frame of your sample data, in the format outlined above.
#' @param benchmark A number between 0 and 1 representing the benchmark (e.g. null) estimate for which confidence limits are calculated for. If not specified, the overall proportion of events is used.
#' @param alpha A number between 0 and 1 representing the desired confidence limit (e.g. 0.95)
#' @param alpha2 A number between 0 and 1 representing the desired confidence limit (e.g. 0.998)
#' @param method Choose between approximate or exact binomial control limits.
#'
#' @examples #My sample data
#' @examples my_data  <- data.frame(id=c('A','B','C','D','E'), n=c(2,5,10,15,18), d=c(20,20,20,20,20))
#'
#' @examples #Score sample data
#' @examples my_scoredata <- funscore(my_data, alpha=0.95, alpha2=0.998, method='approximate')
#'
#' @examples #View scored data
#' @examples head(my_scoredata)
#' @import stats
#' @export


funscore <- function(input, benchmark, alpha=0.95, alpha2=0.998, method='approximate') {

  #Check if a custom benchmark was supplied. If not, use the overall sample proportion.
  if(missing(benchmark)) {
  theta  <- sum(input$n)/sum(input$d)
  } else {
  theta <- benchmark
  }

  #Calculate each observations proportion
  input$r <- input$n/input$d

  #Get 2 sided areas for supplied alphas
  p  <- (1-alpha)/2
  p2 <- (1-alpha2)/2

  #Calculate Scoring according to Exact or Approximate method
  if (method=='approximate') {

    input$z  = abs((input$r-theta) / sqrt((theta*(1-theta))/input$d))
    input$score  <-  cut(input$z,breaks=c(-Inf, qnorm(1-p),Inf),labels=c('In Control','Extreme'))
    input$score2 <-  cut(input$z,breaks=c(-Inf,qnorm(1-p2),Inf),labels=c('In Control','Extreme'))

  } else  if (method=='exact') {

    input$score[dbinom(input$n, input$d, theta) < p] <- "Extreme"
    input$score[dbinom(input$n, input$d, theta) >= p] <- "In Control"
    input$score2[dbinom(input$n, input$d, theta) < p2] <- "Extreme"
    input$score2[dbinom(input$n, input$d, theta) >= p2] <- "In Control"

  }
  #Return data frame with Scoring variables
  return(input)
}
