#' Create a funnel plot
#'
#' This function will return a ggplot2 object. It requires two data frames: the sample data frame and the control limit data frame. An optional sub-group variable can be present in the sample data frame for coloring the funnel plot.
#'
#'
#' @param input A data frame of the raw data with a denominator (total) as d, and numerator (events) as n
#' @param fundata A data frame from the fundata function which holds the control limits to be overlayed.
#' @param byvar A subgroup variable to color the plots by (Optional). Variable name must be wrapped in quotes.
#'
#' @examples #My sample data
#' @examples my_data  <- data.frame(id=c('A','B','C','D','E'), n=c(2,5,10,15,18), d=c(20,20,20,20,20))
#'
#' @examples #Process sample data through fundata
#' @examples my_fpdata <- fundata(my_data, alpha=0.95, alpha2=0.998, method='approximate', step=0.5)
#'
#' @examples #Use sample data and fundata to make the plot.
#' @examples my_plot <- funplot(my_data, my_fpdata)
#'
#' @examples #View plot
#' @examples my_plot
#' @import ggplot2
#' @export


funplot <- function(input, fundata, byvar) {

       gra <-ggplot(data=input, aes(x=input$d,y=input$n/input$d)) +
       geom_line(data=fundata, aes(x=fundata$d, y=fundata$up), colour="orange") +
       geom_line(data=fundata, aes(x=fundata$d, y=fundata$lo), colour="orange") +
       geom_line(data=fundata, aes(x=fundata$d, y=fundata$up2), colour="blue") +
       geom_line(data=fundata, aes(x=fundata$d, y=fundata$lo2), colour="blue") +
	   geom_hline(data=fundata, aes(yintercept=fundata$benchmark), colour="red") +
       coord_cartesian(ylim=c(0,1)) +
       scale_y_continuous(breaks=seq(0,1,0.1)) +
       theme_bw()

   if (missing(byvar)) {
		gra <- gra + geom_point(data=input, size=4.5)
		return(gra)    }

	else {
		gra <- gra + geom_point(data=input, size=4.5, aes_string(colour=byvar))
		return(gra)
		}


}
