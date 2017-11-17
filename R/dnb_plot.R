#' Plots probability mass function of a negative binomial
#' Number of failures until rth success
#' @param r The final success
#' @param p Probability of success
#' @examples
#' dnb_plot(20,0.2)

dnb_plot <- function(r,p,plot_title = "Negative Binomial Mass Function")
{
  x_max = ceiling(qnbinom(0.999,size = r,prob = p))
  xs = seq(0,x_max,by=1)
  p.x = dnbinom(xs,size = r,prob = p)
  ps.df = as_tibble(cbind(xs,p.x))

  ggplot(data = ps.df, aes(x = xs, y = p.x)) +
    geom_col() +
    scale_x_discrete(limits = xs) +
    ggtitle(plot_title) +
    labs(subtitle = paste("With parameters",r,p))
}
