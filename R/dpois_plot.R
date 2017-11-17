#' Plots probability mass function of a Poisson
#' @param lambda The final success
#' @examples
#' dpois_plot(10)

dpois_plot <- function(lambda)
{
  x_max = ceiling(qpois(0.999,lambda))
  xs = seq(0,x_max,by=1)
  p.x = dpois(xs,lambda)
  ps.df = as_tibble(cbind(xs,p.x))

  ggplot(data = ps.df,aes(x = xs,y = p.x)) +
    geom_col() +
    scale_x_discrete(limits = xs)
}
