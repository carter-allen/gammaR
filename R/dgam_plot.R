#' Plots probability density function of a gamma
#' @param alpha The shape parameter
#' @param beta The rate parameter if rate = TRUE, scale parameter otherwise
#' @param rate Sets beta to rate or shape
#' @examples
#' dgam_plot(2,2)

dgam_plot <- function(alpha,beta,rate = TRUE,by = 0.0025)
{
  if(rate)
  {
    x_max = ceiling(qgamma(0.999,shape = alpha,rate = beta))
    lambdas = seq(0,x_max,by = by)
    p.lambda = dgamma(lambdas,shape = alpha,rate = beta)
    ps.df = as_tibble(cbind(lambdas,p.lambda))

    ggplot(data = ps.df, aes(x = lambdas,y=p.lambda)) +
      geom_point()
  }

  else
  {
    x_max = ceiling(qgamma(0.99,shape = alpha,scale = beta))
    lambdas = seq(0,x_max,by = by)
    p.lambda = dgamma(lambdas,shape = alpha,scale = beta)
    ps.df = as_tibble(cbind(lambdas,p.lambda))

    ggplot(data = ps.df, aes(x = lambdas,y = p.lambda)) +
      geom_point()
  }
}
