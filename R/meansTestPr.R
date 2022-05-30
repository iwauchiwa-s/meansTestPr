#' @title Significance test for difference between 2 mean values
#' @description \code{meansTestP} statistical test for mean values
#'
#' @importFrom stats pt
#' @importFrom stats qt
#' @importFrom stats dnorm
#' @importFrom graphics curve
#' @importFrom graphics legend
#' @param av1 mean value of group 1
#' @param sd1 standard deviation of group 1
#' @param nd1 number of group 1
#' @param av2 mean value of group 2
#' @param sd2 standard deviation of group 2
#' @param nd2 number of group 2
#' @param rg1 plot axis min
#' @param rg2 plot axis max
#' @return Deviation Test
#' @export
#' @examples
#' # meansTestP(50, 10, 20, 60, 15, 25, 0, 100)

meansTestP <- function(av1, sd1, nd1, av2, sd2, nd2, rg1, rg2){
  dav <- abs(av1-av2)
  var1 <- sd1^2
  var2 <- sd2^2
  sdpool <- sqrt ( ( nd1*var1 + nd2*var2) / (nd1+nd2) )
  cohen_d <- dav/sdpool
  dof <- round((var1/nd1+var2/nd2)^2/(var1^2/nd1^2/(nd1-1)+var2^2/nd2^2/(nd2-1)))
  t <- (abs(av1-av2))/sqrt(var1/nd1+var2/nd2)
  pv <- pt(-t,df=dof)*2
  vpool <- ((nd1-1)*var1+(nd2-1)*var2)/(nd1+nd2-2)
  cl_l <- (av2-av1)-qt(0.975,nd1+nd2-2)*sqrt(vpool*(1/nd1+1/nd2))
  cl_u <- (av2-av1)+qt(0.975,nd1+nd2-2)*sqrt(vpool*(1/nd1+1/nd2))
  if (pv <= 0.05){
    txj <- 1 # significant
  }
  else{
    txj <- 0 # insignificant
  }

  # x-axix settings
  # xmn <- min(c(av1,av2))-max(c(sd1,sd2))*4
  # xmx <- max(c(av1,av2))+max(c(sd1,sd2))*4

  n <- 1000
  x1 <- seq(rg1, rg2, length=n)
  mx1 <- max( dnorm(x1,av1,sd1) )
  mx2 <- max( dnorm(x1,av2,sd2) )
  mx <- max(mx1,mx2) * 1.1


  # draw the normal distributions
  curve(dnorm(x,av1,sd1),rg1,rg2,col = "blue",lwd=1,xlab="", ylab="", ylim=c(0,mx))
  curve(dnorm(x,av2,sd2),rg1,rg2,add = TRUE, col = "red",lwd=1)
  legend("topleft",
         legend=c("1", "2"),
         lty=c(1,1),
         col=c("blue", "red")
  )
  return(list(Deviation=dav, Cohens_d=cohen_d, t_value=t, P_value=pv, lower_lim=cl_l, upper_lim=cl_u, judge=txj))
}
