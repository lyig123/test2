#' all in one function of iterated HP-filter
#'
#' @param plot
#' @param x the data you want to conduct HP-filter
#'
#' @return cycle component, iterated number, p-value .
#' @export
#'
#' @examples lam <- 100 # tuning parameter for the annaul data
#'
#' # raw HP filter
#' bx_HP <- BoostedHP(x, lambda = lam, iter= FALSE)$trend
#'
#' # by BIC
#' bx_BIC <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "BIC")
#'
#' # by ADF
#' bx_ADF <- BoostedHP(IRE, lambda = lam, iter= TRUE, test_type = "adf", sig_p = 0.050)
#'
#' # summarize the outcome
#' outcome <- cbind(IRE, bx_HP$trend, bx_BIC$trend, bx_ADF$trend)
#'
#' matplot(outcome, type = "l", ylab = "", lwd = rep(2,4))
#'
#'

BIC.bHP <- function(x){

  if(x$test_type == "BIC"){

  message("Retrun the value path of ", x$test_type, ".")
  message("Iterated number of HP filter: ",x$iter_num)
  message("Keep the path of BIC till iterated ", (x$iter_num+1), " times to show the tuning point.")

  #print(x$BIC_hist)

  return(x$BIC_hist)

  }
  else {
    stop("The stationary test type is ",x$test_type, ", not BIC.")
    }
}


