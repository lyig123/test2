#' all in one function of iterated HP-filter
#'
#' @param x the data you want to conduct HP-filter
#' @param lambda the turning parameter
#' @param iter logical parameter, TRUE is to conduct iterated HP-filter, FALSE is not
#' @param test_type the type for creterion
#' @param sig_p significant p-value
#' @param Max_Iter maximum iterated time
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

Pvalue <- function(x,...) UseMethod("Pvalue",x)

Pvalue.bHP <- function(x, plot= TRUE){
  
  if(x$test_type == "adf"){
    
    message("Retrun the value path of ", x$test_type, ".")
    message("Iterated number of HP filter: ",x$iter_num)
    
    
    if (plot == TRUE){
      plot(x$adf_p_hist)
    }
    return(x$adf_p_hist)
    
  }
  else {
    stop("The stationary test type is ",x$test_type, ", not ADF.")
    }
}


