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


summary.bHP <- function(x, digit = 8){
  
  options(digits=digit)
  
  message("This is a summary table of 'bHP' class (",x$test_type,").")
  
  cat("
==============================================================================================================
                                ",
      paste0("Summary Table of '",x$test_type,"' bHP"),
"
==============================================================================================================
",

"Length of the Data:",length(x$raw_data),
if(length(x$iter_num)>0){paste0("; Iterated Number: ", x$iter_num)},
if(x$test_type == "none"){paste0("; Iterated Number: ", (length(x$BIC_hist)-1))},
if(x$test_type == "none-iter"){"; Only Conduct HP-Filter once"},
"; Stopping Criterion: ", x$test_type,"\n", if(x$test_type == "adf"){paste0("P-value (last iteration): ", 
round(x$adf_p_hist[x$iter_num],4))}, if(x$test_type == "BIC"){paste0("BIC value (last iteration): ", 
round(x$BIC_hist[x$iter_num],4))},"\n",
"------------------------------------------------------------------------------------------------------------","\n",    
"\n",
"Raw Data","\n",
"---------","\n",
#"Min. ", "1st Qu.", "Median", "Mean ", "3rd Qu.", "Max. ", "\n",
summary(matrix(x$raw_data)),"\n",


"\n",
"Final Trend Component","\n",
"----------------------","\n",
#"Min. ", "1st Qu.", "Median", "Mean ", "3rd Qu.", "Max. ", "\n",
summary(matrix(x$trend)),"\n",


#----------- start with the path of p-value or BIC -----------------
if(x$test_type == "adf" | x$test_type == "BIC"){"\n"},
if(x$test_type == "adf" | x$test_type == "BIC"){
  "------------------------------------------------------------------------------------------------------------"    
},

if(x$test_type == "adf" | x$test_type == "BIC"){"\n"},

# the path of ADF
 if(x$test_type == "adf"){
    "Path of P-value (head):"},
 if(x$test_type == "adf"){
 head(round(x$adf_p_hist,4))},

# the path of BIC
if(x$test_type == "BIC"){
  "Path of BIC (head):"},
if(x$test_type == "BIC"){
  head(round(x$BIC_hist[1:(x$iter_num)],4))},

if(x$test_type == "adf" | x$test_type == "BIC"){"\n"},

#if(x$test_type == "adf" | x$test_type == "BIC"){
#  "------------------------------------------------------------------------------------------------------------"    
#},



# the path of ADF
if(x$test_type == "adf"){
  "Path of P-value (tail):"},
if(x$test_type == "adf"){
  tail(round(x$adf_p_hist,4))},

if(x$test_type == "BIC"){
  "Path of BIC (tail):"},
if(x$test_type == "BIC"){
  tail(round(x$BIC_hist[1:(x$iter_num)],4))},

#if(x$test_type == "adf" | x$test_type == "BIC"){"\n"},
"
==============================================================================================================
")
    
}    
  

