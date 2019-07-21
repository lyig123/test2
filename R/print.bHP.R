#' all in one function of iterated HP-filter
#'
#' @param type
#' @param trend_hist
#' @param select_trend_hist
#' @param Head
#' @param Tail
#' @param print_type
#' @param digit
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



print.bHP <- function(x,type = "bHP", trend_hist = TRUE, select_trend_hist = c(1), Head = FALSE, Tail = FALSE, print_type = "text", digit = 8){
  options(digits=digit)

  if(type == "generic default"){
    message("This is a generic.default method print of 'bHP' class (",x$test_type,").")
    print.default(x)
  }


  if(type == "bHP"){

    message("This is print method special for 'bHP' class (",x$test_type,").")

    #----------------- manage data frame for "ADF or "BIC" bHP -----------------------
    if(x$test_type == "adf" | x$test_type == "BIC"){

    colname <- c("Raw Data", paste0("Final Trend (",x$iter_num,"th)"),paste0("Trend (",1:x$iter_num,"th)"))

    if(x$test_type == "adf"){
      value_hist <- c("P-value:",paste0(round(x$adf_p_hist[x$iter_num],4)," (",x$iter_num,"th)"),paste0(round(x$adf_p_hist,4),paste0(" (",1:x$iter_num,"th)")))
    }
    if(x$test_type == "BIC"){
      value_hist <- c("BIC value:",paste0(round(x$BIC_hist[x$iter_num],4)," (",x$iter_num,"th)"),paste0(round(x$BIC_hist[1:x$iter_num],4),paste0(" (",1:x$iter_num,"th)")))
    }

    raw_trend <- cbind.data.frame(x$raw_data,x$trend,x$trend_hist)
    data <- rbind(value_hist,raw_trend)
    colnames(data) <- colname


    #--------------------- trend selection ----------------------------------------------------

    if(trend_hist == TRUE){
      data_trend_select <- as.matrix(data)[,c(1,2,select_trend_hist+2)] %>% as.data.frame()
    }
    if(trend_hist == FALSE){
      data_trend_select <- as.matrix(data)[,c(1,2)] %>% as.data.frame()
    }
    # end trend selection of ADF or BIC

    }

    #----------------- manage data frame for "none-iter" bHP -----------------------
    if(x$test_type == "none-iter"){

      colname <- c("Raw Data", "Trend Component")

      # none BIC or P-value (Only Once HP-filter)
      value_hist <- c("Type:","Once HP-filter")

      raw_trend <- cbind.data.frame(x$raw_data,x$trend)
      data <- rbind(value_hist,raw_trend)
      colnames(data) <- colname

      data_trend_select <- data

    }

    #----------------- manage data frame for "none" bHP -----------------------
    if(x$test_type == "none"){

    colname <- c("Raw Data", paste0("Final Trend (",dim(x$trend_hist)[2],"th)"),paste0(1:dim(x$trend_hist)[2],"th Trend"))

    value_hist <- c("BIC:",paste0(round(x$BIC_hist[dim(x$trend_hist)[2]],4)," (",dim(x$trend_hist)[2],"th)"),paste0(round(x$BIC_hist[1:(dim(x$trend_hist)[2])],4),paste0(" (",1:(dim(x$trend_hist)[2]),"th)")))

    raw_trend <- cbind.data.frame(x$raw_data,x$trend,x$trend_hist)
    data <- rbind(value_hist,raw_trend)
    colnames(data) <- colname

    #--------------------- trend selection ----------------------------------------------------
    if(trend_hist == TRUE){
      data_trend_select <- as.matrix(data)[,c(1,2,select_trend_hist+2)] %>% as.data.frame()
    }
    if(trend_hist == FALSE){
      data_trend_select <- as.matrix(data)[,c(1,2)] %>% as.data.frame()
    }
    # end trend selection


    }

    # manage the Time Serise ID

    row.names(data) <- c(" ",1:length(x$raw_data))
    row.names(data_trend_select) <- c(" ",1:length(x$raw_data))

    #----------------------------------------------------------------------------------------
    #--------------------- row selection ----------------------------------------------------

    if(Head == FALSE & Tail == FALSE){row <- 1:(dim(data)[1])}

    if(Head == TRUE & Tail == FALSE){
     if(dim(data)[1] > 6){
       row <- 1:6
     } else{row <- 1:(dim(data)[1])}
    }

    if(Head == FALSE & Tail == TRUE){
      if(dim(data)[1] > 6){
        row <- c(1, ((dim(data)[1]-4):dim(data)[1])  )
      } else{row <- 1:(dim(data)[1])}
    }

    if(Head == TRUE & Tail == TRUE){
      if(dim(data)[1] > 11){
        row <- c(1:6, ((dim(data)[1]-4):dim(data)[1])  )
      } else{row <- 1:(dim(data)[1])}
    }

    data_row_trend_select <- data_trend_select[row,]


    stargazer(data_row_trend_select,summary = F, type = print_type)

    View(data)
    message("You can view the whole content of bHP via the above spreadsheet-style data viewer.")

  }
}


