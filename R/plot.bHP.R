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
#' plot(bx_HP)
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
#' 
#' 


plot.bHP <- function(x, plot_type= "dynamic", interval_t = 0.3,  ylab = "",
                     col_raw = "#2D5375", col_trend_h = "#FBB545", col_trend_f = "red", col_pvalue_BIC = "red", 
                     # the range of alpha is 0-255
                     raw_alpha = 255, trend_h_alpha = 75, trend_f_alpha = 255, pvalue_BIC_alpha = 255,
                     legend_location = "upleft",iteration_location = "downright", cex_text=1.7, cex_legend = 1.5,
                     main = paste0("Figure of ",x$test_type," bHP (",plot_type,")")
                     ){
  
  message(plot_type, " plot of ", x$test_type, " bHP")
  
  # laod default par, for resetting
  data("def_par")
  
  location <- function(textinplot_location){
    switch(textinplot_location,
           upright = c(xticks[length(xticks)-2], yticks[length(yticks)]),
           downright = c(xticks[length(xticks)-2], (yticks[1]+yticks[2])/2),
           upleft = c((2.5)*(xticks[1]+xticks[2])/3, yticks[length(yticks)]),
           downleft = c((2.5)*(xticks[1]+xticks[2])/3, (yticks[1]+yticks[2])/2) )
  }
  
  # POSIXct (date/time) index
  
  if(plot_type == "static"){
    
    if(x$test_type == "none-iter"){
      
      dev.next()
      par(def_par)
      #layout(matrix(1), widths = lcm(12), heights = lcm(12))
      par(las=1)
      col_rgb <- as.numeric(col2rgb(col_raw))
      
      plot( x$raw_data,  pch = 16, col = rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = raw_alpha,  maxColorValue=255)
            ,lwd = 1,ylab = ylab)
      
      col_rgb <- as.numeric(col2rgb(col_trend_f))
      col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = trend_f_alpha,  maxColorValue=255)
      lines(x$trend ,type = "l",col = col,lwd = 2)
      
      x_l <- x$raw_data
      y_l <- NULL
      xy <- xy.coords(x_l, y_l, log = "")
      xy$xlab <- NULL
      
      #xlim_design <- range(xy$x[is.finite(xy$x)])
      #ylim_design <- range(xy$y[is.finite(xy$y)])
      
      localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
      xticks <- localAxis(xy$x,side = 1)
      yticks <- localAxis(xy$y,side = 2)
      # may try col = "gray90"
      abline(NULL, NULL, lty=1, col="gray80", lwd=.08, h=yticks)
      abline(NULL, NULL, lty=1, col="gray80", lwd=.08, h=NULL, v=xticks)
      
      # Title
      # mtext(main, side=3, line=1, cex=1.3, family="Consolas")
      mtext(main, side=3, line=1, cex=1.3)
      
      
      # location of text
      
      temp <- try(location(iteration_location), silent = T)
      if('try-error' %in% class(temp)){
        locate_text <- as.numeric(iteration_location)
      }else{
        locate_text <- as.numeric(location(iteration_location))
      }
      
      
      # location of legend
      
      temp <- try(location(legend_location), silent = T)
      if('try-error' %in% class(temp)){
        locate_legend <- as.numeric(legend_location)
      }else{
        locate_legend <- as.numeric(location(legend_location))
      }
      
      # Text
      text(x = locate_text[1], y = locate_text[2], c("Iterated Only Once"),cex= cex_text,col = "black", font=2)
      
      # Legend
      legend(x = locate_legend[1], y = locate_legend[2], c("Raw Data",  "Trend Component"),  col = c(col_raw,col_trend_f),
             text.col = c(col_raw,col_trend_f), lty = c(-1,  -1), pch = c(16,  15),text.font = 3, cex = cex_legend,
             bg = "white", bty = "n")
      
      
    }
    
    if(x$test_type != "none-iter"){
      
      dev.next()
      par(def_par)
      layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE), c(6), c(4,2), TRUE)
      par(mar = c(3,4,3,3),las=1)
      
      col_rgb <- as.numeric(col2rgb(col_raw))
      
      plot( x$raw_data,  pch = 16, col = rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = raw_alpha,  maxColorValue=255)
            ,lwd = 1,ylab = ylab)
      
      for(i in 1: x$iter_num){
        
        if(i == x$iter_num){
          col_rgb <- as.numeric(col2rgb(col_trend_f))
          col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = trend_f_alpha,  maxColorValue=255)
          lwd <- 1.6
          
        }else{
          col_rgb <- as.numeric(col2rgb(col_trend_h))
          col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = trend_h_alpha,  maxColorValue=255)
          lwd <- 2
        }
        
        lines(x$trend_hist[,i],type = "l",col = col,lwd = lwd) 
      }
      x_l <- x$raw_data
      y_l <- NULL
      xy <- xy.coords(x_l, y_l, log = "")
      xy$xlab <- NULL
      
      #xlim_design <- range(xy$x[is.finite(xy$x)])
      #ylim_design <- range(xy$y[is.finite(xy$y)])
      
      localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
      xticks <- localAxis(xy$x,side = 1)
      yticks <- localAxis(xy$y,side = 2)
      # may try col = "gray90"
      abline(NULL, NULL, lty=1, col="gray80", lwd=.08, h=yticks)
      abline(NULL, NULL, lty=1, col="gray80", lwd=.08, h=NULL, v=xticks)
      
      # Title
      # mtext(main, side=3, line=1, cex=1.3, family="Consolas")
      mtext(main, side=3, line=1, cex=1.3)
      
      
      # location of text
      
      temp <- try(location(iteration_location), silent = T)
      if('try-error' %in% class(temp)){
        locate_text <- as.numeric(iteration_location)
      }else{
        locate_text <- as.numeric(location(iteration_location))
      }
      
      
      # location of legend
      
      temp <- try(location(legend_location), silent = T)
      if('try-error' %in% class(temp)){
        locate_legend <- as.numeric(legend_location)
      }else{
        locate_legend <- as.numeric(location(legend_location))
      }
      
      
      text(x = locate_text[1], y = locate_text[2], paste0("Iterated Times: ",as.character(c(1:x$iter_num)[i])),cex= cex_text,col = "black", font=2)
      
      # Legend
      legend(x = locate_legend[1], y = locate_legend[2], c("Raw Data", "Trend History", "Final Trend"),  col = c(col_raw,col_trend_h,col_trend_f),
             text.col = c(col_raw,col_trend_h,col_trend_f), lty = c(-1, -1, -1), pch = c(16, 15, 15),text.font = 3, cex = cex_legend,
             bg = "white", bty = "n" )
      
      if(x$test_type == "adf"){
        
        par(mar = c(3,4,3,3),las=1)
        
        col_rgb <- as.numeric(col2rgb(col_pvalue_BIC ))
        col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = pvalue_BIC_alpha,  maxColorValue=255)
        
        plot(x$adf_p_hist,main = c("p-value"),bty="l",ylab = "",xlim = c(0,x$iter_num+1),ylim = c(0,range(x$adf_p_hist)[2]), col = col, pch = 19)
        abline(h = x$signif_p ,  col = "gray50")
        text(0.5,(x$signif_p+0.03), paste0("sig p = ",x$signif_p)  , col = "gray50")
      } 
      
      if(x$test_type == "BIC"){
        par(mar = c(3,4,3,3),las=1)
        
        col_rgb <- as.numeric(col2rgb(col_pvalue_BIC ))
        col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = pvalue_BIC_alpha,  maxColorValue=255)
        
        plot(x$BIC_hist,main = c("BIC value"),bty="l",ylab = "",xlim = c(0,x$iter_num+1),ylim = range(x$BIC_hist), col = col, pch = 19)
        abline(h = as.numeric(x$BIC_hist[x$iter_num]),  col = "gray50")
        abline(v = which.min(x$BIC_hist), col = "gray50")
        text(as.numeric(x$iter_num),as.numeric(x$BIC_hist[x$iter_num]+0.05), paste0("min BIC(",x$iter_num,"th)")  , col = "gray50") 
      }
      
      if(x$test_type == "none"){
        par(mar = c(3,4,3,3),las=1)
        
        col_rgb <- as.numeric(col2rgb(col_pvalue_BIC ))
        col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = pvalue_BIC_alpha,  maxColorValue=255)
        
        plot(x$BIC_hist,main = c("BIC value"),bty="l",ylab = "",xlim = c(0,x$iter_num+1),ylim = c(0,range(x$BIC_hist)[2]), col = col, pch = 19)
        abline(h = min(as.numeric(x$BIC_hist)),  col = "gray50")
        abline(v = which.min(x$BIC_hist), col = "gray50")
        text(which.min(x$BIC_hist),as.numeric(min(x$BIC_hist))-0.05, paste0("minimal BIC(",which.min(x$BIC_hist),"th)")  , col = "gray50") 
      }
      
    }
    
    
  }
  
  if(plot_type == "JS"){
    
    data_1 <- data.frame(date = 1:length(x$raw_data), raw_data = x$raw_data, trend_data = x$trend)
    
    p1 <- plot_ly(data_1, x = ~date, color = I(col_raw)) %>%
      add_lines(y = ~raw_data,name = "raw data", legendgroup = "raw data") %>%
      add_lines(y = ~trend_data, color = I(col_trend_f),name = "final trend", legendgroup = "final trend")%>%
      layout(showlegend = T, yaxis = list(title = ylab),title = main) 
    
    if(x$test_type == "none-iter"){
      
      return(p1)
      
    }
    
    if(x$test_type == "adf"){
      
      data_2 <- data.frame(iter_time = 1:x$iter_num, p_value = x$adf_p_hist)
      p2 <- plot_ly(data_2, x = ~iter_time, y = ~p_value,color = I(col_pvalue_BIC), 
                    name = "p-value", legendgroup = "p-value")
    
      return(subplot(p1, p2, heights = c(0.7,0.3), nrows = 2, margin = 0.05))
    }
    
    if(x$test_type == "BIC"){
      
      data_2 <- data.frame(iter_time = 1:x$iter_num, BIC = x$BIC_hist[1:x$iter_num])
        
      p2 <- plot_ly(data_2, x = ~iter_time, y = ~BIC,color = I(col_pvalue_BIC), 
                    name = "BIC", legendgroup = "BIC") %>%
        layout(showlegend = T,yaxis = list(title = "BIC"))
      
      return(subplot(p1, p2, heights = c(0.7,0.3), nrows = 2, margin = 0.05))
    }
    
    if(x$test_type == "none"){
      
      data_2 <- data.frame(iter_time = 1:x$iter_num, BIC = x$BIC_hist[1:x$iter_num]) 
      
      p2 <- plot_ly(data_2, x = ~iter_time, y = ~BIC,color = I(col_pvalue_BIC), 
                    name = "BIC", legendgroup = "BIC") %>%
        layout(showlegend = T,yaxis = list(title = "BIC"))
      
      return(subplot(p1, p2, heights = c(0.7,0.3), nrows = 2, margin = 0.05))
    }
    
  }
  
  if(plot_type == "dynamic"){
    
    if(x$test_type == "none-iter"){
      
      stop("'none-iter' bHP doesn't have dynamic picture: returning NA")
      return(NA_real_)
      
    }
    
    if(x$test_type != "none-iter"){
      
      cat("It may take more seconds if iteration number is large.","\n","Please be patient!","\n")
      
      
      saveGIF({
        
        for(i in 1: x$iter_num){
          
          layout(matrix(c(1,1,2,2), 2, 2, byrow = TRUE),widths = lcm(5), heights = lcm(16))
          par(mar = c(3,4,5,3),las=1)
          
          col_rgb <- as.numeric(col2rgb(col_raw))
          
          plot( x$raw_data,  pch = 16, col = rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = raw_alpha,  maxColorValue=255)
               ,lwd = 1,ylab = ylab)
          
          for(j in 1:i){
            
            if(j == x$iter_num){
              col_rgb <- as.numeric(col2rgb(col_trend_f))
              col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = trend_f_alpha,  maxColorValue=255)
              lwd <- 1.6
              
            }else{
              col_rgb <- as.numeric(col2rgb(col_trend_h))
              col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = trend_h_alpha,  maxColorValue=255)
              lwd <- 2
            }
            
            lines(x$trend_hist[,j],type = "l",col = col,lwd = lwd) 
            
            x_l <- x$raw_data
            y_l <- NULL
            xy <- xy.coords(x_l, y_l, log = "")
            xy$xlab <- NULL
            
            #xlim_design <- range(xy$x[is.finite(xy$x)])
            
            #ylim_design <- range(xy$y[is.finite(xy$y)])
            
            localAxis <- function(..., col, bg, pch, cex, lty, lwd) Axis(...)
            xticks <- localAxis(xy$x,side = 1)
            yticks <- localAxis(xy$y,side = 2)
            # may try col = "gray90"
            abline(NULL, NULL, lty=1, col="gray80", lwd=.08, h=yticks)
            abline(NULL, NULL, lty=1, col="gray80", lwd=.08, h=NULL, v=xticks)
            
            # Title
            # mtext(main, side=3, line=1, cex=1.3, family="Consolas")
            mtext(main, side=3, line=1, cex=1.3)
            
            
          }
          
          # location of text
          
          temp <- try(location(iteration_location), silent = T)
          if('try-error' %in% class(temp)){
            locate_text <- as.numeric(iteration_location)
          }else{
            locate_text <- as.numeric(location(iteration_location))
          }
          
          
          
          # location of legend
          
          temp <- try(location(legend_location), silent = T)
          if('try-error' %in% class(temp)){
            locate_legend <- as.numeric(legend_location)
          }else{
            locate_legend <- as.numeric(location(legend_location))
          }
          
          
          text(x = locate_text[1], y = locate_text[2], paste0("Iterated Times: ",as.character(c(1:x$iter_num)[i])),cex= cex_text,col = "black", font=2)
          
          # Legend
          legend(x = locate_legend[1], y = locate_legend[2], c("Raw Data", "Trend History", "Final Trend"),  col = c(col_raw,col_trend_h,col_trend_f),
                 text.col = c(col_raw,col_trend_h,col_trend_f), lty = c(-1, -1, -1), pch = c(16, 15, 15),text.font = 3, cex = cex_legend,
                 bg = "white", bty = "n" )
          
          if(x$test_type == "adf"){
            
            par(mar = c(3,4,3,3),las=1)
            
            col_rgb <- as.numeric(col2rgb(col_pvalue_BIC ))
            col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = pvalue_BIC_alpha,  maxColorValue=255)
            
            plot(x$adf_p_hist[1:i],main = paste0("p-value = ",round(x$adf_p_hist[i],4)),bty="l",ylab = "",xlim = c(0,x$iter_num+1),ylim = c(0,range(x$adf_p_hist)[2]), col = col, pch = 19)
            abline(h = x$signif_p ,  col = "gray50")
            text(0.5,(x$signif_p+0.03), paste0("sig_p = ",x$signif_p)  , col = "gray50")
          } 
          
          if(x$test_type == "BIC"){
            par(mar = c(3,4,3,3),las=1)
            
            col_rgb <- as.numeric(col2rgb(col_pvalue_BIC ))
            col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = pvalue_BIC_alpha,  maxColorValue=255)
            
            plot(x$BIC_hist[1:i],main = paste0("BIC value = ",round(x$BIC_hist[i],4)),bty="l",ylab = "",xlim = c(0,x$iter_num+1),ylim = c(0,range(x$BIC_hist)[2]), col = col, pch = 19)
            abline(h = as.numeric(x$BIC_hist[x$iter_num]),  col = "gray50")
            text(as.numeric(x$iter_num),as.numeric(x$BIC_hist[x$iter_num]-0.3), paste0("min BIC(",x$iter_num,"th)")  , col = "gray50") 
          }
          
          if(x$test_type == "none"){
            par(mar = c(3,4,3,3),las=1)
            
            col_rgb <- as.numeric(col2rgb(col_pvalue_BIC ))
            col <- rgb(red = col_rgb[1],green = col_rgb[2],blue = col_rgb[3],alpha = pvalue_BIC_alpha,  maxColorValue=255)
            
            plot(x$BIC_hist[1:i],main = paste0("BIC value = ",round(x$BIC_hist[i],4)),bty="l",ylab = "",xlim = c(0,x$iter_num+1),ylim = c(0,range(x$BIC_hist)[2]), col = col, pch = 19)
            abline(h = min(as.numeric(x$BIC_hist)),  col = "gray50")
            abline(v = which.min(x$BIC_hist), col = "gray50")
            text(which.min(x$BIC_hist),as.numeric(min(x$BIC_hist))-0.3, paste0("minimal BIC(",which.min(x$BIC_hist),"th)")  , col = "gray50") 
          }
          
        }
        
      }, movie.name="bHP_ani.gif" , interval = interval_t, nmax = as.numeric(x$iter_num), 
      ani.width = 800, ani.height = 600)
    }
    
    
  }
  
}
