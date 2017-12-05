eda_univariate <- function(data = NULL,file_info = NULL,k = 3){
  start <- Sys.time()
  dist <- function(var){
    a<-cumsum(as.numeric(var))
    b<- a/sum(as.numeric(var))
    c<- length(which(b<=0.8))
    d<- c/length(var)
    return(d)
  }
  if(!is.null(data)){
    mylist.names <- "columns"
    uni <- vector("list", length(mylist.names))
    names(uni) <- mylist.names
    uni$columns <- vector("list", length(names(data)))
    names(uni$columns) <- names(data)
    mydataframe <- c("Column_Names","`IQR_upper_outier%`","`IQR_lower_outier%`",paste("`","m+",k,"sd_outliers%","`",sep = ""),paste("`","m-",k,"sd_outliers%","`",sep = ""),"concentration","priority","performance","notes")
    for(i in 1:length(uni$columns)){
      uni$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
      a1 <- names(data)[i]
      if(is.numeric(data[,i])){
        q1 <-quantile(data[,i],0.25,na.rm = T)
        q3 <- quantile(data[,i],0.75,na.rm =T )
        iqr <- q3 - q1
        a2 <- q3 + 1.5*iqr
        a3 <- q1 - 1.5*iqr
        a4 <- (length(which(data[,i] > a2))*100)/(nrow(data))
        a5 <- (length(which(data[,i] < a3))*100)/(nrow(data))
        a6 <- mean(data[,i],na.rm = T) + k*sd(data[,i],na.rm = T)
        a7 <- mean(data[,i],na.rm = T) - k*sd(data[,i],na.rm = T)
        a8 <- (length(which(data[,i] > a6))*100)/(nrow(data))
        a9 <- (length(which(data[,i] < a7))*100)/(nrow(data))
        a13 <- dist(data[,i]) *100
      }
      else{
        a2 <- NA
        a3 <- NA
        a4 <- NA
        a5 <- NA
        a6 <- NA
        a7 <- NA
        a8 <- NA
        a9 <- NA
        a13 <- NA
      }
      a10 <- NA
      a11 <- NA
      a12 <- NA

      uni$columns[[i]] <- rbind(uni$columns[[i]],data.frame("a1" = a1,"a4"=a4,"a5"=a5,"a8"=a8,"a9"=a9,"a10" = a13,"a11"= a10,"a12" = a11,"a13" = a12))
      row.names(uni$columns[[i]]) <- NULL
      names(uni$columns[[i]]) <- mydataframe
      rm(a1); rm(a2); rm(a3); rm(a4); rm(a5); rm(a6); rm(a7); rm(a8); rm(a9); rm(a10); rm(a11); rm(a12); rm(a13);
    }
    return(uni)
  }
  else if(is.null(data) & !is.null(file_info$data)){
    data <- file_info$data
    mylist.names <- "columns"
    uni <- vector("list", length(mylist.names))
    names(uni) <- mylist.names
    uni$columns <- vector("list", length(names(data)))
    names(uni$columns) <- names(data)
    mydataframe <- c("Column_Names","`IQR_upper_outier%`","`IQR_lower_outier%`",paste("`","m+",k,"sd_outliers%","`",sep = ""),paste("`","m-",k,"sd_outliers%","`",sep = ""),"concentration","priority","performance","notes")
    for(i in 1:length(uni$columns)){
      uni$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
      a1 <- names(data)[i]
      if(is.numeric(data[,i])){
        q1 <-quantile(data[,i],0.25,na.rm = T)
        q3 <- quantile(data[,i],0.75,na.rm =T )
        iqr <- q3 - q1
        a2 <- q3 + 1.5*iqr
        a3 <- q1 - 1.5*iqr
        a4 <- (length(which(data[,i] > a2))*100)/(nrow(data))
        a5 <- (length(which(data[,i] < a3))*100)/(nrow(data))
        a6 <- mean(data[,i],na.rm = T) + k*sd(data[,i],na.rm = T)
        a7 <- mean(data[,i],na.rm = T) - k*sd(data[,i],na.rm = T)
        a8 <- (length(which(data[,i] > a6))*100)/(nrow(data))
        a9 <- (length(which(data[,i] < a7))*100)/(nrow(data))
        a13 <- dist(data[,i]) * 100
      }
      else{
        a2 <- NA
        a3 <- NA
        a4 <- NA
        a5 <- NA
        a6 <- NA
        a7 <- NA
        a8 <- NA
        a9 <- NA
        a13 <- NA
      }
      a10 <- NA
      a11 <- NA
      a12 <- NA
      uni$columns[[i]] <- rbind(uni$columns[[i]],data.frame("a1" = a1,"a4"=a4,"a5"=a5,"a8"=a8,"a9"=a9,"a10" = a13,"a11"= a10,"a12" = a11,"a13" = a12))
      row.names(uni$columns[[i]]) <- NULL
      names(uni$columns[[i]]) <- mydataframe
      rm(a1); rm(a2); rm(a3); rm(a4); rm(a5); rm(a6); rm(a7); rm(a8); rm(a9); rm(a10); rm(a11); rm(a12); rm(a13);
    }
    return(uni)
  }
  else{
    return()
  }
  end <- Sys.time()
  time <- end-start
  print(paste("Took",time,"minutes"))
}
