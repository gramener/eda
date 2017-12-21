eda_univariate <- function(data = NULL,file_info = NULL,columns =NULL,k = 3){
  varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
    vars <- character(0)
    if (any(type %in% "numeric")) {
      vars <- c(vars,names(df)[sapply(df,is.numeric)])
    }
    if (any(type %in% "character")) {
      vars <- c(vars,names(df)[sapply(df,is.character)])
    }
    vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
  }
  dist <- function(var){
    a<-cumsum(as.numeric(var))
    b<- a/sum(as.numeric(var))
    c<- length(which(b<=0.8))
    d<- c/length(var)
    return(d)
  }
  if(!is.null(data)){
    if(!is.null(columns)){
      data <- data[,columns]
    }
    mylist.names <- "columns"
    uni <- vector("list", length(mylist.names))
    names(uni) <- mylist.names
    num_var <- varlist(data,"numeric")
    data <- data[,num_var]
    uni$columns <- vector("list", length(names(data)))
    names(uni$columns) <- names(data)
    mydataframe <- c("Column_Names","`IQR_upper_outier%`","`IQR_lower_outier%`",paste("`","m+",k,"sd_outliers%","`",sep = ""),paste("`","m-",k,"sd_outliers%","`",sep = ""),"concentration","priority","performance","notes")
    for(i in 1:length(uni$columns)){
      if(is.numeric(data[,i])){
      uni$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
        a1 <- names(data)[i]
        q1 <-quantile(data[,i],0.25,na.rm = T)
        q3 <- quantile(data[,i],0.75,na.rm =T )
        iqr <- q3 - q1
        a2 <- q3 + 1.5*iqr
        a3 <- q1 - 1.5*iqr
        a4 <- paste(round(((length(which(data[,i] > a2))*100)/(nrow(data))),1),"%")
        a5 <- paste(round(((length(which(data[,i] < a3))*100)/(nrow(data))),1),"%")
        a6 <- mean(data[,i],na.rm = T) + k*sd(data[,i],na.rm = T)
        a7 <- mean(data[,i],na.rm = T) - k*sd(data[,i],na.rm = T)
        a8 <- paste(round(((length(which(data[,i] > a6))*100)/(nrow(data))),1),"%")
        a9 <- paste(round(((length(which(data[,i] < a7))*100)/(nrow(data))),1),"%")
        a13 <- paste(round((dist(data[,i]) *100)),"%")
        a10 <- NA
        a11 <- NA
        a12 <- NA
      }
      uni$columns[[i]] <- rbind(uni$columns[[i]],data.frame("a1" = a1,"a4"=a4,"a5"=a5,"a8"=a8,"a9"=a9,"a10" = a13,"a11"= a10,"a12" = a11,"a13" = a12))
      row.names(uni$columns[[i]]) <- NULL
      names(uni$columns[[i]]) <- mydataframe
      rm(a1); rm(a2); rm(a3); rm(a4); rm(a5); rm(a6); rm(a7); rm(a8); rm(a9); rm(a10); rm(a11); rm(a12); rm(a13);
    }
    return(uni)
  }
  else if(is.null(data) & !is.null(file_info$data)){
    data <- file_info$data
    if(!is.null(columns)){
      data <- data[,columns]
    }
    mylist.names <- "columns"
    uni <- vector("list", length(mylist.names))
    names(uni) <- mylist.names
    num_var <- varlist(data,"numeric")
    data <- data[,num_var]
    uni$columns <- vector("list", length(data))
    names(uni$columns) <- names(data)
    mydataframe <- c("Column_Names","`IQR_upper_outier%`","`IQR_lower_outier%`",paste("`","m+",k,"sd_outliers%","`",sep = ""),paste("`","m-",k,"sd_outliers%","`",sep = ""),"concentration","priority","performance","notes")
    for(i in 1:length(uni$columns)){
      if(is.numeric(data[,i])){
      uni$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
        a1 <- names(data)[i]
        q1 <-quantile(data[,i],0.25,na.rm = T)
        q3 <- quantile(data[,i],0.75,na.rm =T )
        iqr <- q3 - q1
        a2 <- q3 + 1.5*iqr
        a3 <- q1 - 1.5*iqr
        a4 <- paste(round(((length(which(data[,i] > a2))*100)/(nrow(data))),1),"%")
        a5 <- paste(round(((length(which(data[,i] < a3))*100)/(nrow(data))),1),"%")
        a6 <- mean(data[,i],na.rm = T) + k*sd(data[,i],na.rm = T)
        a7 <- mean(data[,i],na.rm = T) - k*sd(data[,i],na.rm = T)
        a8 <- paste(round(((length(which(data[,i] > a6))*100)/(nrow(data))),1),"%")
        a9 <- paste(round(((length(which(data[,i] < a7))*100)/(nrow(data))),1),"%")
        a13 <- paste(round((dist(data[,i]) *100)),"%")
        a10 <- NA
        a11 <- NA
        a12 <- NA
      }
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
}

