eda_bivariate <- function(data = NULL,file_info = NULL){
  require(dplyr)
  require(data.table)
  varlist <- function (df=NULL,type=c("numeric","character"), pattern="", exclude=NULL) {
    vars <- character(0)
    if (any(type %in% "numeric")) {
      vars <- c(vars,names(df)[sapply(df,is.numeric)])
    }
    if (any(type %in% "character")) {
      vars <- c(vars,names(df)[sapply(df,is.character)])
    }
    vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
  }
  if(!is.null(data)){
  mylist.names <- c("cat_VS_cat","cat_VS_num")
  bivar <- vector("list", length(mylist.names))
  names(bivar) <- mylist.names
  num_var <- varlist(data,"numeric")
  cat_var <- varlist(data,"character")
  my_cat_cat <- NULL
  for(i in 1:(length(cat_var)-1)){
    for( j in 1+i:(length(cat_var)-1)){
    my_cat_cat <- c(my_cat_cat,paste(cat_var[i],cat_var[j],sep = "_VS_"))
    }
  }
  my_cat_num <- NULL
for(i in 1:(length(cat_var))){
  for( j in 1:(length(num_var))){
    my_cat_num <- c(my_cat_num,paste(cat_var[i],num_var[j],sep = "_VS_"))
  }
}
bivar$cat_VS_cat <- vector("list", length(my_cat_cat))
names(bivar$cat_VS_cat) <- my_cat_cat
bivar$cat_VS_num <- vector("list", length(my_cat_num))
names(bivar$cat_VS_num) <- my_cat_num
k <- 0
l <- 0
for(i in 1:(length(cat_var)-1)){
  for( j in 1+i:(length(cat_var)-1)){
    k <- k + 1
    abc <- as.data.frame.matrix(table(data[,cat_var[i]],data[,cat_var[j]]))
    abc <- setDT(abc, keep.rownames = TRUE)[]
    names(abc)[1] <- ""
    bivar$cat_VS_cat[[k]] <- abc
  }
}
for(i in 1:(length(cat_var))){
  for( j in 1:(length(num_var))){
    l <- l + 1
    abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Mean" = round(mean(eval(parse(text =num_var[j])),na.rm =T),2),"Sum" = sum(eval(parse(text =num_var[j])),na.rm =T),"Min" = min(eval(parse(text =num_var[j]))),"Max" = max(eval(parse(text =num_var[j]))))
    abc <- as.data.frame(abc)
    names(abc)[1] <- ""
    bivar$cat_VS_num[[l]] <- abc
    }
}
  }
  else if(is.null(data) & !is.null(file_info$data)){
    data <- file_info$data
    mylist.names <- c("cat_VS_cat","cat_VS_num")
    bivar <- vector("list", length(mylist.names))
    names(bivar) <- mylist.names
    num_var <- varlist(data,"numeric")
    cat_var <- varlist(data,"character")
    my_cat_cat <- NULL
    for(i in 1:(length(cat_var)-1)){
      for( j in 1+i:(length(cat_var)-1)){
        my_cat_cat <- c(my_cat_cat,paste(cat_var[i],cat_var[j],sep = "_VS_"))
      }
    }
    my_cat_num <- NULL
    for(i in 1:(length(cat_var))){
      for( j in 1:(length(num_var))){
        my_cat_num <- c(my_cat_num,paste(cat_var[i],num_var[j],sep = "_VS_"))
      }
    }
    bivar$cat_VS_cat <- vector("list", length(my_cat_cat))
    names(bivar$cat_VS_cat) <- my_cat_cat
    bivar$cat_VS_num <- vector("list", length(my_cat_num))
    names(bivar$cat_VS_num) <- my_cat_num
    k <- 0
    l <- 0
    for(i in 1:(length(cat_var)-1)){
      for( j in 1+i:(length(cat_var)-1)){
        k <- k + 1
        abc <- as.data.frame.matrix(table(data[,cat_var[i]],data[,cat_var[j]]))
        abc <- setDT(abc, keep.rownames = TRUE)[]
        names(abc)[1] <- ""
        bivar$cat_VS_cat[[k]] <- abc
     }
    }
    for(i in 1:(length(cat_var))){
      for( j in 1:(length(num_var))){
        l <- l + 1
        abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Mean" = round(mean(eval(parse(text =num_var[j])),na.rm =T),2),"Sum" = sum(eval(parse(text =num_var[j])),na.rm =T),"Min" = min(eval(parse(text =num_var[j]))),"Max" = max(eval(parse(text =num_var[j]))))
        abc <- as.data.frame(abc)
        names(abc)[1] <- ""
        bivar$cat_VS_num[[l]] <- abc
      }
    }
  }
return(bivar)
}



