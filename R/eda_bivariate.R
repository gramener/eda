eda_bivariate <- function(metadata){
  require(dplyr)
  require(data.table)
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
  data <- metadata$data
  mylist.names <- c("cat_VS_cat","cat_VS_num","save","saveplot")
  bivar <- vector("list", length(mylist.names))
  names(bivar) <- mylist.names
  bivar$save <- function(path){
    require(xlsx)
    bivar_data <- bivar
    if(!file.exists(path)){
      wb1 = createWorkbook()
      sheet_cat_cat = createSheet(wb1,"Bivariate Tables-cat vs cat")
      sheet_cat_num = createSheet(wb1,"Bivariate Tables-cat vs num")
      csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
      q <- 2
      for(i in 1:length(bivar_data$cat_VS_cat)){
        name <- as.data.frame(names(bivar_data$cat_VS_cat)[[i]])
        addDataFrame(name, sheet=sheet_cat_cat,startRow= q-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
        addDataFrame(bivar_data$cat_VS_cat[[i]], sheet=sheet_cat_cat,startRow= q ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        q <- q + 5 + nrow(bivar_data$cat_VS_cat[[i]])
      }
      v <- 2
      for(j in 1:length(bivar_data$cat_VS_num)){
        name <- as.data.frame(names(bivar_data$cat_VS_num)[[j]])
        addDataFrame(name, sheet=sheet_cat_num,startRow= v-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
        addDataFrame(bivar_data$cat_VS_num[[j]], sheet=sheet_cat_num,startRow= v ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        v <- v + 5 + nrow(bivar_data$cat_VS_num[[j]])
      }
      saveWorkbook(wb1, path)
    }
    else{
      wb1<-loadWorkbook(path)
      sheet_cat_cat = createSheet(wb1,"Bivariate Tables-cat vs cat")
      sheet_cat_num = createSheet(wb1,"Bivariate Tables-cat vs num")
      csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
      q <- 2
      for(i in 1:length(bivar_data$cat_VS_cat)){
        name <- as.data.frame(names(bivar_data$cat_VS_cat)[[i]])
        addDataFrame(name, sheet=sheet_cat_cat,startRow= q-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
        addDataFrame(bivar_data$cat_VS_cat[[i]], sheet=sheet_cat_cat,startRow= q  ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        q <- q + 5 + nrow(bivar_data$cat_VS_cat[[i]])
      }
      v <- 2
      for(j in 1:length(bivar_data$cat_VS_num)){
        name <- as.data.frame(names(bivar_data$cat_VS_num)[[j]])
        addDataFrame(name, sheet=sheet_cat_num,startRow= v-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
        addDataFrame(bivar_data$cat_VS_num[[j]], sheet=sheet_cat_num,startRow= v ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        v <- v + 5 + nrow(bivar_data$cat_VS_num[[j]])
      }
      saveWorkbook(wb1, path)
    }
  }
  bivar$saveplot <-  function(path,method= c("pearson","spearman")){
    require(xlsx)
    require(dplyr)
    require(ggplot2)
    require(corrplot)
      x = 1
      y = 1
      z = 1
      num_var <- varlist(data,"numeric")
      cat_var <- varlist(data,"character")
      if(!file.exists(path)){
        wb1<-createWorkbook(type = "xlsx")
        sheet_num_num = createSheet(wb1,"Bivariate Plots-num vs num")
        sheet_cat_num = createSheet(wb1,"Bivariate Plots-cat vs num")
        for(i in 1:(length(num_var)-1)){
          for( j in 1+i:(length(num_var)-1)){
            corr_mat <- cor(data[,c(num_var[i],num_var[j])],method = method)
            png("correlation.png", height=1200, width=2000, res=250, pointsize=8)
            corrplot(corr_mat)
            dev.off()
            addPicture("correlation.png", sheet_num_num, scale = 1, startRow = 4,startColumn = x)
            res<-file.remove("correlation.png")
            png("scatter.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(data,aes(x=data[,num_var[i]],y=data[,num_var[j]])) + geom_point(col="#75AADB") + labs(x=num_var[i],y = num_var[j],title = paste(num_var[i],"versus",num_var[j])))
            dev.off()
            addPicture("scatter.png", sheet_num_num, scale = 1, startRow = 30,startColumn = x)
            res<-file.remove("scatter.png")
            x <- x + 15
          }
        }
        for(i in 1:(length(cat_var))){
          for( j in 1:(length(num_var))){
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Mean" = round(mean(eval(parse(text =num_var[j])),na.rm =T),2))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("meanplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Average",num_var[j],sep=" "),title = paste("Average",num_var[j],"per",cat_var[i])))
            dev.off()
            addPicture("meanplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("meanplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Sum" = sum(eval(parse(text =num_var[j])),na.rm =T))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("sumplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Sum of",num_var[j],sep=" "),title = paste("Sum of",num_var[j],"per",cat_var[i])))
            dev.off()
            addPicture("sumplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("sumplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Min" = min(eval(parse(text =num_var[j]))))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("minplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Minimum",num_var[j],sep=" "),title = paste("Minimum",num_var[j],"per",cat_var[i])))
            dev.off()
            addPicture("minplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("minplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Max" = max(eval(parse(text =num_var[j]))))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("maxplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Maximum",num_var[j],sep=" "),title = paste("Maximum",num_var[j],"per",cat_var[i])))
            dev.off()
            addPicture("maxplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("maxplot.png")
            y <- y +25
            z <- 1
          }
        }
      }
      else{
        wb1<-loadWorkbook(path)
        sheet_num_num = createSheet(wb1,"Bivariate Plots-num vs num")
        sheet_cat_num = createSheet(wb1,"Bivariate Plots-cat vs num")
        for(i in 1:(length(num_var)-1)){
          for( j in 1+i:(length(num_var)-1)){
            corr_mat <- cor(data[,c(num_var[i],num_var[j])],method = method)
            png("correlation.png", height=1200, width=2000, res=250, pointsize=8)
            corrplot(corr_mat)
            dev.off()
            addPicture("correlation.png", sheet_num_num, scale = 1, startRow = 4,startColumn = x)
            res<-file.remove("correlation.png")
            png("scatter.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(data,aes(x=data[,num_var[i]],y=data[,num_var[j]])) + geom_point(col="#75AADB") + labs(x=num_var[i],y = num_var[j],title = paste(num_var[i],"versus",num_var[j])))
            dev.off()
            addPicture("scatter.png", sheet_num_num, scale = 1, startRow = 30,startColumn = x)
            res<-file.remove("scatter.png")
            x <- x + 15
          }
        }
        for(i in 1:(length(cat_var))){
          for( j in 1:(length(num_var))){
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Mean" = round(mean(eval(parse(text =num_var[j])),na.rm =T),2))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("meanplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Average",num_var[j],sep=" "),title = paste("Average",num_var[j],"per",cat_var[i])))
            dev.off()
            addPicture("meanplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("meanplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Sum" = sum(eval(parse(text =num_var[j])),na.rm =T))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("sumplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Sum of",num_var[j],sep=" "),title = paste("Sum of",num_var[j],"per",cat_var[i])))
            dev.off()
            addPicture("sumplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("sumplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Min" = min(eval(parse(text =num_var[j]))))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("minplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Minimum",num_var[j],sep=" "),title = paste("Minimum",num_var[j],"per",cat_var[i])))
            dev.off()
            addPicture("minplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("minplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Max" = max(eval(parse(text =num_var[j]))))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("maxplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Maximum",num_var[j],sep=" "),title = paste("Maximum",num_var[j],"per",cat_var[i])))
            dev.off()
            addPicture("maxplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("maxplot.png")
            y <- y +25
            z <- 1
          }
        }
      }
      saveWorkbook(wb1, path)
  }
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
return(bivar)
}



