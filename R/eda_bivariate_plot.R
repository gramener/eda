eda_bivariate_plot <- function(data = NULL,file_info = NULL,wb,path = NULL,method= c("pearson","spearman"),columns = NULL){
  require(xlsx)
  require(dplyr)
  require(ggplot2)
  require(corrplot)
  varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
    vars <- character(0)
    if (any(type %in% "numeric")) {
      vars <- c(vars,names(df)[sapply(df,is.numeric)])
    }
    if (any(type %in% "factor")) {
      vars <- c(vars,names(df)[sapply(df,is.factor)])
    }
    if (any(type %in% "character")) {
      vars <- c(vars,names(df)[sapply(df,is.character)])
    }
    vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
  }
  wb <- paste(wb,"xlsx",sep = ".")
  if(!is.null(data)){
    if(!is.null(columns)){
      data <- data[,columns]
    }
    x = 1
    y = 1
    z = 1
    num_var <- varlist(data,"numeric")
    cat_var <- varlist(data,"character")
    if(!is.null(path)){
      wbb <- paste(path,wb,sep="/")
    }
    else{
      wbb <- wb
    }
    if(!file.exists(wbb)){
      wb1<-createWorkbook(type = "xlsx")
      sheet_num_num = createSheet(wb1,"Correlation & Scatter Plot")
      sheet_cat_num = createSheet(wb1,"Bar Plots")
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
      wb1<-loadWorkbook(wbb)
      sheet_num_num = createSheet(wb1,"Correlation & Scatter Plot")
      sheet_cat_num = createSheet(wb1,"Bar Plot")
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
      saveWorkbook(wb1, wbb)
  }
  else if(is.null(data) & !is.null(file_info$data)){
    data <- file_info$data
    if(!is.null(columns)){
      data <- data[,columns]
    }
    x = 1
    y = 1
    z = 1
    num_var <- varlist(data,"numeric")
    cat_var <- varlist(data,"character")
    if(!is.null(path)){
      wbb <- paste(path,wb,sep="/")
    }
    else{
      wbb <- wb
    }
    if(!file.exists(wbb)){
      wb1<-createWorkbook(type = "xlsx")
      sheet_num_num = createSheet(wb1,"Correlation & Scatter Plot")
      sheet_cat_num = createSheet(wb1,"Bar Plot")
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
      wb1<-loadWorkbook(wbb)
      sheet_num_num = createSheet(wb1,"Correlation & Scatter Plot")
      sheet_cat_num = createSheet(wb1,"Bar Plot")
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
    saveWorkbook(wb1, wbb)
  }
}
