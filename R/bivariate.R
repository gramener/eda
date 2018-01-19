library(R6)
bivariate <- R6Class(
  "Univariate Analysis",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    cat_VS_cat = list(),
    cat_VS_num = list(),
    data = data.frame(),
    initialize = function(metadata) {
      require(dplyr)
      self$data <- metadata$data
      options(scipen = 999)
      varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
        vars <- character(0)
        if (any(type %in% "numeric")) {
          vars <- c(vars,names(df)[sapply(df,is.numeric)])
        }
        if (any(type %in% "character")) {
          vars <- c(vars,names(df)[sapply(df,is.character)])
        }
        if (any(type %in% "factor")) {
          vars <- c(vars,names(df)[sapply(df,is.factor)])
        }
        vars[(!vars %in% exclude) & grepl(vars,pattern=pattern)]
      }
      if(!missing(metadata))
      {
        data <- metadata$data
        num_var <- varlist(data,"numeric")
        cat_var <- c(varlist(data,"character"),varlist(data,"factor"))
        my_cat_cat <- NULL
        if(!identical(cat_var, character(0)) & length(cat_var) >= 2){
          for(i in 1:(length(cat_var)-1)){
            for( j in 1+i:(length(cat_var)-1)){
              my_cat_cat <- c(my_cat_cat,paste(cat_var[i],cat_var[j],sep = "_VS_"))
            }
          }
        }
        if(!identical(num_var, numeric(0)) & length(num_var) >= 1 & !identical(cat_var, character(0)) & length(cat_var) >= 1){
          my_cat_num <- NULL
          for(i in 1:(length(cat_var))){
            for( j in 1:(length(num_var))){
              my_cat_num <- c(my_cat_num,paste(cat_var[i],num_var[j],sep = "_VS_"))
            }
          }
        }
          self$cat_VS_cat <- vector("list", 2)
          names(self$cat_VS_cat) <- c("count","proportion")
          self$cat_VS_cat$count <- vector("list", length(my_cat_cat))
          names(self$cat_VS_cat$count) <- my_cat_cat
          self$cat_VS_cat$proportion <- vector("list", length(my_cat_cat))
          names(self$cat_VS_cat$proportion) <- my_cat_cat
          self$cat_VS_num <- vector("list", length(my_cat_num))
          names(self$cat_VS_num) <- my_cat_num
          k <- 0
          l <- 0
          if(!identical(cat_var, character(0)) & length(cat_var) >= 2){
          for(i in 1:(length(cat_var)-1)){
            for( j in 1+i:(length(cat_var)-1)){
              k <- k + 1
              abc <- as.data.frame.matrix(table(data[,cat_var[i]],data[,cat_var[j]]))
              abc <- setDT(abc, keep.rownames = TRUE)[]
              names(abc)[1] <- ""
              self$cat_VS_cat$count[[k]] <- abc
              abc <- as.data.frame.matrix(round(prop.table(table(data[,cat_var[i]],data[,cat_var[j]])),4))*100
              abc <- setDT(abc, keep.rownames = TRUE)[]
              names(abc)[1] <- ""
              self$cat_VS_cat$proportion[[k]] <- abc
            }
          }
          }
          else{
            self$cat_VS_cat <- NULL
          }
          if(!identical(num_var, numeric(0)) & length(num_var) >= 1 & !identical(cat_var, character(0)) & length(cat_var) >= 1){
          for(i in 1:(length(cat_var))){
            for( j in 1:(length(num_var))){
              l <- l + 1
              abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Mean" = round(mean(eval(parse(text =num_var[j])),na.rm =T),2),"Sum" = sum(eval(parse(text =num_var[j])),na.rm =T),"Min" = min(eval(parse(text =num_var[j]))),"Max" = max(eval(parse(text =num_var[j]))))
              abc <- as.data.frame(abc)
              names(abc)[1] <- ""
              self$cat_VS_num[[l]] <- abc
            }
          }
          }
          else{
            self$cat_VS_num <- NULL
          }
    }
      },
    save = function(savepath){
      require(xlsx)
      if(!file.exists(savepath)){
        wb1 = createWorkbook()
        sheet_cat_cat = createSheet(wb1,"Bivariate Tables-cat vs cat")
        sheet_cat_num = createSheet(wb1,"Bivariate Tables-cat vs num")
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        q <- 2
        for(i in 1:length(self$cat_VS_cat$count)){
          name1 <- as.data.frame(paste("count of",gsub("_"," ",names(self$cat_VS_cat$count)[[i]])))
          name2 <- as.data.frame(paste("proportion of",gsub("_"," ",names(self$cat_VS_cat$count)[[i]])))
          addDataFrame(name1, sheet=sheet_cat_cat,startRow= q-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
          addDataFrame(self$cat_VS_cat$count[[i]], sheet=sheet_cat_cat,startRow= q ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
          addDataFrame(name2, sheet=sheet_cat_cat,startRow= q-1 ,startColumn=(ncol(self$cat_VS_cat$count[[i]])+5), row.names=FALSE,col.names = FALSE)
          addDataFrame(self$cat_VS_cat$proportion[[i]], sheet=sheet_cat_cat,startRow= q ,startColumn = (ncol(self$cat_VS_cat$count[[i]])+5), row.names=FALSE,colnamesStyle=csTableColNames)
          q <- q + 10 + nrow(self$cat_VS_cat$count[[i]])
        }
        v <- 2
        for(j in 1:length(self$cat_VS_num)){
          name <- as.data.frame(names(self$cat_VS_num)[[j]])
          addDataFrame(name, sheet=sheet_cat_num,startRow= v-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
          addDataFrame(self$cat_VS_num[[j]], sheet=sheet_cat_num,startRow= v ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
          v <- v + 10 + nrow(self$cat_VS_num[[j]])
        }
        saveWorkbook(wb1, savepath)
      }
      else{
        wb1<-loadWorkbook(savepath)
        sheet_cat_cat = createSheet(wb1,"Bivariate Tables-cat vs cat")
        sheet_cat_num = createSheet(wb1,"Bivariate Tables-cat vs num")
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        q <- 2
        for(i in 1:length(self$cat_VS_cat$count)){
          name1 <- as.data.frame(paste("count of",gsub("_"," ",names(self$cat_VS_cat$count)[[i]])))
          name2 <- as.data.frame(paste("proportion of",gsub("_"," ",names(self$cat_VS_cat$count)[[i]])))
          addDataFrame(name1, sheet=sheet_cat_cat,startRow= q-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
          addDataFrame(self$cat_VS_cat$count[[i]], sheet=sheet_cat_cat,startRow= q ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
          addDataFrame(name2, sheet=sheet_cat_cat,startRow= q-1 ,startColumn=(ncol(self$cat_VS_cat$count[[i]])+5), row.names=FALSE,col.names = FALSE)
          addDataFrame(self$cat_VS_cat$proportion[[i]], sheet=sheet_cat_cat,startRow= q ,startColumn = (ncol(self$cat_VS_cat$count[[i]])+5), row.names=FALSE,colnamesStyle=csTableColNames)
          q <- q + 10 + nrow(self$cat_VS_cat$count[[i]])
        }
        v <- 2
        for(j in 1:length(self$cat_VS_num)){
          name <- as.data.frame(names(self$cat_VS_num)[[j]])
          addDataFrame(name, sheet=sheet_cat_num,startRow= v-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
          addDataFrame(self$cat_VS_num[[j]], sheet=sheet_cat_num,startRow= v ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
          v <- v + 10 + nrow(self$cat_VS_num[[j]])
        }
        saveWorkbook(wb1, savepath)
      }
    },
    saveplot =  function(savepath,method= c("pearson","spearman")){
      require(xlsx)
      require(dplyr)
      require(ggplot2)
      require(corrplot)
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
      data <- self$data
      x = 1
      y = 1
      z = 1
      num_var <- varlist(data,"numeric")
      cat_var <- varlist(data,"character")
      if(!file.exists(savepath)){
        wb1<-createWorkbook()
        sheet_num_num = createSheet(wb1,"Bivariate Plots-num vs num")
        sheet_cat_num = createSheet(wb1,"Bivariate Plots-cat vs num")
        for(i in 1:(length(num_var)-1)){
          for( j in 1+i:(length(num_var)-1)){
            corr_mat <- cor(data[,c(num_var[i],num_var[j])],method = method)
            png("correlation.png", height=1200, width=2000, res=250, pointsize=8)
            corrplot(corr_mat,addCoef.col = "black")
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
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Average",num_var[j],sep=" "),title = paste("Average",num_var[j],"per",cat_var[i])) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(aes(label=abc[,2]), vjust=1.6, color="black", size=3.5))
            dev.off()
            addPicture("meanplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("meanplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Sum" = sum(eval(parse(text =num_var[j])),na.rm =T))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("sumplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Sum of",num_var[j],sep=" "),title = paste("Sum of",num_var[j],"per",cat_var[i])) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(aes(label=abc[,2]), vjust=1.6, color="black", size=3.5))
            dev.off()
            addPicture("sumplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("sumplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Min" = min(eval(parse(text =num_var[j]))))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("minplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Minimum",num_var[j],sep=" "),title = paste("Minimum",num_var[j],"per",cat_var[i])) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(aes(label=abc[,2]), vjust=1.6, color="black", size=3.5))
            dev.off()
            addPicture("minplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("minplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Max" = max(eval(parse(text =num_var[j]))))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("maxplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Maximum",num_var[j],sep=" "),title = paste("Maximum",num_var[j],"per",cat_var[i])) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(aes(label=abc[,2]), vjust=1.6, color="black", size=3.5))
            dev.off()
            addPicture("maxplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("maxplot.png")
            y <- y +25
            z <- 1
          }
        }
      }
      else{
        wb1<-loadWorkbook(savepath)
        sheet_num_num = createSheet(wb1,"Bivariate Plots-num vs num")
        sheet_cat_num = createSheet(wb1,"Bivariate Plots-cat vs num")
        for(i in 1:(length(num_var)-1)){
          for( j in 1+i:(length(num_var)-1)){
            corr_mat <- cor(data[,c(num_var[i],num_var[j])],method = method)
            png("correlation.png", height=1200, width=2000, res=250, pointsize=8)
            corrplot(corr_mat,addCoef.col = "black")
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
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Average",num_var[j],sep=" "),title = paste("Average",num_var[j],"per",cat_var[i])) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(aes(label=abc[,2]), vjust=1.6, color="black", size=3.5))
            dev.off()
            addPicture("meanplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("meanplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Sum" = sum(eval(parse(text =num_var[j])),na.rm =T))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("sumplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Sum of",num_var[j],sep=" "),title = paste("Sum of",num_var[j],"per",cat_var[i])) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(aes(label=abc[,2]), vjust=1.6, color="black", size=3.5))
            dev.off()
            addPicture("sumplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("sumplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Min" = min(eval(parse(text =num_var[j]))))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("minplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Minimum",num_var[j],sep=" "),title = paste("Minimum",num_var[j],"per",cat_var[i])) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(aes(label=abc[,2]), vjust=1.6, color="black", size=3.5))
            dev.off()
            addPicture("minplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("minplot.png")
            z <- z +15
            abc <- data %>% group_by(data[,cat_var[i]]) %>% summarize("Max" = max(eval(parse(text =num_var[j]))))
            abc <- as.data.frame(abc)
            names(abc)[1] <- cat_var[i]
            png("maxplot.png", height=1200, width=2000, res=250, pointsize=8)
            print(ggplot(abc,aes(x=abc[,1],y=abc[,2])) + geom_bar(stat = "identity",fill="#75AADB") + labs(x=names(abc)[1],y = paste("Maximum",num_var[j],sep=" "),title = paste("Maximum",num_var[j],"per",cat_var[i])) + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + geom_text(aes(label=abc[,2]), vjust=1.6, color="black", size=3.5))
            dev.off()
            addPicture("maxplot.png", sheet_cat_num, scale = 1, startRow = y,startColumn = z)
            res<-file.remove("maxplot.png")
            y <- y +25
            z <- 1
          }
        }
      }
      saveWorkbook(wb1, savepath)
    },
    output = function(){
      if(is.null(self$cat_VS_cat) & !is.null(self$cat_VS_num)){
        bivar <- self$cat_VS_num
      }
      else if(is.null(self$cat_VS_num) & !is.null(self$cat_VS_cat)){
        bivar <- self$cat_VS_cat
      }
      else{
        bivar <- list(self$cat_VS_cat,self$cat_VS_num)
      }
      return(bivar)
    }
  )
)
