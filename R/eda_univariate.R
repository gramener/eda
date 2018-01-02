library(R6)
univariate <- R6Class(
  "Univariate Analysis",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    columns = list(),
    metadata = list(),
    initialize = function(metadata,k = 3) {
      options(scipen = 999)
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
      if(!missing(metadata))
      {
        self$metadata <- as.list.environment(metadata)
        metadata <- as.list.environment(metadata)
        data <- metadata$data
        num_var <- varlist(data,"numeric")
        data <- data[,num_var]
        self$columns <- vector("list", length(names(data)))
        names(self$columns) <- names(data)
        mydataframe <- c("column_name","iqr_upper_outliers","iqr_lower_outliers",paste("`","m+",k,"sd_outliers","`",sep = ""),paste("`","m-",k,"sd_outliers","`",sep = ""),"concentration","priority","performance","notes")
        for(i in 1:length(self$columns)){
          if(is.numeric(data[,i])){
            self$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
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
          self$columns[[i]] <- list("a1" = a1,"a4"=a4,"a5"=a5,"a8"=a8,"a9"=a9,"a10" = a13,"a11"= a10,"a12" = a11,"a13" = a12)
          names(self$columns[[i]]) <- mydataframe
          rm(a1); rm(a2); rm(a3); rm(a4); rm(a5); rm(a6); rm(a7); rm(a8); rm(a9); rm(a10); rm(a11); rm(a12); rm(a13);
        }
      }
      else{
        num_var <- varlist(data,"numeric")
        data <- data[,num_var]
        self$columns <- vector("list", length(names(data)))
        names(self$columns) <- names(data)
        mydataframe <- c("column_name","iqr_upper_outliers","iqr_lower_outliers",paste("`","m+",k,"sd_outliers","`",sep = ""),paste("`","m-",k,"sd_outliers","`",sep = ""),"concentration","priority","performance","notes")
        for(i in 1:length(self$columns)){
          if(is.numeric(data[,i])){
            self$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
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
          self$columns[[i]] <- list("a1" = a1,"a4"=a4,"a5"=a5,"a8"=a8,"a9"=a9,"a10" = a13,"a11"= a10,"a12" = a11,"a13" = a12)
          names(self$columns[[i]]) <- mydataframe
          rm(a1); rm(a2); rm(a3); rm(a4); rm(a5); rm(a6); rm(a7); rm(a8); rm(a9); rm(a10); rm(a11); rm(a12); rm(a13);
        }
      }
    },
    save = function(path,sheet= "Univariate"){
      require(xlsx)
      unidata <- NULL
      for(i in 1:length(self$columns)){
        unidata <- rbind(unidata,as.data.frame(self$columns[[i]]))
      }
      names(unidata) <- gsub("_"," ",names(unidata))
      if(!file.exists(path)){
        wb1 = createWorkbook()
        sheet = createSheet(wb1,sheet)
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        addDataFrame(unidata, sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        setColumnWidth(sheet, colIndex=1:ncol(unidata), colWidth=20)
        saveWorkbook(wb1, path)
      }
      else{
        wb1<-loadWorkbook(path)
        sheet = createSheet(wb1,sheet)
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        addDataFrame(unidata, sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        setColumnWidth(sheet, colIndex=1:ncol(unidata), colWidth=20)
        saveWorkbook(wb1,path)
      }
    },
    saveplot = function(path ,breaks = NULL){
      require(xlsx)
      bar_one <- function(column){
        df1 <- as.data.frame(table(column))
        df <- as.data.frame(sort(table(column),decreasing = T))
        df$Rank <- seq(length(df$Freq))
        df$Colour[df$Rank <= 3] <- "blue"
        df$Colour[df$Rank > 3] <- "black"
        df2 <- merge(df,df1,by.x = "column",by.y = "column")
        return(df2)
      }
      rankfreq <- function(column){
        df <- as.data.frame(sort(table(column),decreasing = T))
        df$Rank <- seq(length(df$Freq))
        df$Colour[df$Rank <= 10] <- "blue"
        df$Colour[df$Rank > 10] <- "black"
        plot(df$Rank,df$Freq,log='xy',type='b',col = df$Colour,xlab = "Rank",ylab = "Frequency",xaxt = 'n',cex.lab=0.75)
        axis(1, at=1:length(levels(df$column)),labels=levels(df$column))
      }
      k <- 1
      l <- 1
      q <- 1
      metadata1 <- NULL
      metadata <- self$metadata
      data <- metadata$data
      for(i in 1:length(metadata$columns)){
        metadata1 <- rbind(metadata1,as.data.frame(metadata$columns[[i]]))
      }
      num_var <- as.character(metadata1[which(metadata1$type %in% c("discrete","continuous")),"column_name"])
      ord_var <- as.character(metadata1[which(tolower(metadata1$type) == "ordered"),"column_name"])
      cat_var <- as.character(metadata1[which(tolower(metadata1$type) == "character"),"column_name"])
      if(file.exists(path)){
        wb1<-loadWorkbook(path)
        if(!identical(num_var, character(0))){
          sheet_hist = createSheet(wb1,"Univariate-Histogram")
          sheet_box = createSheet(wb1,"Univariate-Box Plots")
        }
        if(!identical(cat_var, character(0))){
          sheet_rank = createSheet(wb1,"Univariate-Rank Frequency")
        }
        if(!identical(ord_var, character(0))){
          sheet_bar = createSheet(wb1,"Univariate-Bar Plot")
        }
        for(i in 1:ncol(data)){
          if(names(data)[i] %in% num_var){
            png("boxplot.png", height=1200, width=2000, res=250, pointsize=8)
            boxplot(data[,i],col = "blue",main = paste("Boxplot of ",names(data)[i]),xlab = names(data)[i])
            dev.off()
            addPicture("boxplot.png", sheet_box, scale = 1, startRow = q,startColumn = 5)
            res<-file.remove("boxplot.png")
            if(is.null(breaks)){
              breaks <- nclass.FD(data[,i])
            }
            png("histogram.png", height=1200, width=2000, res=250, pointsize=8)
            hist(data[,i],breaks = breaks,col = "blue",main = paste("Histogram of ",names(data)[i]),xlab = names(data)[i])
            dev.off()
            addPicture("histogram.png", sheet_hist, scale = 1, startRow = q,startColumn = 5)
            res<-file.remove("histogram.png")
            q <- q + 27
          }
          else if(names(data)[i] %in% cat_var & length(unique(data[,i])) != 1){
            png("rankfreq.png", height=1200, width=2000, res=250, pointsize=8)
            rankfreq(data[,i])
            title(paste("Rank Frequency plot of",names(data)[i]))
            dev.off()
            addPicture("rankfreq.png", sheet_rank, scale = 1, startRow = k,startColumn = 5)
            res<-file.remove("rankfreq.png")
            k <- k + 27
          }
          else if(names(data)[i] %in% ord_var & length(unique(data[,i])) != 1){
            png("barplot.png", height=1200, width=2000, res=250, pointsize=8)
            df <- bar_one(data[,i])
            barplot(table(data[,i]),col=df$Colour,main = paste("Barplot of ",names(data)[i]),xlab = names(data)[i])
            dev.off()
            addPicture("barplot.png", sheet_bar, scale = 1, startRow = l,startColumn = 5)
            res<-file.remove("barplot.png")
            l <- l + 27
          }
        }
        saveWorkbook(wb1, path)
      }
      else{
        wb1<-createWorkbook(type="xlsx")
        if(!identical(num_var, character(0))){
          sheet_hist = createSheet(wb1,"Univariate-Histogram")
          sheet_box = createSheet(wb1,"Box Plots")
        }
        if(!identical(cat_var, character(0))){
          sheet_rank = createSheet(wb1,"Univariate-Rank Frequency")
        }
        if(!identical(ord_var, character(0))){
          sheet_bar = createSheet(wb1,"Univariate-Bar")
        }
        for(i in 1:ncol(data)){
          if(names(data)[i] %in% num_var){
            png("boxplot.png", height=1200, width=2000, res=250, pointsize=8)
            boxplot(data[,i],col = "blue",main = paste("Boxplot of ",names(data)[i]),xlab = names(data)[i])
            dev.off()
            addPicture("boxplot.png", sheet_box, scale = 1, startRow = q,startColumn = 5)
            res<-file.remove("boxplot.png")
            if(is.null(breaks)){
              breaks <- nclass.FD(data[,i])
            }
            png("histogram.png", height=1200, width=2000, res=250, pointsize=8)
            hist(data[,i],breaks = breaks,col = "blue",main = paste("Histogram of ",names(data)[i]),xlab = names(data)[i])
            dev.off()
            addPicture("histogram.png", sheet_hist, scale = 1, startRow = q,startColumn = 5)
            res<-file.remove("histogram.png")
            q <- q + 27
          }
          else if(names(data)[i] %in% cat_var & length(unique(data[,i])) != 1){
            png("rankfreq.png", height=1200, width=2000, res=250, pointsize=8)
            rankfreq(data[,i])
            title(paste("Rank Frequency plot of",names(data)[i]))
            dev.off()
            addPicture("rankfreq.png", sheet_rank, scale = 1, startRow = k,startColumn = 5)
            res<-file.remove("rankfreq.png")
            k <- k + 27
          }
          else if(names(data)[i] %in% ord_var & length(unique(data[,i])) != 1){
            png("barplot.png", height=1200, width=2000, res=250, pointsize=8)
            df <- bar_one(data[,i])
            barplot(table(data[,i]),col=df$Colour,main = paste("Barplot of ",names(data)[i]),xlab = names(data)[i])
            dev.off()
            addPicture("barplot.png", sheet_bar, scale = 1, startRow = l,startColumn = 5)
            res<-file.remove("barplot.png")
            l <- l + 27
          }
        }
        saveWorkbook(wb1, path)
      }
    }
  )
)
