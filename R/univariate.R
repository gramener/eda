library(R6)
univariate <- R6Class(
  "Univariate Analysis",
  portable = FALSE,
  cloneable = FALSE,
  public = list(
    columns = list(),
    metadata = list(),
    ngrams = list(),
    initialize = function(metadata,k = 3) {
      options(scipen = 999)
      dist <- function(var){
        a<-cumsum(as.numeric(var))
        b<- a/sum(as.numeric(var))
        c<- length(which(b<=0.8))
        d<- c/length(var)
        return(d)
      }
      findNGrams <- function(df, n) {
        ngram <- NGramTokenizer(df, Weka_control(min = n, max = n, delimiters = " \\r\\n\\t.,;:\"()?!"))
        ngram2 <- data.frame(table(ngram))
        ngram3 <- ngram2[order(ngram2$Freq, decreasing = TRUE),]
        colnames(ngram3) <- c("Text","Count")
        head(ngram3,10)
      }
      if(!missing(metadata))
      {
        self$metadata <- as.list.environment(metadata)
        metadata <- as.list.environment(metadata)
        data <- metadata$data
        metadata1 <- NULL
        for(i in 1:length(metadata$columns)){
          metadata1 <- rbind(metadata1,as.data.frame(metadata$columns[[i]]))
        }
        num_var <- as.character(metadata1[which(metadata1$type %in% c("discrete","continuous")),"column_name"])
        cat_var <- as.character(metadata1[which(metadata1$type %in% c("character","ordered")),"column_name"])
        text_var <- as.character(metadata1[which(metadata1$type %in% c("text")),"column_name"])
        self$ngrams <- vector("list", length(text_var))
        names(self$ngrams) <- text_var
        self$columns <- vector("list", length(num_var))
        names(self$columns) <- num_var
        for(q in 1:length(text_var)){
          self$ngrams[[q]] <- vector("list",4)
          names(self$ngrams[[q]]) <- c("TwoGram","ThreeGram","FourGram","FiveGram")
        }
        mydataframe <- c("column_name","upper_outliers_IQR","lower_outliers_IQR",paste("upper_outliers_",k,"sigma",sep = ""),paste("lower_outliers_",k,"sigma",sep = ""),"concentration","priority","performance","notes")
        for(i in 1:ncol(data)){
          if(names(data)[i] %in% num_var){
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
            self$columns[[i]] <- list("a1" = a1,"a4"=a4,"a5"=a5,"a8"=a8,"a9"=a9,"a10" = a13,"a11"= a10,"a12" = a11,"a13" = a12)
            names(self$columns[[i]]) <- mydataframe
            rm(a1); rm(a2); rm(a3); rm(a4); rm(a5); rm(a6); rm(a7); rm(a8); rm(a9); rm(a10); rm(a11); rm(a12); rm(a13);
          }
          else if(names(data)[i] %in% text_var){
            for(w in 1:length(text_var)){
            self$ngrams[[w]]$TwoGram <- findNGrams(data[,i], 2)
            self$ngrams[[w]]$ThreeGram <- findNGrams(data[,i], 3)
            self$ngrams[[w]]$FourGram <- findNGrams(data[,i], 4)
            self$ngrams[[w]]$FiveGram <- findNGrams(data[,i], 5)
            }
          }
        }
      }
      },
    save = function(savepath,sheet= "Univariate"){
      require(xlsx)
      unidata <- NULL
      for(i in 1:length(self$columns)){
        unidata <- rbind(unidata,as.data.frame(self$columns[[i]]))
      }
      names(unidata) <- gsub("_"," ",names(unidata))
      if(!file.exists(savepath)){
        wb1 = createWorkbook()
        sheet = createSheet(wb1,sheet)
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        addDataFrame(unidata, sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        setColumnWidth(sheet, colIndex=1:ncol(unidata), colWidth=20)
        saveWorkbook(wb1, savepath)
      }
      else{
        wb1<-loadWorkbook(savepath)
        sheet = createSheet(wb1,sheet)
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        addDataFrame(unidata, sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        setColumnWidth(sheet, colIndex=1:ncol(unidata), colWidth=20)
        saveWorkbook(wb1,savepath)
      }
    },
    saveplot = function(savepath ,breaks = NULL){
      require(xlsx)
      require(tm)
      require(SnowballC)
      require(wordcloud)
      require(RColorBrewer)
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
        axis(1, at=1:length(levels(df$column)),labels=levels(df$column),las = 2)
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
      text_var <- as.character(metadata1[which(metadata1$type %in% c("text")),"column_name"])
      if(file.exists(savepath)){
        wb1<-loadWorkbook(savepath)
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
        if(!identical(text_var, character(0))){
          sheet_wc = createSheet(wb1,"Word cloud")
        }
        for(i in 1:ncol(data)){
          if(names(data)[i] %in% num_var & all(!is.na(data[,i])) ){
            q1 <-quantile(data[,i],0.25,na.rm = T)
            q3 <- quantile(data[,i],0.75,na.rm =T )
            iqr <- q3 - q1
            a2 <- q3 + 1.5*iqr
            a3 <- q1 - 1.5*iqr
            png("boxplot.png", height=1200, width=2000, res=250, pointsize=8)
            boxplot(data[,i],col = "blue",main = paste("Boxplot of ",names(data)[i]),xlab = names(data)[i],yaxt = 'n')
            axis(side= 2, at= c(min(data[,i]),a3,q1,median(data[,i]),q3,a2,max(data[,i])), labels= c(min(data[,i]),a3,q1,median(data[,i]),q3,a2,max(data[,i])),las = 2)
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
          else if(names(data)[i] %in% cat_var & length(unique(data[,i])) != 1 & all(!is.na(data[,i]))){
            png("rankfreq.png", height=1200, width=2000, res=250, pointsize=8)
            rankfreq(data[,i])
            title(paste("Rank Frequency plot of",names(data)[i]))
            dev.off()
            addPicture("rankfreq.png", sheet_rank, scale = 1, startRow = k,startColumn = 5)
            res<-file.remove("rankfreq.png")
            k <- k + 27
          }
          else if(names(data)[i] %in% ord_var & length(unique(data[,i])) != 1 & all(!is.na(data[,i]))){
            png("barplot.png", height=1200, width=2000, res=250, pointsize=8)
            df <- bar_one(data[,i])
            barplot(table(data[,i]),col=df$Colour,main = paste("Barplot of ",names(data)[i]),xlab = names(data)[i])
            dev.off()
            addPicture("barplot.png", sheet_bar, scale = 1, startRow = l,startColumn = 5)
            res<-file.remove("barplot.png")
            l <- l + 27
          }
          else if(names(data)[i] %in% text_var & all(!is.na(data[,i]))){
            png("wordcloud.png", height=1200, width=2000, res=250, pointsize=8)
            corpus <- Corpus(VectorSource(as.character(data[,i])))
            corpus_data<-tm_map(corpus,stripWhitespace)
            corpus_data<-tm_map(corpus_data,tolower)
            corpus_data<-tm_map(corpus_data,removeNumbers)
            corpus_data<-tm_map(corpus_data,removePunctuation)
            corpus_data<-tm_map(corpus_data,removeWords, stopwords("english"))
            corpus_data<-tm_map(corpus_data,removeWords, c("and","the","our","that","for","are","also","more","has","must","have","should","this","with"))
            layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
            par(mar=rep(0, 4))
            plot.new()
            text(x=0.5, y=0.5, paste("Word cloud of",names(data)[i]))
            wordcloud(corpus_data, scale=c(4,0.5), min.freq= 5,max.words = 50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
            dev.off()
            addPicture("wordcloud.png", sheet_wc, scale = 1, startRow = l,startColumn = 5)
            res<-file.remove("wordcloud.png")
            l <- l + 27
          }
        }
        saveWorkbook(wb1, savepath)
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
        if(!identical(text_var, character(0))){
          sheet_wc = createSheet(wb1,"Word cloud")
        }
        for(i in 1:ncol(data)){
          if(names(data)[i] %in% num_var & all(!is.na(data[,i]))){
            q1 <-quantile(data[,i],0.25,na.rm = T)
            q3 <- quantile(data[,i],0.75,na.rm =T )
            iqr <- q3 - q1
            a2 <- q3 + 1.5*iqr
            a3 <- q1 - 1.5*iqr
            png("boxplot.png", height=1200, width=2000, res=250, pointsize=8)
            boxplot(data[,i],col = "blue",main = paste("Boxplot of ",names(data)[i]),xlab = names(data)[i],yaxt = 'n')
            axis(side= 2, at= c(min(data[,i]),a3,q1,median(data[,i]),q3,a2,max(data[,i])), labels= c(min(data[,i]),a3,q1,median(data[,i]),q3,a2,max(data[,i])),las = 2)
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
          else if(names(data)[i] %in% cat_var & length(unique(data[,i])) != 1 & all(!is.na(data[,i]))){
            png("rankfreq.png", height=1200, width=2000, res=250, pointsize=8)
            rankfreq(data[,i])
            title(paste("Rank Frequency plot of",names(data)[i]))
            dev.off()
            addPicture("rankfreq.png", sheet_rank, scale = 1, startRow = k,startColumn = 5)
            res<-file.remove("rankfreq.png")
            k <- k + 27
          }
          else if(names(data)[i] %in% ord_var & length(unique(data[,i])) != 1 & all(!is.na(data[,i]))){
            png("barplot.png", height=1200, width=2000, res=250, pointsize=8)
            df <- bar_one(data[,i])
            barplot(table(data[,i]),col=df$Colour,main = paste("Barplot of ",names(data)[i]),xlab = names(data)[i])
            dev.off()
            addPicture("barplot.png", sheet_bar, scale = 1, startRow = l,startColumn = 5)
            res<-file.remove("barplot.png")
            l <- l + 27
          }
          else if(names(data)[i] %in% text_var & all(!is.na(data[,i]))){
            png("wordcloud.png", height=1200, width=2000, res=250, pointsize=8)
            corpus <- Corpus(VectorSource(as.character(data[,i])))
            corpus_data<-tm_map(corpus,stripWhitespace)
            corpus_data<-tm_map(corpus_data,tolower)
            corpus_data<-tm_map(corpus_data,removeNumbers)
            corpus_data<-tm_map(corpus_data,removePunctuation)
            corpus_data<-tm_map(corpus_data,removeWords, stopwords("english"))
            corpus_data<-tm_map(corpus_data,removeWords, c("and","the","our","that","for","are","also","more","has","must","have","should","this","with"))
            layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
            par(mar=rep(0, 4))
            plot.new()
            text(x=0.5, y=0.5, paste("Word cloud of",names(data)[i]))
            wordcloud(corpus_data, scale=c(4,0.5), min.freq= 5,max.words = 50, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
            dev.off()
            addPicture("wordcloud.png", sheet_wc, scale = 1, startRow = l,startColumn = 5)
            res<-file.remove("wordcloud.png")
            l <- l + 27
          }
        }
        saveWorkbook(wb1, savepath)
      }
    },
    output = function(){
      unidata <- NULL
      for(i in 1:length(self$columns)){
        unidata <- rbind(unidata,as.data.frame(self$columns[[i]]))
      }
      return(unidata)
    }
  )
)
