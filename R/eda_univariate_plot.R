eda_univariate_plot <- function(data = NULL,file_info,meta_data,wb,breaks = NULL,path = NULL){
  require(xlsx)
  wb <- paste(wb,"xlsx",sep = ".")
  start <- Sys.time()
  j <- 1
  k <- 1
  l <- 1
  q <- 1
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
  metadata1 <- NULL
  for(i in 1:length(meta_data$columns)){
    metadata1 <- rbind(metadata1,meta_data$columns[[i]])
  }
  num_var <- as.character(metadata1[which(metadata1$Type %in% c("discrete","continuous")),"Column_Names"])
  ord_var <- as.character(metadata1[which(tolower(metadata1$Type) == "ordered"),"Column_Names"])
  cat_var <- as.character(metadata1[which(tolower(metadata1$Type) == "character"),"Column_Names"])
  if(!is.null(data)){
    if(!is.null(path)){
      wbb <- paste(path,wb,sep="/")
    }
    else{
      wbb <- wb
    }
    if(file.exists(wbb)){
      wb1<-loadWorkbook(wbb)
      if(!identical(num_var, character(0))){
      sheet_hist = createSheet(wb1,"Histogram Plots")
      sheet_box = createSheet(wb1,"Box Plots")
        }
      if(!identical(cat_var, character(0))){
      sheet_rank = createSheet(wb1,"Rank Frequency Plots")
        }
      if(!identical(ord_var, character(0))){
      sheet_bar = createSheet(wb1,"Bar Plots")
      }
      for(i in 1:ncol(data)){
        if(names(data)[i] %in% num_var){
          png("boxplot.png", height=1200, width=2000, res=250, pointsize=8)
          boxplot(data[,i],col = "blue",main = paste("Boxplot of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("boxplot.png", sheet_box, scale = 1, startRow = 4,startColumn = j)
          res<-file.remove("boxplot.png")
          if(is.null(breaks)){
            breaks <- nclass.FD(data[,i])
          }
          png("histogram.png", height=1200, width=2000, res=250, pointsize=8)
          hist(data[,i],breaks = breaks,col = "blue",main = paste("Histogram of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("histogram.png", sheet_hist, scale = 1, startRow = q,startColumn = 5)
          res<-file.remove("histogram.png")
          j <- j + 15
          q <- q + 27
        }
        else if(names(data)[i] %in% cat_var & length(unique(data[,i])) != 1){
          png("rankfreq.png", height=1200, width=2000, res=250, pointsize=8)
          rankfreq(data[,i])
          title(paste("Rank Frequency plot of",names(data)[i]))
          dev.off()
          addPicture("rankfreq.png", sheet_rank, scale = 1, startRow = 4,startColumn = k)
          res<-file.remove("rankfreq.png")
          k <- k + 15
        }
        else if(names(data)[i] %in% ord_var & length(unique(data[,i])) != 1){
          png("barplot.png", height=1200, width=2000, res=250, pointsize=8)
          df <- bar_one(data[,i])
          barplot(table(data[,i]),col=df$Colour,main = paste("Barplot of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("barplot.png", sheet_bar, scale = 1, startRow = 4,startColumn = l)
          res<-file.remove("barplot.png")
          l <- l + 15
        }
      }
      saveWorkbook(wb1, wbb)
    }
    else{
      wb1<-createWorkbook(type="xlsx")
      if(!identical(num_var, character(0))){
      sheet_hist = createSheet(wb1,"Histogram Plots")
      sheet_box = createSheet(wb1,"Box Plots")
        }
      if(!identical(cat_var, character(0))){
      sheet_rank = createSheet(wb1,"Rank Frequency Plots")
        }
      if(!identical(ord_var, character(0))){
      sheet_bar = createSheet(wb1,"Bar Plots")
      }
      for(i in 1:ncol(data)){
        if(names(data)[i] %in% num_var){
          png("boxplot.png", height=1200, width=2000, res=250, pointsize=8)
          boxplot(data[,i],col = "blue",main = paste("Boxplot of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("boxplot.png", sheet_box, scale = 1, startRow = 4,startColumn = j)
          res<-file.remove("boxplot.png")
          if(is.null(breaks)){
            breaks <- nclass.FD(data[,i])
          }
          png("histogram.png", height=1200, width=2000, res=250, pointsize=8)
          hist(data[,i],breaks = breaks,col = "blue",main = paste("Histogram of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("histogram.png", sheet_hist, scale = 1, startRow = q,startColumn = 5)
          res<-file.remove("histogram.png")
          j <- j + 15
          q <- q + 27
        }
        else if(names(data)[i] %in% cat_var & length(unique(data[,i])) != 1){
          png("rankfreq.png", height=1200, width=2000, res=250, pointsize=8)
          rankfreq(data[,i])
          title(paste("Rank Frequency plot of",names(data)[i]))
          dev.off()
          addPicture("rankfreq.png", sheet_rank, scale = 1, startRow = 4,startColumn = k)
          res<-file.remove("rankfreq.png")
          k <- k + 15
        }
        else if(names(data)[i] %in% ord_var & length(unique(data[,i])) != 1){
          png("barplot.png", height=1200, width=2000, res=250, pointsize=8)
          df <- bar_one(data[,i])
          barplot(table(data[,i]),col=df$Colour,main = paste("Barplot of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("barplot.png", sheet_bar, scale = 1, startRow = 4,startColumn = l)
          res<-file.remove("barplot.png")
          l <- l + 15
        }
      }
      saveWorkbook(wb1, wbb)
    }
  }
  else if(is.null(data) & !is.null(file_info$data)){
    data <- file_info$data
    if(!is.null(path)){
      wbb <- paste(path,wb,sep="/")
    }
    else{
      wbb <- wb
    }
    if(file.exists(wbb)){
      wb1<-loadWorkbook(wbb)
      if(!identical(num_var, character(0))){
      sheet_hist = createSheet(wb1,"Histogram Plots")
      sheet_box = createSheet(wb1,"Box Plots")
        }
      if(!identical(cat_var, character(0))){
      sheet_rank = createSheet(wb1,"Rank Frequency Plots")
        }
      if(!identical(ord_var, character(0))){
      sheet_bar = createSheet(wb1,"Bar Plots")
      }
      for(i in 1:ncol(data)){
        if(names(data)[i] %in% num_var){
          png("boxplot.png", height=1200, width=2000, res=250, pointsize=8)
          boxplot(data[,i],col = "blue",main = paste("Boxplot of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("boxplot.png", sheet_box, scale = 1, startRow = 4,startColumn = j)
          res<-file.remove("boxplot.png")
          if(is.null(breaks)){
            breaks <- nclass.FD(data[,i])
          }
          png("histogram.png", height=1200, width=2000, res=250, pointsize=8)
          hist(data[,i],breaks = breaks,col = "blue",main = paste("Histogram of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("histogram.png", sheet_hist, scale = 1, startRow = q,startColumn = 5)
          res<-file.remove("histogram.png")
          j <- j + 15
          q <- q + 27
        }
        else if(names(data)[i] %in% cat_var & length(unique(data[,i])) != 1){
          png("rankfreq.png", height=1200, width=2000, res=250, pointsize=8)
          rankfreq(data[,i])
          title(paste("Rank Frequency plot of",names(data)[i]))
          dev.off()
          addPicture("rankfreq.png", sheet_rank, scale = 1, startRow = 4,startColumn = k)
          res<-file.remove("rankfreq.png")
          k <- k + 15
        }
        else if(names(data)[i] %in% ord_var & length(unique(data[,i])) != 1){
          png("barplot.png", height=1200, width=2000, res=250, pointsize=8)
          df <- bar_one(data[,i])
          barplot(table(data[,i]),col=df$Colour,main = paste("Barplot of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("barplot.png", sheet_bar, scale = 1, startRow = 4,startColumn = l)
          res<-file.remove("barplot.png")
          l <- l + 15
        }
      }
      saveWorkbook(wb1, wbb)
    }
    else{
      wb1<-createWorkbook(type="xlsx")
      if(!identical(num_var, character(0))){
      sheet_hist = createSheet(wb1,"Histogram Plots")
      sheet_box = createSheet(wb1,"Box Plots")
        }
      if(!identical(cat_var, character(0))){
      sheet_rank = createSheet(wb1,"Rank Frequency Plots")
        }
      if(!identical(ord_var, character(0))){
      sheet_bar = createSheet(wb1,"Bar Plots")
      }
      for(i in 1:ncol(data)){
        if(names(data)[i] %in% num_var){
          png("boxplot.png", height=1200, width=2000, res=250, pointsize=8)
          boxplot(data[,i],col = "blue",main = paste("Boxplot of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("boxplot.png", sheet_box, scale = 1, startRow = 4,startColumn = j)
          res<-file.remove("boxplot.png")
          if(is.null(breaks)){
            breaks <- nclass.FD(data[,i])
          }
          png("histogram.png", height=1200, width=2000, res=250, pointsize=8)
          hist(data[,i],breaks = breaks,col = "blue",main = paste("Histogram of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("histogram.png", sheet_hist, scale = 1,startRow = q,startColumn = 5)
          res<-file.remove("histogram.png")
          j <- j + 15
          q <- q + 27
        }
        else if(names(data)[i] %in% cat_var & length(unique(data[,i])) != 1){
          png("rankfreq.png", height=1200, width=2000, res=250, pointsize=8)
          rankfreq(data[,i])
          title(paste("Rank Frequency plot of",names(data)[i]))
          dev.off()
          addPicture("rankfreq.png", sheet_rank, scale = 1, startRow = 4,startColumn = k)
          res<-file.remove("rankfreq.png")
          k <- k + 15
        }
        else if(names(data)[i] %in% ord_var & length(unique(data[,i])) != 1){
          png("barplot.png", height=1200, width=2000, res=250, pointsize=8)
          df <- bar_one(data[,i])
          barplot(table(data[,i]),col=df$Colour,main = paste("Barplot of ",names(data)[i]),xlab = names(data)[i])
          dev.off()
          addPicture("barplot.png", sheet_bar, scale = 1, startRow = 4,startColumn = l)
          res<-file.remove("barplot.png")
          l <- l + 15
        }
      }
      saveWorkbook(wb1, wbb)
    }
  }
  end <- Sys.time()
  time <- end-start
  print(paste("Took",time,"minutes"))
}
