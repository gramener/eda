eda_univariate_plot <- function(dataset,wb,breaks =10){
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
  rankfreq <- function(column){
    df <- as.data.frame(sort(table(column),decreasing = T))
    df$Rank <- seq(length(df$Freq))
    df$Colour[df$Rank <= 10] <- "blue"
    df$Colour[df$Rank > 10] <- "black"
    plot(df$Rank,df$Freq,log='xy',type='b',col = df$Colour,main= "Rank Frequency plot",xlab = "Rank",ylab = "Frequency",xaxt = 'n')
    axis(1, at=1:length(levels(column)),labels=levels(column))
  }
  if(file.exists(wb)){
    wb1<-loadWorkbook(wb)
    for(i in 1:ncol(dataset)){
      if(names(dataset)[i] %in% varlist(dataset,"numeric")){
        sheet = createSheet(wb1,names(dataset)[i])
        png("boxplot.png", height=1200, width=1200, res=250, pointsize=8)
        boxplot(dataset[,i],col = "blue")
        dev.off()
        addPicture("boxplot.png", sheet, scale = 1, startRow = 4,startColumn = 1)
        res<-file.remove("boxplot.png")
        png("histogram.png", height=1200, width=1200, res=250, pointsize=8)
        hist(dataset[,i],breaks = breaks,main = paste("Histogram of ",names(dataset)[i]),xlab = names(dataset)[i])
        dev.off()
        addPicture("histogram.png", sheet, scale = 1, startRow = 4,startColumn = 20)
        res<-file.remove("histogram.png")
      }
      else if(names(dataset)[i] %in% varlist(dataset,"factor")){
        png("rankfreq.png", height=1200, width=1200, res=250, pointsize=8)
        rankfreq(dataset[,i])
        dev.off()
        sheet = createSheet(wb1,names(dataset)[i])
        addPicture("rankfreq.png", sheet, scale = 1, startRow = 4,startColumn = 1)
        res<-file.remove("rankfreq.png")
        png("barplot.png", height=1200, width=1200, res=250, pointsize=8)
        barplot(table(dataset[,i]))
        dev.off()
        addPicture("barplot.png", sheet, scale = 1, startRow = 4,startColumn = 20)
        res<-file.remove("barplot.png")
        
      }
    }
    saveWorkbook(wb1, wb)
  }
  else{
    wb1<-createWorkbook(type="xlsx")
    for(i in 1:ncol(dataset)){
      if(names(dataset)[i] %in% varlist(dataset,"numeric")){
        sheet = createSheet(wb1,names(dataset)[i])
        png("boxplot.png", height=1200, width=1200, res=250, pointsize=8)
        boxplot(dataset[,i],col = "blue",main = paste("Boxplot of ",names(dataset)[i]),xlab = names(dataset)[i],cex.lab=0.75)
        dev.off()
        addPicture("boxplot.png", sheet, scale = 1, startRow = 4,startColumn = 1)
        res<-file.remove("boxplot.png")
        png("histogram.png", height=1200, width=1200, res=250, pointsize=8)
        hist(dataset[,i],breaks = breaks,col = "blue",main = paste("Histogram of ",names(dataset)[i]),xlab = names(dataset)[i],cex.lab=0.75)
        dev.off()
        addPicture("histogram.png", sheet, scale = 1, startRow = 4,startColumn = 20)
        res<-file.remove("histogram.png")
      }
      else if(names(dataset)[i] %in% varlist(dataset,"factor")){
        png("rankfreq.png", height=1200, width=1200, res=250, pointsize=8)
        rankfreq(dataset[,i])
        dev.off()
        sheet = createSheet(wb1,names(dataset)[i])
        addPicture("rankfreq.png", sheet, scale = 1, startRow = 4,startColumn = 1)
        res<-file.remove("rankfreq.png")
        png("barplot.png", height=1200, width=1200, res=250, pointsize=8)
        barplot(table(dataset[,i]),col="blue",main = paste("Barplot of ",names(dataset)[i]),xlab = names(dataset)[i],cex.lab=0.75)
        dev.off()
        addPicture("barplot.png", sheet, scale = 1, startRow = 4,startColumn = 20)
        res<-file.remove("barplot.png")
        
      }
    }
    saveWorkbook(wb1, wb)
  }
}

