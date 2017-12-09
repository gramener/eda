.varlist <- function (df=NULL,type=c("numeric","factor","character"), pattern="", exclude=NULL) {
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
.top_levels <- function(col){
  if(!all(is.na(col))){
    lev <- c()
    tab <- as.data.frame(table(col))
    tab_sort <- head(tab[order(tab[,2],decreasing =T),],5)
    tab_sort$abc <- paste(as.character(tab_sort[,1]),tab_sort[,2],sep=":")
    abc <- ""
    for(j in 1:nrow(tab_sort)){
      abc <- paste(abc,tab_sort[j,3],sep = " , ")
    }
    lev <- substring(abc,3)
    return(lev)
  }
  else{
    return("")
  }
}
.date <- function(var){
  require(lubridate)
  x <- c('February 20th 1973',
         "february  14, 2004",
         "Sunday, May 1, 2000",
         "Sunday, May 1, 2000",
         "february  14, 04",
         'Feb 20th 73',
         "January 5 1999 at 7pm",
         "jan 3 2010",
         "Jan 1, 1999",
         "jan 3   10",
         "01 3 2010",
         "1 3 10",
         '1 13 89',
         "5/27/1979",
         "12/31/99",
         "DOB:12/11/00",
         'Thu, 1 July 2004 22:30:00',
         'Thu, 1st of July 2004 at 22:30:00',
         'Thu, 1July 2004 at 22:30:00',
         'Thu, 1July2004 22:30:00',
         'Thu, 1July04 22:30:00',
         "21 Aug 2011, 11:15:34 pm",
         "1979-05-27",
         "79-05-27" ,
         "2-5-1979",
         "7-5-79",
         '00-13-10',
         "3 jan 2000",
         "17 april 85",
         "27/5/1979",
         '20 01 89',
         '00/13/10',
         "14 12 00")
  abc<-guess_formats(x,c("mdY", "BdY", "Bdy", "bdY", "bdy","d/m/Y","d/m/y","y-m-d","Y-m-d","d-m-Y","d-m-Y"))
  return(!all(is.na(as.Date(as.character(var),format=abc))))
}

.dist <- function(var){
  a<-cumsum(as.numeric(var))
  b<- a/sum(as.numeric(var))
  c<- length(which(b<=0.8))
  d<- c/length(var)
  return(d)
}

.bar_one <- function(column){
  df1 <- as.data.frame(table(column))
  df <- as.data.frame(sort(table(column),decreasing = T))
  df$Rank <- seq(length(df$Freq))
  df$Colour[df$Rank <= 3] <- "blue"
  df$Colour[df$Rank > 3] <- "black"
  df2 <- merge(df,df1,by.x = "column",by.y = "column")
  return(df2)
}
.rankfreq <- function(column){
  df <- as.data.frame(sort(table(column),decreasing = T))
  df$Rank <- seq(length(df$Freq))
  df$Colour[df$Rank <= 10] <- "blue"
  df$Colour[df$Rank > 10] <- "black"
  plot(df$Rank,df$Freq,log='xy',type='b',col = df$Colour,xlab = "Rank",ylab = "Frequency",xaxt = 'n',cex.lab=0.75)
  axis(1, at=1:length(levels(df$column)),labels=levels(df$column))
}
