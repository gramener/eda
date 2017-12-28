eda_metadata <- function(path = NULL,data = NULL,header =T,sep = ",",skip = 0){
  require(tools)
  require(readr)
  require(data.table)
  require(openxlsx)
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
  top_levels <- function(col){
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
  date_eda <- function(var){
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
  if(!is.null(path)){
  name <- basename(path)
  ext<-file_ext(path)
  a <- guess_encoding(path, n_max = 1000)
  if(identical(a$encoding, character(0))){
    encod <- ""
  }
  else{
    encod<- a$encoding[1]
  }
  b <- file.info(path)
  if(ext == "csv"){
    data <- as.data.frame(fread(path,header = header,skip=skip,sep = sep,stringsAsFactors = F ))
    row.names(data) <- NULL
    rows<-nrow(data)
    cols <- ncol(data)
  }
  else if(ext == "xlsx"){
    data <- as.data.frame(read.xlsx(path,1))
    row.names(data) <- NULL
    rows<-nrow(data)
    cols <- ncol(data)
  }
  if(ext == "txt"){
    data <- as.data.frame(fread(path,header = header,skip=skip,sep = sep,stringsAsFactors = F ))
    row.names(data) <- NULL
    rows<-nrow(data)
    cols <- ncol(data)
  }
  size <- b$size
  mod_time <- b$mtime
  file_info <- list("file_info" = data.frame("file name"=name,"file ext" = ext,"file encoding" = encod,"file size" = paste(size,"bytes",sep = " "),"number of rows"= rows,"number of columns"= cols,"last modified"= mod_time),"data" = data)
  data <- as.data.frame(file_info$data)
  row.names(data) <- NULL
  mylist.names <- c("File_name","Description","Source","Format","File_size","Encoding","Row_count","Column_count","Row_description","Sampling_method","Prepared_on","Modified_on","Prepared_by","columns","data","save")
  metadata <- vector("list", length(mylist.names))
  names(metadata) <- mylist.names
  metadata$columns <- vector("list", length(names(data)))
  names(metadata$columns) <- names(data)
  mydataframe <- c("Column_Names","Type","Key","Description","Missing","Missing_percentage","Uniques","Top","Min","Q1","Mean","Median","Q3","Max","Std")
  metadata$Description <- ""
  metadata$Source <- ""
  metadata$Row_description <- ""
  metadata$Sampling_method <- ""
  metadata$Prepared_by <- ""
  metadata$Prepared_on <- as.character(Sys.Date())
  metadata$Format <- as.character(file_info$file_info$file.ext)
  metadata$File_name <- as.character(file_info$file_info$file.name)
  metadata$File_size <- as.character(file_info$file_info$file.size)
  metadata$Encoding <- as.character(file_info$file_info$file.encoding)
  metadata$Row_count <- file_info$file_info$number.of.rows
  metadata$Column_count <- file_info$file_info$number.of.columns
  metadata$Modified_on <- as.character(file_info$file_info$last.modified)
  metadata$data <- data
  metadata$save <- function(path ,sheet= "Metadata"){
    require(xlsx)
    metadata1 <- NULL
    meta_data <- metadata
    for(i in 1:length(meta_data$columns)){
      metadata1 <- rbind(metadata1,meta_data$columns[[i]])
    }
    names(metadata1) <- gsub("_"," ",names(metadata1))
    names(meta_data) <- gsub("_"," ",names(meta_data))
    meta_data$metadata <- metadata1
    if(!file.exists(path)){
      wb1 = createWorkbook()
      sheet = createSheet(wb1,sheet)
      csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
      addDataFrame(meta_data$metadata, sheet=sheet,startRow=(length(meta_data)+4),startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
      addDataFrame(data.frame(names(meta_data)[1:(length(meta_data)-4)],as.character(meta_data[1:(length(meta_data)-4)])), sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,col.names = FALSE)
      setColumnWidth(sheet, colIndex=1:ncol(meta_data$metadata), colWidth=20)
      saveWorkbook(wb1, path)
    }
    else{
      wb1<-loadWorkbook(path)
      sheet = createSheet(wb1,sheet)
      csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
      addDataFrame(meta_data$metadata, sheet=sheet,startRow=(length(meta_data)+4),startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
      addDataFrame(data.frame(names(meta_data)[1:(length(meta_data)-4)],as.character(meta_data[1:(length(meta_data)-4)])), sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,col.names = FALSE)
      setColumnWidth(sheet, colIndex=1:ncol(meta_data$metadata), colWidth=20)
      saveWorkbook(wb1, path)
    }
  }
  for(i in 1:length(metadata$columns)){
    metadata$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
    a1 <- names(data)[i]
    if(date_eda(data[,i]) & (names(data)[i] %in% varlist(data,"factor") | names(data)[i] %in% varlist(data,"character"))){
      a2 <- "date"
    }
    else if(names(data)[i] %in% varlist(data,"factor") | names(data)[i] %in% varlist(data,"character")){
      a2 <- "character"
    }
    else if(names(data)[i] %in% varlist(data,"numeric")){
      for(j in 1:100){
        if(grepl("\\.",data[j,i])){
          a2 <- "continuous"
        }
        else{
          a2<- "discrete"
        }
      }
    }
    else if(is.logical(data[,i])){
      a2 <- "boolean"
    }
    a3 <- ifelse((length(unique(data[,i]))/nrow(data)) == 1,"yes","no")
    a4 <- NA
    a5 <- sum(is.na(data[,i]))
    a16 <- paste(round((a5*100/nrow(data)),1),"%")
    a6 <- length(unique(data[,i]))
    a7 <- top_levels(data[,i])
    if(a2 %in% c("continuous","discrete")){
      a8 <- min(data[,i])
      a9 <- quantile(data[,i],0.25,na.rm=T)
      a10 <- ifelse(mean(data[,i],na.rm=T) < 1,round(mean(data[,i],na.rm=T),2),round(mean(data[,i],na.rm=T)))
      a11 <- median(data[,i],na.rm=T)
      a12 <- quantile(data[,i],0.75,na.rm=T)
      a13 <- max(data[,i])
      a14 <- ifelse(sd(data[,i],na.rm=T) < 1,round(sd(data[,i],na.rm=T),2),round(sd(data[,i],na.rm=T)))
    }
    else{
      a8 <- ""
      a9 <- ""
      a10 <- ""
      a11 <- ""
      a12 <- ""
      a13 <- ""
      a14 <- ""
    }
    metadata$columns[[i]] <- rbind(metadata$columns[[i]],data.frame("Column_Names"=a1,"Type"=a2,"Key"=a3,"Description"=a4,"Missing"=a5,"Missing_percentage" = a16,"Uniques"=a6,"Top"=a7,"Min"=a8,"Q1"=a9,"Mean"=a10,"Median"=a11,"Q3"=a12,"Max"=a13,"Std"=a14))
    row.names(metadata$columns[[i]]) <- NULL
  }
  return(metadata)
  }
  else if(is.null(path) & !is.null(data)){
    mylist.names <- c("File_name","Description","Source","Format","File_size","Encoding","Row_count","Column_count","Row_description","Sampling_method","Prepared_on","Modified_on","Prepared_by","columns","data","save")
    metadata <- vector("list", length(mylist.names))
    names(metadata) <- mylist.names
    metadata$columns <- vector("list", length(names(data)))
    names(metadata$columns) <- names(data)
    mydataframe <- c("Column_Names","Type","Key","Description","Missing","Missing_percentage","Uniques","Top","Min","Q1","Mean","Median","Q3","Max","Std")
    metadata$Description <- ""
    metadata$Source <- ""
    metadata$Row_description <- ""
    metadata$Sampling_method <- ""
    metadata$Prepared_by <- ""
    metadata$Prepared_on <- as.character(Sys.Date())
    metadata$Format <- ""
    metadata$File_name <- ""
    metadata$File_size <- ""
    metadata$Encoding <- ""
    metadata$Row_count <- nrow(data)
    metadata$Column_count <- ncol(data)
    metadata$Modified_on <- ""
    metadata$data <- data
    metadata$save <- function(path ,sheet= "Metadata"){
      require(xlsx)
      metadata1 <- NULL
      meta_data <- metadata
      for(i in 1:length(meta_data$columns)){
        metadata1 <- rbind(metadata1,meta_data$columns[[i]])
      }
      names(metadata1) <- gsub("_"," ",names(metadata1))
      names(meta_data) <- gsub("_"," ",names(meta_data))
      meta_data$metadata <- metadata1
      if(!file.exists(path)){
        wb1 = createWorkbook()
        sheet = createSheet(wb1,sheet)
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        addDataFrame(meta_data$metadata, sheet=sheet,startRow=(length(meta_data)+4),startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        addDataFrame(data.frame(names(meta_data)[1:(length(meta_data)-4)],as.character(meta_data[1:(length(meta_data)-4)])), sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,col.names = FALSE)
        setColumnWidth(sheet, colIndex=1:ncol(meta_data$metadata), colWidth=20)
        saveWorkbook(wb1, path)
      }
      else{
        wb1<-loadWorkbook(path)
        sheet = createSheet(wb1,sheet)
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        addDataFrame(meta_data$metadata, sheet=sheet,startRow=(length(meta_data)+4),startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        addDataFrame(data.frame(names(meta_data)[1:(length(meta_data)-4)],as.character(meta_data[1:(length(meta_data)-4)])), sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,col.names = FALSE)
        setColumnWidth(sheet, colIndex=1:ncol(meta_data$metadata), colWidth=20)
        saveWorkbook(wb1, path)
      }
    }
    for(i in 1:length(metadata$columns)){
      metadata$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
      a1 <- names(data)[i]
      if(date_eda(data[,i]) & (names(data)[i] %in% varlist(data,"factor") | names(data)[i] %in% varlist(data,"character"))){
        a2 <- "date"
      }
      else if(names(data)[i] %in% varlist(data,"factor") | names(data)[i] %in% varlist(data,"character")){
        a2 <- "character"
      }
      else if(names(data)[i] %in% varlist(data,"numeric")){
        for(j in 1:100){
          if(grepl("\\.",data[j,i])){
            a2 <- "continuous"
          }
          else{
            a2<- "discrete"
          }
        }
      }
      else if(is.logical(data[,i])){
        a2 <- "boolean"
      }
      a3 <- ifelse((length(unique(data[,i]))/nrow(data)) == 1,"yes","no")
      a4 <- NA
      a5 <- sum(is.na(data[,i]))
      a16 <- paste(round((a5*100/nrow(data)),1),"%")
      a6 <- length(unique(data[,i]))
      a7 <- top_levels(data[,i])
      if(a2 %in% c("continuous","discrete")){
        a8 <- min(data[,i])
        a9 <- quantile(data[,i],0.25,na.rm=T)
        a10 <- ifelse(mean(data[,i],na.rm=T) < 1,round(mean(data[,i],na.rm=T),2),round(mean(data[,i],na.rm=T)))
        a11 <- median(data[,i],na.rm=T)
        a12 <- quantile(data[,i],0.75,na.rm=T)
        a13 <- max(data[,i])
        a14 <- ifelse(sd(data[,i],na.rm=T) < 1,round(sd(data[,i],na.rm=T),2),round(sd(data[,i],na.rm=T)))
      }
      else{
        a8 <- ""
        a9 <- ""
        a10 <- ""
        a11 <- ""
        a12 <- ""
        a13 <- ""
        a14 <- ""
      }
      metadata$columns[[i]] <- rbind(metadata$columns[[i]],data.frame("Column_Names"=a1,"Type"=a2,"Key"=a3,"Description"=a4,"Missing"=a5,"Missing_percentage" = a16,"Uniques"=a6,"Top"=a7,"Min"=a8,"Q1"=a9,"Mean"=a10,"Median"=a11,"Q3"=a12,"Max"=a13,"Std"=a14))
      row.names(metadata$columns[[i]]) <- NULL
    }
    return(metadata)
  }
}
