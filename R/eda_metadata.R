library(R6)
eda_metadata <- R6Class(
  "Eda Metadata",
  public = list(
    Description = "",
    Source = "",
    Row_description = "",
    Sampling_method = "",
    Prepared_by = "",
    Prepared_on = "",
    Format = "",
    File_name = "",
    File_size = "",
    Encoding = "",
    Row_count = 0,
    Column_count = 0,
    Modified_on = "",
    columns = list(),
    data = data.frame(),
    initialize = function(path = NULL,data = NULL,header =T,sep = ",",skip = 0) {
      options(scipen = 999)
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
      if(!missing(path))
      {
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
        self$columns <- vector("list", length(names(data)))
        names(self$columns) <- names(data)
        mydataframe <- c("Column_Name","Type","Key","Description","Missing","Missing_percentage","Uniques","Top","Min","Q1","Mean","Median","Q3","Max","Std")
        self$Description <- ""
        self$Source <- path
        self$Row_description <- ""
        self$Sampling_method <- ""
        self$Prepared_by <- ""
        self$Prepared_on <- as.character(Sys.Date())
        self$Format <- as.character(file_info$file_info$file.ext)
        self$File_name <- as.character(file_info$file_info$file.name)
        self$File_size <- as.character(file_info$file_info$file.size)
        self$Encoding <- as.character(file_info$file_info$file.encoding)
        self$Row_count <- file_info$file_info$number.of.rows
        self$Column_count <- file_info$file_info$number.of.columns
        self$Modified_on <- as.character(file_info$file_info$last.modified)
        self$data <- data
        for(i in 1:length(self$columns)){
          self$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
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
          a4 <- ""
          a5 <- sum(is.na(data[,i]))
          a16 <- paste(round((a5*100/nrow(data)),1),"%")
          a6 <- length(unique(data[,i]))
          a7 <- top_levels(data[,i])
          if(a2 %in% c("continuous","discrete")){
            a8 <- as.double(min(data[,i],na.rm = T))
            a9 <- as.double(quantile(data[,i],0.25,na.rm=T))
            a10 <- ifelse(as.double(mean(data[,i],na.rm=T)) < 1,as.double(round(mean(data[,i],na.rm=T),2)),as.double(round(mean(data[,i],na.rm=T))))
            a11 <- as.double(median(data[,i],na.rm=T))
            a12 <- as.double(quantile(data[,i],0.75,na.rm=T))
            a13 <- as.double(max(data[,i],na.rm = T))
            a14 <- ifelse(as.double(sd(data[,i],na.rm=T)) < 1,as.double(round(sd(data[,i],na.rm=T),2)),as.double(round(sd(data[,i],na.rm=T))))
          }
          else{
            a8 <- NA
            a9 <- NA
            a10 <- NA
            a11 <- NA
            a12 <- NA
            a13 <- NA
            a14 <- NA
          }
          self$columns[[i]] <- rbind(self$columns[[i]],data.frame("Column_Name"=a1,"Type"= a2,"Key"=a3,"Description"=a4,"Missing"=a5,"Missing_percentage" = a16,"Uniques"=a6,"Top"=a7,"Min"=a8,"Q1"=a9,"Mean"=a10,"Median"=a11,"Q3"=a12,"Max"=a13,"Std"=a14))
          row.names(self$columns[[i]]) <- NULL
        }
      }
      else if(missing(path) & !missing(data)){
        row.names(data) <- NULL
        self$columns <- vector("list", length(names(data)))
        names(self$columns) <- names(data)
        mydataframe <- c("Column_Name","Type","Key","Description","Missing","Missing_percentage","Uniques","Top","Min","Q1","Mean","Median","Q3","Max","Std")
        C
        self$data <- data
        for(i in 1:length(self$columns)){
          self$columns[[i]] <- setNames(data.frame(matrix(ncol = length(mydataframe), nrow = 0)), mydataframe)
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
          a4 <- ""
          a5 <- sum(is.na(data[,i]))
          a16 <- paste(round((a5*100/nrow(data)),1),"%")
          a6 <- length(unique(data[,i]))
          a7 <- top_levels(data[,i])
          if(a2 %in% c("continuous","discrete")){
            a8 <- as.double(min(data[,i],na.rm = T))
            a9 <- as.double(quantile(data[,i],0.25,na.rm=T))
            a10 <- ifelse(as.double(mean(data[,i],na.rm=T)) < 1,as.double(round(mean(data[,i],na.rm=T),2)),as.double(round(mean(data[,i],na.rm=T))))
            a11 <- as.double(median(data[,i],na.rm=T))
            a12 <- as.double(quantile(data[,i],0.75,na.rm=T))
            a13 <- as.double(max(data[,i],na.rm = T))
            a14 <- ifelse(as.double(sd(data[,i],na.rm=T)) < 1,as.double(round(sd(data[,i],na.rm=T),2)),as.double(round(sd(data[,i],na.rm=T))))
          }
          else{
            a8 <- NA
            a9 <- NA
            a10 <- NA
            a11 <- NA
            a12 <- NA
            a13 <- NA
            a14 <- NA
          }
          self$columns[[i]] <- rbind(self$columns[[i]],data.frame("Column_Name"=a1,"Type"= a2,"Key"=a3,"Description"=a4,"Missing"=a5,"Missing_percentage" = a16,"Uniques"=a6,"Top"=a7,"Min"=a8,"Q1"=a9,"Mean"=a10,"Median"=a11,"Q3"=a12,"Max"=a13,"Std"=a14))
          row.names(self$columns[[i]]) <- NULL
        }
      }
    },
    Save = function(path ,sheet= "Metadata"){
      require(xlsx)
      metadata <- NULL
      for(i in 1:length(self$columns)){
        metadata <- rbind(metadata,self$columns[[i]])
      }
      names(metadata) <- gsub("_"," ",names(metadata))
      meta_data <- list(Description = self$Description,
                        Source = self$Source,
                        Row_description = self$Row_description,
                        Sampling_method = self$Sampling_method,
                        Prepared_by = self$Prepared_by,
                        Prepared_on = self$Prepared_on,
                        Format = self$Format,
                        File_name = self$File_name,
                        File_size = self$File_size,
                        Encoding = self$Encoding,
                        Row_count = self$Row_count,
                        Column_count = self$Column_count,
                        Modified_on = self$Modified_on)
      names(meta_data) <- gsub("_"," ",names(meta_data))
      if(!file.exists(path)){
        wb1 = createWorkbook()
        sheet = createSheet(wb1,sheet)
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        addDataFrame(metadata, sheet=sheet,startRow=(length(metadata)+4),startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        addDataFrame(data.frame(names(meta_data),as.character(meta_data)), sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,col.names = FALSE)
        setColumnWidth(sheet, colIndex=1:ncol(metadata), colWidth=20)
        saveWorkbook(wb1, path)
      }
      else{
        wb1<-loadWorkbook(path)
        sheet = createSheet(wb1,sheet)
        csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
        addDataFrame(metadata, sheet=sheet,startRow=(length(metadata)+4),startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
        addDataFrame(data.frame(names(meta_data),as.character(meta_data)), sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,col.names = FALSE)
        setColumnWidth(sheet, colIndex=1:ncol(metadata), colWidth=20)
        saveWorkbook(wb1, path)
      }
    }
  )
)
