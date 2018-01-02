library(R6)
eda_metadata <- R6Class(
  "Metadata",
  public = list(
    description = "",
    source = "",
    row_description = "",
    sampling_method = "",
    prepared_by = "",
    prepared_on = "",
    format = "",
    file_name = "",
    file_size = "",
    encoding = "",
    row_count = 0,
    column_count = 0,
    modified_on = "",
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
        mydataframe <- c("column_name","type","key","description","missing","missing_percentage","uniques","top","min","q1","mean","median","q3","max","std")
        self$description <- ""
        self$source <- path
        self$row_description <- ""
        self$sampling_method <- ""
        self$prepared_by <- ""
        self$prepared_on <- as.character(Sys.Date())
        self$format <- as.character(file_info$file_info$file.ext)
        self$file_name <- as.character(file_info$file_info$file.name)
        self$file_size <- as.character(file_info$file_info$file.size)
        self$encoding <- as.character(file_info$file_info$file.encoding)
        self$row_count <- file_info$file_info$number.of.rows
        self$column_count <- file_info$file_info$number.of.columns
        self$modified_on <- as.character(file_info$file_info$last.modified)
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
          self$columns[[i]] <- list("column_name"=a1,"type"= a2,"key"=a3,"description"=a4,"missing"=a5,"missing_percentage" = a16,"uniques"=a6,"top"=a7,"min"=a8,"q1"=a9,"mean"=a10,"median"=a11,"q3"=a12,"max"=a13,"std"=a14)
        }
      }
      else if(missing(path) & !missing(data)){
        row.names(data) <- NULL
        self$columns <- vector("list", length(names(data)))
        names(self$columns) <- names(data)
        mydataframe <- c("column_name","type","key","description","missing","missing_percentage","uniques","top","min","q1","mean","median","q3","max","std")
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
          self$columns[[i]] <- list("column_name"=a1,"type"= a2,"key"=a3,"description"=a4,"missing"=a5,"missing_percentage" = a16,"uniques"=a6,"top"=a7,"min"=a8,"q1"=a9,"mean"=a10,"median"=a11,"q3"=a12,"max"=a13,"std"=a14)
        }
      }
    },
    save = function(path ,sheet= "Metadata"){
      require(xlsx)
      metadata <- NULL
      for(i in 1:length(self$columns)){
        metadata <- rbind(metadata,as.data.frame(self$columns[[i]]))
      }
      names(metadata) <- gsub("_"," ",names(metadata))
      meta_data <- list(Description = self$description,
                        Source = self$source,
                        Row_description = self$row_description,
                        Sampling_method = self$sampling_method,
                        Prepared_by = self$prepared_by,
                        Prepared_on = self$prepared_on,
                        Format = self$format,
                        File_name = self$file_name,
                        File_size = self$file_size,
                        Encoding = self$encoding,
                        Row_count = self$row_count,
                        Column_count = self$column_count,
                        Modified_on = self$modified_on)
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
