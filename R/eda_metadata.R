eda_metadata <- function(data = NULL,file_info = NULL,columns = NULL){
  if(!is.null(data)){
    if(!is.null(columns)){
      data <- data[,columns]
    }
    mylist.names <- c("File_name","Description","Source","Format","File_size","Encoding","Number_of_rows","Number_of_columns","Each_row_is","Sampling_method","Prepared_On","Modified_On","columns")
    metadata <- vector("list", length(mylist.names))
    names(metadata) <- mylist.names
    metadata$columns <- vector("list", length(names(data)))
    names(metadata$columns) <- names(data)
    mydataframe <- c("Column_Names","Type","Key","Description","Missing","Missing_Percentage","Uniques","Top","Min","Q1","Mean","Median","Q3","Max","Std")
    metadata$Description <- ""
    metadata$Source <- ""
    metadata$Each_row_is <- ""
    metadata$Sampling_method <- ""
    metadata$Prepared_On <- as.character(Sys.Date())
    if(is.null(file_info)){
      metadata$Format <- ""
      metadata$File_name <- ""
      metadata$Encoding <- ""
      metadata$File_size <- ""
      metadata$Number_of_rows <- nrow(data)
      metadata$Number_of_columns <- ncol(data)
      metadata$Modified_On <- ""
    }
    else{
      metadata$Format <- as.character(file_info$file_info$file.ext)
      metadata$File_name <- as.character(file_info$file_info$file.name)
      metadata$File_size <- as.character(file_info$file_info$file.size)
      metadata$Encoding <- as.character(file_info$file_info$file.encoding)
      metadata$Number_of_rows <- file_info$file_info$number.of.rows
      metadata$Number_of_columns <- file_info$file_info$number.of.columns
      metadata$Modified_On <- as.character(file_info$file_info$last.modified)
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
        a8 <- NA
        a9 <- NA
        a10 <- NA
        a11 <- NA
        a12 <- NA
        a13 <- NA
        a14 <- NA
      }
      metadata$columns[[i]] <- rbind(metadata$columns[[i]],data.frame("Column_Names"=a1,"Type"=a2,"Key"=a3,"Description"=a4,"Missing"=a5,"Missing_percentage" = a16,"Uniques"=a6,"Top"=a7,"Min"=a8,"Q1"=a9,"Mean"=a10,"Median"=a11,"Q3"=a12,"Max"=a13,"Std"=a14))
      row.names(metadata$columns[[i]]) <- NULL
    }
    return(metadata)
  }
  else if(is.null(data) & !is.null(file_info$data)){
    data <- as.data.frame(file_info$data)
    if(!is.null(columns)){
      data <- data[,columns]
    }
    row.names(data) <- NULL
    mylist.names <- c("File_name","Description","Source","Format","File_size","Encoding","Number_of_rows","Number_of_columns","Each_row_is","Sampling_method","Prepared_On","Modified_On","columns")
    metadata <- vector("list", length(mylist.names))
    names(metadata) <- mylist.names
    metadata$columns <- vector("list", length(names(data)))
    names(metadata$columns) <- names(data)
    mydataframe <- c("Column_Names","Type","Key","Description","Missing","Missing_percentage","Uniques","Top","Min","Q1","Mean","Median","Q3","Max","Std")
    metadata$Description <- ""
    metadata$Source <- ""
    metadata$Each_row_is <- ""
    metadata$Sampling_method <- ""
    metadata$Prepared_On <- as.character(Sys.Date())
    if(is.null(file_info)){
      metadata$Format <- ""
      metadata$File_name <- ""
      metadata$Encoding <- ""
      metadata$File_size <- ""
      metadata$Number_of_rows <- nrow(data)
      metadata$Number_of_columns <- ncol(data)
      metadata$Modified_On <- ""
    }
    else{
      metadata$Format <- as.character(file_info$file_info$file.ext)
      metadata$File_name <- as.character(file_info$file_info$file.name)
      metadata$File_size <- as.character(file_info$file_info$file.size)
      metadata$Encoding <- as.character(file_info$file_info$file.encoding)
      metadata$Number_of_rows <- file_info$file_info$number.of.rows
      metadata$Number_of_columns <- file_info$file_info$number.of.columns
      metadata$Modified_On <- as.character(file_info$file_info$last.modified)
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
        a8 <- NA
        a9 <- NA
        a10 <- NA
        a11 <- NA
        a12 <- NA
        a13 <- NA
        a14 <- NA
      }
      metadata$columns[[i]] <- rbind(metadata$columns[[i]],data.frame("Column_Names"=a1,"Type"=a2,"Key"=a3,"Description"=a4,"Missing"=a5,"Missing_percentage" = a16,"Uniques"=a6,"Top"=a7,"Min"=a8,"Q1"=a9,"Mean"=a10,"Median"=a11,"Q3"=a12,"Max"=a13,"Std"=a14))
      row.names(metadata$columns[[i]]) <- NULL
    }
    return(metadata)
  }
  else{return()}
}
