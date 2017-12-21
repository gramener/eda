read_eda <- function(path,header =T,sep = ",",skip = 0){
  require(tools)
  require(readr)
  require(data.table)
  require(openxlsx)
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
  return(file_info)
}
