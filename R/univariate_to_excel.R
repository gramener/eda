univariate_to_excel <- function(uni_data,wb ,sheet= "Univariate",path = NULL){
  start <- Sys.time()
  require(xlsx)
  unidata1 <- NULL
  for(i in 1:length(uni_data$columns)){
    unidata1 <- rbind(unidata1,uni_data$columns[[i]])
  }
  names(unidata1) <- gsub("_"," ",names(unidata1))
  names(unidata1) <- gsub("`","",names(unidata1))
  uni_data$data <- unidata1
  if(!is.null(path)){
    wbb <- paste(path,wb,sep="/")
  }
  else{
    wbb <- wb
  }
  if(!file.exists(wbb)){
    wb1 = createWorkbook()
    sheet = createSheet(wb1,sheet)
    csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
    addDataFrame(uni_data$data, sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
    saveWorkbook(wb1, wb)
  }
  else{
    wb1<-loadWorkbook(wbb)
    sheet = createSheet(wb1,sheet)
    csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
    addDataFrame(uni_data$data, sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
    setColumnWidth(sheet, colIndex=1:ncol(uni_data$data), colWidth=20)
    saveWorkbook(wb1,wb)
  }
  end <- Sys.time()
  time <- end-start
  print(paste("Took",time,"minutes"))
}
