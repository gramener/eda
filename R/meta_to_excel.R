meta_to_excel <- function(meta_data,wb ,sheet= "Metadata",path = NULL){
  require(xlsx)
  wb <- paste(wb,"xlsx",sep = ".")
  metadata1 <- NULL
  for(i in 1:length(meta_data$columns)){
    metadata1 <- rbind(metadata1,meta_data$columns[[i]])
  }
  names(metadata1) <- gsub("_"," ",names(metadata1))
  meta_data$metadata <- metadata1
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
    addDataFrame(meta_data$metadata, sheet=sheet,startRow=(length(meta_data)+4),startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
    addDataFrame(data.frame(names(meta_data)[1:(length(meta_data)-2)],as.character(meta_data[1:(length(meta_data)-2)])), sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,col.names = FALSE)
    saveWorkbook(wb1, wbb)
  }
  else{
    wb1<-loadWorkbook(wbb)
    sheet = createSheet(wb1,sheet)
    csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
    addDataFrame(meta_data$metadata, sheet=sheet,startRow=(length(meta_data)+4),startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
    addDataFrame(data.frame(names(meta_data)[1:(length(meta_data)-2)],as.character(meta_data[1:(length(meta_data)-2)])), sheet=sheet,startRow=2,startColumn=1, row.names=FALSE,col.names = FALSE)
    setColumnWidth(sheet, colIndex=1:ncol(meta_data$metadata), colWidth=20)
    saveWorkbook(wb1, wbb)
  }
}
