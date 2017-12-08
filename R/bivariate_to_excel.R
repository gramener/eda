bivariate_to_excel <- function(bivar_data,wb,path = NULL){
  require(xlsx)
  start <- Sys.time()
  wb <- paste(wb,"xlsx",sep=".")
  if(!is.null(path)){
    wbb <- paste(path,wb,sep="/")
  }
  else{
    wbb <- wb
  }
  if(!file.exists(wbb)){
    wb1 = createWorkbook()
    sheet_cat_cat = createSheet(wb1,"Character Vs Character")
    sheet_cat_num = createSheet(wb1,"Character Vs Numerical")
    csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
    q <- 2
    for(i in 1:length(bivar_data$cat_VS_cat)){
      name <- as.data.frame(names(bivar_data$cat_VS_cat)[[i]])
      addDataFrame(name, sheet=sheet_cat_cat,startRow= q-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
      addDataFrame(bivar_data$cat_VS_cat[[i]], sheet=sheet_cat_cat,startRow= q ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
      q <- q + 5 + nrow(bivar_data$cat_VS_cat[[i]])
    }
    v <- 2
    for(j in 1:length(bivar_data$cat_VS_num)){
      name <- as.data.frame(names(bivar_data$cat_VS_num)[[j]])
      addDataFrame(name, sheet=sheet_cat_num,startRow= v-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
      addDataFrame(bivar_data$cat_VS_num[[j]], sheet=sheet_cat_num,startRow= v ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
      v <- v + 5 + nrow(bivar_data$cat_VS_num[[j]])
    }
    saveWorkbook(wb1, wb)
  }
  else{
    wb1<-loadWorkbook(wbb)
    sheet_cat_cat = createSheet(wb1,"Character Vs Character")
    sheet_cat_num = createSheet(wb1,"Character Vs Numerical")
    csTableColNames <- CellStyle(wb1) + Font(wb1, isBold=TRUE) + Alignment(wrapText=TRUE, h="ALIGN_CENTER") + Border(color="black", position=c("TOP", "BOTTOM"), pen=c("BORDER_THIN", "BORDER_THICK"))
    q <- 2
    for(i in 1:length(bivar_data$cat_VS_cat)){
      name <- as.data.frame(names(bivar_data$cat_VS_cat)[[i]])
      addDataFrame(name, sheet=sheet_cat_cat,startRow= q-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
      addDataFrame(bivar_data$cat_VS_cat[[i]], sheet=sheet_cat_cat,startRow= q  ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
     q <- q + 5 + nrow(bivar_data$cat_VS_cat[[i]])
      }
    v <- 2
    for(j in 1:length(bivar_data$cat_VS_num)){
     name <- as.data.frame(names(bivar_data$cat_VS_num)[[j]])
     addDataFrame(name, sheet=sheet_cat_num,startRow= v-1 ,startColumn=1, row.names=FALSE,col.names = FALSE)
     addDataFrame(bivar_data$cat_VS_num[[j]], sheet=sheet_cat_num,startRow= v ,startColumn=1, row.names=FALSE,colnamesStyle=csTableColNames)
     v <- v + 5 + nrow(bivar_data$cat_VS_num[[j]])
      }
    saveWorkbook(wb1, wb)
  }
  end <- Sys.time()
  time <- end-start
  print(paste("Took",time,"minutes"))
}

