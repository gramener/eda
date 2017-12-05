\name{eda_univariate_plot}
\alias{eda_univariate_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
eda_univariate_plot(dataset = NULL, file_info, meta_data, wb, breaks = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
      If the dataset was imported using functions other than the read_eda function, pass this dataset to the data argument without specifying the file_info argument.
}
  \item{file_info}{
       If data was imported using read_eda function,pass tis to the file_info argument without specifying the data argument.
}
  \item{meta_data}{
       Output of the eda_metadata function needs to be passed here.
}
  \item{wb}{
       The name of an existing or a new workbook to be created.
}
  \item{breaks}{
       The number of bins in the histogram.By default Freedman Diaconis rule is used to determine the number of bins.
}
  \item{path}{
       The path of the existing workbook where the univariate analysis needs to be exported.By default the path is the current working directory.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
function (dataset = NULL, file_info, meta_data, wb, breaks = NULL)
{
    require(xlsx)
    start <- Sys.time()
    j <- 1
    k <- 1
    l <- 1
    bar_one <- function(column) {
        df1 <- as.data.frame(table(column))
        df <- as.data.frame(sort(table(column), decreasing = T))
        df$Rank <- seq(length(df$Freq))
        df$Colour[df$Rank <= 3] <- "blue"
        df$Colour[df$Rank > 3] <- "black"
        df2 <- merge(df, df1, by.x = "column", by.y = "column")
        return(df2)
    }
    rankfreq <- function(column) {
        df <- as.data.frame(sort(table(column), decreasing = T))
        df$Rank <- seq(length(df$Freq))
        df$Colour[df$Rank <= 10] <- "blue"
        df$Colour[df$Rank > 10] <- "black"
        plot(df$Rank, df$Freq, log = "xy", type = "b", col = df$Colour,
            xlab = "Rank", ylab = "Frequency", xaxt = "n", cex.lab = 0.75)
        axis(1, at = 1:length(levels(df$column)), labels = levels(df$column))
    }
    metadata1 <- NULL
    for (i in 1:length(meta_data$columns)) {
        metadata1 <- rbind(metadata1, meta_data$columns[[i]])
    }
    num_var <- as.character(metadata1[which(metadata1$Type \%in\%
        c("discrete", "continuous")), "Column_Names"])
    ord_var <- as.character(metadata1[which(tolower(metadata1$Type) ==
        "ordered"), "Column_Names"])
    cat_var <- as.character(metadata1[which(tolower(metadata1$Type) ==
        "character"), "Column_Names"])
    if (!is.null(dataset)) {
        if (file.exists(wb)) {
            wb1 <- loadWorkbook(wb)
            sheet_hist = createSheet(wb1, "Histogram")
            sheet_rank = createSheet(wb1, "Rank Frequency")
            sheet_bar = createSheet(wb1, "Bar Plot")
            sheet_box = createSheet(wb1, "Box Plot")
            for (i in 1:ncol(dataset)) {
                if (names(dataset)[i] \%in\% num_var) {
                  png("boxplot.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  boxplot(dataset[, i], col = "blue", main = paste("Boxplot of ",
                    names(dataset)[i]), xlab = names(dataset)[i])
                  dev.off()
                  addPicture("boxplot.png", sheet_box, scale = 1,
                    startRow = 4, startColumn = j)
                  res <- file.remove("boxplot.png")
                  if (is.null(breaks)) {
                    breaks <- nclass.FD(dataset[, i])
                  }
                  png("histogram.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  hist(dataset[, i], breaks = breaks, col = "blue",
                    main = paste("Histogram of ", names(dataset)[i]),
                    xlab = names(dataset)[i])
                  dev.off()
                  addPicture("histogram.png", sheet_hist, scale = 1,
                    startRow = 4, startColumn = j)
                  res <- file.remove("histogram.png")
                  j <- j + 15
                }
                else if (names(dataset)[i] \%in\% cat_var & length(unique(dataset[,
                  i])) != 1) {
                  png("rankfreq.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  rankfreq(dataset[, i])
                  title(paste("Rank Frequency plot of", names(dataset)[i]))
                  dev.off()
                  addPicture("rankfreq.png", sheet_rank, scale = 1,
                    startRow = 4, startColumn = k)
                  res <- file.remove("rankfreq.png")
                  k <- k + 15
                }
                else if (names(dataset)[i] \%in\% ord_var & length(unique(dataset[,
                  i])) != 1) {
                  png("barplot.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  df <- bar_one(dataset[, i])
                  barplot(table(dataset[, i]), col = df$Colour,
                    main = paste("Barplot of ", names(dataset)[i]),
                    xlab = names(dataset)[i])
                  dev.off()
                  addPicture("barplot.png", sheet_bar, scale = 1,
                    startRow = 4, startColumn = l)
                  res <- file.remove("barplot.png")
                  l <- l + 15
                }
            }
            saveWorkbook(wb1, wb)
        }
        else {
            wb1 <- createWorkbook(type = "xlsx")
            sheet_hist = createSheet(wb1, "Histogram")
            sheet_rank = createSheet(wb1, "Rank Frequency")
            sheet_bar = createSheet(wb1, "Bar Plot")
            sheet_box = createSheet(wb1, "Box Plot")
            for (i in 1:ncol(dataset)) {
                if (names(dataset)[i] \%in\% num_var) {
                  png("boxplot.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  boxplot(dataset[, i], col = "blue", main = paste("Boxplot of ",
                    names(dataset)[i]), xlab = names(dataset)[i])
                  dev.off()
                  addPicture("boxplot.png", sheet_box, scale = 1,
                    startRow = 4, startColumn = j)
                  res <- file.remove("boxplot.png")
                  if (is.null(breaks)) {
                    breaks <- nclass.FD(dataset[, i])
                  }
                  png("histogram.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  hist(dataset[, i], breaks = breaks, col = "blue",
                    main = paste("Histogram of ", names(dataset)[i]),
                    xlab = names(dataset)[i])
                  dev.off()
                  addPicture("histogram.png", sheet_hist, scale = 1,
                    startRow = 4, startColumn = j)
                  res <- file.remove("histogram.png")
                  j <- j + 15
                }
                else if (names(dataset)[i] \%in\% cat_var & length(unique(dataset[,
                  i])) != 1) {
                  png("rankfreq.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  rankfreq(dataset[, i])
                  title(paste("Rank Frequency plot of", names(dataset)[i]))
                  dev.off()
                  addPicture("rankfreq.png", sheet_rank, scale = 1,
                    startRow = 4, startColumn = k)
                  res <- file.remove("rankfreq.png")
                  k <- k + 15
                }
                else if (names(dataset)[i] \%in\% ord_var & length(unique(dataset[,
                  i])) != 1) {
                  png("barplot.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  df <- bar_one(dataset[, i])
                  barplot(table(dataset[, i]), col = df$Colour,
                    main = paste("Barplot of ", names(dataset)[i]),
                    xlab = names(dataset)[i])
                  dev.off()
                  addPicture("barplot.png", sheet_bar, scale = 1,
                    startRow = 4, startColumn = l)
                  res <- file.remove("barplot.png")
                  l <- l + 15
                }
            }
            saveWorkbook(wb1, wb)
        }
    }
    else if (is.null(dataset) & !is.null(file_info$data)) {
        dataset <- file_info$data
        if (file.exists(wb)) {
            wb1 <- loadWorkbook(wb)
            sheet_hist = createSheet(wb1, "Histogram")
            sheet_rank = createSheet(wb1, "Rank Frequency")
            sheet_bar = createSheet(wb1, "Bar Plot")
            sheet_box = createSheet(wb1, "Box Plot")
            for (i in 1:ncol(dataset)) {
                if (names(dataset)[i] \%in\% num_var) {
                  png("boxplot.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  boxplot(dataset[, i], col = "blue", main = paste("Boxplot of ",
                    names(dataset)[i]), xlab = names(dataset)[i])
                  dev.off()
                  addPicture("boxplot.png", sheet_box, scale = 1,
                    startRow = 4, startColumn = j)
                  res <- file.remove("boxplot.png")
                  if (is.null(breaks)) {
                    breaks <- nclass.FD(dataset[, i])
                  }
                  png("histogram.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  hist(dataset[, i], breaks = breaks, col = "blue",
                    main = paste("Histogram of ", names(dataset)[i]),
                    xlab = names(dataset)[i])
                  dev.off()
                  addPicture("histogram.png", sheet_hist, scale = 1,
                    startRow = 4, startColumn = j)
                  res <- file.remove("histogram.png")
                  j <- j + 15
                }
                else if (names(dataset)[i] \%in\% cat_var & length(unique(dataset[,
                  i])) != 1) {
                  png("rankfreq.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  rankfreq(dataset[, i])
                  title(paste("Rank Frequency plot of", names(dataset)[i]))
                  dev.off()
                  addPicture("rankfreq.png", sheet_rank, scale = 1,
                    startRow = 4, startColumn = k)
                  res <- file.remove("rankfreq.png")
                  k <- k + 15
                }
                else if (names(dataset)[i] \%in\% ord_var & length(unique(dataset[,
                  i])) != 1) {
                  png("barplot.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  df <- bar_one(dataset[, i])
                  barplot(table(dataset[, i]), col = df$Colour,
                    main = paste("Barplot of ", names(dataset)[i]),
                    xlab = names(dataset)[i])
                  dev.off()
                  addPicture("barplot.png", sheet_bar, scale = 1,
                    startRow = 4, startColumn = l)
                  res <- file.remove("barplot.png")
                  l <- l + 15
                }
            }
            saveWorkbook(wb1, wb)
        }
        else {
            wb1 <- createWorkbook(type = "xlsx")
            sheet_hist = createSheet(wb1, "Histogram")
            sheet_rank = createSheet(wb1, "Rank Frequency")
            sheet_bar = createSheet(wb1, "Bar Plot")
            sheet_box = createSheet(wb1, "Box Plot")
            for (i in 1:ncol(dataset)) {
                if (names(dataset)[i] \%in\% num_var) {
                  png("boxplot.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  boxplot(dataset[, i], col = "blue", main = paste("Boxplot of ",
                    names(dataset)[i]), xlab = names(dataset)[i])
                  dev.off()
                  addPicture("boxplot.png", sheet_box, scale = 1,
                    startRow = 4, startColumn = j)
                  res <- file.remove("boxplot.png")
                  if (is.null(breaks)) {
                    breaks <- nclass.FD(dataset[, i])
                  }
                  png("histogram.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  hist(dataset[, i], breaks = breaks, col = "blue",
                    main = paste("Histogram of ", names(dataset)[i]),
                    xlab = names(dataset)[i])
                  dev.off()
                  addPicture("histogram.png", sheet_hist, scale = 1,
                    startRow = 4, startColumn = j)
                  res <- file.remove("histogram.png")
                  j <- j + 15
                }
                else if (names(dataset)[i] \%in\% cat_var & length(unique(dataset[,
                  i])) != 1) {
                  png("rankfreq.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  rankfreq(dataset[, i])
                  title(paste("Rank Frequency plot of", names(dataset)[i]))
                  dev.off()
                  addPicture("rankfreq.png", sheet_rank, scale = 1,
                    startRow = 4, startColumn = k)
                  res <- file.remove("rankfreq.png")
                  k <- k + 15
                }
                else if (names(dataset)[i] \%in\% ord_var & length(unique(dataset[,
                  i])) != 1) {
                  png("barplot.png", height = 1200, width = 2000,
                    res = 250, pointsize = 8)
                  df <- bar_one(dataset[, i])
                  barplot(table(dataset[, i]), col = df$Colour,
                    main = paste("Barplot of ", names(dataset)[i]),
                    xlab = names(dataset)[i])
                  dev.off()
                  addPicture("barplot.png", sheet_bar, scale = 1,
                    startRow = 4, startColumn = l)
                  res <- file.remove("barplot.png")
                  l <- l + 15
                }
            }
            saveWorkbook(wb1, wb)
        }
    }
    end <- Sys.time()
    time <- end - start
    print(paste("Took", time, "minutes"))
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
