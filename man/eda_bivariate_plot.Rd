\name{eda_bivariate_plot}
\alias{eda_bivariate_plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
   Outputs Bivariate Plots to Excel
}
\description{
Bivariate Plots for: \cr
    1. Numeric - Categorical Variable : Bar Plot for Sum, Average, Min, Max Records \cr
    2. Numeric - Numeric : Correlation Plot \cr
}
\usage{
eda_bivariate_plot(dataset = NULL, file_info = NULL, wb, path = NULL,method = c("pearson","spearman"), columns = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
      If the dataset was imported using functions other than the read_eda function, pass this dataset to the data argument without specifying the file_info argument.
}
  \item{file_info}{
       If data was imported using read_eda function,pass tis to the file_info argument without specifying the data argument.
}
  \item{wb}{
       The name of an existing or a new workbook to be created.
}
  \item{path}{
       The path of the existing workbook where the bivariate analysis needs to be exported.By default the path is the current working directory.
}
  \item{method}{
       "pearson" or "spearman" - The method used to construct the correlation plot.
}
  \item{columns}{
     Specify the columns of interest as either column names or column index.
}
}
