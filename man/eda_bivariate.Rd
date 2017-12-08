\name{eda_bivariate}
\alias{eda_bivariate}
\title{
Provides Bivariate Analysis in a Tabular format
}
\description{
 Bivariate Tables for \cr
    1. Categorical - Categorical Variable : Cross Tab of Count of Records \cr
    2. Numeric - Categorical Variable : Sum, Average, Min, Max of Records \cr
}
\usage{
eda_bivariate(data = NULL, file_info = NULL)
}
\arguments{
  \item{data}{
       If the dataset was imported using functions other than the read_eda function, pass this dataset to the data argument without specifying the file_info argument.
}
  \item{file_info}{
     If data was imported using read_eda function,pass tis to the file_info argument without specifying the data argument.
}
}
