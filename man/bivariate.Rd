\name{bivariate}
\alias{bivariate}
\title{
Provides Bivariate Analysis in a Tabular format
}
\description{
 Bivariate Tables for \cr
    1. Categorical - Categorical Variable : Cross Tab of Count and Proportions of Records \cr
    2. Numeric - Categorical Variable : Sum, Average, Min, Max of Records \cr
}
\usage{
bivariate$new(metadata)
bivariate$save(savepath)
bivariate$saveplot(savepath,method= c("pearson","spearman"))
}
\arguments{
  \item{metadata}{
       Specify the metadata generated using the metadata function.
}
  \item{savepath}{
       The path of the xlsx file you want to save the analysis in. If the file does not exist it will create a new file of the specified name.
}
  \item{method}{
       The correlation computation method. Can be either pearson or spearman
}
}
