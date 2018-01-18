\name{univariate}
\alias{univariate}
\title{
  Univariate analysis
}
\description{
    Univariate Analysis - Functional capability to read any file format and provide a tabular exportable format containing : \cr
  a. Column Name \cr
  b. percentage of values above/below the IQR Limit \cr
  c. percentage of values above/below the Mean Limit \cr
  d. Concentration - (80 percentage of data is covered by x percent of the Values) - Provides x \cr
  e. Priority Metric [Column to be left blank - to be filled by User] \cr
  f.  Performance Metric [Column to be left blank - to be filled by User] \cr
  g.  Notes - [Column to be left blank - to be filled by User on any quick observations/annotations] \cr
}
\usage{
uni <- univariate$new(metadata, k = 3)
uni$save(savepath,sheet= "Univariate")
uni$saveplot(savepath ,breaks = NULL)
}
\arguments{
  \item{metadata}{
       Specify the metadata generated using the metadata function.
}
  \item{k}{
       The "k" in the outlier detection technique(mean +/- k* stardard deviation). By default k = 3.
}
  \item{savepath}{
       The path of the xlsx file you want to save the analysis in. If the file does not exist it will create a new file of the specified name.
}
  \item{sheet}{
       The name of the worksheet in which the analysis will be outputted.By default the sheet name is "Univariate"
}
  \item{breaks}{
       The number of breaks in the histogram.
}
}
