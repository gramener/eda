\name{eda_univariate}
\alias{eda_univariate}
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
eda_univariate(metadata, k = 3)
}
\arguments{
  \item{metadata}{
       Specify the metadata generated using the eda_metadata function.
}
  \item{k}{
       The "k" in the outlier detection technique(mean +/- k* stardard deviation). By default k = 3.
}
}
