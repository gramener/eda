\name{eda_metadata}
\alias{eda_metadata}
\title{
   Computes and lists Metadata.
}
\description{
  Metadata Creation - Functional capability to read any file format and provide a tabular exportable format containing : \cr
  1. Column Name \cr
  2. Column Type - Numeric(Integer/Float),Date,Categorical(Normal/Ordered),Location based(Lat/Long) \cr
  3. Unique Key - this needs to be guessed - if row count of data is equal to distinct count of the records for this column, mark it as a unique key \cr
  4. Description - [Column to be left blank - to be filled by User] \cr
  5. Notes - [Column to be left blank - to be filled by User on any quick observations/annotations] \cr
  6. Missing Value percentage  \cr
  7. No. of Unique Values \cr
  8. Min Value \cr
  9. Max Value \cr
  10. Q1 Value \cr
  11. Q3 Value \cr
  12. Average Value \cr
  13. Standard Deviation Value \cr
  14. Median Value \cr
  15. Top 5 Values \cr
}
\usage{
eda_metadata(path = NULL,data = NULL,header =T,sep = ",",skip = 0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
    \item{path}{
     Specify the path of the file to be read into R.If it does not contain an absolute path, the file name is relative to the current working directory.
}
  \item{data}{
       If the dataset is imported using other import packages, pass the dataset in the data argument.
}
\item{header}{
       A logical value indicating whether the file contains the names of the variables as its first line.
}
  \item{sep}{
       The field separator character. Values on each line of the file are separated by this character.By default the separator is ",".
}
  \item{skip}{
      Integer: the number of lines of the data file to skip before beginning to read data.By default skip = 0.
}
}
