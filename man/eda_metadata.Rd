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
eda_metadata(data = NULL, file_info = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{
       If the dataset was imported using functions other than the read_eda function, pass this dataset to the data argument without specifying the file_info argument.
}
  \item{file_info}{
     If data was imported using read_eda function,pass this to the file_info argument without specifying the data argument.
}
}
