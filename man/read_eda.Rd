\name{read_eda}
\alias{read_eda}
\title{
  Reads in a file.
}
\description{
  Reads a file in table format and creates a data frame from it, with rows corresponding to lines and columns to fields in the file.
}
\usage{
read_eda(path, header = T, skip = 0, sep = ",")
}
\arguments{
  \item{path}{
       Specify the path of the file to be read into R.If it does not contain an absolute path, the file name is relative to the current working directory.
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
