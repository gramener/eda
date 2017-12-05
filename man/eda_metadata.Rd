\name{eda_metadata}
\alias{eda_metadata}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
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
     If data was imported using read_eda function,pass tis to the file_info argument without specifying the data argument.
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

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
