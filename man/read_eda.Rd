\name{read_eda}
\alias{read_eda}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
  Reads a file in table format and creates a data frame from it, with rows corresponding to lines and columns to fields in the file.
}
\usage{
read_eda(path, header = T, skip = 0, sep = ",")
}
%- maybe also 'usage' for other objects documented here.
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
      integer: the number of lines of the data file to skip before beginning to read data.By default skip = 0.
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
