\name{univariate_to_excel}
\alias{univariate_to_excel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
    Exports the univariate analysis carried out by the eda_metadata function to an excel file.
}
\usage{
univariate_to_excel(uni_data, wb, sheet = "Univariate")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{uni_data}{
       Output of the eda_univariate function needs to be passed here.
}
  \item{wb}{
       The name of an existing or a new workbook to be created.
}
  \item{sheet}{
       The name of the sheet within the workbook where the univariate analysis will be exported.By default the name of the sheet is "Univariate"
}
  \item{path}{
       The path of the existing workbook where the univariate analysis needs to be exported.By default the path is the current working directory.
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
