\name{univariate_to_excel}
\alias{univariate_to_excel}
\title{
    Exports univariate analysis to excel.
}
\description{
    Exports the univariate analysis carried out by the eda_univariate function to an excel file.
}
\usage{
univariate_to_excel(uni_data, wb, sheet = "Univariate")
}
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
