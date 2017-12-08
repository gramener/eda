\name{bivariate_to_excel}
\alias{bivariate_to_excel}
\title{
     Exports Bivariate Analysis to Excel
}
\description{
     Exports the bivariate analysis carried out by the eda_bivariate function to an excel file.
}
\usage{
univariate_to_excel(bivar_data, wb,path = NULL)
}
\arguments{
  \item{bivar_data}{
       Output of the eda_bivariate function needs to be passed here.
}
  \item{wb}{
       The name of an existing or a new workbook to be created.
}
  \item{path}{
       The path of the existing workbook where the bivariate analysis needs to be exported.By default the path is the current working directory.
}
}
