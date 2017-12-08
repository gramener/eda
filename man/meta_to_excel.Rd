\name{meta_to_excel}
\alias{meta_to_excel}
\title{
  Exports Metadata to excel
}
\description{
  Exports the metadata computed by the eda_metadata function to an excel file.
}
\usage{
meta_to_excel(meta_data, wb, sheet = "Metadata")
}
\arguments{
  \item{meta_data}{
       Output of the eda_metadata function needs to be passed here.
}
  \item{wb}{
       The name of an existing or a new workbook to be created.
}
  \item{sheet}{
       The name of the sheet within the workbook where the metadata will be exported.By default the name of the sheet is "Metadata"
}
  \item{path}{
       The path of the existing workbook where the metadata needs to be exported.By default the path is the current working directory.
}
}
