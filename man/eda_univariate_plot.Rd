\name{eda_univariate_plot}
\alias{eda_univariate_plot}
\title{
   Plots the univariate charts
}
\description{
  Provides charts containing : \cr
  a. Column Name - Rank Freq Chart for Categorical Variables - (Unordered) \cr
  b. Column Name - Bar Chart for Categorical Variables - (Ordered) \cr
  c. Column Name - Histogram Chart for Numeric(Integer & Float) Variables \cr
  d. Column Name - Box Plot Chart for Numeric(Integer & Float) Variables \cr
}
\usage{
eda_univariate_plot(data = NULL, file_info, meta_data, wb, breaks = NULL)
}
\arguments{
  \item{data}{
      If the dataset was imported using functions other than the read_eda function, pass this dataset to the data argument without specifying the file_info argument.
}
  \item{file_info}{
       If data was imported using read_eda function,pass this to the file_info argument without specifying the data argument.
}
  \item{meta_data}{
       Output of the eda_metadata function needs to be passed here.
}
  \item{wb}{
       The name of an existing or a new workbook to be created.
}
  \item{breaks}{
       The number of bins in the histogram.By default Freedman Diaconis rule is used to determine the number of bins.
}
  \item{path}{
       The path of the existing workbook where the univariate analysis needs to be exported.By default the path is the current working directory.
}
}

