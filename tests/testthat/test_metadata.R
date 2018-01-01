library(eda)
library(yaml)
context("Meta data")

test_that("Metadata YAML file is consistent", {
  expect_equal(mget(c("Description","Source","Row_description","Sampling_method","Prepared_by","Prepared_on","Format",
                      "File_name","File_size","Encoding","Row_count","Column_count","Modified_on","columns"), envir=eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","bank.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","bank.csv",".yaml",sep = "")))
}
)


