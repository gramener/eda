library(eda)
library(yaml)
context("Meta data")

test_that("Test if bank metadata YAML file is consistent", {
  expect_equal(mget(c("Description","Source","Row_description","Sampling_method","Prepared_by","Prepared_on","Format",
                      "File_name","File_size","Encoding","Row_count","Column_count","Modified_on","columns"), envir=eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","bank.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","bank.csv",".yaml",sep = "")))
  }
)


test_that("Test if barc-hair-care-brands metadata YAML file is consistent", {
  expect_equal(mget(c("Description","Source","Row_description","Sampling_method","Prepared_by","Prepared_on","Format",
                      "File_name","File_size","Encoding","Row_count","Column_count","Modified_on","columns"), envir=eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","barc-hair-care-brands.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","barc-hair-care-brands.csv",".yaml",sep = "")))
}
)


test_that("Test if nas metadata YAML file is consistent", {
  expect_equal(mget(c("Description","Source","Row_description","Sampling_method","Prepared_by","Prepared_on","Format",
                      "File_name","File_size","Encoding","Row_count","Column_count","Modified_on","columns"), envir=eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","nas.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","nas.csv",".yaml",sep = "")))
}
)


test_that("Test if per-growth-by-metric metadata YAML file is consistent", {
  expect_equal(mget(c("Description","Source","Row_description","Sampling_method","Prepared_by","Prepared_on","Format",
                      "File_name","File_size","Encoding","Row_count","Column_count","Modified_on","columns"), envir=eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","per-growth-by-metric.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","per-growth-by-metric.csv",".yaml",sep = "")))
}
)


test_that("Test if singapore-flat-prices metadata YAML file is consistent", {
  expect_equal(mget(c("Description","Source","Row_description","Sampling_method","Prepared_by","Prepared_on","Format",
                      "File_name","File_size","Encoding","Row_count","Column_count","Modified_on","columns"), envir=eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","singapore-flat-prices.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","singapore-flat-prices.csv",".yaml",sep = "")))
}
)


test_that("Test if telecom metadata YAML file is consistent", {
  expect_equal(mget(c("Description","Source","Row_description","Sampling_method","Prepared_by","Prepared_on","Format",
                      "File_name","File_size","Encoding","Row_count","Column_count","Modified_on","columns"), envir=eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","telecom.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","telecom.csv",".yaml",sep = "")))

}
)

test_that("Test if vas metadata YAML file is consistent", {
  expect_equal(mget(c("Description","Source","Row_description","Sampling_method","Prepared_by","Prepared_on","Format",
                      "File_name","File_size","Encoding","Row_count","Column_count","Modified_on","columns"), envir=eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","vas.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","vas.csv",".yaml",sep = "")))
}
)
