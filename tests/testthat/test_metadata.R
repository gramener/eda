library(eda)
library(yaml)
context("Meta data")

test_that("Test if bank metadata and YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv_files","bank.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml_files/","bank.csv",".yaml",sep = "")))
  }
)



test_that("Test if singapore-flat-prices metadata and YAML file is consistent", {
  expect_equal(mget(c("description","source","row_description","sampling_method","prepared_by","prepared_on","format",
                      "file_name","file_size","encoding","row_count","column_count","modified_on","columns"), envir=metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv_files","singapore-flat-prices.csv",sep = "/"))),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml_files/","singapore-flat-prices.csv",".yaml",sep = "")))
}
)




