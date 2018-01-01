library(eda)
context("Meta data")

test_that("Metadata YAML file is consistent", {
  eda <- eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","bank.csv",sep = "/"))
  expect_equal(mget(columns, envir=eda),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","bank.csv",".yaml",sep = "")))
  eda <- eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","barc-hair-care-brands.csv",sep = "/"))
  expect_equal(mget(columns, envir=eda),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","barc-hair-care-brands.csv",".yaml",sep = "")))
  eda <- eda_metadata$new(path = paste("C:/Users/Nolan/Desktop/eda/data/samples/csv files","nas.csv",sep = "/"))
  expect_equal(mget(columns, envir=eda),read_yaml(paste("C:/Users/Nolan/Desktop/eda/data/samples/yaml outputs/","nas.csv",".yaml",sep = "")))
})


