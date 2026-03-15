library(pkgload)
library(testthat)

pkgload::load_all(".", export_all = TRUE, helpers = TRUE, quiet = TRUE)
test_dir("tests/testthat")
