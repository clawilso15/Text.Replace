test_that("corpus proper messages and warnings", {
  expect_error(Text.Replace::create_corpus(df = 5, text_col = 'comments'))
  
  root <- find_root(is_rstudio_project)
  dest_All <- file.path(root, 'inst','extdata')
  sample_data <- paste(dest_All, 'surveyData_2.csv', sep = "/")
  expect_message(Text.Replace::create_corpus(readr::read_csv(sample_data), "comment"))
})
# test_that("corpus created", {
#   expect_error(pv(FV = "1000", .09, n = 5))
#   expect_error(pv(FV = list(1, 2, 3), .09, n = 5))
#   expect_message(pv(FV = 1000, .36, n = 5))
# })