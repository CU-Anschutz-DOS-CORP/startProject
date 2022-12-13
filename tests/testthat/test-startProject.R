testthat::context("startProject")

testthat::test_that("startProject works", {
    startProject(main.dir = paste0(Sys.getenv("HOME"), "/junk"), 
                 proj.name = "test_proj", client = "John Smith, MD",
                 client.dept = "QA", main.statistician = "Rocio Lopez", 
                 stats.collab = "Susana Arrigain, Anne Tang",
                 subfolders = "communications, data, graphs, memo, orig_data, others, r, sas, temp",
                 templates = "memo, sas, R, Rmd")
})
