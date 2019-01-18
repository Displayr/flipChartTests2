context("Miscellaneous")
library("flipStandardCharts")

set.seed(654321)
unnamed <- matrix(rpois(60, 4), 20, 3) # all positives
named <- matrix(unnamed, 20, 3, dimnames = list(letters[1:20], LETTERS[1:3]))

test_that("Vector of line widths",
{         
    filestem <- "misc-linethickness-vector"
    pp <- Line(named, line.thickness = c(1,3,5))
    expect_true(TestWidget(pp, filestem))
    
    filestem <- "misc-linethickness-string"
    pp <- Line(named, line.thickness = "1,3,5")
    expect_true(TestWidget(pp, filestem))
})