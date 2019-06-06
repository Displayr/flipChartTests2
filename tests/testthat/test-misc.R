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

one.row <- structure(c(8, 12, 67), .Dim = c(1L, 3L), 
    .Dimnames = list("A", c("Detractors", "Passives", "Promotes")))
test_that("Hover works with 1 row",
{
    # DS-2508: plotly seems to have problem converting single values to vectors
    pp <- Bar(one.row, data.label.show = T, type = "Stacked")
    expect_true(TestWidget(pp, "misc-hover-onerow-bar"))
    
    pp <- Column(one.row, data.label.show = T)
    expect_true(TestWidget(pp, "misc-hover-onerow-column"))
})