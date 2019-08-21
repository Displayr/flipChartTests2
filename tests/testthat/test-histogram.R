context("Histogram")
library(flipStandardCharts)

set.seed(1234)
xx <- rpois(20, 5)
dups <- c(1:1000, 557, 779)
gapped <- c(1:3, 5, 7, 8:9)
smallvals <- rep(c(0.0, 0.2, 1.0), 1:3 * 10)
fewvals <- c(1,1,1,3,10, 2:6)

    pp <- Histogram(xx)
    expect_true(TestWidget(pp, "hist-bin-default"))

test_that("Default bins",
{
    dat <- c("xx", "raredups", "gapped", "smallvals", "fewvals")
    for (dd in dat)
    {
        cmd <- sprintf("Histogram(%s)", dd)
        filestem <- sprintf("hist-defbin-%s", dd)
        pp <- eval(parse(text=cmd))
        expect_true(TestWidget(pp, filestem))
    }
})
    
    
test_that("Manual bins",
{
    ii <- c(1:10, 15, 20, 30, 50)
    for (i in ii)
    {
        filestem <- sprintf("hist-bin-%d", i)
        pp <- Histogram(xx, maximum.bins = i)
        expect_true(TestWidget(pp, filestem))
    }
})
    
    