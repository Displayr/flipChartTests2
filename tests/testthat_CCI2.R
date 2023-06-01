## timings from /https://app.travis-ci.com/github/Displayr/flipChartTests2/builds/260587552
## test-historgram: 70
## labeledscatter: 30
## margin: 150
## misc: 14
## pie: 182
## scatter: 472
## venn: 30
if (identical(Sys.getenv("CIRCLECI"), "true"))
{
    test.files <- list.files("tests/testthat", pattern = "\\.R$")
    test.files <- gsub("test-|\\.R$", "", test.files)
    test.filter <- grep("^scatter", test.files,
                        invert = TRUE, value = TRUE)
    if (!dir.exists("reports"))
        dir.create("reports")
    out.file <- paste0("reports/test_results", Sys.getenv("CIRCLE_NODE_INDEX"), ".xml")
    exit.code <- flipDevTools::RunTestsOnCircleCI(filter = paste0(test.filter, collapse = "|"),
                                                  load_package = "none", output_file = out.file)
    ## Ignore exit code so job continues to save snapshots
    ## q(status = exit.code, save = "no")
}
