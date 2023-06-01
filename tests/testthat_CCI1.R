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
    if (!dir.exists("reports"))
        dir.create("reports")
    out.file <- paste0("reports/test_results", Sys.getenv("CIRCLE_NODE_INDEX"), ".xml")
    exit.code <- flipDevTools::RunTestsOnCircleCI(filter = "^scatter",
                                                  load_package = "none", output_file = out.file)
    ## Ignore exit code so job continues to save snapshots
    ## q(status = exit.code, save = "no")
}
