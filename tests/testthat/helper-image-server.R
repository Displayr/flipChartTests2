# Serves the images in tests/testthat/images over HTTP locally, so the tests that fetch image URLs don't need external
# network access. The images were downloaded on 30 October 2026 from
#   https://displayrcors.displayr.com/images/<name>_grey.svg
# for <name> in apple, soup, bread, car, chicken, rocket, sickperson, thumbsup, elephant, tree, weight and tools.
# The server stops when the calling test finishes.
localImageServer <- function(.local_envir = parent.frame())
{
    app <- webfakes::new_app()
    app$use(webfakes::mw_static(root = testthat::test_path("images")))
    webfakes::local_app_process(app, .local_envir = .local_envir)
}
