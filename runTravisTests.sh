#!/bin/bash

if [ -d tests/testthat ]; then
    echo "tfile <- tempfile(tmpdir = '.', fileext = '.txt')" > test.R
    echo "options('testthat.progress.max_fails' = Inf)" >> test.R
    echo "cat('Writing output to', tfile, '\n')" >> test.R
    echo "capture.output(res<-devtools::test(), file = tfile, type = 'output')" >> test.R
    echo "out <- readLines(tfile)" >> test.R
    echo "cat(out, sep = '\n')" >> test.R
    echo "tmp <- unlist(strsplit(split='[[:space:]]+', tail(out, 1)))" >> test.R
    echo "pos.fail <- grep('FAIL', tmp)" >> test.R
    echo "n.fail <- as.numeric(tmp[pos.fail+1])" >> test.R
    echo "res <- as.data.frame(res); out <- data.frame(file = unlist(res[['file']]), warning = unlist(res[['warning']]))" >> test.R 
    echo "write.csv(out, file='test_results.csv')" >> test.R
    echo "quit(status = !identical(n.fail, 0), save='no')" >> test.R
    Rscript --default-packages="datasets,utils,grDevices,graphics,stats,methods" test.R
    exit $?
fi
exit 0
