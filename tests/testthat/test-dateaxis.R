context("Date axis")
library("flipStandardCharts")
library("flipChartTests")

date0 <- structure(17759, class = "Date")

test_that("Different intervals",
{
    for (n in c(5, 10, 11, 15))
    {
        xx <- cbind(A=1:n, B=(1:n)+1, C=(1:n)+2)
        for (step in c(1, 3, 8, 25, 60, 300))
        {
            rownames(xx) <- as.character(date0 + (1:n * step))
            
            filestem <- sprintf("dateaxis-line-n%d-s%d", n, step)
            expect_error(pp <- Line(xx, data.label.show = TRUE), NA)
            expect_true(TestWidget(pp, filestem))
            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
    
            filestem <- sprintf("dateaxis-bar-n%d-s%d", n, step)
            names(xx) <- date0 + (1:length(xx) * step)
            expect_error(pp <- Bar(xx, data.label.show = TRUE), NA)
            expect_true(TestWidget(pp, filestem))
            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
        }
    } 
})

quarterly <- c(`2017-07-01` = 168195.865366635, `2017-10-01` = 211481.421829429, 
               `2018-01-01` = 332698.464796557, `2018-04-01` = 288242.273265781, 
               `2018-07-01` = 288785.055473877, `2018-10-01` = 377010.090510404, 
               `2019-01-01` = 306371.382229722, `2019-04-01` = 374781.170650826, 
               `2019-07-01` = 212468.244162981)
survey.dates <- structure(c(10, 7.83333333333333, 6.5, 7), .Dim = c(4L, 1L), .Dimnames = list(
                c("16-Jul-18-22-Jul-18", "23-Jul-18-29-Jul-18", "30-Jul-18-05-Aug-18", 
                "06-Aug-18-12-Aug-18"), "How would you rate your overall experience of visiting the sales centre?"), 
                statistic = "Average", name = "table.QB8.How.would.you.rate.your.overall.experience.of.visiting.the.sales.centre.by.Week",
                questions = c("QB8 How would you rate your overall experience of visiting the sales centre?", "Week"))               

test_that("Default tick labels",
{
    expect_true(TestWidget(Column(quarterly), "dateaxis-quarterly-column"))
    expect_true(TestWidget(Column(survey.dates), "dateaxis-survey-column"))
    expect_true(TestWidget(Bar(quarterly), "dateaxis-quarterly-bar"))
    expect_true(TestWidget(Bar(survey.dates), "dateaxis-survey-bar"))
    expect_true(TestWidget(Line(quarterly), "dateaxis-quarterly-line"))
    expect_true(TestWidget(Area(survey.dates), "dateaxis-survey-ares"))
})
