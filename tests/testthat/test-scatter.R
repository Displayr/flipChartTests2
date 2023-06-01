context("Scatter plot")
library("flipChartBasics")
library("flipStandardCharts")
library("flipChartTests")

# Set up dataframe containing different types of data types
set.seed(1234)
dat <- data.frame('Score' = rnorm(20),
                  'Cost ($)' = abs(rnorm(20)), # check plotly is handling '$' properly
                  'Age' = rpois(20, 40),
                  'Class' = factor(sample(LETTERS[4:1], 20, replace = TRUE), levels = LETTERS[4:1]), # reverse order to check DS-1645
                  'Sporadic' = c(1:5, NA, 6:10, NA, NA, 11:12, NA, NA, 13:15), # missing values
                  'Date' = as.Date(sprintf("2017-01-%02d", 20:1)),
                   check.names = FALSE, stringsAsFactors = FALSE)
rownames(dat) <- letters[1:20]

# Set up matrix to use the different variable types
grid2 <- expand.grid(0:6, 0:6)
tmp <- cbind(rbind(c(NA, 6), c(6, NA), grid2[2:49,]), grid2[c(1, 49:1),])

columns.str <- sprintf("scatter.x.column = %.0f, scatter.y.column = %d,
                        scatter.colors.column = %d, scatter.sizes.column = %d",
                       tmp[,1], tmp[,2], tmp[,3], tmp[,4])
names(columns.str) <- apply(tmp, 1, paste, collapse="")

# These are only the options that can be used by both Labeled and (plotly) Scatterplots
# Line of best fit is already tested in test-backgrounds.R
opts <- c('default' = 'colors = ChartColors(5, "Blues")',
         'categoricalcolor' = 'scatter.colors.as.categorical = TRUE, legend.font.color = "red"',
         'numericalcolor' = 'scatter.colors.as.categorical = FALSE, colors = grey(1:4/5)',
         'nolegend' = 'legend.show = FALSE, colors = "red"',
         'markerbig' = 'marker.size = 20, grid.show = FALSE',
         'thickxgrid' = 'x.grid.width = 10, global.font.color = "red", global.font.family = "Courier"')

n <- length(opts)
index <- 1
for (func in c("LabeledScatter"))
{
    for (ii in 1:length(columns.str))
    {
        jj <- n - (index %% n)
        filestem <- paste0(tolower(func), "-", names(columns.str)[ii], "-", names(opts)[jj])
        test_that(filestem, {

            cmd <- paste0("pp <- ", func, "(dat, ", columns.str[ii], ", ", opts[jj], ")")
            expect_error(suppressWarnings(eval(parse(text = cmd))), NA)

            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
            expect_true(TestWidget(pp, filestem, delay = 0.2))
        })
        index <- index + 1
    }
}

test_that("Check missing values in colors",
{
    expect_warning(pp <- Scatter(dat, scatter.colors.column = 5, scatter.sizes.column = 0,
        data.label.show = TRUE, scatter.colors.as.categorical = FALSE), 
        "qualitative palette")
    expect_true(TestWidget(pp, "scatter-NAs-colors-numeric"))
    expect_warning(pp <- Scatter(dat, scatter.colors.column = 5, scatter.sizes.column = 0,
        data.label.show = TRUE, scatter.colors.as.categorical = TRUE),
        "missing values")
    expect_true(TestWidget(pp, "scatter-NAs-colors-categorical"))
})

# Start new loop otherwise opts and columns.str combinations will change in old snapshots
opts <- c('numformat' = 'x.tick.format = ".2f", y.tick.format = ".1e"',
          'pctformat' = 'x.tick.format = ".0%", y.tick.format = ".1%"',
          'dateformat' = 'x.tick.format = "%B %d %Y", y.tick.format = "%d/%m"',
          'strformat' = 'x.tick.format = "Category", y.tick.format = "Category"')
n <- length(opts)
index <- 1
for (func in c("Scatter", "LabeledScatter"))
{
    for (ii in 1:length(columns.str))
    {
        jj <- n - (index %% n)
        filestem <- paste0(tolower(func), "-", names(columns.str)[ii], "-", names(opts)[jj])
        test_that(filestem, {

            cmd <- paste0("pp <- ", func, "(dat, ", columns.str[ii], ", ", opts[jj], ")")
            expect_error(suppressWarnings(eval(parse(text = cmd))), NA)

            #print(pp)
            #readline(prompt=paste0(filestem, ": press [enter] to continue: "))
            expect_true(TestWidget(pp, filestem))
        })
        index <- index + 1
    }
}

dat <- structure(list(q5 = c("Feminine", "Health-conscious", "Innocent",
"Older", "Open to new experiences", "Rebellious", "Sleepy", "Traditional",
"Weight-conscious", "Feminine", "Health-conscious", "Innocent",
"Older", "Open to new experiences", "Rebellious", "Sleepy", "Traditional",
"Weight-conscious", "Feminine", "Health-conscious", "Innocent",
"Older", "Open to new experiences", "Rebellious", "Sleepy", "Traditional",
"Weight-conscious", "Feminine", "Health-conscious", "Innocent",
"Older", "Open to new experiences", "Rebellious", "Sleepy", "Traditional",
"Weight-conscious", "Feminine", "Health-conscious", "Innocent",
"Older", "Open to new experiences", "Rebellious", "Sleepy", "Traditional",
"Weight-conscious", "Feminine", "Health-conscious", "Innocent",
"Older", "Open to new experiences", "Rebellious", "Sleepy", "Traditional",
"Weight-conscious"), `%` = c(6.125, 2, 10.5, 64.625, 22.375,
25.5, 9.5, 91.25, 0.5, 57.125, 57.75, 21.625, 22.5, 8.875, 4.75,
23.25, 14.625, 76.125, 22.375, 53.5, 11.375, 5.375, 50.625, 64,
9.75, 3, 63.875, 8.875, 2.5, 10, 39, 16.75, 17.75, 13.5, 54.75,
0, 61.5, 57.875, 44.625, 9.875, 16.625, 3.75, 29.75, 3.75, 76.625,
9.375, 30.625, 6.875, 6.75, 49.25, 44.75, 5.5, 4.375, 40.375),
    SUMMARY = structure(c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
    2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L,
    3L, 3L, 3L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 4L, 5L, 5L, 5L,
    5L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L, 6L
    ), .Label = c("Coke", "Diet Coke", "Coke Zero", "Pepsi",
    "Diet Pepsi", "Pepsi Max"), class = "factor"), `z-Statistic` = c(-12.8771918076981,
    -18.4404507119152, -4.47221089448898, 27.6417129999749, -2.32698196051918,
    0.112104259695292, -3.7779884321726, 40.740773418202, -23.5251145235949,
    14.9437368552787, 10.5358595621617, 0.478215172271431, -4.15028275717022,
    -14.1372820515931, -16.283355997672, 3.64872231752742, -11.3536043112269,
    15.3516665852145, -5.88269099434869, 8.28919418458265, -6.5094447586997,
    -14.6658156248808, 11.3479668884978, 20.0398433687628, -6.10463992225897,
    -18.1668205603603, 8.62605521172934, -6.81226946417321, -13.8888824603993,
    -0.835885541534322, 17.7278335212906, -0.955844109966775,
    0.150893281700141, 4.00085308649445, 25.8127242040984, -18.8616968168454,
    16.0905376765747, 9.23271670740177, 14.9725563973619, -12.9805632632526,
    -10.5288633705525, -17.839843438055, 7.15368971971144, -18.8535674130722,
    14.3241382844543, -8.6971935386435, 1.68253058976011, -5.53589921821859,
    -9.17625409915884, 18.5921144376916, 16.0744763067871, -5.46143832078373,
    -12.5560472432887, 2.39470802929194)), class = "data.frame", row.names = c(NA,
-54L), scatter.variable.indices = c(x = 1, y = 2, sizes = 0,
colors = 3, groups = 3), scatter.mult.yvals = TRUE)

alist <- list(list(type = "Marker border", data = "z-Statistic", threstype = "above threshold",
    threshold = "3", color = "#3E7DCC", size = NULL, width = 7,
    offset = NULL, shiftleft = NULL, shiftright = NULL, format = NULL,
    prefix = NULL, suffix = NULL, font.family = NULL, font.weight = NULL,
    font.style = NULL), list(type = "Marker border", data = "z-Statistic",
    threstype = "below threshold", threshold = "-3", color = "#C44E41",
    size = NULL, width = 7, offset = NULL, shiftleft = NULL,
    shiftright = NULL, format = NULL, prefix = NULL, suffix = NULL,
    font.family = NULL, font.weight = NULL, font.style = NULL))

test_that("Check axis order",
{
    pp <- Scatter(dat, scatter.sizes.column = 0, scatter.colors.column = 3, 
            annotation.list = alist)
    expect_true(TestWidget(pp, "scatter-axis-order"))
})

dfChar <- structure(list(x = c(31.9839237228019, NA, 29.527868643733, 13.3040145222493,
        NA, 15.1168042438377, NA), y = structure(7:1, .Label = c("Investment loan",
        "Home loan", "Personal loan", "Credit card", "Transaction account",
        "Overdraft", "Overall NPS"), class = "factor"), sizes = c(8,
        8, 8, 8, 8, 8, 8), colors = c("Bendigo Bank", "Bendigo Bank",
        "Bendigo Bank", "Bendigo Bank", "Bendigo Bank", "Bendigo Bank",
        "Bendigo Bank")), row.names = 71:77, assigned.rownames = TRUE, scatter.variable.indices = c(x = 1,
        y = 2, sizes = 3, colors = 4, groups = 4), class = "data.frame")

dfFac <- structure(list(Score = c(1.08444117668306, 0.42912468881105,
        0.506055892157574, -0.564451999093283, 2.41583517848934), `Cost ($)` = c(0.440547872353227,
        0.693720246937475, 1.44820491038647, 0.0151383003641817, 0.46589754040611
        ), Age = c(48L, 33L, 32L, 41L, 41L), Class = structure(c(4L,
        4L, 4L, 4L, 4L), .Label = c("D", "C", "B", "A"), class = "factor"),
            Sporadic = c(3L, 5L, NA, 8L, 15L), Date = structure(c(17184,
            17182, 17181, 17178, 17167), class = "Date")), row.names = c("c",
        "e", "f", "i", "t"), class = "data.frame")

test_that("Show legend",
{
    for (func in c("LabeledScatter", "Scatter"))
    {
        for (df in c("dfChar", "dfFac"))
        {
            filestem <- paste0(tolower(func), "-legend-", df)
            cmd_def <- paste0("pp <- ", func, "(", df, ")")
            expect_error(suppressWarnings(eval(parse(text = cmd_def))), NA)
            expect_true(TestWidget(pp, filestem))
            
            filestem <- paste0(tolower(func), "-legendshow-", df)
            cmd_show <- paste0("pp <- ", func, "(", df, ", legend.show = 'Show')")
            expect_error(suppressWarnings(eval(parse(text = cmd_show))), NA)
            expect_true(TestWidget(pp, filestem))
        }
    }
            
})


