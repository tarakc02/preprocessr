library(preprocessr)
context("tidy_string")

test_that("tidy_string removes unwanted characters", {
    expect_equal(tidy_string("123 Main St", lowercase=TRUE), "123mainst")
    expect_equal(tidy_string("123 Main St", lowercase=FALSE), "123MainSt")
    expect_equal(tidy_string("123-   Main    St.,"), "123mainst")
    expect_equal(tidy_string("123 Main St.", length = 8), "123mains")
})

test_that("tidy_string is not tripped up by factors", {
    expect_equal(tidy_string(factor("123 Main St")), tidy_string("123 Main St"))
})

test_that("tidy_string handles custom regexes", {
    expect_equal(tidy_string("123 Main St", pattern = "St.?|\\s"), "123main")
})

test_that("tidy_string handles NA inputs", { 
    expect_equal(tidy_string(c("123 Main St.", NA)), c("123mainst", NA))
})