library(preprocessr)
context("tidy_zip")

test_that("tidy_zip handles common zip code inputs", {
    expect_equal(tidy_zip("94720"), "94720")    
    expect_equal(tidy_zip("94720-4200"), "94720")
    expect_equal(tidy_zip("123"), "00123")
    expect_equal(tidy_zip("123-4700"), "00123")
})

test_that("tidy_zip deals with factors", {
    expect_equal(tidy_zip(factor("123")), tidy_zip("123"))
})

test_that("tidy_zip deals with numeric inputs", {
    expect_equal(tidy_zip(123), tidy_zip("123"))
    expect_equal(tidy_zip(94720), tidy_zip("94720"))
})

test_that("tidy_string handles NA inputs", { 
    expect_equal(tidy_zip(c("123", NA)), c("00123", NA))
})