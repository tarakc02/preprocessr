library(preprocessr)
context("tidy_phone")

test_that("tidy_phone handles common telephone inputs", {
    expect_equal(tidy_phone("555-555-5555"), "5555555555")    
    expect_equal(tidy_phone("(555) 555-5555"), "5555555555")
    expect_equal(tidy_phone("1-(555) 555-5555"), "5555555555")
    expect_equal(tidy_phone("+23 (555) 555-5555"), "5555555555")
    expect_equal(tidy_phone("555.555.5555"), "5555555555")
    expect_equal(tidy_phone("555 5555"), "5555555")
    expect_equal(tidy_phone("(H) 555 5555"), "5555555")
    expect_equal(tidy_phone("Business: 555-555-5555"), "5555555555")
})

test_that("tidy_phone handles NA inputs", { 
    expect_equal(tidy_phone(c("555-5555", NA)), c("5555555", NA))
})

test_that("tidy_phone can truncate from the left", {
    expect_equal(tidy_phone("555-555-5555 ext 547", side = "left"), "5555555555")
})

test_that("tidy_phone deals with factors", {
    expect_equal(tidy_phone(factor("555-555-5555")), tidy_phone("555-555-5555"))
})
