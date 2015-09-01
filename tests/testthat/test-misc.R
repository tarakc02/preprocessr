library(preprocessr)
context("miscellaneous text cleaner uppers")

test_that("camel_underscore", {
    expect_equal(camel_underscore("thisIsALongBumpyName"), "this_is_a_long_bumpy_name")
    expect_equal(camel_underscore("ThisIsALongBumpyName"), "this_is_a_long_bumpy_name")
})

test_that("underscore_camel", { 
    expect_equal(underscore_camel("what_a_name"), "whatAName")
    expect_equal(underscore_camel("Caps_In_THE_NAME"), "capsInTheName")
})
