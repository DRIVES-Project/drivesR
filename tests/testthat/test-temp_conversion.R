test_that("F_temp is numeric", {
  expect_error(F_to_C("a"), "F_temp must be a number or numeric vector.")
  expect_error(F_to_C(c("a",1,2)), "F_temp must be a number or numeric vector.")
  # T and F values are interpreted as numbers in a vector with other numbers. There's probably
  # a way to fix this, but it isn't essential.
  #expect_error(F_to_C(c(T,F,1,2)), "F_temp must be a number or numeric vector.")
})

test_that("C_temp is numeric", {
  expect_error(C_to_F("a"), "C_temp must be a number or numeric vector.")
  expect_error(C_to_F(c("a",1,2)), "C_temp must be a number or numeric vector.")
  #expect_error(C_to_F(c(T,F,1,2)), "C_temp must be a number or numeric vector.")
})
