test_that("it converts dataframe column names to uppercase", {
  # Arrange
  # Act
  # Assert

  com_df <- data.frame(unitid = 123456,
                       majorcip = c(2.34, 12.34, 2.3400, 2, 12),
                       degreelevel = 17,
                       studentid = 900)

  df_actual <- prep_com_data_frame(com_df)
  expect_equal(sum(grepl(colnames(df_actual), pattern = "[[:lower:]]")), 0)
})

test_that("it applies character data type conversions.", {

  com_df <- data.frame(unitid = 123456,
                       majorcip = c(2.34, 12.34, 2.3400, 2, 12),
                       degreelevel = 17,
                       studentid = 900)

  expect_type(prep_com_data_frame(com_df)$UNITID, 'character')
  expect_type(prep_com_data_frame(com_df)$MAJORCIP, 'character')
  expect_type(prep_com_data_frame(com_df)$DEGREELEVEL, 'character')
  expect_type(prep_com_data_frame(com_df)$STUDENTID, 'character')
})

test_that("it detects invalid cip code formats and throws an error", {

  # not 1, 2, or 6 digits and no period in cip code
  df_actual <- data.frame(unitid = 123456,
                          degreelevel = 5,
                          student_id = 100,
                          majorcip = 345)

  expect_error(prep_com_data_frame(df_actual))
})

test_that("it detects cip codes with mixed data types and throws an error", {

  # mixing types (does it mean string instead of numeric or float?)
  df_actual <- data.frame(unitid = 123456,
                          degreelevel = 5,
                          student_id = 100,
                          majorcip = c("34.", "34.01", "340000"))

  expect_error(prep_com_data_frame(df_actual))
})

test_that("it handles CIP codes appropriately", {

  com_df <- data.frame(unitid = 123456,
                       majorcip = c(2.34, 12.34, 2.3400, 2, 12),
                       degreelevel = 17,
                       studentid = 900)

  expect_equal(toString(prep_com_data_frame(com_df)$MAJORCIP), "02.3400, 12.3400, 02.3400, 02.0000, 12.0000")

  com_df2 <- data.frame(unitid = 123456,
                        majorcip = c("000000", "111111", "040000"),
                        degreelevel = 7,
                        studentid = 999)

  expect_equal(toString(prep_com_data_frame(com_df2)$MAJORCIP), "00.0000, 11.1111, 04.0000")
})
