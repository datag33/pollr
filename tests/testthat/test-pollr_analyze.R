test_that("pollr_analyze works with a valid input", {

  pollr_analyze_results <- pollr_analyze(
    survey_data = df_survey,
    question_varname = "A1_title"
  )

  expect_type(pollr_analyze_results, "list")
  #expect_named(pollr_analyze_results, c("info", "data", "design", "results", "tab", "plot"))
  expect_equal(sum(pollr_analyze_results$results$prop), 100)
  expect_equal(sum(pollr_analyze_results$results$n), nrow(df_survey))
})


test_that("pollr_analyze fails with invalid inputs", {

  expect_error(
    pollr_analyze(survey_data = df_survey, question_varname = "A5"),
    "Variable `A5` not found in `survey_data`."
  )

  expect_error(
    pollr_analyze(survey_data = df_survey, question_varname = 3),
    "`question_varname` must be a single character string"
  )

  expect_error(
    pollr_analyze(survey_data = "not_a_dataframe", question_varname = "A1_title"),
    "`survey_data` must be a data.frame"
  )


})
