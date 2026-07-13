test_that("Shiny server calculates and validates personal inputs", {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(Sys.getenv("GRIP_PROJECT_ROOT"))
  invisible(capture.output(source("app.R", local = environment())))
  test_server <- function(input, output, session) grip_server(input, output, session, data = app_data, models = app_models)
  shiny::testServer(test_server, {
    session$setInputs(calculate = 0)
    session$flushReact()
    session$setInputs(
      outcome = "best_single_grip", sex = "Female", age = 40, height_cm = 165,
      size_method = "BMI", bmi = 24, observed_grip_kg = NA_real_, advanced = FALSE,
      calculate = 1
    )
    session$flushReact()
    value <- session$userData$result()
    expect_true(value$ok)
    expect_true(is.finite(value$prediction$predicted))
    expect_false(value$comparison$performed)

    session$setInputs(age = 10, calculate = 2)
    session$flushReact()
    value <- session$userData$result()
    expect_false(value$ok)
    expect_match(value$error, "between 18 and 80")
  })
})
