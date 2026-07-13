source(file.path("R", "load_all.R"))

app_data <- readRDS(file.path("data", "nhanes_grip_adults.rds"))
app_models <- readRDS(file.path("models", "grip_models.rds"))

person_from_input <- function(input) {
  use_advanced <- isTRUE(input$advanced)
  list(
    age = input$age,
    sex = input$sex,
    height_cm = input$height_cm,
    bmi = if (identical(input$size_method, "BMI")) input$bmi else NULL,
    weight_kg = if (identical(input$size_method, "Weight")) input$weight_kg else NULL,
    observed_grip_kg = input$observed_grip_kg,
    outcome = input$outcome,
    arm_circumference_cm = if (use_advanced) input$arm_circumference_cm else NULL,
    arm_length_cm = if (use_advanced) input$arm_length_cm else NULL,
    activity_met_min_week = if (use_advanced) input$activity_met_min_week else NULL,
    handedness = if (use_advanced) input$handedness else NULL,
    any_hand_pain = if (use_advanced) input$any_hand_pain else NULL,
    prior_surgery = if (use_advanced) input$prior_surgery else NULL,
    posture = if (use_advanced) input$posture else NULL
  )
}

grip_server <- function(input, output, session, data = app_data, models = app_models) {
  result <- shiny::eventReactive(input$calculate, {
    tryCatch({
      person <- person_from_input(input)
      prediction <- predict_grip(person, models)
      comparison <- compare_grip(person, prediction, analytic_data = data)
      list(ok = TRUE, prediction = prediction, comparison = comparison)
    }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))
  }, ignoreInit = TRUE)
  session$userData$result <- result

  output$error_message <- shiny::renderUI({
    x <- result()
    if (isTRUE(x$ok)) return(NULL)
    shiny::div(class = "alert alert-danger", role = "alert", x$error)
  })

  output$prediction_box <- shiny::renderUI({
    x <- result()
    shiny::req(isTRUE(x$ok))
    bslib::value_box(
      title = "Expected grip",
      value = paste0(round(x$prediction$predicted, 1), " kg"),
      p(if (x$prediction$model_type == "extended") "Extended model" else "Core model")
    )
  })

  output$interval_box <- shiny::renderUI({
    x <- result()
    shiny::req(isTRUE(x$ok))
    bslib::value_box(
      title = "95% individual interval",
      value = paste0(round(x$prediction$lower, 1), "–", round(x$prediction$upper, 1), " kg"),
      p(x$prediction$reference_scope)
    )
  })

  output$comparison_box <- shiny::renderUI({
    x <- result()
    shiny::req(isTRUE(x$ok))
    c <- x$comparison
    if (!isTRUE(c$performed)) {
      bslib::value_box(title = "Personal comparison", value = "Not run",
                       p("Add an observed grip value to calculate percentiles."))
    } else {
      bslib::value_box(
        title = "Personal comparison",
        value = c$label,
        p(paste0("Adjusted percentile ", round(c$adjusted_percentile),
                 "; two-sided p = ", format.pval(c$empirical_p, digits = 2, eps = 0.001)))
      )
    }
  })

  marker_person <- shiny::reactive({
    x <- result()
    shiny::req(isTRUE(x$ok))
    p <- x$prediction$person
    p$predicted <- x$prediction$predicted
    p
  })

  output$distribution_plot <- plotly::renderPlotly({
    p <- plot_weighted_distribution(data, marker_person()$outcome, marker_person())
    plotly::ggplotly(p, tooltip = c("x", "y"), dynamicTicks = TRUE) |>
      plotly::layout(legend = list(orientation = "h"), margin = list(l = 60, r = 20, b = 60, t = 75))
  })

  output$age_plot <- plotly::renderPlotly({
    p <- plot_age_percentiles(data, marker_person()$outcome, marker_person())
    plotly::ggplotly(p, tooltip = c("x", "y"), dynamicTicks = TRUE) |>
      plotly::layout(margin = list(l = 60, r = 20, b = 60, t = 75))
  })

  output$details <- shiny::renderUI({
    x <- result()
    shiny::req(isTRUE(x$ok))
    pred <- x$prediction
    comp <- x$comparison
    comparison_text <- if (isTRUE(comp$performed)) {
      paste0(
        "Observed grip: ", round(comp$observed, 1), " kg. Raw age/sex percentile: ",
        round(comp$raw_percentile), ". Adjusted percentile: ", round(comp$adjusted_percentile),
        ". Empirical two-sided p-value: ", format.pval(comp$empirical_p, digits = 3, eps = 0.001), "."
      )
    } else {
      comp$message
    }
    shiny::tagList(
      shiny::h3("How to read this result"),
      shiny::p(paste0("The survey-weighted model predicts ", round(pred$predicted, 1),
                      " kg. Its weighted GAM sensitivity estimate is ", round(pred$gam_sensitivity, 1), " kg.")),
      shiny::p(comparison_text),
      shiny::p(class = "text-secondary", "A statistically unusual result is not a diagnosis. Device, protocol, posture, effort, pain, and surgery can affect grip measurements.")
    )
  })

  list(result = result)
}

app_ui <- bslib::page_sidebar(
  title = "Grip strength reference",
  theme = bslib::bs_theme(version = 5, primary = "#2166AC"),
  fillable = TRUE,
  sidebar = bslib::sidebar(
    width = 360,
    shiny::p(class = "text-secondary", "US NHANES 2011–2014 adults aged 18–80. Required fields have an asterisk."),
    shiny::selectInput("outcome", "Measurement type *",
                       choices = c("Best single hand" = "best_single_grip",
                                   "Sum of best right + left" = "bilateral_grip")),
    shiny::selectInput("sex", "NHANES comparison sex *",
                       choices = c("Select…" = "", "Female" = "Female", "Male" = "Male"), selected = ""),
    shiny::numericInput("age", "Age (years) *", value = NA, min = 18, max = 80, step = 1),
    shiny::numericInput("height_cm", "Height (cm) *", value = NA, min = 120, max = 230, step = 0.1),
    shiny::radioButtons("size_method", "Body-size input *", choices = c("BMI", "Weight"), inline = TRUE),
    shiny::conditionalPanel("input.size_method === 'BMI'",
                            shiny::numericInput("bmi", "BMI (kg/m²)", value = NA, min = 12, max = 70, step = 0.1)),
    shiny::conditionalPanel("input.size_method === 'Weight'",
                            shiny::numericInput("weight_kg", "Weight (kg)", value = NA, min = 30, max = 300, step = 0.1)),
    shiny::numericInput("observed_grip_kg", "Observed grip (kg, optional)", value = NA, min = 1, max = 200, step = 0.1),
    shiny::checkboxInput("advanced", "Use optional extended factors", value = FALSE),
    shiny::conditionalPanel(
      "input.advanced",
      shiny::numericInput("arm_circumference_cm", "Arm circumference (cm)", value = NA, min = 10, max = 70, step = 0.1),
      shiny::numericInput("arm_length_cm", "Arm length (cm)", value = NA, min = 20, max = 60, step = 0.1),
      shiny::numericInput("activity_met_min_week", "Activity (MET-min/week)", value = NA, min = 0, max = 50000, step = 10),
      shiny::selectInput("handedness", "Handedness", c("Right", "Left", "Ambidextrous")),
      shiny::selectInput("any_hand_pain", "Recent hand pain", c("No", "Yes")),
      shiny::selectInput("prior_surgery", "Prior hand/wrist surgery", c("No", "Yes")),
      shiny::selectInput("posture", "Test posture", c("Standing", "Seated"))
    ),
    shiny::actionButton("calculate", "Calculate reference", class = "btn-primary w-100"),
    shiny::p(class = "small text-secondary mt-3", "Leave observed grip blank for prediction only. Use the same measurement protocol as the selected outcome.")
  ),
  shiny::uiOutput("error_message"),
  bslib::layout_columns(
    shiny::uiOutput("prediction_box"),
    shiny::uiOutput("interval_box"),
    shiny::uiOutput("comparison_box"),
    col_widths = c(4, 4, 4)
  ),
  bslib::navset_card_tab(
    bslib::nav_panel("Distribution", plotly::plotlyOutput("distribution_plot", height = "560px")),
    bslib::nav_panel("Age percentiles", plotly::plotlyOutput("age_plot", height = "560px")),
    bslib::nav_panel("Interpretation", shiny::uiOutput("details"))
  ),
  bslib::card(
    bslib::card_header("Scope and limitations"),
    shiny::p("This is a non-clinical population reference for the 2011–2014 non-institutionalized US population. NHANES released a binary male/female comparison category for these data. Grip is not a complete measure of upper-body strength.")
  )
)

app <- shiny::shinyApp(ui = app_ui, server = grip_server)
app
