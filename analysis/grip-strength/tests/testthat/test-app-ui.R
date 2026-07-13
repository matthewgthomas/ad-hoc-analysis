test_that("app UI uses labelled native controls and responsive bslib layout", {
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)
  setwd(Sys.getenv("GRIP_PROJECT_ROOT"))
  dir.create(project_file("artifacts", "cache"), recursive = TRUE, showWarnings = FALSE)
  Sys.setenv(R_USER_CACHE_DIR = project_file("artifacts", "cache"))
  invisible(capture.output(source("app.R", local = environment())))
  html <- htmltools::renderTags(app_ui)$html
  doc <- xml2::read_html(html)
  controls <- xml2::xml_find_all(doc, "//input | //select | //button")
  expect_gt(length(controls), 12)
  labelled <- vapply(controls, function(node) {
    id <- xml2::xml_attr(node, "id")
    aria <- xml2::xml_attr(node, "aria-label")
    title <- xml2::xml_attr(node, "title")
    text <- trimws(xml2::xml_text(node))
    has_for_label <- !is.na(id) && length(xml2::xml_find_all(doc, paste0("//label[@for='", id, "']"))) > 0
    has_parent_label <- length(xml2::xml_find_all(node, "ancestor::label")) > 0
    has_for_label || has_parent_label || (!is.na(aria) && nzchar(aria)) ||
      (!is.na(title) && nzchar(title)) || nzchar(text)
  }, logical(1))
  expect_true(all(labelled))
  positive_tabindex <- xml2::xml_find_all(doc, "//*[@tabindex and number(@tabindex) > 0]")
  expect_length(positive_tabindex, 0)
  expect_match(html, "bslib-sidebar-layout")
  expect_match(html, "bslib-grid")
})
