source("renv/activate.R")
local({
  project_library <- Sys.getenv("R_LIBS_USER", unset = "")
  if (nzchar(project_library) && dir.exists(project_library)) .libPaths(c(project_library, .libPaths()))
  options(repos = c(CRAN = "https://cloud.r-project.org"))
})
