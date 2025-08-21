# Determine whether running inside asar or asar.unpacked
jre_path <- file.path(getwd(), "jre")
Sys.setenv(JAVA_HOME = jre_path)

jre_bin    <- file.path(jre_path, "bin")
jre_server <- file.path(jre_bin, "server")
Sys.setenv(PATH = paste(
  jre_server,
  jre_bin,
  Sys.getenv("PATH"),
  sep = .Platform$path.sep
))

suppressPackageStartupMessages({
  library(shiny)
  library(httpuv)
})

args <- commandArgs(trailingOnly = TRUE)
portArg <- NA_integer_
if (length(args) >= 2 && args[1] == "--port"){
  portArg <- suppressWarnings(as.integer(args[2]))
}

if (is.na(portArg) || portArg <= 0){
  portArg <- httpuv::randomPort()
}
cat(">>>PORT:", portArg, "\n"); flush.console()
app_dir <- file.path(getwd(), "shiny-app")
options(shiny.launch.browser = FALSE)
shiny::runApp(app_dir, host = "127.0.0.1", port = portArg, launch.browser = FALSE)
