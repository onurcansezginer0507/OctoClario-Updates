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

library(shiny)



app_dir <- file.path(getwd(), "shiny-app")

if (!dir.exists(app_dir)) {
  cat(">>>ERROR_START<<< Shiny app folder not found: ", app_dir, "\n")
  flush.console()
  stop("App directory missing")
}

port <- httpuv::randomPort()

cat(">>>PORT:", port, "\n")
flush.console()

shiny::runApp(app_dir, host = "127.0.0.1", port = port, launch.browser = FALSE)
