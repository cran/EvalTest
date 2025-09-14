#' Launch the EvalTest Shiny application
#'
#' This function starts the Shiny application included in the EvalTest package,
#' which aims to evaluate diagnostic tests performance.
#'
#' @return The function does not return a value; it launches a Shiny application.
#'
#' @examples
#' if (interactive()) {
#'   library(EvalTest)
#'   run_app()
#' }
#'
#' @importFrom shiny shinyApp fluidRow fileInput radioButtons selectInput numericInput
#' @importFrom shiny actionButton icon plotOutput downloadButton tableOutput
#' @importFrom shiny renderTable renderPlot req observeEvent eventReactive reactiveVal
#' @importFrom shiny showModal modalDialog updateSelectInput downloadHandler
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody box
#' @importFrom pROC roc coords auc ci.auc ci.se ggroc
#' @importFrom ggplot2 annotate geom_ribbon labs ggtitle coord_cartesian ggsave
#' @importFrom DT datatable DTOutput renderDT
#' @importFrom readxl read_excel
#' @importFrom openxlsx createWorkbook addWorksheet writeData saveWorkbook
#' @importFrom ggpubr theme_classic2
#' @importFrom stats complete.cases
#' @importFrom binom binom.confint


#' @export
run_app <- function() {
  # Get the path of the Shiny app inside the package
  app_dir <- system.file("app", package = "EvalTest")
  if (app_dir == "") {
    stop("Could not find the app directory. Try re-installing the package.", call. = FALSE)
  }
  # Run the Shiny app
  shiny::runApp(app_dir, display.mode = "auto")
}
