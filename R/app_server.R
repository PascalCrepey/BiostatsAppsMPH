#' @import shiny
#' @import data.table
app_server <- function(input, output,session) {
  # List the first level callModules here
  callModule(mod_logistic_regression_server, "logistic_regression_ui_1")
  callModule(mod_power_server, "power_ui_1")
}
