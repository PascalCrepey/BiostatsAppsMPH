# Module UI
  
#' @title   mod_logistic_regression_ui and mod_logistic_regression_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_logistic_regression
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_logistic_regression_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_logistic_regression
#' @export
#' @keywords internal
    
mod_logistic_regression_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_logistic_regression_ui("logistic_regression_ui_1")
    
## To be copied in the server
# callModule(mod_logistic_regression_server, "logistic_regression_ui_1")
 
