# Module UI
  
#' @title   mod_power_ui and mod_power_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_power
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_power_ui <- function(id){
  ns <- NS(id)
  tagList(
  
  )
}
    
# Module Server
    
#' @rdname mod_power
#' @export
#' @keywords internal
    
mod_power_server <- function(input, output, session){
  ns <- session$ns
}
    
## To be copied in the UI
# mod_power_ui("power_ui_1")
    
## To be copied in the server
# callModule(mod_power_server, "power_ui_1")
 
