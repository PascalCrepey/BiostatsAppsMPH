#' @import shiny
#' @import shinydashboard
app_ui <- function() {
  sidebar <- dashboardSidebar(
    sidebarMenu(id = "sideMenu",
                menuItem("Logistic Regression", icon = icon("th"), tabName = "LogisticRegression",
                         badgeLabel = "new", badgeColor = "green", selected = TRUE),
                menuItem("Power", icon = icon("th"), tabName = "Power",
                         badgeLabel = "new", badgeColor = "green", selected = FALSE)
    )
  )
  body <- dashboardBody(tabItems(
    tabItem(tabName = "LogisticRegression",
            fluidPage(
              mod_logistic_regression_ui("logistic_regression_ui_1")
            )
    ),
    tabItem(tabName = "Power",
            fluidPage(
              mod_power_ui("power_ui_1")
            )
    )
  ))
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    dashboardPage(
      dashboardHeader(title = "BiostatApps"),
      sidebar,
      body
    )
  )
}

#' @import shiny
golem_add_external_resources <- function(){
  
  # addResourcePath(
  #   'www', system.file('app/www', package = 'BiostatApps')
  # )
 
  tags$head(
    golem::activate_js(),
    golem::favicon()
    # Add here all the external resources
    # If you have a custom.css in the inst/app/www
    # Or for example, you can add shinyalert::useShinyalert() here
    #tags$link(rel="stylesheet", type="text/css", href="www/custom.css")
  )
}
