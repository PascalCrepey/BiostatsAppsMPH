# Module UI
# library(data.table)
# library(mosaicData)
# library(ggplot2)
# library(cowplot)



#' @title   mod_probit_regression_ui and mod_probit_regression_server
#' @description  A shiny Module.
#'
#' @param id shiny id
#' @param input internal
#' @param output internal
#' @param session internal
#'
#' @rdname mod_probit_regression
#'
#' @keywords internal
#' @export 
#' @importFrom shiny NS tagList 
mod_probit_regression_ui <- function(id){
  ns <- NS(id)
  tagList(
    withMathJax(),
    plotOutput(ns('plot_regress'),
               height = "500px"),
    
    hr(),
    
    fluidRow(
      column(3,
             h4(strong("Formula: \n")),
             h5("$$p(y=1|x) = \\phi(\\beta_0 + \\beta_1.x)$$"),
             uiOutput(ns("MEM")),
             uiOutput(ns("AME"))
      ),
      column(3,       
             sliderInput(ns("Slider_a"),
                         "Value of \\(\\beta_0\\):",
                         min = -10,
                         max = 10,
                         value = 0,
                         step = 0.01)
      ),
      column(3,
             sliderInput(ns("Slider_b"),
                         "Value of \\(\\beta_1\\):",
                         min = -1,
                         max = 1,
                         value = 0,
                         step = 0.01)
      ),
      column(2,
             actionButton(ns("buttonRegress"), "Regress")
      )
    )
  )
}

# Module Server

#' @rdname mod_probit_regression
#' @import data.table
#' @import ggplot2
#' @import cowplot
#' @export
#' @keywords internal
mod_probit_regression_server <- function(input, output, session){
  ns <- session$ns
  #data("Whickham")
  regress_data <- copy(setDT(Whickham))
  #browser()
  regress_data <- regress_data[,outcome := 1 * (outcome == "Dead")]

  regress_data[, label_age_class := cut(age, breaks = 10, 
                                        include.lowest = TRUE)]
  regress_data[, age_class := (min(age) + max(age)) / 2,
               by = "label_age_class"]
  
  setnames(regress_data,
           c("outcome", "age", "age_class"),
           c("y", "x", "x_class"))
  
  model_glm <- glm(y ~ x,
                   family = binomial(link = "probit"),
                   data = regress_data)
  
  observeEvent(input$buttonRegress, {
    updateSliderInput(session = session,
                      inputId = "Slider_a", 
                      value = as.numeric(model_glm$coefficients[1]))
    updateSliderInput(session = session,
                      inputId = "Slider_b", 
                      value = as.numeric(model_glm$coefficients[2]))
  })
  
  data_estimated <- reactive({
    regress_data[, 
                 .(y, 
                   x,
                   estimated = pnorm(input$Slider_a +
                                       input$Slider_b * x))]
  })
  
  data_aggregated <- reactive({
    regress_data[ , 
                  .(observed = mean(y),
                    estimated = pnorm(input$Slider_a +
                                      input$Slider_b * x_class),
                    observed_logodds = qnorm(mean(y)),
                    estimated_logodds = input$Slider_a +
                      input$Slider_b * x_class),
                  by = "x_class"]})
  
  output$plot_regress <- renderPlot({
    FigPlot_y <- 
      ggplot(data = data_estimated(), 
             aes(x = x)) +
      theme_minimal(16) +
      geom_point(aes(y = y), size = 3) +
      xlab("x") +
      ylab("y") +
      geom_line(aes(y = estimated), colour = "red",
                lwd = 1.5) +
      labs(title = "y=f(x)") +
      theme(plot.title = element_text(hjust = 0.5,
                                      face = "bold.italic"))
    
    FigPlot_p <- 
      ggplot(data = data_aggregated(), 
             aes(x = x_class)) +
      theme_minimal(16) +
      geom_point(aes(y = observed), size = 3) +
      xlab("x") +
      ylab("p(y=1|x)") +
      geom_line(aes(y = estimated), colour = "red",
                lwd = 1.5) +
      labs(title = "p(y=1)=f(x)") +
      theme(plot.title = element_text(hjust = 0.5,
                                      face = "bold.italic"))
    
    FigPlot_logodds <- 
      ggplot(data = data_aggregated(), 
             aes(x = x_class)) +
      theme_minimal(16) +
      geom_point(aes(y = observed_logodds), size = 3) +
      xlab("x") +
      ylab(expression({phi^{-1}}(p(y==1 ~ "|" ~ x)))) +
      geom_line(aes(y = estimated_logodds), colour = "red",
                lwd = 1.5) +
      labs(title = expression({phi^{-1}}(p(y==1 ~ "|" ~ x))==f(x))) +
      theme(plot.title = element_text(hjust = 0.5, 
                                      face = "bold.italic"))
    
    plot_grid(FigPlot_y,
              FigPlot_p, 
              FigPlot_logodds,
              nrow = 1)
  })
  
  output$MEM <- renderUI({ 
    withMathJax(sprintf("\\(MEM_{x}\\) = %.03f%%", round(input$Slider_b * 
                                                              pnorm(input$Slider_a + 
                                                                      input$Slider_b * mean(regress_data$x))*100, 2)))
  })
  output$AME <- renderUI({ 
    withMathJax(sprintf("\\(AME_{x}\\) = %.03f%%", round(mean(input$Slider_b * 
                                                         pnorm(input$Slider_a + 
                                                                 input$Slider_b * (regress_data$x)))*100, 2)))
  })
  
}

