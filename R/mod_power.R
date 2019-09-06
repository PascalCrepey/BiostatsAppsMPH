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
     tabsetPanel(id = ns("general"), 
                tabPanel("You don't know it but H0 is TRUE ! Be careful to type 1 error",        
                         fluidRow(
                           # Sidebar with a slider input for number of bins
                           column(4, tabsetPanel(id = ns("navbar"),
                                                 tabPanel("Parameters",wellPanel(
                                                   sliderInput(ns("refbloodpressure"),"Reference blood pressure",
                                                               min=100,
                                                               max=150,
                                                               value=130, animate=T, step=1),
                                                   sliderInput(ns("alpha"),"alpha",
                                                               min=0.01,
                                                               max=0.2,
                                                               value=0.05, animate=T, step=0.01),
                                                   sliderInput(ns("sd"),"standard deviation",
                                                               min=5,
                                                               max=70,
                                                               value=25, animate=T, step=5),
                                                   sliderInput(ns("groupsize"), "Group size",
                                                               min = 50,
                                                               max = 1000,
                                                               value = 100, animate=T, step=50),
                                                   
                                                   sliderInput(ns("nbstudies"),"Number of studies",
                                                               min=1,
                                                               max=10,
                                                               value=1, animate=T, step=1)
                                                   
                                                 ))
                           )),
                           # Show a plot of the generated distribution
                           column(8,tabsetPanel(
                             tabPanel("Estimated blood pressure",
                                      #h4("Results are shown for similar studies using the same case-control settings."),
                                      fluidRow(column(12,plotOutput(ns("effPlot")))),
                                      hr(),
                                      #h5("VE is computed with 1-OR, OR is computed with a logistic regression on each simulated case-control settings."),
                                      fluidRow(column(12,verbatimTextOutput(ns("summaryEfficacy"))))
                             )
                           )
                           )
                         )),
                tabPanel("You don't know it but H0 is FALSE ! Be careful to type 2 error",
                         fluidRow(
                           # Sidebar with a slider input for number of bins
                           column(4, tabsetPanel(id = ns("navbar2"),
                                                 tabPanel("Parameters",wellPanel(
                                                   sliderInput(ns("refbloodpressure2"),"Reference blood pressure",
                                                               min=100,
                                                               max=150,
                                                               value=130, animate=T, step=1),
                                                   sliderInput(ns("truebloodpressure"),"True blood pressure in the study population",
                                                               min=100,
                                                               max=150,
                                                               value=132, animate=T, step=1),
                                                   sliderInput(ns("sd2"),"standard deviation",
                                                               min=5,
                                                               max=100,
                                                               value=25, animate=T, step=5),
                                                   #                                        sliderInput("effectsize2", "Estimated effect size",
                                                   #                                                    min = 1,
                                                   #                                                    max = 10,
                                                   #                                                    value = 2, animate=T, step=0.5),
                                                   #                                        h4("To change the conditions of the studies:"), 
                                                   sliderInput(ns("groupsize2"), "Group size",
                                                               min = 50,
                                                               max = 1000,
                                                               value = 100, animate=T, step=50),
                                                   
                                                   sliderInput(ns("nbstudies2"),"Number of studies",
                                                               min=1,
                                                               max=10,
                                                               value=1, animate=T, step=1)
                                                   
                                                 ))
                           )),
                           # Show a plot of the generated distribution
                           column(8,tabsetPanel(
                             tabPanel("Estimated blood pressure",
                                      #h4("Results are shown for similar studies using the same case-control settings."),
                                      fluidRow(column(12,plotOutput(ns("effPlot2")))),
                                      hr(),
                                      #h5("VE is computed with 1-OR, OR is computed with a logistic regression on each simulated case-control settings."),
                                      fluidRow(column(12,verbatimTextOutput(ns("summaryEfficacy2")))) 
                             ), 
                             tabPanel("Distributions of measures", 
                                      fluidRow(column(12,plotOutput(ns("distributions")))))
                           )
                           )
                         )
                )
    )
  
  )
}
    
# Module Server
    
#' @rdname mod_power
#' @export
#' @keywords internal
    
mod_power_server <- function(input, output, session){
  ns <- session$ns
  
  resultsOneRef <- reactive({ 
    
    sizeExposed=input$groupsize
    
    nsim=input$nbstudies
    res=lapply(1:nsim, function(.nn) {
      
      exposed=rnorm(sizeExposed, mean=input$refbloodpressure, sd=input$sd)
      
      t=t.test(exposed, conf.level = 1-input$alpha)
      res=data.frame(
        study=.nn,
        estEfficacy=t$estimate,
        ymax = t$conf.int[2], 
        ymin = t$conf.int[1]
      )
      return(res)
    })
    res=rbindlist(res)
    return(res)
  })
  
  resultsTwoRef <- reactive({ 
    
    sizeExposed=input$groupsize2
    
    nsim=input$nbstudies2
    res=lapply(1:nsim, function(.nn) {
      
      exposed=rnorm(sizeExposed, mean=input$truebloodpressure, sd=input$sd2)
      
      t=t.test(exposed)
      res=data.frame(
        study=.nn,
        estEfficacy=t$estimate,
        ymax = t$conf.int[2], 
        ymin = t$conf.int[1]
      )
      return(res)
    })
    res=rbindlist(res)
    return(res)
  })
  
  output$summaryEfficacy <- renderPrint({
    data=resultsOneRef()
    #print(data[,estEfficacy])
    cat("Estimated mean blood pressure:", round(mean(data[,estEfficacy]),2),paste0(round((1-input$alpha)*100,0),"%CI ["),round(mean(data[,ymin]),2),";",round(mean(data[,ymax]),2),"].\n")
    
  })
  output$summaryEfficacy2 <- renderPrint({
    data=resultsTwoRef()
    #print(data[,estEfficacy])
    myttestpower=power.t.test(n=input$groupsize2, sd=input$sd2, delta=-(input$refbloodpressure2-input$truebloodpressure), sig.level = 0.05, alternative = "one.sided")
    #     cat(input$groupsize2)
    #     cat(input$refbloodpressure2)
    #     cat(input$truebloodpressure)
    #     print(myttestpower)
    cat("Achieved power: ",paste0(round(myttestpower$power*100,1),"%\n"))
    cat("We have a ", paste0(100-round(myttestpower$power*100,1),"%"), "probability of failing to reject H0 when H0 is false.")
    #cat("Estimated mean blood pressure:", round(mean(data[,estEfficacy]),2),paste0(round((1-input$alpha)*100,0),"%CI ["),round(mean(data[,ymin]),2),";",round(mean(data[,ymax]),2),"].\n")
    
  })
  
  output$effPlot <- renderPlot({
    data=resultsOneRef()
    
    p <- ggplot(data, aes(x=study,y=estEfficacy))
    p <- p + geom_point() + geom_errorbar(aes(ymin=ymin,ymax=ymax), width=0.2) + scale_y_continuous(limits = c(100,150)) +scale_x_continuous(limits = c(0,input$nbstudies+1))
    p <- p + geom_hline(yintercept=input$refbloodpressure, color="red") + annotate("text", label = "True blood pressure", x = 0, y = input$refbloodpressure+1, size = 4, hjust = 0, colour = "red")
    #p <- p + geom_hline(yintercept=input$refbloodpressure-input$effectsize, color="red")
    if(input$nbstudies>1) p <- p + labs(x="Different studies", y="Estimated blood pressure") 
    else p <- p + labs(x="Study", y="Estimated blood pressure")
    p + theme(axis.text.x=element_blank(),#axis.line=element_blank(),
              #axis.text.y=element_blank(),#axis.ticks=element_blank(),
              #axis.title.y=element_blank(),
              legend.position="none"#, panel.background=element_blank()#,panel.border=element_blank(),panel.grid.major=element_blank(),
              #panel.grid.minor=element_blank(),plot.background=element_blank()
    )
    #     p<-ggplot(data=data, aes(timeF))
    #     if("Scenario 0" %in% input$scenarios) p<- p + geom_line(aes(y=get(paste0("newCasesS0")), colour=paramsS0()$scenarioName), size=2, position=position_jitter(width= 1.5, height= 0)) 
    #     p<-p+ theme(legend.position = "top") + 
    #       guides(color=guide_legend(title="Scenarios:")) + xlab("") +
    #       scale_y_continuous(name='New influenza symptomatic infection per day')
    #     p
  })
  output$effPlot2 <- renderPlot({
    data=resultsTwoRef()
    
    p <- ggplot(data, aes(x=study,y=estEfficacy))
    p <- p + geom_point() + geom_errorbar(aes(ymin=ymin,ymax=ymax), width=0.2) + scale_y_continuous(limits = c(100,150))+scale_x_continuous(limits = c(0,input$nbstudies2+1))
    p <- p + geom_hline(yintercept=input$truebloodpressure, color="red")+ annotate("text", label = "True blood pressure in the study population", x = 0, y = input$truebloodpressure+1, size = 4, hjust = 0, colour = "red")
    p <- p + geom_hline(yintercept=input$refbloodpressure2, color="black")+ annotate("text", label = "Reference blood pressure", x = 0, y = input$refbloodpressure2+1, size = 4, hjust = 0, colour = "black")
    #p <- p + geom_hline(yintercept=input$refbloodpressure-input$effectsize, color="red")
    if(input$nbstudies>1) p <- p + labs(x="Different studies", y="Estimated blood pressure") 
    else p <- p + labs(x="Study", y="Estimated blood pressure")
    p + theme(axis.text.x=element_blank(),#axis.line=element_blank(),
              #axis.text.y=element_blank(),#axis.ticks=element_blank(),
              #axis.title.y=element_blank(),
              legend.position="none"#, panel.background=element_blank()#,panel.border=element_blank(),panel.grid.major=element_blank(),
              #panel.grid.minor=element_blank(),plot.background=element_blank()
    )
    #     p<-ggplot(data=data, aes(timeF))
    #     if("Scenario 0" %in% input$scenarios) p<- p + geom_line(aes(y=get(paste0("newCasesS0")), colour=paramsS0()$scenarioName), size=2, position=position_jitter(width= 1.5, height= 0)) 
    #     p<-p+ theme(legend.position = "top") + 
    #       guides(color=guide_legend(title="Scenarios:")) + xlab("") +
    #       scale_y_continuous(name='New influenza symptomatic infection per day')
    #     p
  })
  
  output$distributions<-renderPlot({
    minval=min(input$refbloodpressure2, input$truebloodpressure)-10*ceiling(3*input$sd2/sqrt(input$groupsize2)/10)
    maxval=max(input$refbloodpressure2, input$truebloodpressure)+10*ceiling(3*input$sd2/sqrt(input$groupsize2)/10)
    #reference distrib
    data=data.table(x=seq(minval, maxval, 0.1))
    data[, normRef:=dnorm(x, mean=input$refbloodpressure2, sd = input$sd2/sqrt(input$groupsize2))/sqrt(dchisq(x,input$groupsize2)/input$groupsize2)]
    data[, normPop:=dnorm(x, mean=input$truebloodpressure, sd = input$sd2/sqrt(input$groupsize2))/sqrt(dchisq(x,input$groupsize2)/input$groupsize2)]
    p <- ggplot(data, aes(x=x)) +
      geom_line(aes(y=normRef), color="black") +
      geom_line(aes(y=normPop), color="blue") +
      geom_abline(slope=0, intercept = 0)+
      geom_vline(xintercept = input$refbloodpressure2, color="black")+
      geom_vline(xintercept = input$truebloodpressure, color="blue") +
      geom_vline(xintercept=input$refbloodpressure2+1.96*input$sd2/sqrt(input$groupsize2), color="green")+
      scale_y_continuous(limits=c(0,1.1*max(data[,.(normRef, normPop)])))
    p
  })
  
}
    
## To be copied in the UI
# mod_power_ui("power_ui_1")
    
## To be copied in the server
# callModule(mod_power_server, "power_ui_1")
 
