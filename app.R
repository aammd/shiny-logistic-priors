library(shiny)
library(bslib)
library(thematic)
library(ggplot2)


thematic::thematic_shiny(font = "auto")


# take a vector, make a histogram
ggvec2hist <- function(vec){
  ggplot(data.frame(x = vec), aes(x = x)) + geom_histogram(bins = 42)
}

#' modal_interactive UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_modal_interactive_ui <- function(id){
  ns <- NS(id)
  navbarPage(
    withMathJax(),
    theme = bslib::bs_theme(
      bg = "#006d2c", fg = "#edf8e9", primary = "white",
      base_font = font_google("Bree Serif")
    ),
    tags$h1("Prior predictions on the logit scale"),
    helpText(
      "What does a given prior mean? One great way to interpret a prior is via simulation. 
      To do this, first simulate values from the prior, then use those values to simulate observations. Here you can explore the effect of the prior on the intercept of a simple logistic regression."
    ),
    fluidRow(
      uiOutput(ns('formula'))
    ),
    fluidRow(
      column(2,
             numericInput(ns("num"), "sample size", min = 1, max = 500, 
                          step = 1, value = 300),
             numericInput(ns("mean"), "Prior mean", value = 0),
             numericInput(ns("sd"), "Prior standard dev", value = 10),
             actionButton(ns("simulate"), "Simulate!")
      ),
      column(10,
             plotOutput(ns("prior_logit"),   height = 300),
             plotOutput(ns("prior_prob"),    height = 300),
             plotOutput(ns("prior_predict"), height = 300)
      )
    )   
  )
}

#' modal_interactive Server Functions
#'
#' @noRd 
mod_modal_interactive_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    prior_sim <- eventReactive(input$simulate,
                               {rnorm(mean = input$mean,
                                      sd = input$sd,
                                      n = input$num)})
    
    output$prior_logit <- renderPlot({
      ggvec2hist(prior_sim()) + 
        labs(title = "Prior on logit scale") + 
        coord_cartesian(xlim = c(-10,10))
    })
    output$prior_prob <- renderPlot({
      ggvec2hist(plogis(prior_sim())) + 
        labs(title = "Prior on probability scale") + 
        coord_cartesian(xlim = c(0,1))
    })
    
    prior_pred_sim <- reactive({
      rbinom(n = length(prior_sim()),
             size = 20,
             prob = plogis(prior_sim()))})
    
    output$prior_predict <- renderPlot({
      
      ggvec2hist(prior_pred_sim()) + 
        labs(title = "Prior predictions") + 
        coord_cartesian(xlim = c(0,20))
    })
    
    output$formula <- renderUI({withMathJax(paste0('$$
       \\begin{align}
       Y & \\sim \\text{Binomial}(20, p) \\\\
       \\text{logit}(p) &= \\alpha \\\\
       \\alpha & \\sim \\text{Normal}(',
       input$mean,
       ', ',
       input$sd, 
       ') \\\\
       \\end{align}
       $$'))})
    
  })
}

logit_prior <- function(){
  ui <- fluidPage(
    mod_modal_interactive_ui("norm")
  )
  
  server <-  function(input, output, session) {
    mod_modal_interactive_server("norm")
  }
  shinyApp(ui, server)
}

logit_prior()

## To be copied in the UI
# mod_modal_interactive_ui("modal_interactive_ui_1")

## To be copied in the server
# mod_modal_interactive_server("modal_interactive_ui_1")
