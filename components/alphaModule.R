alphaUI <- function(id){
  
  alphaMappingUI <- fluidRow(
    column(8,
           selectizeInput(NS(id,"alpha_by"),
                          "Opacity represents...",
                          choices=c("nothing",continuous),
                          selected="nothing",
                          options = list(create = TRUE))
    ),
    
    column(4,
           conditionalPanel(
             condition = "input.alpha_by != 'nothing' ", ## This is a js condition not R syntax !
             checkboxInput(NS(id,"alpha_literal"), "Col already contains Opacity")
           )
    ))
  
  alphaAdjustmentUI <- conditionalPanel(
    condition = "input.alpha_by == 'nothing'", ## This is a js condition not R syntax !
    sliderInput(NS(id,"alpha_adj"),
                "",
                value=0.9,
                min=0,max=1)
  )
  
  alphaRangeUI <- conditionalPanel(
    condition = "input.alpha_by != 'nothing'  && !input.alpha_literal ", ## This is a js condition not R syntax !
    sliderInput(NS(id,"alpha_rng"),
                "",
                value=c(0.4,1),
                min=0,max=1)
  )
}

alphaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    alphaMapping<- reactive({
      if(input$alpha_by=="nothing"){
        input$alpha_adj
      }else{
        rlang::parse_expr(input$alpha_by)
      }
    })
    
    alphaScale<-reactive({
      if(input$alpha_literal || input$alpha_by=="nothing"){
        retval <- scale_alpha_identity(guide="none")
      }else{
        if(input$alpha_by %in% discrete){
          retval <- scale_alpha_discrete(range=input$alpha_rng)
        }else{ # surely if it's not a continuous variable, its a discrete one...
          retval <- scale_alpha_continuous(range=input$alpha_rng)
        }
      }
      
      return(retval)
    })
    
    
    return(list(
      mapping=alphaMapping(),
      scale=alphaScale()
    ))
  })
}


alphaApp <- function() {
  ui <- fluidPage(
    alphaUI("alph")
  )
  server <- function(input, output, session) {
    alphaServer("alph")
  }
  shinyApp(ui, server)  
}
