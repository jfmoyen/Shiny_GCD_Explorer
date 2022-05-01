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
