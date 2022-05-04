sizeMapping<- reactive({
  if(input$size_by=="nothing"){
    input$size_adj
  }else{
    if(input$cex_literal){
      parsestr <- paste(input$size_by,"*",input$size_adj)
      rlang::parse_expr(parsestr)
    }else{
      rlang::parse_expr(input$size_by)
    }
    
  }
})

sizeScale<-reactive({
  if(input$cex_literal || input$size_by=="nothing"){
    retval <- scale_size_identity(guide="none")
  }else{
    if(input$size_by %in% metaData$discreteVariables ){
      retval <- scale_size_discrete(range=input$size_rng)
    }else{ # surely if it's not a continuous variable, its a discrete one...
      retval <- scale_size_continuous(range=input$size_rng)
    }
  }
  
  return(retval)
})