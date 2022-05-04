colorMapping<- reactive({
  if(input$color_by=="nothing"){
    input$col_fixed
  }else{
    rlang::parse_expr(input$color_by)
  }
})

colorScale<-reactive({
  if(input$col_literal || input$color_by=="nothing" ){
    retval <- scale_color_identity(guide="none")
  }else{
    if(!input$col_literal){
      if(input$color_by %in% metaData$discreteVariables ){
        retval <- scale_color_discrete()
      }else{ # surely if it's not a continuous variable, its a discrete one...
        retval <- scale_color_viridis_c()
      }
    }
  }
  
  return(retval)
})