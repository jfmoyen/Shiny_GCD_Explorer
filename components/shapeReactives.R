shapeMapping<- reactive({
  if(input$shape_by=="nothing"){
    19
  }else{
    rlang::parse_expr(input$shape_by)
  }
})

shapeScale<-reactive({
  if(input$pch_literal || input$shape_by=="nothing"){
    retval <- scale_shape_identity(guide="none")
  }else{
    if(input$shape_by %in% discrete){
      retval <- scale_shape_discrete()
    }else{ # surely if it's not a continuous variable, its a discrete one...
      retval <- scale_shape_binned()
    }
  }
  
  return(retval)
})