#### Facetting
#' We have 3 cases for faceting. They are controlled by input$facet_by,
#' which may contain three sort of content:
#' 1) "nothing" - we make a single graph without facets;
#' 2) A discrete data-var - we facet based on this (discrete) variable
#' 3) A continuous data-var - this one is more tricky. We do several things in this case:
#' a) Show an UI element "splitter" (controlled in UI) 
#' b) Calculate default values for splitting the range (defaultSplit)
#' c) (once) Update the splitter widget to display them (observeEvent)
#' d) Read the values of the splitter and actually split the data (faceting)
#' Before (c), we must temporarily stop updating the reactivity graph to avoid 
#' redrawing the graph twice (once because we changed the variable and once because we changed the split)


## As a convenience, the actual data that will be split
facetingData<-reactive({
  calcCoreTibble(the_data,input$facet_by)
})

## Automatic data splitting
defaultSplit <- reactive({
  
  # Split in three (with NAs this will make 4 classes more often)
  qt<- quantile(facetingData(),c(0.33,0.66),na.rm=T )
  
  #  clever manipulation to get an approximate rounding ! 
  dec <- 1- floor(log10(qt[2]/5))
  round(qt,dec)
})

## Here we make the proper faceting command
faceting <- reactive({
  
  if(input$facet_by=="nothing"){
    return(NULL)    
  }
  
  if(input$facet_by %in% discrete){
    return( facet_wrap(vars(!!rlang::parse_expr(input$facet_by))) )
  }else{
    # Read the break points from the input
    cutValues <- input$splitter %>% 
      str_split(";") %>%
      unlist() %>%
      as.numeric
    
    # Add extremities
    cutValues <- c(0,cutValues,Inf)
    
    # Cut the data range according to the computed values and make factors
    fct <- cut( facetingData(),cutValues )
    
    return(facet_wrap(fct)) 
    ## AH-HA ! we facet based on fct not vars(fct)
    # i.e. on a factor-vector, NOT a col in the dataframe !
  } 
})

## This will be fired whenever the user changes input$facet_by to a new variable
observeEvent(input$facet_by,
             {
               # We prevent from redrawing the graph (or indeed, doing anything at all
               # with info coming from the splitter field) until we have computed the
               # new values (and the rest of the reactive graph)    
               freezeReactiveValue(input, "splitter")
               
               if( !(input$facet_by %in% discrete || input$facet_by=="nothing" ) ){
                 updateTextInput(inputId = "splitter",
                                 value=paste(defaultSplit(),collapse=";"))
               }
             })