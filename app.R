library(shiny)
library(tidyverse)
library(readr)
library(colourpicker)
#library(plotly)
library(shinyWidgets)
library(readxl)
#library(shinyjs)

##### DEBUG
reactlog::reactlog_enable()
#reactiveConsole(TRUE) # For inline experiments, not real debugging...

#*************************#
####     DATA PREP     ####
#*************************#
# Read file
# the_data <- read_delim("atacazo.txt", delim = "\t", 
#                       escape_double = FALSE, trim_ws = TRUE)
# the_name <- "Atacazo"

the_data <- read_excel("Classif_Var_LFB_NA.xlsx",.name_repair="universal") %>% 
  sample_frac(1/10) %>% 
  rowid_to_column("ID") 
  


the_name <- "Granites Jacob et al. 21"
## Names are replaced by "syntaxic" names. This is a hassle for A/CNK etc but makes life generally easier
# Another (best?) way would be to backtick every selection (with paste() ... )
# But how would that play out with calculated values ? Badly, I think.

# # Add custom columns
# the_data %>% add_column(user_tag1=NA,
#                         user_tag2=NA,
#                         user_tag3=NA,
#                         user_tag4=NA,
#                         user_tag5=NA,
#                         selected=F) %>%
#   {.} -> the_data

# Get "plottable" values
discrete <- names(the_data)[!sapply(the_data,is.numeric)]
#discrete <- c(discrete,"user_tag1","user_tag2","user_tag3","user_tag4","user_tag5")
continuous <- names(the_data)[sapply(the_data,is.numeric)]
## NB via selectize inputs it is possible to create new continuous variables, 
# but probably not new discrete ones. We can expect "discrete" to be known and up-to-date
# .. but NOT continuous !
# Well, unless we make it reactive.
# But this would mean defining all the interface server-side.
## Tests to be done on discrete only

# Major elements (would be nice to grab from GCDkit !)
# mjrs <- c("SiO2", "Al2O3","TiO2","FeO","FeOt","MgO","Fe2O3","Fe2O3t","MnO","CaO","Na2O","K2O","P2O5")


## Initial tagging conditions
# A table (of the same shape as the whole dataset) with the user-defined tag columns
userTags_0 <- tibble(user_tag1=rep(NA,nrow(the_data)) )

# Keep a record of the latest selected tag, for convenience
lastUsedTag_0 <- "user_tag1"
lastUsedVal_0 <- "My value"

#*************************#
####     UTILITIES     ####
#*************************#

calcCoreTibble<-function(data,query){
    #' this is named after good old GCDkit calcCore
    #'  (a remarkably clever and efficient way to do the same thing before tidyr...)
    #' Returns a vector containing the data corresponding to the col
    #' or combination of cols
    #' @param data: Tibble to filter
    #' @param query: Query string
    
    if(query==""){return(NULL)}
    
    # This is dark rlang magic...
    exprs <- vars(!!rlang::parse_expr(query))
    dataRange <-  rlang::eval_tidy(exprs[[1]], data = data)
    
    return(dataRange)
}


makeJSConditionString<-function(control,charvec,negate=F){
    #' Build a string correspondong to the js code to toggle a condPanel
    #' if the value returned by another control are in a list of choices
    #' We built in fact the js equivalent to
    #' input$control %in% charvec
    #' @param charvec the allowed choices
    #' @param control the control to read input from
    #' @param negate reverse the condition
    the_conds<-str_c('"',charvec,'"') %>% str_flatten(collapse = ',')
    cond_str<-paste('[',the_conds,'].includes(input.',control,')',sep='')
    
    if(negate){
        return(paste('!',cond_str,sep=""))
    }else{
        return(cond_str)
    }
}


#*************************#
####     DEFINE UI     ####
#*************************#

### Import UI components

source("./components/binaryPlotUI.R",local=T)

source("./components/facetControlUI.R",local=T)

source("./components/colorControlUI.R",local=T)
source("./components/shapeControlUI.R",local=T)
source("./components/sizeControlUI.R",local=T)
source("./components/alphaControlUI.R",local=T)

source("./components/filterControlUI.R",local=T)
#source("./components/tagControlUI.R",local=T)

### Make UI
ui <- fluidPage(

    # Application title
    titlePanel(the_name),

    sidebarLayout(
        
        #### SIDEBAR ####
        sidebarPanel(
            tabsetPanel(
           
           #### TAB 1 : main graph options, axes #### 
            tabPanel("Plot def",
                binaryPlotXAxis,
                binaryPlotYAxis,
                hr(style = "border-top: 1px solid #000000;"),
            
            #### Facets section ####
                facetDefinitionUI,
                facetSplitUI
            ),
                
            ##### TAB 2 : AESTHETIC MAPPINGS (other than x, y) #####
            tabPanel("Caption",
                     
                #### Color section ####
                colMappingUI,
                uniqueColUI,
                hr(style = "border-top: 1px solid #000000;"),
            
                #### Shape section ####
                pchMappingUI,
                hr(style = "border-top: 1px solid #000000;"),
            
                #### Size section ####
                sizeMappingUI,
                sizeAdjustmentUI,
                sizeRangeUI,
                hr(style = "border-top: 1px solid #000000;"),
            
                #### Alpha section ####
                alphaMappingUI,
                alphaAdjustmentUI,
                alphaRangeUI

            ), ### end of tab "aeshetics"
            
           ##### TAB 3 : DATA FILTERING AND TAGGING #####
           tabPanel("Filter",
                filterPatternUI
                #tagUI
                )

            ) ### end of tabset
        ), ### End of sidebarPanel

        #### MAIN PANEL ####
        mainPanel(
    
            textOutput("sampleInfo"),
            plotOutput("binPlot",
                       click = "binPlot_click",
                       dblclick = "binPlot_dblclick",
                       brush = brushOpts(
                           id = "binPlot_brush",
                           resetOnNew = FALSE)
                       )
        )
    ) ### End layout

) ### End UI

#*************************#
#### SERVER SIDE LOGIC ####
#*************************#

server <- function(input, output, session) {

#### 1) State variables ####
  
  # These variables record the "state" of the application.
  # They are things that must be modified only once, upon specific actions
  # rather than kept permanently up-do-date, so they are not fed by the reactive graph
  # Instead, we modify them manually, upon certain user actions (click...)
  # via observeEvents()
  # In a shiny context, this is dangerous...
  
  #### information on data ####
  # Ultimately, this will be combined with the original data to generate a plottingData table
  
  ## Keep track of selected points
  dataProcessing <- reactiveValues(
    # A sub-table that contains the samples currently selected
    selectedData = NULL)
    
  #### Manual override for the scale of the graph ####
  ranges <- reactiveValues(x = NULL, y = NULL)

#### 2) Reactive expressions - plumbing ####
  
    #### Some useful variables to keep track of ####
    # Count selected samples
    selectedSamplesCount <- reactive({ nrow(dataProcessing$selectedData) })
  
    #### Facetting ####
    # This yields a facetting() reactive (to be used in plot)
    # also updates some of the input widgets (in faceting)
  
    source("./components/facetReactives.R",local=T)

    #### Aesthetics ####
    # All of the following return a *Mappting() and a *Scale() reactive
    # to be used in plot
    
    ## Colour 
    source("./components/colorReactives.R",local=T)

    ## Shape
    source("./components/shapeReactives.R",local=T)
    
    ## Size
    source("./components/sizeReactives.R",local=T)
    
    ## Alpha
    source("./components/alphaReactives.R",local=T)
    
    #### Text outputs ####
   
    ## Graph title bar
    output$sampleInfo<-renderText({
      paste("Full dataset:",nrow(the_data),
            "; filtered:",nrow(plottingData() ),
            "; selected:",selectedSamplesCount() )
    })

    #### Tagging ####

    #### Tags ####
    ## Tag box title
    
    #source("./components/tagReactives.R",local=T)
    
#### 5) The plot ####
    
    #### Generate the table actually used for plotting, by gathering various info ####
    ## We create a "live" version of the data table with any extra info we may need
    plottingData<-reactive({
      
      # Initialize (full ds)  
      current_data <- the_data
      
      # Filter the data based on filter input - if legal; otherwise do nothing.
      try(
        current_data <- the_data %>% filter(!!rlang::parse_expr(input$filterPattern)),
        silent=T
      )
      
      return(current_data)
    }) 

    #### The plot proper (here a binary plot) ####
    output$binPlot <- renderPlot({

        # Build the plot
        p <- plottingData() %>% ggplot()+
            geom_point(aes(x=!!rlang::parse_expr(input$X),
                           y=!!rlang::parse_expr(input$Y),
                           color=!!colorMapping(),
                           shape=!!shapeMapping(),
                           size=!!sizeMapping(),
                           alpha=!!alphaMapping() ))+
            
        #### Axes ####    
            scale_x_continuous()+
            scale_y_continuous()
        
        ## Log scale
        if(input$xlog){
            p<-p + scale_x_log10()
        }
        
        if(input$ylog){
            p<-p + scale_y_log10()
        }
        
        #### Zooming
        p<- p+coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
       
        #### Aesthetics & Caption ####
        ## Colours
        p <- p + colorScale()

        ## Shape
        p <- p + shapeScale()
        
        ## Size
        p <- p + sizeScale()

        ## Alpha
        p <- p + alphaScale()

        #### Facets ####
        p <- p + faceting()
        
        #### Highlight selected ####
        p <- p + highlights()
        
        p

    },height = function() {
        if (session$clientData$output_binPlot_width <= 1500) {
            (session$clientData$output_binPlot_width)*(3/4)
        } else { (session$clientData$output_binPlot_width)*(7/16) }}
  )

    #### Interaction with the plot ####

    #### Keep track of the last brush rectangle ####
     
    lastBrush <- reactiveValues(x = NULL, y = NULL)
    
    observeEvent(input$binPlot_brush, {

      ## Keep track of selected points
      dataProcessing$selectedData <- brushedPoints(plottingData(),input$binPlot_brush,allRows=F)
      
     ## Keep track of rectangle size
      if (!is.null(input$binPlot_brush)) {
        lastBrush$x <- c(input$binPlot_brush$xmin, input$binPlot_brush$xmax)
        lastBrush$y <- c(input$binPlot_brush$ymin, input$binPlot_brush$ymax)
      } else {
        lastBrush$x <- NULL
        lastBrush$y <- NULL
      }
     })

    observeEvent(input$binPlot_dblclick, {

      ranges$x <- lastBrush$x
      ranges$y <- lastBrush$y
      
      lastBrush$x <- NULL
      lastBrush$y <- NULL
      
      session$resetBrush("binPlot_brush")
    })

    observeEvent(input$binPlot_click,{
      # pass
      # this has the side effect of clearing the brush !
    })
   
    ## Add some hover code here ! 
    
    highlights<-reactive({
      if(is.null(dataProcessing$selectedData)){return(NULL)}
      
      geom_point(data=dataProcessing$selectedData,
                 aes(x=!!rlang::parse_expr(input$X),y=!!rlang::parse_expr(input$Y)),
                 ### Shape ?
                 color="yellow",
                 size=2*max(input$size_adj,input$size_rng,na.rm=T), ## CAN BE IMPROVED !
                 alpha=0.5)
    })
}

#*************************#
####    RUN THE APP    ####
#*************************#
shinyApp(ui = ui, server = server)
