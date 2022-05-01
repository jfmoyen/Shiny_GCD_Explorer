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

the_data <- read_excel("Classif_Var_LFB_NA.xlsx",.name_repair="universal") %>% sample_frac(1/10)
the_name <- "Granites Jacob et al. 21"
## Names are replaced by "syntaxic" names. This is a hassle for A/CNK etc but makes life generally easier
# Another (best?) way would be to backtick every selection (with paste() ... )
# But how would that play out with calculated values ? Badly, I think.

# Get "plottable" values
discrete <- names(the_data)[!sapply(the_data,is.numeric)]
continuous <- names(the_data)[sapply(the_data,is.numeric)]
## NB via selectize inputs it is possible to create new continuous variables, 
# but probably not new discrete ones. We can expect "discrete" to be known and up-to-date
# .. but NOT continuous !
# Well, unless we make it reactive.
# But this would mean defining all the interface server-side.
## Tests to be done on discrete only

# Major elements (would be nice to grab from GCDkit !)
# mjrs <- c("SiO2", "Al2O3","TiO2","FeO","FeOt","MgO","Fe2O3","Fe2O3t","MnO","CaO","Na2O","K2O","P2O5")

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
                alphaRangeUI,

            ), ### end of tab "aeshetics"
            
           ##### TAB 3 : DATA FILTERING #####
           tabPanel("Filter",
                filterPatternUI
           )
           
           
        ) ### End of sidebarPanel
), ### end of tabset

        #### MAIN PANEL ####
        mainPanel(
            textOutput("sampleInfo"),
            plotOutput("binPlot",
                       dblclick = "binPlot_dblclick",
                       brush = brushOpts(
                           id = "binPlot_brush",
                           resetOnNew = TRUE)
                       )
            #plotlyOutput('binPlot')
        )
    ) ### End layout

) ### End UI

#*************************#
#### SERVER SIDE LOGIC ####
#*************************#

server <- function(input, output, session) {

#### 1) Dynamic UI components ####

#### 2) Current data table ####
    ### We create a "live" version of the data table with any extra info we may need
    dataLive<-reactive({
      
      # Initialize (full ds)  
      current_data <- the_data
      
      # Filter the data based on filter input - if legal; otherwise do nothing.
      try(
        current_data <- the_data %>% filter(!!rlang::parse_expr(input$filterPattern)),
        silent=T
      )
        
      return(current_data)
    }) 
    
#### 3) Reactive variables ####
    
    #### Scaling info ####
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    #### Facetting ####
    source("./components/facetReactives.R",local=T)

    #### Aesthetics ####
    # All are reactive expressions, and they also need properly defined scales
    # So we define both a reactive mapping, and a reactive scale
    
    ## Colour 
    source("./components/colorReactives.R",local=T)

    ## Shape
    source("./components/shapeReactives.R",local=T)
    
    ## Size
    source("./components/sizeReactives.R",local=T)
    
    ## Alpha
    source("./components/alphaReactives.R",local=T)
    
#### 4) Misc outputs ####
    
    #### Filtering ####
    output$sampleInfo<-renderText({
      paste("Full dataset:",nrow(the_data),"; filtered:",nrow(dataLive() ) )
    })

#### 5) The plot ####

    output$binPlot <- renderPlot({
     #output$binPlot <- renderPlotly({

        # Build the plot
        p <- dataLive() %>% ggplot()+
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
        
        p
        #ggplotly(p)

    },height = function() {
        if (session$clientData$output_binPlot_width <= 1500) {
            (session$clientData$output_binPlot_width)*(3/4)
        } else { (session$clientData$output_binPlot_width)*(7/16) }}
  )

    #### 6) User interaction ####
    
    observeEvent(input$binPlot_dblclick, {
        brush <- input$binPlot_brush
        if (!is.null(brush)) {
            ranges$x <- c(brush$xmin, brush$xmax)
            ranges$y <- c(brush$ymin, brush$ymax)
            
        } else {
            ranges$x <- NULL
            ranges$y <- NULL
        }
    })
}

#*************************#
####    RUN THE APP    ####
#*************************#
shinyApp(ui = ui, server = server)
