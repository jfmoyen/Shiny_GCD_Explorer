## UI components

output$doTagButton<-renderUI({
  actionButton("tag_do",paste("Tag",selectedSamplesCount(),"samples"))
})

output$tagBoxTitle<-renderUI({
  paste("Tag",selectedSamplesCount(),"samples using:")
})

output$tagBoxError<-renderText({
  if( isValidTag() ){NULL}else{
    "Column already present in dataset!\n(nothing will happen)"
  }
})

## Reactive plumbing
tagging <- reactiveValues(
  # A table (of the same length as the whole dataset) with the user-defined tag columns
  userTags = tibble(ID=the_data$ID,
                    user_tag1=rep(NA,nrow(the_data)) ),
  
  # Keep a record of the latest selected tag (and last-used value), for convenience
  lastUsedTag = "user_tag1",
  lastUsedVal = "My value"
)

# Is the user-selected tag valid ?
isValidTag <- reactive({
  !(input$tag_col %in% c(names(the_data),NULL # Potentially add other forbidden names
                         ) )
})

## Observers (one-shot)
# Most of the tag work is done only upon clicking the button !

## Controller for the tagging UI
output$showTagBox <- reactive({ as.character(selectedSamplesCount()>0) })
outputOptions(output, 'showTagBox', suspendWhenHidden = FALSE)

observeEvent(input$tag_do,{
  ## Preparation
  # Currently existing tag fields
  tagFields <- names(tagging$userTags)
  
  # Required tagField
  requiredField <- input$tag_col
  
  # Required value
  requiredVal <- input$tag_val
  
  ## Check that the field is not already present in the dataset
  # if it is, do nothing
  if( requiredField %in% colnames(the_data) ){return(NULL)}
  
  ## See if the column exists in userTags - if not, create it
  if(! (requiredField %in% tagFields) ){
    tagging$userTags <- add_column(tagging$userTags,requiredField)
  }
  
  ## Add to the selection table the tag value
  browser()
  dataProcessing$selectedData %>% mutate(c(!!rlang::parse_expr(requiredField)=requiredVal)) %>% 
    select(c("ID",requiredField))
  
  # Update the col in the right place
  # tagging$userTags <- tagging$userTags %>% left_join(dataProcessing$selectedData) %>%
  #         filter()
  
  # Rows to update: dataProcessing$selectedData$ID
  
  ## housekeeping (last used values)
  
  ## update the input widgets to reflect these changes
  # in particular all the aesthetic ones !
  # so we need to keep somewhere a list of the acceptable variables...
  #
  
})

# If we change the tag col we must re-populate the list of existing tags...
observeEvent(input$tag_col,{
  # activtate/deactivate "do" button
  
})