output$tagBoxTitle<-renderUI({
  paste("Tag",selectedSamplesCount(),"samples using:")
})

tagging <- reactiveValues(
  # A table (of the same length as the whole dataset) with the user-defined tag columns
  userTags = tibble(user_tag1=rep(NA,nrow(the_data)) ),
  
  # Keep a record of the latest selected tag (and last-used value), for convenience
  lastUsedTag = "user_tag1",
  lastUsedVal = "My value"
)


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
  validate(need( !(requiredField %in% colnames(the_data))),"Column already present in dataset!" )
  # and now, what ?
  
  ## See if the column exists in userTags - if not, create it
  if(! (requiredField %in% tagFields) ){
    tagging$userTags <- add_column(tagging$userTags,!!rlang::parse_expr(requiredField))
  }
  
  # Update the col in the right place
  # --> we first need to create UIDs !!! (as we know from SQL, hey ?)
  
  # housekeeping (taglist etc)
  
  # update the input widgets to reflect these changes
  # in particuar all the aesthetic ones !
  # also the ones for tagging
  
})

# If we change the tag col we must re-populate the list of existing tags...
observeEvent(input$tag_col,{
  
})