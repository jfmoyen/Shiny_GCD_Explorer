## UI components

# This is a pseudo-boolean that controls the visibility of the tab box
output$showTagBox <- reactive({ as.character(selectedSamplesCount()>0) })

# Title for the tag box in UI, also counting the samples
output$tagBoxTitle<-renderUI({
  paste("Tag",selectedSamplesCount(),"samples using:")
})

# In case of "bad" column selected
output$tagBoxError<-renderText({
  if( isValidTag() ){NULL}else{
    "Column already present in dataset!\n(nothing will happen)"
  }
})

# The button, updated with the right number of samples
output$doTagButton<-renderUI({
  actionButton("tag_do",paste("Tag",selectedSamplesCount(),"samples"))
})


## Reactive plumbing

# We keep them in tagging:
tagging <- reactiveValues(

  # A table (of the same length as the whole dataset) with the user-defined tag columns
  userTags =  tibble(ID=the_data$ID),

  # Keep a record of the latest selected tag (and last-used value)
  lastUsedTag = "user_tag1",
  lastUsedVal = "My value",

  # The list of variables that we can use for aes()
  by_variables = c("nothing",discrete, continuous)

)

# Is the user-selected tag valid ?
isValidTag <- reactive({
  !(input$tag_col %in% c(names(the_data),NULL # Potentially add other forbidden names
                         ) )
})

## Functions
updateTagTable<-function(tagTable,requiredField, requiredVal, selectedData){
#browser()
  #tagTable[selectedData$ID,requiredField]<-requiredVal
  inserted_values <- select(selectedData,ID) %>% 
                  add_column(!!(requiredField) := requiredVal)
  
  tagTable <- rows_update(tagTable,inserted_values,by="ID")
  
  return(tagTable)
  }

addTag <- function(newTag){
  # When we create a tag we must update in all sort of places !
  
  # Add to the tag table
  tagging$userTags <- add_column(tagging$userTags,!!(newTag) := "")

  # Add to the select lists
  tagging$by_variables <- c(tagging$by_variables,newTag)
  
  # Add to the discrete variables
  metaData$discreteVariables <- c(metaData$discreteVariables,newTag)

  # Update the tag widgets
  # updateSelectizeInput(inputId="tag_col",
  #                         choices=setdiff(names(tagging$userTags),"ID") )

  updateSelectizeInput(inputId="tag_val",
                       choices = lastUsedVal_0,
                       selected = lastUsedVal_0 )

  # updateTagValuesWidget()
  
  # update the input widgets to reflect these changes
  widgetsToUpdate <- c("color_by","shape_by","size_by","facet_by")

  .updateW <- function(thewidget){
    # Record current selection
    curr_sel <- input[[thewidget]]
    
    # Update
    updateSelectizeInput(inputId=thewidget,
                            choices=tagging$by_variables,
                            selected=curr_sel)

  }

  walk(widgetsToUpdate,.updateW)
  
}

updateTagValuesWidget <- function(){
  # Update the widget with tag values to reflect what currently exists, 
  # and the latest selected value
  
  # What hav we got in the column ?
  alreadyInCol <- unique(tagging$userTags[[input$tag_col]])

  if(length(alreadyInCol) > 0){
    updateSelectizeInput(inputId="tag_val",
                         choices=alreadyInCol)
    if(tagging$lastUsedVal %in% alreadyInCol){
      updateSelectizeInput(inputId="tag_val",
                           selected=tagging$lastUsedVal)
    }
  }else{
    updateSelectizeInput(inputId="tag_val",
                         choices=lastUsedVal_0)
  }
  
}


## Observers (one-shot)

## Most of the tag work is done only upon clicking the button !

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
  if( !isValidTag() ){return(NULL)}

  ## See if the column exists in userTags - if not, create it and update what needs to be
  if(! (requiredField %in% tagFields) ){
   addTag(requiredField)
  }

  ## Update the list of user tags
  tagging$userTags <- updateTagTable(tagging$userTags, requiredField, requiredVal, dataProcessing$selectedData)

  ## housekeeping (last used values)
  tagging$lastUsedTag <- requiredField
  tagging$lastUsedVal <- requiredVal
  
  updateSelectizeInput(inputId="tag_col",selected=requiredField)
  updateTagValuesWidget()
  updateSelectizeInput(inputId="tag_val",selected=requiredVal)
  

})

# If we change the tag col we must re-populate the list of existing tags...
observeEvent(input$tag_col,{

 # cat("repopulating tags for",input$tag_col,"...\n")
  if(isValidTag() ){
    shinyjs::enable("tag_do")
    
    updateTagValuesWidget()
    # alreadyInCol <- unique(tagging$userTags[[input$tag_col]])
    # cat("found in col:",alreadyInCol,"\n")
    # 
    # if(length(alreadyInCol) > 0){
    #   updateSelectizeInput(inputId="tag_val",
    #                        choices=alreadyInCol)
    #   cat("populating tag_val\n")
    #   if(tagging$lastUsedVal %in% alreadyInCol){
    #     updateSelectizeInput(inputId="tag_val",
    #                          selected=tagging$lastUsedVal)
    #     cat("adding",tagging$lastUsedVal,"\n")
    #   }
    # }else{
    #   updateSelectizeInput(inputId="tag_val",
    #                        choices=lastUsedVal_0)
    # }
  
  }else{
    shinyjs::disable("tag_do")
  }


})

