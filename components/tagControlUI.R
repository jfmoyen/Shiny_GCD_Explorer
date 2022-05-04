## Initial tagging conditions

# Keep a record of the latest selected tag, for convenience
lastUsedTag_0 <- "user_tag1"
lastUsedVal_0 <- "My value"

tagUI <- conditionalPanel(condition="output.showTagBox == 'TRUE' ",
                 hr(style = "border-top: 1px solid #000000;"),
                 htmlOutput("tagBoxTitle",class="h4"),
                 selectizeInput("tag_col",
                                "Tag to set or update:",
                                choices=lastUsedTag_0,
                                selected=lastUsedTag_0,
                                options = list(create = TRUE)),
                 textOutput("tagBoxError"),
                 selectizeInput("tag_val",
                                "Value:",
                                choices = lastUsedVal_0,
                                selected = lastUsedVal_0,
                                options = list(create = TRUE)),
                 uiOutput("doTagButton")
                 #actionButton("tag_do","Tag selected")
)