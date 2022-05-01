# filterPatternUI <- fluidRow(
#                   column(8,
#                          textInput(
#                            "filterPattern",
#                            "Type search pattern below, e.g. SiO2/5 > 10")
#                          ),
#                   
#                   column(4,
#                            checkboxInput("filter_keep_NA", "Keep NA's?")
#                         )
#                 )


filterPatternUI <- textInput(
                         "filterPattern",
                         "Type search pattern below, e.g. SiO2/5 > 10")            
