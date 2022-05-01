### Colour controls

cat("Sourcing shape controls...")

pchMappingUI <- fluidRow(
  column(8,
         
         selectInput("shape_by",
                     "Symbols represent...",
                     choices=c("nothing",discrete,continuous), 
                     selected="nothing")),
  column(4,
         conditionalPanel(
           condition = "input.shape_by != 'nothing'", ## This is a js condition not R syntax !
           checkboxInput("pch_literal", "Col already contains Symbols"))
  )
  
)


cat("..done\n")