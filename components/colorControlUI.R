### Colour controls

cat("Sourcing colour controls...")

colMappingUI <- fluidRow(
  column(8,
         
         selectInput("color_by",
                     "Colours represent...",
                     choices=c("nothing",discrete, continuous),
                     selected="nothing")),
  
  column(4,
         conditionalPanel(
           condition = "input.color_by != 'nothing'", ## This is a js condition not R syntax !
           checkboxInput("col_literal", "Col already contains colors"))
  )
  
)

# Unique color
uniqueColUI<-conditionalPanel(
  condition = "input.color_by == 'nothing'", ## This is a js condition not R syntax !
  colourInput("col_fixed", "", "red",palette="limited"))

cat("..done\n")