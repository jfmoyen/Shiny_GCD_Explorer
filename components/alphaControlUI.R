### Alpha controls

cat("Sourcing alpha controls...")

alphaMappingUI <- fluidRow(
  column(8,
         selectizeInput("alpha_by",
                        "Opacity represents...",
                        choices=c("nothing",continuous),
                        selected="nothing",
                        options = list(create = TRUE))
  ),
  
  column(4,
         conditionalPanel(
           condition = "input.alpha_by != 'nothing' ", ## This is a js condition not R syntax !
           checkboxInput("alpha_literal", "Col already contains Opacity")
         )
  ))

alphaAdjustmentUI <- conditionalPanel(
  condition = "input.alpha_by == 'nothing'", ## This is a js condition not R syntax !
  sliderInput("alpha_adj",
              "",
              value=0.9,
              min=0,max=1)
)

alphaRangeUI <- conditionalPanel(
  condition = "input.alpha_by != 'nothing'  && !input.alpha_literal ", ## This is a js condition not R syntax !
  sliderInput("alpha_rng",
              "",
              value=c(0.4,1),
              min=0,max=1)
)

cat("..done\n")