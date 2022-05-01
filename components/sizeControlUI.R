### Colour controls

cat("Sourcing size controls...")

sizeMappingUI<-fluidRow(
  column(8,
         selectizeInput("size_by",
                        "Size represents...",
                        choices=c("nothing",continuous,discrete),
                        selected="nothing",
                        options = list(create = TRUE))),
  
  column(4,
         conditionalPanel(
           condition = "input.size_by != 'nothing'", ## This is a js condition not R syntax !
           checkboxInput("cex_literal", "Col already contains Size"))
  )
)

sizeAdjustmentUI<-conditionalPanel(
  condition = "input.size_by == 'nothing' || input.cex_literal", ## This is a js condition not R syntax !
  sliderInput("size_adj",
              "",
              value=2,
              min=0,max=10)
)

sizeRangeUI<-conditionalPanel(
  condition = "input.size_by != 'nothing' && !input.cex_literal", ## This is a js condition not R syntax !
  sliderInput("size_rng",
              "",
              value=c(1,4),
              min=0,max=10)
)

cat("..done\n")