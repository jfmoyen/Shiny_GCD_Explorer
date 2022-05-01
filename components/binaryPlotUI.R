#### Binary plots
## Widget definition & configuration

cat("Sourcing binary plot controls...")

## X-axis
binaryPlotXAxis <- fluidRow(
  column(9,
         selectizeInput("X",
                        "x-axis:",
                        choices=continuous,
                        selected="SiO2",
                        options = list(create = TRUE))),
  column(3,
         checkboxInput("xlog","log?")
  )
)

## Y-axis
binaryPlotYAxis <- fluidRow(
  column(9,
         selectizeInput("Y",
                        "Y-axis:",
                        choices=continuous,
                        selected="MgO",
                        options = list(create = TRUE))),
  column(3,
         checkboxInput("ylog","log?")
  ),
  
  uiOutput("yRangeUI")
)

cat("..done\n")