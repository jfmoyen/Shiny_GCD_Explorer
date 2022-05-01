### Facet controls

cat("Sourcing facet controls...")

facetDefinitionUI <- selectizeInput("facet_by",
               "Make separate plots by...",
               choices=c("nothing",discrete,continuous),
               selected="nothing",
               options = list(create = TRUE))


facetSplitUI <-conditionalPanel(
  condition = makeJSConditionString("facet_by",c(discrete,"nothing"),negate=T), # For variables that are NOT discrete
  textInput("splitter",
            "Break points (separated by ;)",
            value="")
)

cat("..done\n")