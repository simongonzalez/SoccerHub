tabPanel(value = "typepitchtab", title = tags$h5("Events", icon("list")),
         sidebarLayout(
           sidebarPanel(width = 3,
                        uiOutput('selectTypePitchui'),
                        materialSwitch(inputId = "typePitchDivide", value = TRUE,
                                       label = "Divide Pitch", status = "warning"),
                        materialSwitch(inputId = "typePitchDensity", value = TRUE,
                                       label = "Density Plot", status = "danger")
           ),
           # just an empty main panel
           mainPanel(
             plotlyOutput('plotTypePitch')
           )
         )
)