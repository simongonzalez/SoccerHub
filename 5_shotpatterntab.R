tabPanel(value = "shotpatterntab", title = tags$h5("Shots", icon("futbol")),
         tabsetPanel(id = "shotstabs",
                     tabPanel(value = "shotpitchtab", title = "Sequences",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             radioButtons("shotsequenceTypePitch", 
                                                          label = NULL,
                                                          choices = c('All Shots', 'Single Shots'), 
                                                          selected = 'All Shots', inline = T),
                                             uiOutput('shotsequenceTeamPitchui'),
                                             uiOutput('shotsequencePlayPitchui'),
                                             uiOutput('shotsequencePlayPitchsortui')
                                ),
                                # just an empty main panel
                                mainPanel(
                                  plotlyOutput('shotplotPlaySequencePitch')
                                )
                              )      
                     ),
                     tabPanel(value = "shotcomparisons", title = "Comparisons",
                              box(
                                title = "Shots Comparison", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                highchartOutput("shots_comparison_plot")
                              )
                     )
         )
)