tabPanel(value = "passpatterntab", title = tags$h5("Passes", icon("project-diagram")),
         tabsetPanel(id = "passestabs",
                     tabPanel(value = "playpitchtab", title = "Sequences",
                              sidebarLayout(
                                sidebarPanel(width = 3,
                                             uiOutput('sequenceTeamPitchui'),
                                             uiOutput('sequencePlayPitchui'),
                                             materialSwitch(inputId = "sequencePlayPitchType", value = TRUE,
                                                            label = "Animate", status = 'success'),
                                             uiOutput('sequencePlayPitchNui')
                                ),
                                # just an empty main panel
                                mainPanel(
                                  plotlyOutput('plotPlaySequencePitch')
                                )
                              )      
                     ),
                     tabPanel(value = "passesangles", title = "Length/Angle",
                              uiOutput('passpatternplotteamui'),
                              htmlOutput('passpatternplot')),
                     tabPanel(value = "passesNetwork", title = "Pass Networks",
                              uiOutput('passNetwork_passpatternplotteamui'),
                              selectInput('passNetworkTypePlot', 
                                          label = 'Pattern', 
                                          choices = c('Player to Player',
                                                      'Pass Pattern', 'Pass Height',
                                                      'Body Part Involved', 
                                                      'Pass Type', 'Pass Outcome')),
                              visNetworkOutput('passNetwork_passpatternplot')),
                     tabPanel(value = "passesComparisons", title = "Comparisons",
                              box(
                                title = "Pass Accuracy", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                highchartOutput("Pass_Accuracy_plot")
                              ),
                              box(
                                title = "Pass Body Part", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                highchartOutput("Pass_Body_Part_plot")
                              ),
                              box(
                                title = "Pass Height", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                highchartOutput("Pass_Height_plot")
                              ),
                              box(
                                title = "Pass Type", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                highchartOutput("Pass_Type_plot")
                              ),
                              box(
                                title = "Play Pattern", status = "primary", solidHeader = TRUE,
                                collapsible = TRUE,
                                highchartOutput("Play_Pattern_plot")
                              ) 
                     )
                     
         )
)