tabPanel(value = "matchtab", title = tags$h5("Match", icon("clock")),
         sidebarLayout(
           sidebarPanel(width = 3,
                        uiOutput('competitionSelectionui'),
                        uiOutput('selectTeamui'),
                        uiOutput('selectSeasonui'),
                        uiOutput('selectStageui'),
                        uiOutput('selectVsui'),
                        uiOutput('selectDateui')
           ),
           # just an empty main panel
           mainPanel(
             fluidRow(box(
               title = "Home", status = "primary", solidHeader = TRUE,
               collapsible = TRUE,
               textOutput('homeNameui'),
               formattableOutput("teamhomeplot", height = 250)
             ),
             box(
               title = "Away", status = "primary", solidHeader = TRUE,
               collapsible = TRUE,
               textOutput('awayNameui'),
               formattableOutput("teamawayplot", height = 250)
             ))
           )
         )
)
