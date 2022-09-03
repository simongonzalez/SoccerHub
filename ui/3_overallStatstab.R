tabPanel(value = "overallStatstab", title = tags$h5("Overall Stats", icon("chart-line")),
         box(
           title = "Offensive", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           highchartOutput("offensive_plot")
         ),
         box(
           title = "Deffensive", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           highchartOutput("defensive_plot")
         ),
         box(
           title = "Pass and possession", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           highchartOutput("passpossession_plot")
         ),
         box(
           title = "Errors", status = "primary", solidHeader = TRUE,
           collapsible = TRUE,
           highchartOutput("error_plot")
         )
)