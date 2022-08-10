output$selectTypePitchui <- renderUI({
  selectInput('selectTypePitch', 'Event type',
              choices = sort(unique(dfevent$type.name)))
})

#pitch plot play....................................................................
#................................................................................
output$plotTypePitch <- renderPlotly({
  plotdftype <- dfevent[dfevent$type.name == input$selectTypePitch,]
  
  if(input$typePitchDensity){
    #type density
    p <- ggplot(plotdftype, aes(x=x, y=y)) +
      #pitch lines
      geom_segment(aes(x = x1, y = y1, 
                       xend = x2, yend = y2), colour = "black", data = pitchLines) +
      #pitch points
      geom_point(aes(x = x, y = y), color = 'black',
                 data = pitchPoints, show.legend = FALSE) +
      #pitch center
      geom_point(aes(x = x, y = y), size = 25, 
                 alpha = 0.6, color = 'black', fill = NA, pch = 1,
                 data = data.frame(x = 60, y = 40), show.legend = FALSE) +
      geom_point(data = plotdftype, aes(x, y, color = team.name, 
                                        text = playerName)) +
      scale_color_manual(breaks = sort(unique(plotdftype$team.name)),
                         values=c("red", "white")) +
      #geom_text(data = plotdftype, aes(x, y, color = type.name, label = player.name)) +
      stat_density_2d(aes(fill = ..density..), 
                      geom = "raster", contour = FALSE, show.legend = F) +
      scale_fill_distiller(palette= "Spectral", direction=1) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      theme_void()
  }else{
    #type points
    p <- ggplot() +
      #pitch rectangle
      geom_rect(data=rectangledf, 
                aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                color="#5db746", fill = "#5db746") +
      geom_rect(data=rectangledf2, 
                aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), 
                color="#46a147", fill = "#46a147") +
      #pitch lines
      geom_segment(aes(x = x1, y = y1, 
                       xend = x2, yend = y2), colour = "black", data = pitchLines) +
      #pitch points
      geom_point(aes(x = x, y = y), color = 'black',
                 data = pitchPoints, show.legend = FALSE) +
      #pitch center
      geom_point(aes(x = x, y = y), size = 25, 
                 alpha = 0.6, color = 'black', fill = NA, pch = 1,
                 data = data.frame(x = 60, y = 40), show.legend = FALSE) +
      geom_point(data = plotdftype, aes(x, y, color = team.name, text = playerName)) +
      scale_color_manual(breaks = sort(unique(plotdftype$team.name)),
                         values=c("red", "white")) +
      #geom_text(data = plotdftype, aes(x, y, color = type.name, label = player.name)) +
      stat_density_2d(aes(fill = ..level..), geom = "polygon") + 
      theme_void()
  }
  
  if(input$typePitchDivide){
    p <- p + facet_wrap(~team.name)
  }
  
  ggplotly(p, tooltip = c("text"))
})