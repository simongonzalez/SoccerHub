output$eventdescriptiontable <-  renderFormattable({
  
  formattable(eventsDescription, list(
    event = formatter("span", 
                      style = x ~ formattable::style(font.weight = "bold")),
    description = formatter("span", 
                            style = x ~ formattable::style(font.weight = "italic"))
  ))
})