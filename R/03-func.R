# 03-func.R



# cleaning function -------------------------------------------------------

# load_hp_var <- function (data = planning_data, variable, order) {
#   
#   selectedVar <- data[[variable]]
#   
#   data %>%
#     mutate(selectedVar = replace_na(selectedVar, "Data Not Available")) %>%
#     rename(fips = FIPSCode) %>%
#     filter(ProfileYear == "2022") %>%
#     group_by(selectedVar) %>%
#     summarise(cnt = n(), .groups = "drop") %>%
#     mutate(freq = round(cnt / sum(cnt), 2),
#            freq_perc = paste0(freq * 100, "%")) %>%
#       arrange(match(selectedVar, order))
# 
# }



# highcharts function -----------------------------------------------------

create_pie <- function (data, 
                        var, 
                        freq, 
                        my_colors = c(main_orange, light_blue, light_accent, NA_color),
                        my_legend) {
  highchart() %>%
    hc_add_series(
      type = "pie", 
      data = data,
      hcaes(.data[[var]], .data[[freq]]),
      size = "100%", name = "", 
      borderColor = "#020202",
      borderWidth = .15
    ) %>%
    hc_plotOptions(
      pie =
        list(
          innerSize = "70%", # Set the inner size for the donut effect
          allowPointSelect = TRUE, # Enable point selection on the chart
          cursor = "pointer", # Set cursor style for selectable points
          dataLabels = list(
            enabled = TRUE, 
            format = "{point.freq_perc:.0f}",
            overflow = TRUE,
            distance = 10,
            padding = 2,
            style = list(
              color = dark_accent,
              fontFamily = "Jost",
              fontWeight = "500",
              fontSize = "20px",
              wordBreak = 'break-all',
              textOverflow = 'allow')
          ), # Disable data labels on the slices
          showInLegend = TRUE # Show legend for the chart
        )
    ) %>%
    # tooltip information
    hc_tooltip(
      useHTML = TRUE,
      headerFormat = "",
      pointFormat = "<div style='text-align: center;'> <b>{point.selectedVar}</b><br>{point.freq_perc}<br>n= {point.cnt} </div>",
      shadow = FALSE,
      borderWidth = 1,
      hideDelay = 1000,
      backgroundColor = "#fff",
      style = list(
        fontFamily = "Jost",
        fontSize = "15px"
      )
    ) %>%
    hc_colors(
      colors = my_colors
    ) %>%
    # adding the theme
    hc_add_theme(astho_theme) %>%
    # legend
    hc_legend(
      enabled = TRUE,
      reversed = FALSE,
      layout = "vertical",
      align = "center",
      verticalAlign = "bottom",
      itemMarginBottom = 3,
      itemMarginTop = 3,
      margin = 22,
      title = list(
        text = paste0(my_legend),
        style = list(
          textDecoration = "underline",
          fontFamily = "Jost",
          fontSize = "16px"
        )
      )
    ) %>%
    hc_exporting(
      enabled = TRUE,
      formAttributes = list(target = "_blank"),
      sourceHeight = 700, # come back
      sourceWidth = 1200, # come back
      allowHTML = TRUE, # come back
      url = "https://exporter.opifex.org",
      buttons = list(
        contextButton = list(
          symbol = "menu",
          symbolStrokeWidth = 2,
          symbolFill = icon_accent,
          symbolStroke = icon_accent,
          menuItems = export
        )
      )
    )
}





# Highcharter Map Function ------------------------------------------------
# Data in the highchartsData excel file.

create_map <- function(data, 
                       my_value,
                       my_type = "b",
                       data_class, 
                       my_colors = main_colors,
                       my_click = NA,
                       title_text,
                       subtitle_text,
                       caption_text,
                       legend_text = NA,
                       my_tooltip,
                       my_reverse = TRUE
) {

  
  hc <- highchart(type = "map") %>%
    hc_add_custom_map_cat(
      data = data,
      value = my_value,
      joinBy = "fips",
      mapType = my_type,
      borderColor = "#020202",
      borderWidth = .25,
      dataLabels = list(
        enabled = FALSE, 
        format = "{point.state}"
      )
    ) %>%
    hc_chart(
      height = 600
    ) %>%
    hc_colorAxis(
      dataClassColor = "category",
      dataClasses = data_class
    ) %>%
    hc_colors(
      colors = my_colors
    ) %>%
    hc_plotOptions(
      series = list(
        events = list(
          click = my_click
        )
      )
    ) %>%
    hc_title(
      text = title_text, # title text
      margin = 10
    ) %>%
    hc_subtitle(
      text = subtitle_text, # text under title
      align = "center"
    ) %>%
    hc_caption(
      text = caption_text, # source and data notes
      useHTML = TRUE,
      margin = 20,
      align = "center"
    ) %>%
    hc_legend(
      reversed = my_reverse,
      enabled = TRUE,
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom",
      itemMarginBottom = 3,
      itemMarginTop = 3,
      margin = 22,
      title = list(
        text = legend_text
      )
    ) %>%
    hc_tooltip(
      headerFormat = "<span style=\"color:{point.color}\">\u25CF</span> {point.JurisdictionName}",
      pointFormat = my_tooltip,
      useHTML = TRUE
    ) %>%
    hc_add_theme(
      astho_theme
    ) %>%
    hc_exporting(
      enabled = TRUE,
      formAttributes = list(
        target = "_blank" # opens in new window
      ),
      sourceHeight = 700, # sets height of download
      sourceWidth = 1200, # sets width of download
      allowHTML = TRUE, 
      url = "https://exporter.opifex.org", #serve owned by John Coene
      buttons = list(
        contextButton = list(
          symbol = "menu",
          symbolStrokeWidth = 2,
          symbolFill = icon_accent,
          symbolStroke = icon_accent,
          menuItems = export
        )
      )
    )
  return(hc)
}


# Highcharter Line Function -----------------------------------------------
create_line <- function(group = 1, 
                        data, 
                        my_x = "ProfileYear",
                        my_y = "value",
                        my_group = "",
                        my_name = "ProfileYear",
                        my_colors = main_colors, 
                        x_text = "Profile Year",
                        y_text,
                        x_category = "ProfileYear",
                        my_max = NULL,
                        my_format = "{value}",
                        subtitle_text, 
                        title_text,
                        caption_text,
                        my_tooltip,
                        legend_enable = FALSE,
                        legend_text = "Group",
                        input = "",
                        my_export = TRUE) {
  
  # if statement for whether or not to include the group
  if (group == 1) {
    hc <- highchart() %>%
      hc_add_series(
        type = "line",
        data = data,
        hcaes(
          x = .data[[my_x]],
          y = .data[[my_y]],
          group = !!sym(input[[my_group]]),
          name = .data[[my_name]]
        )
      )
  } else { # else if the group is not 1 (AKA no grouping on the line graph)
    hc <- highchart() %>%
      hc_add_series(
        type = "line",
        data = data,
        hcaes(
          x = .data[[my_x]],
          y = .data[[my_y]],
          name = .data[[my_name]]
        )
      )
  }
  
  hc <- hc %>%
    hc_plotOptions(
      series = list(
        marker = list(
          enabled = TRUE # gives a marker on each line data point
        )
      ),
      line = list(
        lineWidth = 4 # Set the desired line thickness
      )
    ) %>%
    hc_colors(
      colors = my_colors
    ) %>%
    hc_xAxis(
      title = list(text = x_text), # x-axis label
      categories = unique(as.list(data[[x_category]]))
    ) %>%
    hc_yAxis(
      title = list(text = y_text), # y-axis label
      max = my_max,
      min = 0,
      labels = list(
        format = my_format
      )
    ) %>%
    hc_title(
      text = title_text, # title text
      margin = 40,
      widthAdjust = -60
    ) %>%
    hc_subtitle(
      text = subtitle_text # text under title
    ) %>%
    hc_caption(
      text = caption_text, # source and data notes
      useHTML = TRUE,
      margin = 20
    ) %>%
    hc_legend(
      enabled = legend_enable, # enabled legend
      layout = "horizontal",
      align = "center",
      verticalAlign = "bottom",
      itemMarginBottom = 3,
      itemMarginTop = 3,
      margin = 22,
      title = list(
        text = legend_text
      )
    ) %>%
    hc_tooltip(
      pointFormat = my_tooltip,
      useHTML = TRUE
    ) %>%
    hc_add_theme(
      astho_theme
    ) %>%
    hc_exporting(
      enabled = my_export,
      formAttributes = list(target = "_blank"),
      sourceHeight = 700, # come back
      sourceWidth = 1200, # come back
      allowHTML = TRUE, # come back
      url = "https://exporter.opifex.org",
      buttons = list(
        contextButton = list(
          symbol = "menu",
          symbolStrokeWidth = 2,
          symbolFill = icon_accent,
          symbolStroke = icon_accent,
          menuItems = export
        )
      )
    )
  
  return(hc)
} # closes highcharter function for line graphs



# # Highcharter Bar Function ------------------------------------------------
# # Data in the highchartsData excel file.
# 
# # ns = ... is for namespaced click event within the function (drill and click)
# create_column <- function(id, 
#                           data, 
#                           type = "column",
#                           annotation_text = "",
#                           annotation_x = NULL,
#                           annotation_y = NULL,
#                           caption_align = "left",
#                           my_colors = main_colors, 
#                           subtitle_text = NULL, 
#                           drill = NULL, 
#                           ns = NULL) { 
#   
#   # assigning the id as a filter to a new vector
#   myFilter <- id
#   
#   highchartsData <- highchartsData %>%
#     filter(inputId == myFilter)
#   
#   if (highchartsData$group == "regular") {
#     hc <- 
#       hchart(
#         data,
#         type = type,
#         hcaes(
#           x = .data[[highchartsData$myX]],
#           y = .data[[highchartsData$myY]]
#         ),
#         colorByPoint = TRUE
#       ) %>%
#       hc_xAxis(
#         title = list(text = "") # x-axis label
#       )
#   } else if (highchartsData$group == "group") {
#     hc <- 
#       hchart(
#         data,
#         type = type,
#         hcaes(
#           x = .data[[highchartsData$myX]],
#           y = .data[[highchartsData$myY]],
#           group = .data[[highchartsData$myGroup]]
#         )
#       ) %>%
#       hc_xAxis(
#         title = list(text = "") # x-axis label
#       )
#   } else if (highchartsData$group == "stack") {
#     hc <- 
#       hchart(
#         data,
#         type = type,
#         hcaes(
#           x = .data[[highchartsData$myX]],
#           y = .data[[highchartsData$myY]],
#           group = .data[[highchartsData$myGroup]]
#         ),
#         stacking = "normal"
#       ) %>%
#       hc_add_annotation(
#         labels = list(
#           list(
#             point = 
#               list(
#                 x = annotation_x, 
#                 y = annotation_y, 
#                 xAxis = 0, 
#                 yAxis = 0
#               ),
#             text = annotation_text
#           )
#         ),
#         labelOptions = list(
#           style =list(
#             fontFamily = "Jost",
#             fontSize = "12px",
#             fontWeight = "normal",
#             paddingLeft = "5px" 
#           ),
#           align = "left",
#           allowOverlap = TRUE
#           # distance = "10"
#           # verticalAlign = "middle"
#         )
#       ) %>% 
#       hc_xAxis(
#         title = list(text = "") # x-axis label
#       )
#   } else if (highchartsData$group == "drill") {
#     
#     # JS Click Function
#     click_chart <- JS(
#       # a bit complicated by this is the only way to get the namespace to work
#       paste0("function(event) {
#            Shiny.onInputChange(
#            '", 
#              ns('chart_clicked'), 
#              "',
#            event.point.name);}"
#       )
#     ) 
#     
#     hc <- 
#       hchart(
#         data,
#         type = type,
#         hcaes(
#           x = .data[[highchartsData$myX]],
#           y = .data[[highchartsData$myY]],
#           drilldown = .data[[highchartsData$myGroup]]
#         ),
#         name = unique(as.list(data[[highchartsData$myName]])),
#         colorByPoint = TRUE
#       ) %>%
#       hc_xAxis(
#         title = list(text = unique(as.list(data[[highchartsData$myName]]))) # x-axis label
#       ) %>%
#       hc_chart(type = type, 
#                events = list(
#                  load = 
#                    JS("function() {
#                       console.log(this)
#                       }"),
#                  drilldown = 
#                    JS("function(e) {
#                       this.xAxis[0].update({title: {text: e.point.group_label + ' for ' + e.point.name}})
#                       }"),
#                  drillup = 
#                    JS("function(e) {
#                       this.xAxis[0].update({title: {text: ''}})
#                       }")
#                )
#       ) %>%
#       hc_drilldown(
#         activeAxisLabelStyle = list(
#           fontFamily = "Jost",
#           fontSize = "16px",
#           fontWeight = "normal",
#           textDecoration = "none",
#           color = "#666"
#         ),
#         drillUpButton = list(
#           position = list(
#             verticalAlign = "top",
#             y = -35
#           ),
#           theme = list(
#             fill = "none",
#             stroke = "white",
#             style = list(
#               fontFamily = "Jost",
#               fontSize = "14px",
#               fontWeight = "normal",
#               color = "#C65227"
#             )
#           )
#         ),
#         breadcrumbs = list(
#           format = "back to {level.name}",
#           showFullPath = TRUE
#         ),
#         allowPointDrilldown = TRUE,
#         series = list_parse(drill)
#       ) %>%
#       hc_plotOptions(
#         series = list(
#           showInLegend = FALSE,
#           colorByPoint = FALSE
#         )
#       )
#   } else if (highchartsData$group == "drill and click") {
#     
#     # JS Click Function
#     click_chart <- JS(
#       # a bit complicated by this is the only way to get the namespace to work
#       paste0("function(event) {
#            Shiny.onInputChange(
#            '", 
#              ns('chart_clicked'), 
#              "',
#            event.point.name);}"
#       )
#     ) 
#     
#     hc <- 
#       hchart(
#         data,
#         type = type,
#         hcaes(
#           x = .data[[highchartsData$myX]],
#           y = .data[[highchartsData$myY]],
#           drilldown = .data[[highchartsData$myGroup]]
#         ),
#         name = unique(as.list(data[[highchartsData$myName]])),
#         colorByPoint = FALSE
#       ) %>%
#       hc_colors(
#         colors = my_colors
#       ) %>%
#       hc_xAxis(
#         title = list(text = unique(as.list(data[[highchartsData$myName]]))) # x-axis label
#       ) %>%
#       hc_chart(
#         events = list(
#           load = 
#             JS("function() {
#                         console.log(this)
#                       }"),
#           drilldown =
#             JS("function(e) {
#                         this.xAxis[0].update({title: {text: e.point.name + ' Activities'}})
#                       }"),
#           drillup = JS(
#             paste0("function() {
#                         this.xAxis[0].update({title: {text: ''}});
#                         Shiny.onInputChange('", ns('chart_clicked'), "', null);}"))
#         )
#       ) %>%
#       hc_drilldown(
#         activeAxisLabelStyle = list(
#           fontFamily = "Jost",
#           fontSize = "16px",
#           fontWeight = "normal",
#           textDecoration = "none",
#           color = "#666"
#         ),
#         drillUpButton = list(
#           position = list(
#             verticalAlign = "top",
#             y = -35
#           ),
#           theme = list(
#             fill = "none",
#             stroke = "white",
#             style = list(
#               fontFamily = "Jost",
#               fontSize = "14px",
#               fontWeight = "normal",
#               color = "#C65227"
#             )
#           )
#         ),
#         breadcrumbs = list(
#           format = "back to {level.name}",
#           showFullPath = TRUE
#         ),
#         allowPointDrilldown = TRUE,
#         series = list_parse(drill)
#       ) %>%
#       hc_plotOptions(
#         series = list(
#           events = list(
#             click = click_chart)
#         )
#       )
#   }
#   
#   hc <- 
#     hc %>%
#     hc_yAxis(
#       title = list(text = highchartsData$yText), # y-axis label
#       max = highchartsData$myMax,
#       min = 0,
#       labels = list(
#         format = highchartsData$myFormat
#       )
#     ) %>%
#     hc_title(
#       text = highchartsData$titleText, # title text
#       margin = 40,
#       widthAdjust = -60
#     ) %>%
#     hc_subtitle(
#       text = subtitle_text # text under title
#     ) %>%
#     hc_caption(
#       text = highchartsData$captionText, # source and data notes
#       useHTML = TRUE,
#       margin = 20,
#       align = caption_align
#     ) %>%
#     hc_legend(
#       enabled = highchartsData$legendEnable, # enabled legend
#       layout = "horizontal",
#       align = "center",
#       verticalAlign = "bottom",
#       itemMarginBottom = 3,
#       itemMarginTop = 3,
#       margin = 22,
#       title = list(
#         text = unique(as.list(data[[highchartsData$legendText]]))
#       )
#     ) %>%
#     hc_tooltip(
#       pointFormat = highchartsData$myTooltip,
#       useHTML = TRUE
#     ) %>%
#     hc_add_theme(
#       astho_theme
#     ) %>%
#     hc_exporting(
#       enabled = highchartsData$myExport,
#       formAttributes = list(target = "_blank"),
#       sourceHeight = 700, # set the size of the export
#       sourceWidth = 1200, # set the size of the export
#       allowHTML = TRUE, 
#       url = "https://exporter.opifex.org", # exports to server owned by John Coene
#       buttons = list(
#         contextButton = list(
#           symbol = "menu", # controls hamburger icon for export 
#           symbolStrokeWidth = 2,
#           symbolFill = icon_accent,
#           symbolStroke = icon_accent,
#           menuItems = export
#         )
#       )
#     )
#   
#   return(hc)
# }


