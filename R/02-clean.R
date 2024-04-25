# 02-clear.R


# set colors --------------------------------------------------------------

#define colors-------------------------------------------------------
main_blue <- "#005182"
light_blue <- "#70C4E8"
main_orange <- "#C65227"
dark_orange <- "#90361F"
main_yellow <- "#EBB41F"
light_yellow <- "#FFD780"
NA_color <- "#E6E1E7"
dark_accent <- "#242c3d"
light_accent <- "#cae6f2"
icon_accent <- "#47BA83"
main_colors <- c(light_blue, main_orange, main_yellow, icon_accent, main_blue)
dark_main_colors <- c("#325F87","#D86B45", "#A67B05","#287E58", "#15395A") #these are for bubble

#gov colors --------------------------------------------------------
gov_color_2 <- c(main_blue, main_orange)
gov_color_3 <- c(light_yellow, main_yellow, main_orange)
gov_color_4 <- c(main_yellow, main_blue, light_blue, main_orange)
gov_color_5 <- c(light_blue, main_orange, main_blue, icon_accent, main_yellow)
gov_oranges <- c(light_yellow, main_yellow,main_orange,dark_orange)

#structure colors --------------------------------------------------
struct_base <- c(main_orange, main_blue)
struct_color_3 <-c(light_blue, light_accent, NA_color)
struct_color_2 <-c(light_blue, NA_color)


# define vizualization theme ----------------------------------------------

astho_theme <- hc_theme(
  colors = main_colors,
  chart = list(
    backgroundColor = NULL),
  style = list(
    fontFamily = "Jost"),
  title = list(
    style = list(
      color = dark_accent,
      fontFamily = "Jost",
      fontWeight = "500",
      fontSize = "20px")),
  subtitle = list(
    style = list(
      color = dark_accent,
      fontFamily = "Jost",
      fontSize = "14px")),
  caption = list(
    style = list(
      color = "#7e7f7f",
      fontFamily = "Jost",
      fontSize = "12px")),
  xAxis = list(
    labels = list(
      style = list(
        fontFamily = "Jost",
        fontSize = "20px",
        fontWeight = "normal",
        color = "#666")),
    title = list(
      style = list(
        color = dark_accent,
        fontFamily = "Jost",
        fontWeight = "500",
        fontSize = "15px"))),
  yAxis = list(
    labels = list(
      style = list(
        fontFamily = "Jost",
        fontSize = "20px",
        fontWeight = "normal",
        color = "#666")),
    title = list(
      style = list(
        color = dark_accent,
        fontFamily = "Jost",
        fontWeight = "500",
        fontSize = "15px"))),
  legend = list(
    itemStyle = list(
      fontFamily = "Jost",
      color = dark_accent,
      fontSize = "17px",
      fontWeight = "normal",
      color = "#666"),
    title = list(
      style = list(
        textDecoration = "none",
        fontFamily = "Jost",
        fontSize = "16px"))),
  tooltip = list(
    padding = 10,
    borderRadius = 20,
    backgroundColor = "#fff",
    style = list(
      fontFamily = "Jost",
      fontSize = "14px")),
  itemHoverStyle = list(
    color = light_accent)
)


# export options ----------------------------------------------------------

export <- list(
  list(
    text = "PNG",
    onclick = JS("function () {
                   this.exportChart({ type: 'image/png' }); }")),
  list(
    text = "JPEG",
    onclick = JS("function () {
                   this.exportChart({ type: 'image/jpeg' }); }")),
  list(
    text = "PDF",
    onclick = JS("function () {
                   this.exportChart({ type: 'application/pdf' }); }"))
) 



# clean data --------------------------------------------------------------

hr_choices <- list("Personnel Recruitment" = "HRRecruitment", 
                   "Personnel Selection" = "HRSelection",
                   "Compensation & Incentive Management" = "HRCompensation",
                   "Benefits Management" = "HRBenefits",
                   "Onboarding" = "HROnboarding",
                   "Performance Evaluation" = "HREvaluation",
                   "Staff Learning & Development" = "HRDevelopment",
                   "Employee Engagement & Relations" = "HREngagement",
                   "Human Resource Planning" = "HRPlanning",
                   "Occupational Health & Safety" = "HRHealthSafety",
                   "Labor Relations" = "HRRelations")

sizeOrder <- c("Less than 2 million", "2 million to 10 million", "More than 10 million")
densityOrder <- c("Less than 20 people per square mile", "20 - 100 people per square mile", "100.1 - 500 people per square mile", "More than 500 people per square mile", "Data Not Available")
HR4_Order <- c("Public Health Agency Only", "Both Public Health Agency and Other Government Agency", "Other Government Agency Only", "Data Not Available")
HR3_Order <- c("Public Health Agency Only", "Both Public Health Agency and Other Government Agency", "Data Not Available")
Func5_Order <- c("Public health division/department*", "Umbrella Agency", "Other Agency", "More than one other entity", "Data Not Available")
Func4_Order <- c("Public health division/department*", "Umbrella Agency", "Other Agency", "Data Not Available")
MajorResp_Order <- c("Umbrella agency has responsibility", "Public health division/department has responsibility", "Not applicable, freestanding/independent agency", "Data Not Available")



Attrition <- Attrition %>%
  mutate(region = REGION)

govData$state <- as.factor(govData$JurisdictionName)

#creating fips
govData <- govData %>% 
  mutate(fips = as.numeric(FIPSCode))


Attrition$JurisdictionName <- gsub("Island Areas", "All Island Areas", Attrition$JurisdictionName)
Workforce$JurisdictionName <- gsub("Island Areas", "All Island Areas", Workforce$JurisdictionName)

# Workforce$governance <- na_if(Workforce$governance, "Island Area")


govData$HRRecruitment <- factor(as.character(govData$HRRecruitment), levels = HR4_Order)
govData$HRSelection <- factor(as.character(govData$HRSelection), levels = HR4_Order)
govData$HRCompensation <- factor(as.character(govData$HRCompensation), levels = HR4_Order)
govData$HRBenefits <- factor(as.character(govData$HRBenefits), levels = HR4_Order)
govData$HROnboarding <- factor(as.character(govData$HROnboarding), levels = HR4_Order)
govData$HREvaluation <- factor(as.character(govData$HREvaluation), levels = HR4_Order)
govData$HRDevelopment <- factor(as.character(govData$HRDevelopment), levels = HR3_Order) #3
govData$HREngagement <- factor(as.character(govData$HREngagement), levels = HR4_Order)
govData$HRPlanning <- factor(as.character(govData$HRPlanning), levels = HR4_Order)
govData$HRHealthSafety <- factor(as.character(govData$HRHealthSafety), levels = HR4_Order)
govData$HRRelations <- factor(as.character(govData$HRRelations), levels = HR4_Order)
govData$FunctionFinancial <-factor(as.character(govData$FunctionFinancial), levels = Func5_Order)
govData$FunctionFacilities <-factor(as.character(govData$FunctionFacilities), levels = Func5_Order)
govData$FunctionIT <-factor(as.character(govData$FunctionIT), levels = Func5_Order)
govData$FunctionLegal <-factor(as.character(govData$FunctionLegal), levels = Func5_Order)
govData$FunctionHR <-factor(as.character(govData$FunctionHR), levels = Func5_Order)
govData$FunctionWorkDev <-factor(as.character(govData$FunctionWorkDev), levels = Func4_Order) #4
govData$FunctionCommPR <-factor(as.character(govData$FunctionCommPR), levels = Func4_Order) #4
govData$FunctionDEI <-factor(as.character(govData$FunctionDEI), levels = Func5_Order)
govData$FunctionPerformance <-factor(as.character(govData$FunctionPerformance), levels = Func4_Order) #4
govData$FunctionESF <-factor(as.character(govData$FunctionESF), levels = Func4_Order) #4
govData$MajorRespBehav <- factor(as.character(govData$MajorRespBehav), levels = MajorResp_Order)
govData$MajorRespEnviro <- factor(as.character(govData$MajorRespEnviro), levels = MajorResp_Order)
govData$MajorRespLTCare <- factor(as.character(govData$MajorRespLTCare), levels = MajorResp_Order)
govData$MajorRespMedicaid <- factor(as.character(govData$MajorRespMedicaid), levels = MajorResp_Order)
govData$MajorRespPA <- factor(as.character(govData$MajorRespPA), levels = MajorResp_Order)
govData$MajorRespREDI <- factor(as.character(govData$MajorRespREDI), levels = MajorResp_Order)
govData$MajorRespMCH <- factor(as.character(govData$MajorRespMCH), levels = MajorResp_Order)
govData$MajorRespAging <- factor(as.character(govData$MajorRespAging), levels = MajorResp_Order)
govData$MajorRespSubstnc <- factor(as.character(govData$MajorRespSubstnc), levels = MajorResp_Order)
govData$MajorRespCorrections <- factor(as.character(govData$MajorRespCorrections), levels = MajorResp_Order)
govData$size <- factor(as.character(govData$size), levels = sizeOrder)
Workforce$size <- factor(as.character(Workforce$size), levels = sizeOrder)
Attrition$size <- factor(as.character(Attrition$size), levels = sizeOrder)
govData$density <- factor(as.character(govData$density), levels = densityOrder)
Workforce$density <- factor(as.character(Workforce$density), levels = densityOrder)
Attrition$density <- factor(as.character(Attrition$density), levels = densityOrder)


# bubble ---------------------------------------------------------------

bubble <- Workforce %>%
  filter(occupationFunction == "All Occupational Functions") %>%
  filter(occupationClassification == "All Occupational Classifications") %>%
  filter(ProfileYear == "2022") %>%
  drop_na(governance) %>%
  drop_na(perPopEmp) %>%
  filter(JurisdictionType != "Island Areas") %>%
  mutate(perPopEmp = round(perPopEmp, 1))

bubble$employeeTotal <- format(round(bubble$employeeTotal, 0), big.mark = ",")

bubble_x <- c("Agency: ", "Profile Year: ", "Employees per 100,000: ", "Total Employees:")
bubble_y <- c("{point.JurisdictionName}", "{point.ProfileYear}", "{point.perPopEmp}", "{point.employeeTotal}")

bubble_title_text <- paste0("Bubble Chart of Non-Temporary Employees per 100,000")
bubble_subtitle_text <- paste("By by Governance Classification, 2022")
bubble_caption_text <- paste(HTML("<b>Source:</b> Association of State and Territorial 
Health Officials (ASTHO) Profile of State and Territorial Public Health Survey. 
<br>
<b>Notes:</b>
(1) Data represent the number of employees at a point-in-time during survey fielding. <br>
(2) This measure excludes temporary and contract workers and excludes vacant positions."))


bubble_tt <- tooltip_table(bubble_x, bubble_y)


bubble_hc <- bubble %>%
  hchart(
    "packedbubble",
    hcaes(
      name = PostalAbbreviation,
      value = perPopFTE,
      group = governance
    )
  ) %>% # come back
  hc_chart(
    height = 600
  ) %>%
  hc_title(
    text = bubble_title_text, # title text
    margin = 10
  ) %>%
  hc_subtitle(
    text = bubble_subtitle_text, # text under title
    align = "center"
  ) %>%
  hc_caption(
    text = bubble_caption_text, # source and data notes
    useHTML = TRUE,
    margin = 20,
    align = "center"
  ) %>%
  hc_colors(
    color = dark_main_colors
  ) %>%
  hc_plotOptions(
    packedbubble = list(
      maxSize = "100%",
      zMin = 0,
      layoutAlgorithm = list(
        gravitationalConstant = 0.05,
        splitSeries = TRUE, # TRUE to group points
        seriesInteraction = TRUE,
        dragBetweenSeries = FALSE,
        parentNodeLimit = TRUE
      ),
      dataLabels = list(
        enabled = TRUE,
        format = "{point.name}"
      ),
      style = list(
        color = "black",
        textOutline = "none",
        fontWeight = "normal"
      )
    )
  ) %>%
  # tooltip
  hc_tooltip(
    pointFormat = bubble_tt,
    useHTML = TRUE,
    padding = 10,
    borderRadius = 20,
    backgroundColor = "#fff",
    style = list(
      fontFamily = "Jost",
      fontSize = "14px"
    )
  ) %>%
  # adding the theme
  hc_add_theme(astho_theme) %>%
  # exporting function for png, jpeg, and pdfs
  hc_exporting(
    enabled = TRUE,
    formAttributes = list(target = "_blank"),
    sourceHeight = 700, # come back
    sourceWidth = 1200, # come back
    allowHTML = TRUE, # come back
    url = "https://exporter.opifex.org",
    buttons = list(contextButton = list(
      symbol = "menu",
      symbolStrokeWidth = 2,
      symbolFill = icon_accent,
      symbolStroke = icon_accent,
      menuItems = export
    ))
  )


# vacant ---------------------------------------------------------------


vacancy_perc <- data.frame(
  ProfileYear = c(2016, 2019, 2022),
  vacantpositions = c(1.5, 2.7, 3.2),
  N = c(44, 47, 48),
  regularPositionTotal = c(NA, 1783, 1766),
  vacPositions = c(92, 179, 201),
  Employees = c(NA, 1604, 1565),
  VacantPct = c(NA, "10.0% Vacant", "11.4% Vacant")
)
  
vacancy_perc$ProfileYear <- as.factor(vacancy_perc$ProfileYear)
  
vacant_line <-highchart() %>%
  hc_add_series(vacancy_perc, 
                "line", 
                hcaes(
                  x = ProfileYear, 
                  y = vacantpositions
                  ),
                dataLabels = list(
                  enabled = TRUE,
                  format = "<b>{point.vacantpositions} per 100,000 </b> <br> <b>{point.VacantPct}</b>",
                  style = list(
                    fontFamily = "Jost",
                    fontSize = "16px",
                    fontWeight = "normal",
                    fontColor = "#232C3D"
                  )
                )
  ) %>%
  hc_colors(
    colors = light_blue
  ) %>%
  hc_tooltip(
    outside = TRUE,
    pointFormat = "<b>Vacant Positions per 100,000:</b> {point.vacantpositions} <br> <b>Percent Vacant:</b> {point.VacantPct} <br> <b>Number of Reporting Agencies:</b> {point.N}",
    valueDecimals = 1,
    padding = 10,
    borderRadius = 20,
    backgroundColor = "#fff",
    style = list(
      fontFamily = "Jost",
      fontSize = "14px"
    )
  ) %>%
  hc_title(
    text = paste0("Public Health Agencies’ Vacancies per 100,000 Population"), # title text
    margin = 40,
    widthAdjust = -60
  ) %>%
  hc_subtitle(
    text = "2016 – 2022" # text under title
  ) %>%
  hc_caption(
    text = paste(HTML("<b>Source:</b> Association of State and Territorial 
Health Officials (ASTHO) Profile of State and Territorial Public Health Survey. 
<br>
<b> Notes: </b>
    (1) Vacant percent is not displayed for 2016 due to a change in how the percentage is calculated.
    (2) Vacant positions are defined as positions that have been recruited and positions in the process of onboarding.
    Vacancy counts do not include positions that are required to be left vacant.
    (3) Jurisdictions not included due to missing data: Alabama, California, District of Columbia, Montana, New Jersey, Oregon, and Rhode Island (2019); Alabama, Connecticut, Florida, Nevada, New Hampshire, Rhode Island, and Tennessee (2022).")), # source and data notes
    useHTML = TRUE,
    margin = 20
  ) %>%
  hc_plotOptions(
    series = list(marker = list(enabled = TRUE)),
    line = list(
      lineWidth = 4,
      dataLabels = list(
        enabled = TRUE,
        style = list(
          fontFamily = "Jost",
          fontSize = "16px",
          fontWeight = "normal",
          fontColor = "#232C3D"
        )
      )
    )
  ) %>%
  hc_add_theme(astho_theme) %>%
  # labeling the x axis
  hc_xAxis(
    title = "",
    type = "category",
    labels = list(
      style = list(
        fontFamily = "Jost",
        fontSize = "15px",
        fontWeight = "normal",
        color = "#0C0C0C"
      )
    )
  ) %>%
  # labeling the y axis
  hc_yAxis(
    title = list(text = "Vacant Positions per 100,000 Population"),
    min = 0,
    tickInterval = .5,
    labels = list(
      style = list(
        fontFamily = "Jost",
        fontSize = "18px",
        fontWeight = "normal",
        color = "#666"
      ),
      format = "{value}"
    )
  ) %>%
  hc_legend(enabled = FALSE)



# total employees ---------------------------------------------------------

total_emp <- Workforce %>%
  mutate(ProfileYear = as.factor(ProfileYear)) %>%
  filter(JurisdictionType != "Island Areas") %>%
  mutate(value = perPopEmp) %>%
  filter(occupationFunction == "All Occupational Functions") %>%
  filter(occupationClassification == "All Occupational Classifications") %>%
  filter(JurisdictionName %in% "All States") %>%
  # drop_na(value) %>%
  mutate(cat = "Total Employees") %>%
  mutate_if(is.numeric, round, digits = 1) %>%
  mutate(totalN = paste(perPopEmpN))

total_emp$employeeTotalCom <- format(round(total_emp$employeeTotal, 0), big.mark = ",")



# temp --------------------------------------------------------------------

temp_data <- data.frame(
  Year = c("2019", "2022"),
  Employees = c(1.6, 2.2),
  perPop = c(4.1, 5.3),
  perPopEmp = c(26.5, 24.3),
  N = c(44, 44),
  Increase = c(NA, "38.9% increase from 2019")
)

temp <- temp_data %>%
  hchart("line",
         hcaes(x = Year, y = Employees),
         dataLabels = list(
           enabled = TRUE,
           format = "<b>{point.Increase}</b>",
           style = list(
             fontFamily = "Jost",
             fontSize = "16px",
             fontWeight = "normal"
           )
         )
  ) %>%
  hc_plotOptions(
    series = list(marker = list(enabled = TRUE)),
    line = list(lineWidth = 4)
  ) %>% # Set the desired line thickness
  
  hc_tooltip(
    outside = TRUE,
    pointFormat = "<b>TCWs to Every 10 Non-Temporary Employees:</b> {point.Employees}  <br> <b>TCWs per 100,000 Population:</b> {point.perPop} <br> <b>Number of Reporting Agencies:</b> {point.N}",
    valueDecimals = 1,
    padding = 10,
    borderRadius = 20,
    backgroundColor = "#fff",
    style = list(
      fontFamily = "Jost",
      fontSize = "14px"
    )
  ) %>%
  hc_colors(
    colors = light_blue
  ) %>%
  hc_title(
    text = paste0("Change in TWs to Every 10 Non-TWs"), # title text
    margin = 40,
    widthAdjust = -60
  ) %>%
  hc_subtitle(
    text = "2019 to 2022" # text under title
  ) %>%
  hc_caption(
    text = paste(HTML("<b>Source:</b> Association of State and Territorial 
Health Officials (ASTHO) Profile of State and Territorial Public Health Survey. 
<br>
<b> Notes: </b> (1) The number of non-temporary employees is calculated by 
                      subtracting the agency-reported number of vacant positions 
                      from the agency-reported total number of employment positions 
                      (i.e., current employees including vacant positions). 
                      (2) Vacant positions are defined as positions that have been 
                      recruited and positions in the process of onboarding. Vacancy 
                      counts do not include positions that are required to be left vacant. 
                      (3) Data included in the above figures includes health agencies 
                      that reported data for measures of employment positions, number 
                      of vacancies, and number of temporary or contract workers in 
                      either year of data collection. (4) Jurisdictions not 
                      included due to missing data: Alabama, New Jersey, Oregon, 
                      and Rhode Island (2019); Alabama, Florida, and New Hampshire (2022).")), # source and data notes
    useHTML = TRUE,
    margin = 20
  ) %>%
  hc_add_theme(astho_theme) %>%
  hc_legend(enabled = FALSE) %>%
  # labeling the x axis
  hc_xAxis(
    title = list(text = ""),
    labels = list(
      style = list(
        fontFamily = "Jost",
        fontSize = "16px",
        fontWeight = "normal",
        color = "#666"
      )
    )
  ) %>%
  hc_yAxis(
    title = list(text = "TWs to Every 10 Non-TWs"),
    min = 0,
    tickInterval = .5,
    labels = list(
      style = list(
        fontFamily = "Jost",
        fontSize = "16px",
        fontWeight = "normal",
        color = "#666"
      ),
      format = "{value}"
    )
  )



# vacancy map -------------------------------------------------------------

vac_data_class <- vacant_change %>%
  mutate(value = vacant_change) %>%
  group_by(vacant_change) %>%
  summarise(value = unique(value), .groups = "drop") %>%
  arrange(value) %>%
  rename(name = vacant_change, from = value) %>%
  mutate(to = from) %>%
  list_parse() # end of data classes



# hr map ------------------------------------------------------------------

hr_map <- govData %>%
  mutate(fips = as.numeric(FIPSCode)) %>%
  filter(ProfileYear == "2022") %>%
  mutate(selected_value = FunctionHR,
         selected_value = replace_na(selected_value, "Data Not Available"),
         cat = "Human Resources")

Func5_Order <- c("Public health division/department*", "Umbrella Agency", "Other Agency", "More than one other entity", "Data Not Available")
hr_map$selected_value <-factor(as.character(hr_map$selected_value), levels = Func5_Order)


hr_map_data_class <- hr_map %>%
  mutate(value = selected_value) %>%
  group_by(selected_value) %>%
  summarise(value = unique(value), .groups = "drop") %>%
  arrange(value) %>%
  rename(name = selected_value, from = value) %>%
  mutate(to = from) %>%
  list_parse() # end of data classes

hr_map_colors <- c(struct_base, struct_color_3)


# workforce dev map -------------------------------------------------------


wrk_map <- govData %>%
  mutate(fips = as.numeric(FIPSCode)) %>%
  filter(ProfileYear == "2022") %>%
  mutate(selected_value = FunctionWorkDev,
         selected_value = replace_na(selected_value, "Data Not Available"),
         cat = "Workforce Development")

Func4_Order <- c("Public health division/department*", "Umbrella Agency", "Other Agency", "Data Not Available")
wrk_map$selected_value <-factor(as.character(wrk_map$selected_value), levels = Func4_Order)


wrk_map_data_class <- wrk_map %>%
  mutate(value = selected_value) %>%
  group_by(selected_value) %>%
  summarise(value = unique(value), .groups = "drop") %>%
  arrange(value) %>%
  rename(name = selected_value, from = value) %>%
  mutate(to = from) %>%
  list_parse() # end of data classes

wrk_map_colors <- c(struct_base, struct_color_2)



# hr areas ----------------------------------------------------------------
# 
# hr_areas <- govData %>%
#   select(JurisdictionName, starts_with("HR"))
# 
# hr_areas <- gather(hr_areas, item, value, HRRecruitment:HRRelations, factor_key=TRUE)
# 
# hr_areas <- hr_areas %>%
#   group_by(item, value) %>%
#   summarise(count = n()) %>%
#   ungroup() %>%
#   mutate(perc = (count/59))
# 
# hr_areas_wide <- dcast(hr_areas, item ~ value, value.var="perc")
# library(writexl)
# write_xlsx(hr_areas_wide, "data/hr_areas_wide.xlsx")
