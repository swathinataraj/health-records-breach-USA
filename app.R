#libraries required
library(shinydashboard)
library(shiny)
library(rgdal)
library(leaflet)
library(tidyr)
library(dplyr)
library(janitor)
library(data.table)
library(DT)
library(raster)
library(readxl)
library(reshape2)
library(plyr)
library(plotly)
library(shinyjs)

#list of choices for checkboxes 
ch <- list(
  "ALABAMA (AL)" = "AL", 
  "ALASKA (AK)" = "AK", 
  "ARIZONA (AZ)" = "AZ",
  "ARKANSAS (AR)" = "AR",
  "CALIFORNIA (CA)" = "CA",
  "COLORADO (CO)" = "CO",
  "CONNECTICUT (CT)" = "CT",
  "DELAWARE (DE)" = "DE",
  "DISTRICT OF COLUMBIA (DC)" = "DC",
  "FLORIDA (FL)" = "FL",
  "GEORGIA (GA)" = "GA",
  "HAWAII (HI)" = "HI",
  "IDAHO (ID)" = "ID",
  "ILLINOIS (IL)" = "IL",
  "INDIANA (IN)" = "IN",
  "IOWA (IA)" = "IA",
  "KANSAS (KS)" = "KS",
  "KENTUCKY (KY)" = "KY",
  "LOUISIANA (LA)" = "LA",
  "MAINE (ME)" = "ME",
  "MARYLAND (MD)" = "MD",
  "MASSACHUSETTS (MA)" = "MA",
  "MICHIGAN (MI)" = "MI",
  "MINNESOTA (MN)" = "MN",
  "MISSISSIPPI (MS)" = "MS",
  "MISSOURI (MO)" = "MO",
  "MONTANA (MT)" = "MT",
  "NEBRASKA (NE)" = "NE",
  "NEVADA (NV)" = "NV",
  "NEW HAMPSHIRE (NH)" = "NH",
  "NEW JERSEY (NJ)" = "NJ",
  "NEW MEXICO (NM)" = "NM",
  "NEW YORK (NY)" = "NY",
  "NORTH CAROLINA (NC)" = "NC",
  "NORTH DAKOTA (ND)" = "ND",
  "OHIO (OH)" = "OH",
  "OKLAHOMA (OK)" = "OK",
  "OREGON (OR)" = "OR",
  "PENNSYLVANIA (PA)" = "PA",
  "RHODE ISLAND (RI)" = "RI",
  "SOUTH CAROLINA (SC)" = "SC",
  "SOUTH DAKOTA (SD)" = "SD",
  "TENNESSEE (TN)" = "TN",
  "TEXAS (TX)" = "TX",
  "UTAH (UT)" = "UT",
  "VERMONT (VT)" = "VT",
  "VIRGINIA (VA)" = "VA",
  "WASHINGTON (WA)" = "WA",
  "WEST VIRGINIA (WV)" = "WV",
  "WISCONSIN (WI)" = "WI",
  "WYOMING (WY)" = "WY",
  "PUERTO RICO (PR)" = "PR")

#user interaction function
ui <- fluidPage(useShinyjs(),
  dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "Breach Dashboard"),
  
  #side bar options
  dashboardSidebar(
    selectInput("select", h3("Search Breached Records by"), 
                choices = list("---Select---" = 0, "State" = 1, "Breach Submission Date Range" = 2,
                               "Type of Breach" = 3), selected = 0),
    conditionalPanel(
      condition = "input.select == 1",
      actionLink("selectall","Select All"),
      checkboxGroupInput("checkGroup", 
                         h3("Select one or more state/states"), 
                         choices = ch,
                         selected = -1)
      
      ),
    conditionalPanel(
      condition = "input.select == 2",
      dateRangeInput("dates", h3("Date range"),
                     start = "2009-08-12", end="2016-12-09")
    ),
    conditionalPanel(
      condition = "input.select == 3",
             radioButtons("radio", h3("Choose breach type"),
                          choices = list("Theft" = "Theft", "Unauthorized Access/Disclosure" = "Unauthorized Access/Disclosure",
                                         "Hacking/IT Incident" = "Hacking/IT Incident","Loss" = "Loss","Improper Disposal" = "Improper Disposal",
                                         "Other" = "Other"),selected = "Theft"),
                          h3("Show pie chart of all breach types"),
                          actionButton("button", "Hide"),
                          actionButton("button2", "Show")
                          
                
      
      )),
  
  #body of the dashboard with conditional panels
  dashboardBody(
    conditionalPanel(
      condition = "input.select == 1",
      fluidRow( box(width = 12,
                    leafletOutput("mymap"))
                
      ),
      fluidRow( box(width = 12,
                    DT::dataTableOutput("data_table")))
      
    ),
    conditionalPanel(
      condition = "input.select == 2",
      h5("Records are between the dates 2009-08-12 and 2016-12-09 in YYYY-MM-DD format"),
      fluidRow( box(width = 15,
                    DT::dataTableOutput("records_by_date")
        ))
    ),
    conditionalPanel(
      condition = "input.select == 3",
      h3("Breach Types"),
      fluidRow( box(width = 15,
                    DT::dataTableOutput("records_by_breachtype"),
                    plotlyOutput("pie_plot"))
      ))
    
  )
               
))

#load data from the excel sheet
crime_data <- read_excel("health_data_breach.xlsx",sheet = 1, col_names = TRUE)
crime_data <- setNames(crime_data, c("NameOfCoveredEntity","State","CoveredEntityType","IndividualsAffected","BreachSubmissionDate","TypeOfBreach","LocationOfBreachedInformation","BusinessAssociatePresent","WebDescription"))

#outlier replace function 
outlierReplace = function(dataframe, cols, rows, newValue = NA) {
  if (any(rows)) {
    set(dataframe, rows, cols, newValue)
  }
}
outlierReplace(crime_data, c("IndividualsAffected"), which(crime_data$IndividualsAffected > 500000, NA))

#converting to integer and mean imputation to replace missing values 
crime_data$IndividualsAffected <- as.integer(crime_data$IndividualsAffected)
crime_data$IndividualsAffected[is.na(crime_data$IndividualsAffected)] <- mean(crime_data$IndividualsAffected, na.rm=TRUE)
crime_data$IndividualsAffected <- as.integer(crime_data$IndividualsAffected)

#Formatting Breach submission date to have yyyy-mm-dd format and converting to Date type
for(i in 1:length(crime_data$BreachSubmissionDate))
{
  pattern <- "^[0-9]{5}$"
  replacement <- as.Date(as.character(excel_numeric_to_date(as.numeric(crime_data$BreachSubmissionDate[i]))))
  crime_data$BreachSubmissionDate[i] <- sub(pattern, replacement, crime_data$BreachSubmissionDate[i])
  
}

for(i in 1:length(crime_data$BreachSubmissionDate))
{
  pattern <- "^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2}$"
  replacement <- as.Date(crime_data$BreachSubmissionDate[i],"%m/%d/%y")
  crime_data$BreachSubmissionDate[i] <- sub(pattern, replacement, crime_data$BreachSubmissionDate[i])
  
}
crime_data$BreachSubmissionDate <- as.Date(crime_data$BreachSubmissionDate)

#Removing last column for better visualization
crime_data$WebDescription <- NULL

#creating new data frames for further visualization 
breach_type <- data.frame(crime_data$NameOfCoveredEntity, crime_data$State, crime_data$CoveredEntityType, crime_data$IndividualsAffected, crime_data$BreachSubmissionDate, crime_data$TypeOfBreach, crime_data$LocationOfBreachedInformation, crime_data$BusinessAssociatePresent)
breach_type <- setNames(breach_type, c("NameOfCoveredEntity","State","CoveredEntityType","IndividualsAffected","BreachSubmissionDate","TypeOfBreach","LocationOfBreachedInformation","BusinessAssociatePresent"))
breach_type <- breach_type %>% drop_na(TypeOfBreach)

quest2 <- data.frame(crime_data$TypeOfBreach)
quest2 <- setNames(quest2, c("TypeOfBreach"))
quest2 <- quest2 %>% drop_na(TypeOfBreach)
quest2_0 <- count(quest2, c("TypeOfBreach"))
quest2_0  <- quest2_0[order(-quest2_0$freq),]
quest2_x <- quest2_0[1:6,]


#server function
server <- function(input, output, session) {
  
  
  quest4 <- data.frame(crime_data$State,crime_data$IndividualsAffected)
  quest4 <- setNames(quest4, c("State","IndividualsAffected"))
  quest4[order(-quest4$IndividualsAffected),]
  quest4 <- quest4 %>% drop_na(State)
  quest4_0 <- aggregate(quest4$IndividualsAffected, by=list(State=quest4$State), FUN=sum)
  quest4_0 <- setNames(quest4_0, c("State","IndividualsAffectedSum"))
  
  #Reading Shape file
  states <- readOGR(".", "cb_2016_us_state_500k")
  states <- subset(states, is.element(states$STUSPS, quest4_0$State))
  
  #bins for colouring polygons in choropleth map based on IndividualsAffectedSum in each State
  bins <- c(2500,42500,82500,122500,162500,202500,2422500)
  pal <- colorBin("YlOrRd", domain = quest4_0$IndividualsAffectedSum, bins = bins)
  
  #checkboxgroup reactive input
  data_input <- reactive({
    vector_check <- as.vector(input$checkGroup)
    quest4_0 <- quest4_0[quest4_0$State %in% vector_check,]
  })
  
  data_input_matched <- reactive({
    data_input()[order(match(data_input()$State, states$STUSPS)),]
  })
  
  #Render data table
  output$data_table <-  DT::renderDataTable(
    data_input(), options = list(lengthChange = FALSE)
  ) 
  
  
  date_records <- reactive({
   crime_data %>%
      filter(BreachSubmissionDate > input$dates[1],
          BreachSubmissionDate < input$dates[2])
     })
  
  breachtype_records <- reactive({
    subset(breach_type, TypeOfBreach == input$radio, select=NameOfCoveredEntity:BusinessAssociatePresent)
  })
  
  
  labels <- sprintf(
    "<strong>%s</strong></br> <strong>Total Individuals Affected: %s</strong>",
    states$NAME, quest4_0$IndividualsAffectedSum
  ) %>% lapply(htmltools::HTML)
  
  #Render leaflet for base map
  output$mymap <- renderLeaflet({
    leaflet() %>%
      setView(-96, 37.8, 4) %>%
      addProviderTiles(providers$Stamen.Toner)
  })
  
  #Observe event - If select all is clicked, update checkgroup input to select all states and update the map. 
  #If select all is deselected, update back to base map
  observeEvent(input$selectall,{
    if(input$selectall == 0)
      {
      return(NULL)
      }
    else if (input$selectall%%2 == 0)
    {
      
      updateCheckboxGroupInput(session,"checkGroup",choices=ch)
        leafletProxy("mymap") %>%
          clearShapes() %>% clearControls()
    }
    else
    {
      updateCheckboxGroupInput(session,"checkGroup",choices=ch,selected=ch)
      leafletProxy("mymap") %>% 
        clearShapes() %>% clearControls() %>%
        addPolygons(
          data = states,
          fillColor = pal(quest4_0$IndividualsAffectedSum),
          weight = 1,
          smoothFactor = 0.5,
          color = "white",
          fillOpacity = 0.8,
          highlight = highlightOptions(
            weight = 5,
            color = "#666666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels) %>%
        addLegend(pal = pal,
                  values = quest4_0$IndividualsAffectedSum,
                  opacity = 0.7,
                  title = NULL,
                  position = "topright")
      
    }
  })
  
  #If user select one or more check boxes, update map accordingly
  observeEvent(data_input_matched(),{
    if((length(data_input_matched()$State) > 0) && (length(data_input_matched()$State) < 52))
    {
      leafletProxy("mymap") %>%
        clearShapes() %>% clearControls() %>% 
        addPolygons(
          data = subset(states, is.element(states$STUSPS, data_input_matched()$State)),
          fillColor = pal(data_input_matched()$IndividualsAffectedSum),
          weight = 1,
          smoothFactor = 0.5,
          color = "white",
          fillOpacity = 0.8,
          highlight = highlightOptions(
            weight = 5,
            color = "#666666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE)) %>%
             addLegend(pal = pal,
                values = data_input_matched()$IndividualsAffectedSum,
                opacity = 0.7,
                title = NULL,
                position = "bottomright")
      
    }
    else if(length(data_input_matched()$State) == 0)
    {
      leafletProxy("mymap") %>%
        clearShapes() %>% clearControls() 
    }
    else if(length(data_input_matched()$State) == 52)
    {
      updateCheckboxGroupInput(session,"checkGroup",choices=ch,selected=ch)
      leafletProxy("mymap") %>% 
        clearShapes() %>% clearControls() %>%
        addPolygons(
          data = states,
          fillColor = pal(quest4_0$IndividualsAffectedSum),
          weight = 1,
          smoothFactor = 0.5,
          color = "white",
          fillOpacity = 0.8,
          highlight = highlightOptions(
            weight = 5,
            color = "#666666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels) %>%
        addLegend(pal = pal,
                  values = quest4_0$IndividualsAffectedSum,
                  opacity = 0.7,
                  title = NULL,
                  position = "topright")
      
    }
  })
  

  #Plotly to render pie plot
  output$pie_plot <- renderPlotly({
    plot_ly(quest2_x, labels = ~TypeOfBreach, values = ~freq,type = "pie") %>% 
      layout(title = "Pie Chart showing contribution of each breach types")
  })
  
  observeEvent(input$button2,{
      show("pie_plot")
    })
   
  observeEvent(input$button,{
      hide("pie_plot")
    })
  
  output$records_by_date = DT::renderDataTable(
   date_records(), options = list(lengthChange = FALSE)
  )
  
  output$records_by_breachtype = DT::renderDataTable(
    breachtype_records(), options = list(lengthChange = FALSE)
  )
  

}

#Run the shiny app
shinyApp(ui, server)

