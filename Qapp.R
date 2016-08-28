library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(googleVis)
library(ggmap)
library(lubridate)


myvar=0
if (myvar==1){
setwd("D:/Official/09_Analytics/QualityDashBoard")
COR_EFRMaster=fread("DataNeeded.csv", stringsAsFactors = FALSE, header= TRUE)
ENAMasterSource=fread("SOASource.csv", stringsAsFactors = FALSE, header= TRUE)
ENAMasterSource$Created_on <- as.Date(ENAMasterSource$Created_on, "%d.%m.%Y")
#https://www.r-bloggers.com/date-formats-in-r/
BranchMapping=fread("BranchGeoCode.csv",stringsAsFactors = FALSE, header=TRUE)
BranchMapping=data.frame(BranchMapping)
ENAMasterSource=left_join(ENAMasterSource,BranchMapping,by=c("OrigOffice"="Branch"))
ENAMasterSource$CreatedMonth=month(as.POSIXlt(ENAMasterSource$Created_on, format="%Y-%m-%d"))
ENAMasterSource$ConvertedSLtoFL <- ifelse(ENAMasterSource$Category=="Cat1 Material Request SL responsibility" & ENAMasterSource$FB_Responsibility=="Front Line Responsibility", 1, 0)
ENAMasterSource$ConvertedFLtoSL<- ifelse(ENAMasterSource$Category=="Cat2 Material Request FL responsibility" & ENAMasterSource$FB_Responsibility=="Supply Line Responsibility", 1, 0)
}

#filteredData = subset(COR_EFRMaster,COR_EFRMaster$Branches=="Las Vegas")

f <- list(family = "Courier New, monospace",size = 18,color = "#7f7f7f")
x <- list(title = "Month",titlefont = f,tickmode = "array",tickvals = 1:12,
          ticktext = c("Jan", "Feb", "Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec"))
x1 <- list(title = "Category",titlefont = f,tickmode = "array",
           tickvals =c("Cat1 Material Request SL responsibility","Cat2 Material Request FL responsibility","Cat3 Improvement proposal (No Mat Req)","ConvertedSLtoFL","ConvertedFLtoSL"),
          ticktext = c("Cat1","Cat2","Cat3","SL to FL","FL to SL"))
vars=unique(COR_EFRMaster$Branches)
varsYN = c(TRUE,FALSE)
VarCol = colnames(COR_EFRMaster)
CatSel= unique(ENAMasterSource$FB_Responsibility)
YearSel = unique(ENAMasterSource$Created_on_year)
library(shiny)
ui <- fluidPage(title = "Quality Dashboard",
                
                tabsetPanel(
                  tabPanel(title = "Index Page"

                  ),
                  tabPanel(title = "Category View",
                           plotlyOutput("Categorychart",width = 600)
                  ),
                  tabPanel(title = "Dashboard Chart",
                           fluidRow(
                             column(3,selectInput(inputId = "Chars","Choose Branch",vars,multiple = FALSE)),
                             column(3,selectInput(inputId = "ColSelection","Choose Columns",VarCol,multiple = TRUE))
                           ),
                           fluidRow(
                             #column(2,"Saml"),
                             column(6,
                                    fluidRow(plotlyOutput("DashRegionChart")),
                                    fluidRow(plotlyOutput("DashCOR90Chart"))
                                    ),
                             column(6,
                                    fluidRow(plotlyOutput("DashEFRChart")),
                                    fluidRow(plotlyOutput("DashFYCORChart"))
                                    )
                           )
                  ),
                  tabPanel(title = "Data View",
                           dataTableOutput("mytable")
                  ),
                  tabPanel(title = "Map Interactive",
                         
                          fluidRow(
                            column(2),
                            column(3,selectInput(inputId = "CategoryChoose","Choose Category",CatSel,multiple = FALSE))
                                  ),
                          leafletOutput("mapKONEQFB", height = 500)
                  ),
                  tabPanel(title = "Other Reports",
                           fluidRow(
                             column(3,selectInput(inputId = "yearChoose","Choose Year",YearSel,multiple = FALSE))
                             #column(3,selectInput(inputId = "ColSelection","Choose Columns",VarCol,multiple = TRUE))
                                    ),
                           fluidRow(
                             column(12,
                                    fluidRow(plotlyOutput("ScatRpt1")),
                                    fluidRow(plotlyOutput("ScatRpt2"))
                             )
                           )
                  ),
                  tabPanel(title = "Branch Locations",
                           leafletOutput("mapKONEQFB_Branch")
                           
                  )
                )
)
server <- function(input, output) {

  brachSelection= reactive({input$Chars})
  LegendYesNO = reactive({input$OptLegend})
  ColSel = reactive({input$ColSelection})
  
  ################################################# Preparing Geo Code fetching####################################
  
  ###### Binding Branch & disctrict based on Branchcode OrigOffice
  #BranchCity=unique(BranchMapping$BranchDesc)
  #BranchCity=BranchCity[!is.na(BranchCity)]
  #Dcountry=data.frame(BranchCity,stringsAsFactors = FALSE)
  #geocodes <- geocode(as.character(Dcountry$BranchCity))
  #Dcountry <- data.frame(Dcountry[,1],geocodes,stringsAsFactors = FALSE)
  #write.csv(BranchMapping,file="BranchGeoCode.csv")

  ################################################# Category charts ####################################
  subdf2015=subset(ENAMasterSource,Created_on_year==2015)
  subdf2016=subset(ENAMasterSource,Created_on_year==2016)
  ENAmaster2015_GroupbyCat= subdf2015%>%group_by(Category)%>%summarise(cnt=n())
  ENAmaster2016_GroupbyCat= subdf2016%>%group_by(Category)%>%summarise(cnt=n())
  
  CountYr_ConSLFL= ENAMasterSource %>% group_by(Created_on_year, ConvertedSLtoFL) %>% tally()
  ENAmaster2015_GroupbyCat=rbind(ENAmaster2015_GroupbyCat,c("ConvertedSLtoFL",subset(CountYr_ConSLFL, Created_on_year==2015 & ConvertedSLtoFL == 1)$n))
  ENAmaster2016_GroupbyCat=rbind(ENAmaster2016_GroupbyCat,c("ConvertedSLtoFL",subset(CountYr_ConSLFL, Created_on_year==2016 & ConvertedSLtoFL == 1)$n))
  
  CountYr_ConFLSL= ENAMasterSource %>% group_by(Created_on_year, ConvertedFLtoSL) %>% tally()
  ENAmaster2015_GroupbyCat=rbind(ENAmaster2015_GroupbyCat,c("ConvertedFLtoSL",subset(CountYr_ConFLSL, Created_on_year==2015 & ConvertedFLtoSL == 1)$n))
  ENAmaster2016_GroupbyCat=rbind(ENAmaster2016_GroupbyCat,c("ConvertedFLtoSL",subset(CountYr_ConFLSL, Created_on_year==2016 & ConvertedFLtoSL == 1)$n))
  
  output$Categorychart<-renderPlotly({
    
    plot_ly(x = ENAmaster2015_GroupbyCat$Category ,y = ENAmaster2015_GroupbyCat$cnt,name ="2015 Feedbacks",type="bar")
    add_trace(x = ENAmaster2016_GroupbyCat$Category, y=ENAmaster2016_GroupbyCat$cnt,name="2016 Feedbacks", type="bar") %>%
      layout(xaxis=x1,yaxis=list(title = "Quantity", titlefont = f))%>%
      layout(title="Feedback Comparison")
    #rm(subdf)
  })
  
  ################################################# EFR charts ####################################
  output$DashRegionChart=renderPlotly({
  WestDF= COR_EFRMaster %>%
      group_by(EFRMonth) %>%
      #summarise(avg_EFR = mean(YTDEFR,na.rm=TRUE))
      summarise_each(funs(mean(., na.rm = TRUE)),YTDEFR,EFRResult2015,EFRTarget2016,COR90D,COR90D_YTD2015,COR90D_YTD2016,FYCOR,FYCOR_YTD2015,FYCOR_YTD2016)
    
    #filteredData = subset(COR_EFRMaster,COR_EFRMaster$Branches==SelBranch)
    plot_ly(x = WestDF$EFRMonth, y=WestDF$YTDEFR,name="EFR Actual 2016", line = list(shape = "spline"))%>%
      add_trace(x = WestDF$EFRMonth, y = WestDF$EFRTarget2016, name = "EFR Target 2016", line = list(shape = "spline")) %>%
      add_trace(x = WestDF$EFRMonth, y = WestDF$EFRResult2015, name = "EFR Result 2015", line = list(shape = "spline")) %>%
      layout(xaxis=x,yaxis=list(title = "EFR", titlefont = f,tick0=0))%>%
      layout(legend = list(x = 0.5, y = max(WestDF$YTDEFR)))%>%
      layout(showlegend = LegendYesNO(),title="Regional consolidated chart")
  })
  output$DashEFRChart=renderPlotly({
    SelBranch = "San Diego"
    if (!is.null(brachSelection())) {
      SelBranch = brachSelection()
    }
    filteredData = subset(COR_EFRMaster,COR_EFRMaster$Branches==SelBranch)
    plot_ly(x = filteredData$EFRMonth, y=filteredData$YTDEFR,name="EFR Actual 2016", line = list(shape = "spline"))%>%
      add_trace(x = filteredData$EFRMonth, y = filteredData$EFRTarget2016, name = "EFR Target 2016", line = list(shape = "spline")) %>%
      add_trace(x = filteredData$EFRMonth, y = filteredData$EFRResult2015, name = "EFR Result 2015", line = list(shape = "spline")) %>%
      layout(xaxis=x,yaxis=list(title = "EFR", titlefont = f,tick0=0))%>%
    layout(legend = list(x = 0.5, y = max(filteredData$YTDEFR)))%>%
      layout(showlegend = LegendYesNO(), title="Early Failure rate chart")
  })
  output$DashCOR90Chart=renderPlotly({
    SelBranch = "San Diego"
    if (!is.null(brachSelection())) {
      SelBranch = brachSelection()
    }
    filteredData = subset(COR_EFRMaster,COR_EFRMaster$Branches==SelBranch)
    plot_ly(x = filteredData$EFRMonth, y=filteredData$COR90D,name="COR 90 Actual 2016", line = list(shape = "spline"))%>%
      add_trace(x = filteredData$EFRMonth, y = filteredData$COR90D_YTD2016, name = "COR 90 Target 2016", line = list(shape = "spline")) %>%
      add_trace(x = filteredData$EFRMonth, y = filteredData$COR90D_YTD2015, name = "COR 90 Result 2015", line = list(shape = "spline")) %>%
      layout(xaxis=x,yaxis=list(title = "Call Out Rate 90 Days", titlefont = f,tick0=0))%>%
      layout(legend = list(x = 0.5, y = max(filteredData$COR90D)))%>%
      layout(showlegend = LegendYesNO(),title="Call out Rate 90 Days")
  })
  output$DashFYCORChart=renderPlotly({
    SelBranch = "San Diego"
    if (!is.null(brachSelection())) {
      SelBranch = brachSelection()
    }
    filteredData = subset(COR_EFRMaster,COR_EFRMaster$Branches==SelBranch)
    plot_ly(x = filteredData$EFRMonth, y=filteredData$FYCOR,name="FYCOR Actual 2016", line = list(shape = "spline"))%>%
      add_trace(x = filteredData$EFRMonth, y = filteredData$FYCOR_YTD2016, name = "FYCOR Target 2016", line = list(shape = "spline")) %>%
      add_trace(x = filteredData$EFRMonth, y = filteredData$FYCOR_YTD2015, name = "FYCOR Result 2015", line = list(shape = "spline")) %>%
      layout(xaxis=x,yaxis=list(title = "First Year call out rate", titlefont = f,tick0=0))%>%
      layout(legend = list(x = 0.5, y = max(filteredData$FYCOR)))%>%
      layout(showlegend = LegendYesNO(),title="First Year callout rate")
  })
 
  ###############Tab 2 starts here
  output$mytable = renderDataTable({
    SelBranch = "San Diego"
    if (!is.null(brachSelection())) {
      SelBranch = brachSelection()
    }
    
    #columns = "EFRMonth"
    #if (!is.null(input$ColSelection)) {
   #   columns = input$ColSelection
    #}
    #filteredData = subset(COR_EFRMaster,COR_EFRMaster$Branches==SelBranch)
    #####Option 1
    #datatable(Newpoc[,columns,drop=FALSE], filter="top",options = list(lengthChange = FALSE),callback=JS("
    #                                                                               //hide column filters for the first two columns
    #                                                                              $.each([0, 1], function(i, v) {
    #                                                                              $('input.form-control').eq(v).hide()
    #                                                                              });"))
    ##### Option 1 ends
    datatable(COR_EFRMaster,filter = "top",options=list(pagelength=12))
    #datatable(filteredData[,columns,drop=FALSE], filter="top",options = list(lengthChange = FALSE))
  })
  ####################################### Map interactive ####################################
  output$mapKONEQFB_Branch = renderLeaflet({
    #require(leaflet)
    m=leaflet(BranchMapping) %>%
      addTiles() %>%
      #setView(lng=mean(ENAMasterSource$lon),lat=mean(ENAMasterSource$lat), zoom=3)%>%
      addCircleMarkers(data = BranchMapping, lng = ~ lon, lat = ~ lat, radius = 5,clusterOptions = markerClusterOptions())%>%
      addCircles(popup= ~BranchDesc)
    print(m)
  })
  output$mapKONEQFB = renderLeaflet({
    #require(leaflet)
    ENAMasterSource_Sub=subset(ENAMasterSource,!is.na(BranchDesc))
    ENAMasterSource_Sub=subset(ENAMasterSource_Sub,FB_Responsibility==input$CategoryChoose)
    m=leaflet(na.omit(ENAMasterSource_Sub)) %>% addTiles() %>%
      #setView(lng=mean(ENAMasterSource$lon),lat=mean(ENAMasterSource$lat), zoom=3)%>%
      addCircleMarkers(data = ENAMasterSource_Sub, lng = ~ lon, lat = ~ lat, radius = 5,clusterOptions = markerClusterOptions())
      #addCircles(popup= ~Category)
    print(m)
  })
 #write.csv(ENAMasterSource,file = "testssd.csv")
  ####################################### Other charts ####################################
 
  output$ScatRpt1<-renderPlotly({
    subdf=subset(ENAMasterSource,Created_on_year==input$yearChoose)
    subdf_GroupbyMonth= subdf%>%group_by(CreatedMonth)%>%summarise(cnt=n())
   plot_ly(x = subdf_GroupbyMonth$CreatedMonth ,y = subdf_GroupbyMonth$cnt,name = "FeedbackQuantity",type = "bar")%>%
     layout(xaxis=x,yaxis=list(title = "Quantity", titlefont = f))
   #rm(subdf)
  })
  output$ScatRpt2<-renderPlotly({
    ENAmaster2015_GroupbyMonth= subdf2015%>%group_by(CreatedMonth)%>%summarise(cnt=n())
    ENAmaster2016_GroupbyMonth= subdf2016%>%group_by(CreatedMonth)%>%summarise(cnt=n())
    plot_ly(x = ENAmaster2015_GroupbyMonth$CreatedMonth ,y = ENAmaster2015_GroupbyMonth$cnt,name ="2015 Feedbacks",type="bar")
    add_trace(x = ENAmaster2016_GroupbyMonth$CreatedMonth, y=ENAmaster2016_GroupbyMonth$cnt,name="2016 Feedbacks", type="bar") %>%
      layout(xaxis=x,yaxis=list(title = "Quantity", titlefont = f))%>%
      layout(title="Feedback Comparison")
    #rm(subdf)
  })
  
}
shinyApp(server = server, ui = ui)