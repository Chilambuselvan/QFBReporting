#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#install.packages("ggThemeAssist")
# install.packages('addinslist')
library(shiny)
subdf2015=subset(ENAMasterSource,Created_on_year==2015)
subdf2016=subset(ENAMasterSource,Created_on_year==2016)

ENAmaster2015_GroupbyCat= subdf2015%>%group_by(Category)%>%summarise(cnt=n())
ENAmaster2016_GroupbyCat= subdf2016%>%group_by(Category)%>%summarise(cnt=n())

ENAmaster2015_GroupbyDef= subdf2015%>%group_by(Defect_type_group)%>%summarise(cnt=n())
ENAmaster2016_GroupbyDef= subdf2016%>%group_by(Defect_type_group)%>%summarise(cnt=n())

CountYr_ConSLFL= ENAMasterSource %>% group_by(Created_on_year, ConvertedSLtoFL) %>% tally()
ENAmaster2015_GroupbyCat=rbind(ENAmaster2015_GroupbyCat,c("ConvertedSLtoFL",subset(CountYr_ConSLFL, Created_on_year==2015 & ConvertedSLtoFL == 1)$n))
ENAmaster2016_GroupbyCat=rbind(ENAmaster2016_GroupbyCat,c("ConvertedSLtoFL",subset(CountYr_ConSLFL, Created_on_year==2016 & ConvertedSLtoFL == 1)$n))

CountYr_ConFLSL= ENAMasterSource %>% group_by(Created_on_year, ConvertedFLtoSL) %>% tally()
ENAmaster2015_GroupbyCat=rbind(ENAmaster2015_GroupbyCat,c("ConvertedFLtoSL",subset(CountYr_ConFLSL, Created_on_year==2015 & ConvertedFLtoSL == 1)$n))
ENAmaster2016_GroupbyCat=rbind(ENAmaster2016_GroupbyCat,c("ConvertedFLtoSL",subset(CountYr_ConFLSL, Created_on_year==2016 & ConvertedFLtoSL == 1)$n))

ENAmastr15_GbyDisFbyC2= subdf2015 %>%filter(substr(FB_Responsibility, 1, 3)=="Fro") %>% group_by(District,CreatedMonth,Region) %>% tally()
ENAmastr16_GbyDisFbyC2= subdf2016 %>%filter(substr(FB_Responsibility, 1, 3)=="Fro") %>% group_by(District,CreatedMonth,Region) %>% tally()

ENAmaster2015_GroupbyCauseCode= subdf2015%>%filter(substr(FB_Responsibility, 1, 3)=="Fro")%>%group_by(Cause_code)%>%summarise(cnt=sum(TotalCosts))
ENAmaster2016_GroupbyCauseCode= subdf2016%>%filter(substr(FB_Responsibility, 1, 3)=="Fro")%>%group_by(Cause_code)%>%summarise(cnt=sum(TotalCosts))

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  brachSelection= reactive({input$Chars})
  LegendYesNO = reactive({input$OptLegend})
  ColSel = reactive({input$ColSelection})
  reactCat=reactive({input$CategoryChoose})
  reactYearChoose=reactive({input$yearChoose})
  reactChooseDis=reactive({input$ChooseDistrict})
  reactyearPareto=reactive({input$yearChoosePareto})
  ################################################# Preparing Geo Code fetching####################################

  ###### Binding Branch & disctrict based on Branchcode OrigOffice
  #BranchCity=unique(BranchMapping$BranchDesc)
  #BranchCity=BranchCity[!is.na(BranchCity)]
  #Dcountry=data.frame(BranchCity,stringsAsFactors = FALSE)
  #geocodes <- geocode(as.character(Dcountry$BranchCity))
  #Dcountry <- data.frame(Dcountry[,1],geocodes,stringsAsFactors = FALSE)
  #write.csv(BranchMapping,file="BranchGeoCode.csv")
  ################################################# Category charts ####################################

   ################################################# Category view ####################################
  output$Categorychart<-renderPlotly({
    plot_ly(x = ENAmaster2015_GroupbyCat$Category ,y = ENAmaster2015_GroupbyCat$cnt,name ="2015 Feedbacks",type="bar")
    add_trace(x = ENAmaster2016_GroupbyCat$Category, y=ENAmaster2016_GroupbyCat$cnt,name="2016 Feedbacks", type="bar") %>%
      layout(xaxis=x1,yaxis=list(title = "Quantity", titlefont = f))%>%
      layout(title="Feedback Comparison")
    #rm(subdf)
  })
  output$CategorychartCat2<-renderPlotly({
    plot_ly(x = ENAmaster2015_GroupbyDef$Defect_type_group ,y = ENAmaster2015_GroupbyDef$cnt,name ="2015 Feedbacks",type="bar")
    add_trace(x = ENAmaster2016_GroupbyDef$Defect_type_group, y=ENAmaster2016_GroupbyDef$cnt,name="2016 Feedbacks", type="bar") %>%
      layout(yaxis=list(title = "Quantity", titlefont = f))%>%
      layout(title="Category 2 Feedbacks VS Defect type")
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

  ################################################# EFR DataTable charts ####################################
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
      addCircles(popup= ~BranchDesc)%>%
      #addProviderTiles("NASAGIBS.ViirsEarthAtNight2012")%>%
      #addProviderTiles("Esri.WorldImagery")
    print(m)
  })
  output$mapKONEQFB = renderLeaflet({
    #require(leaflet)
    ENAMasterSource_Sub=subset(ENAMasterSource,!is.na(BranchDesc))
    ENAMasterSource_Sub=subset(ENAMasterSource_Sub,FB_Responsibility==reactCat())
    m=leaflet(na.omit(ENAMasterSource_Sub)) %>% addTiles() %>%
      #setView(lng=mean(ENAMasterSource$lon),lat=mean(ENAMasterSource$lat), zoom=3)%>%
      addCircleMarkers(data = ENAMasterSource_Sub, lng = ~ lon, lat = ~ lat, radius = 5,clusterOptions = markerClusterOptions())
    #addCircles(popup= ~Category)
    print(m)
  })
  #write.csv(ENAMasterSource,file = "testssd.csv")
  ####################################### Other charts ####################################

  output$ScatRpt1<-renderPlotly({
    subdf=subset(ENAMasterSource,Created_on_year==reactYearChoose())
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
  #table(ENAmastr15_GbyDisFbyC2$District)
  ####################################### Canada charts ####################################
  output$LineChartCanada<-renderPlotly({
    Plot2015= ENAmastr15_GbyDisFbyC2%>%filter(Region=="Canada")%>%
   # select(-District,-Region)%>%
    group_by(CreatedMonth)%>%summarise(cnt=sum(n))
    Plot2016= ENAmastr16_GbyDisFbyC2%>%filter(Region=="Canada")%>%
      #select(-District,-Region)%>%
      group_by(CreatedMonth)%>%summarise(cnt=sum(n))
     #ENAmaster2016_GroupbyMonth= subdf2016%>%group_by(CreatedMonth)%>%summarise(cnt=n())
    plot_ly(x = Plot2015$CreatedMonth ,y = cumsum(Plot2015$cnt),name ="2015 Actual",line = list(shape = "spline")) %>%

    add_trace(x = Plot2016$CreatedMonth,y=cumsum(Plot2016$cnt),name="2016 Actual", line = list(shape = "spline")) %>%
      add_trace(x = Plot2016$CreatedMonth,y=cumsum(Plot2016$cnt),showlegend=FALSE, text=cumsum(Plot2016$cnt), mode="text",textposition = "top left") %>%
      layout(xaxis=x,yaxis=list(title = "Quantity", titlefont = f))%>%
      layout(title="CANADA Cat 2 Claims 2016")
    #rm(subdf)

  })
  output$LineChartEastCanada<-renderPlotly({
    Plot2015= ENAmastr15_GbyDisFbyC2%>%filter(District=="Eastern Canada")%>%
      # select(-District,-Region)%>%
      group_by(CreatedMonth)%>%summarise(cnt=sum(n))
    Plot2016= ENAmastr16_GbyDisFbyC2%>%filter(District=="Eastern Canada")%>%
      #select(-District,-Region)%>%
      group_by(CreatedMonth)%>%summarise(cnt=sum(n))
    #ENAmaster2016_GroupbyMonth= subdf2016%>%group_by(CreatedMonth)%>%summarise(cnt=n())
    plot_ly(x = Plot2015$CreatedMonth ,y = cumsum(Plot2015$cnt),name ="2015 Actual",line = list(shape = "spline")) %>%

      add_trace(x = Plot2016$CreatedMonth,y=cumsum(Plot2016$cnt),name="2016 Actual", line = list(shape = "spline")) %>%
      add_trace(x = Plot2016$CreatedMonth,y=cumsum(Plot2016$cnt),showlegend=FALSE,text=cumsum(Plot2016$cnt), mode="text",textposition = "top left") %>%
      layout(xaxis=x,yaxis=list(title = "Quantity", titlefont = f))%>%
      layout(title="Eastern CANADA Cat 2 Claims 2016")

  })
  output$LineChartWestCanada<-renderPlotly({
    Plot2015= ENAmastr15_GbyDisFbyC2%>%filter(District=="Western Canada")%>%
      # select(-District,-Region)%>%
      group_by(CreatedMonth)%>%summarise(cnt=sum(n))
    Plot2016= ENAmastr16_GbyDisFbyC2%>%filter(District=="Western Canada")%>%
      #select(-District,-Region)%>%
      group_by(CreatedMonth)%>%summarise(cnt=sum(n))
    #ENAmaster2016_GroupbyMonth= subdf2016%>%group_by(CreatedMonth)%>%summarise(cnt=n())
    plot_ly(x = Plot2015$CreatedMonth ,y = cumsum(Plot2015$cnt),name ="2015 Actual",line = list(shape = "spline")) %>%

      add_trace(x = Plot2016$CreatedMonth,y=cumsum(Plot2016$cnt),name="2016 Actual", line = list(shape = "spline")) %>%
      add_trace(x = Plot2016$CreatedMonth,y=cumsum(Plot2016$cnt),showlegend=FALSE,text=cumsum(Plot2016$cnt), mode="text",textposition = "top left") %>%
      layout(xaxis=x,yaxis=list(title = "Quantity", titlefont = f))%>%
      layout(title="Western CANADA Cat 2 Claims 2016")

  })
  output$LineChartCentCanada<-renderPlotly({
    Plot2015= ENAmastr15_GbyDisFbyC2%>%filter(District=="Central Canada")%>%
      # select(-District,-Region)%>%
      group_by(CreatedMonth)%>%summarise(cnt=sum(n))
    Plot2016= ENAmastr16_GbyDisFbyC2%>%filter(District=="Central Canada")%>%
      #select(-District,-Region)%>%
      group_by(CreatedMonth)%>%summarise(cnt=sum(n))
    #ENAmaster2016_GroupbyMonth= subdf2016%>%group_by(CreatedMonth)%>%summarise(cnt=n())
    plot_ly(x = Plot2015$CreatedMonth ,y = cumsum(Plot2015$cnt),name ="2015 Actual",line = list(shape = "spline")) %>%

      add_trace(x = Plot2016$CreatedMonth,y=cumsum(Plot2016$cnt),name="2016 Actual", line = list(shape = "spline")) %>%
      add_trace(x = Plot2016$CreatedMonth,y=cumsum(Plot2016$cnt),showlegend=FALSE,text=cumsum(Plot2016$cnt), mode="text",textposition = "top left") %>%
      layout(xaxis=x,yaxis=list(title = "Quantity", titlefont = f))%>%
      layout(title="Central CANADA Cat 2 Claims 2016")

  })
  ####################################### RMA charts ####################################
  output$RMALineChartRegion<-renderPlotly({

    Plot2015=subdf2015 %>%
      group_by(CreatedMonth) %>%
      filter(District== reactChooseDis())%>%
      mutate(RMAPer = sum(RMAReceived)/sum(ReturnDelReq=="YES")) %>%
      summarise(RMAAvgPer=mean(RMAPer))
    Plot2016=subdf2016 %>%
      group_by(CreatedMonth) %>%
      filter(District== reactChooseDis())%>%
      mutate(RMAPer = sum(RMAReceived)/sum(ReturnDelReq=="YES")) %>%
      summarise(RMAAvgPer=mean(RMAPer))
      plot_ly(x = Plot2015$CreatedMonth ,y = round(Plot2015$RMAAvgPer*100,2),name ="2015 Actual",line = list(shape = "spline")) %>%
      add_trace(x = Plot2015$CreatedMonth ,y = round(Plot2015$RMAAvgPer*100,2),showlegend=FALSE,text=paste0(round(Plot2015$RMAAvgPer*100,2),"%"), mode="text",textposition = "top left") %>%
      add_trace(x = Plot2016$CreatedMonth ,y = round(Plot2016$RMAAvgPer*100,2),name="2016 Actual", line = list(shape = "spline")) %>%
      add_trace(x = Plot2016$CreatedMonth ,y = round(Plot2016$RMAAvgPer*100,2),showlegend=FALSE,text=paste0(round(Plot2016$RMAAvgPer*100,2),"%"), mode="text",textposition = "top left") %>%
      layout(xaxis=x,yaxis=list(title = "Percentage", titlefont = f))%>%
      layout(title="RMA Claims success percentage")
    })
  ####################################### Improvement charts ####################################
  dataTop10CosPlot2015=reactive({
    Top10CosPlot2015=subdf2015 %>%
      group_by(KONE_SO_code) %>%
      #filter(District== input$ChooseDistrict)%>%
      filter(substr(FB_Responsibility, 1, 3)=="Fro") %>%
      summarise(TotCos=sum(TotalCosts))%>%
      top_n(10,TotCos)%>%
      arrange(desc(TotCos))
  })
  dataTop10CosPlot2016 = reactive({
    Top10CosPlot2016=subdf2016 %>%
      group_by(KONE_SO_code) %>%
      filter(substr(FB_Responsibility, 1, 3)=="Fro") %>%
      #filter(District== input$ChooseDistrict)%>%
      summarise(TotCos=sum(TotalCosts))%>%
      top_n(10,TotCos)%>%
      arrange(desc(TotCos))
  })
  output$TotCostPlot<-renderPlotly({
    TotCosPlot2015=subdf2015 %>%
      group_by(CreatedMonth) %>%
      filter(substr(FB_Responsibility, 1, 3)=="Fro") %>%
      summarise(TotCos=sum(TotalCosts))
    TotCosPlot2016=subdf2016 %>%
      group_by(CreatedMonth) %>%
      filter(substr(FB_Responsibility, 1, 3)=="Fro") %>%
      summarise(TotCos=sum(TotalCosts))
    plot_ly(x = TotCosPlot2015$CreatedMonth ,y = TotCosPlot2015$TotCos,name ="2015",line = list(shape = "spline"))%>%
    add_trace(x = TotCosPlot2016$CreatedMonth, y=TotCosPlot2016$TotCos,name="2016", type="bar") %>%
      add_trace(x = TotCosPlot2016$CreatedMonth, y=TotCosPlot2016$TotCos,text=paste0(round(TotCosPlot2016$TotCos,0),"USD") ,mode="text",textposition = "top right",showlegend=FALSE,hoverinfo="none") %>%
      add_trace(x = TotCosPlot2015$CreatedMonth, y=TotCosPlot2015$TotCos,text=paste0(round(TotCosPlot2015$TotCos,0),"USD"), mode="text",textposition = "top left",showlegend=FALSE,hoverinfo="none") %>%
      layout(xaxis=x,yaxis=list(title = "Total Cost in USD", titlefont = f))%>%
      layout(title="Total Cost of Cat2 Feedback Month wise")
    #rm(subdf)
  })
  output$CostPlotVsCausecode<-renderPlotly({
    plot_ly(x = ENAmaster2015_GroupbyCauseCode$Cause_code ,y = ENAmaster2015_GroupbyCauseCode$cnt,name ="2015",type="bar")%>%
      add_trace(x = ENAmaster2016_GroupbyCauseCode$Cause_code, y=ENAmaster2016_GroupbyCauseCode$cnt,name="2016", type="bar") %>%
      add_trace(x = ENAmaster2016_GroupbyCauseCode$Cause_code, y=ENAmaster2016_GroupbyCauseCode$cnt,text=paste0(round(ENAmaster2016_GroupbyCauseCode$cnt,0),"USD") ,mode="text",textposition = "top right",showlegend=FALSE,hoverinfo="none") %>%
      add_trace(x = ENAmaster2015_GroupbyCauseCode$Cause_code, y=ENAmaster2015_GroupbyCauseCode$cnt,text=paste0(round(ENAmaster2015_GroupbyCauseCode$cnt,0),"USD"), mode="text",textposition = "top left",showlegend=FALSE,hoverinfo="none") %>%
      layout(xaxis=list(title = "Cause Code", titlefont = f),yaxis=list(title = "Total Cost in USD", titlefont = f))%>%
      layout(title="Category 2 TotalCost VS Cause code")
    #rm(subdf)
  })
  output$TopTenClaims<-renderPlotly({
    if (reactyearPareto()==2016)
    {
      plot_ly(data=dataTop10CosPlot2016(),x = KONE_SO_code, y=TotCos,name="2016", type="bar") %>%
        add_trace(x = KONE_SO_code, y=cumsum(TotCos),name="2016",line = list(shape = "spline")) %>%
        add_trace(x = KONE_SO_code, y=cumsum(TotCos),text=KONE_SO_code,mode="text",textposition ="bottom right",
                  showlegend=FALSE,hoverinfo="none",textorientation="vertical") %>%
        layout(xaxis=list(title = "KONE SO Code", titlefont = f),yaxis=list(title = "Total Cost in USD", titlefont = f))%>%
        layout(title="2016 Category 2 TotalCost VS KONE SO Code")
    }
    else
    {
      plot_ly(data=dataTop10CosPlot2015(),x = KONE_SO_code ,y = TotCos,name ="2015",type="bar")%>%
        add_trace(x = KONE_SO_code ,y = cumsum(TotCos),name ="2015",line = list(shape = "spline"))%>%
        add_trace(x = KONE_SO_code, y=cumsum(TotCos),text=KONE_SO_code,mode="text",textposition ="bottom right",
                  showlegend=FALSE,hoverinfo="none",textorientation="vertical") %>%
        layout(xaxis=list(title = "KONE SO Code", titlefont = f),yaxis=list(title = "Total Cost in USD", titlefont = f))%>%
        layout(title="2015 Category 2 TotalCost VS KONE SO Code")
    }
  })
  output$tableTopTenClaims = renderDataTable({
    if (reactyearPareto()==2016)
    {
      datatable(dataTop10CosPlot2016(),rownames = FALSE, colnames = c("KONE SO CODE","Total cost in USD($)"),class = 'cell-border stripe')
    }
    else
    {
      datatable(dataTop10CosPlot2015(),rownames = FALSE, colnames = c("KONE SO CODE","Total cost in USD($)"),class = 'cell-border stripe')
    }
    #datatable(filteredData[,columns,drop=FALSE], filter="top",options = list(lengthChange = FALSE))
  })
  ####################################### Neveda charts ####################################
  # df=EFRDel%>%
  #   filter(as.numeric(DelYear)>=2015) %>%
  #   group_by(DelYear,DelMonth,District) %>%
  #   summarise(cnt=n_distinct(Unique.SL.Order))
  # DelbyMonth=spread(df,DelMonth,cnt)
  })
