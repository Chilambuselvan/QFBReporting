#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
# please Contact e.chilambuselvan@kone.com / +919600093567
#https://java.com/en/download/manual.jsp
library(shinydashboard)
library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(googleVis)
library(ggmap)
library(lubridate)
library(XLConnect)
library(tidyr)

#devtools::install_github("rstudio/addinexamples", type = "source")

myvar=0
if (myvar==1){
#"C:/Official/QFBReporting/Data"
#F:/Official/TReporting/QFbReporting/QFBReporting/Data
  setwd("F:/Official/TReporting/QFbReporting/QFBReporting/Data")

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
  RMASearchstr <- c("NOT", "EXPIRED")
  RMASearchstr<-as.data.frame(t(RMASearchstr))
  ENAMasterSource$RMAReceived=as.integer(grepl(paste(RMASearchstr, collapse = "|"), ENAMasterSource$`Ret_Del'y_Nr`))
  wb = loadWorkbook("EFR Del.xls")
  EFRDel = readWorksheet(wb, sheet = 1, header = TRUE)
  ENAMasterSource$Orig._shipping_month=substr(ENAMasterSource$Orig._shipping_month,6,7)
  SalesOff_SOP=unique(ENAMasterSource[c("Sold-to_Party","OrigOffice")])
  EFRDel=left_join(EFRDel,SalesOff_SOP,by=c("A_Customer"="Sold-to_Party"))
  EFRDel=left_join(EFRDel,BranchMapping,by=c("OrigOffice"="Branch"))
  wb= loadWorkbook("EFR Feedback.xls")
  EFRFeedback = readWorksheet(wb, sheet = 1, header = TRUE)

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
  RegionSel=unique(ENAMasterSource$District)
  MonSel = month.abb[unique(ENAMasterSource$CreatedMonth)]
}

#filteredData = subset(COR_EFRMaster,COR_EFRMaster$Branches=="Las Vegas")


#match("Jan",month.abb)
# Define UI for application that draws a histogram
shinyUI(
  dashboardPage(
    dashboardHeader(title = "ENA Reporting"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Category View", tabName = "CatView", icon = icon("dot-circle-o")),
        menuItem("DashBoard", tabName = "Dashboard", icon = icon("dashboard")),
        menuItem("EFR DATA View", tabName = "EFRView", icon = icon("table")),
        menuItem("MAP Interactive", tabName = "MapInteractive", icon = icon("map-o")),
        menuItem("OtherReports", tabName = "OthReport", icon = icon("list-alt")),
        menuItem("Branch Locations", tabName = "BranchLoc", icon = icon("map")),
        menuItem("CANADA report", tabName = "CanReport", icon = icon("archive")),
        menuItem("RMA Report", tabName = "RMAReport", icon = icon("random")),
        menuItem("Quality Improvement Area", tabName = "QFbImprovement", icon = icon("bars"))
        # menuItem("Closed opportunities", tabName = "widgets2", icon = icon("th"))
      )
    ),

    dashboardBody(
      tabItems(
        tabItem(tabName = "CatView",
                fluidRow(
                  column(6,plotlyOutput("Categorychart",width = 600)),
                  column(6,plotlyOutput("CategorychartCat2",width = 600)
                  )
                )
        ),

        tabItem(tabName = "Dashboard",
                fluidRow(
                  column(3,selectInput(inputId = "Chars","Choose Branch",vars,multiple = FALSE))
                  #column(3,selectInput(inputId = "ColSelection","Choose Columns",VarCol,multiple = TRUE))
                ),
                fluidRow(
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
        tabItem(tabName = "EFRView",
                dataTableOutput("mytable")
        ),
        tabItem(tabName = "MapInteractive",
                fluidRow(
                  column(2),
                  column(3,selectInput(inputId = "CategoryChoose","Choose Category",CatSel,multiple = FALSE))
                ),
                leafletOutput("mapKONEQFB", height = 500)
        ),
        tabItem(tabName = "OthReport",
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
        tabItem(tabName = "BranchLoc",
                leafletOutput("mapKONEQFB_Branch")
        ),
        tabItem(tabName = "CanReport",
                fluidRow(
                  column(6,
                         fluidRow(plotlyOutput("LineChartCanada")),
                         fluidRow(plotlyOutput("LineChartEastCanada"))
                  ),
                  column(6,
                         fluidRow(plotlyOutput("LineChartCentCanada")),
                         fluidRow(plotlyOutput("LineChartWestCanada"))
                  )
                )
        ),
        tabItem(tabName = "RMAReport",
                fluidRow(
                  column(3,selectInput(inputId = "ChooseDistrict","Choose District",RegionSel,multiple = FALSE))
                  #column(3,selectInput(inputId = "MonthChooseCanReport","Choose Month",MonSel,multiple = FALSE))

                ),
                fluidRow(
                  #column(2,"Saml"),
                  column(12,
                         fluidRow(plotlyOutput("RMALineChartRegion"))
                  )
                )
        ),
        tabItem(tabName = "QFbImprovement",
                fluidRow(
                  column(10,fluidRow(plotlyOutput("TotCostPlot")))
                ),
                fluidRow(
                  column(10,fluidRow(plotlyOutput("CostPlotVsCausecode")))
                ),
                fluidRow(
                  fluidRow(
                    column(3,selectInput(inputId = "yearChoosePareto","Choose Year",YearSel,multiple = FALSE))
                  ),
                  fluidRow(
                    column(10,fluidRow(plotlyOutput("TopTenClaims")))
                  ),
                  fluidRow(
                    column(1),
                    column(6,fluidRow(
                      dataTableOutput("tableTopTenClaims")
                    ))
                  ))
        )


      )
    )#dashboard Body Close
  )#dashboard page close
)