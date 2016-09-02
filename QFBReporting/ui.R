#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
# please Contact e.chilambuselvan@kone.com / +919600093567
#

library(shiny)
library(leaflet)
library(plotly)
library(dplyr)
library(DT)
library(data.table)
library(googleVis)
library(ggmap)
library(lubridate)

#devtools::install_github("rstudio/addinexamples", type = "source")

myvar=0
if (myvar==1){
#"C:/Official/QFBReporting/Data" 
#F:/Official/TReporting/QFbReporting/QFBReporting/Data
  setwd("D:/Official/09_Analytics/QualityDashBoard/QFBReporting/Data")
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
RegionSel=unique(ENAMasterSource$District)
MonSel = month.abb[unique(ENAMasterSource$CreatedMonth)]
#match("Jan",month.abb)
# Define UI for application that draws a histogram
shinyUI(fluidPage(title = "Quality Dashboard",
                  
                  tabsetPanel(
                    tabPanel(title = "Index Page"
                             
                    ),
                    tabPanel(title = "Category View",
                             fluidRow(
                               column(6,plotlyOutput("Categorychart",width = 600)),
                               column(6,plotlyOutput("CategorychartCat2",width = 600)
                               )
                             )
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
                             
                    ),
                    tabPanel(title = "Canada Report",
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
                    tabPanel(title = "RMA Report",
                             fluidRow(
                                column(3,selectInput(inputId = "ChooseDistrict","Choose District",RegionSel,multiple = FALSE))
                                #column(3,selectInput(inputId = "MonthChooseCanReport","Choose Month",MonSel,multiple = FALSE))
                             
                             ),
                             fluidRow(
                               #column(2,"Saml"),
                               column(12,
                                      fluidRow(plotlyOutput("RMALineChartRegion"))
                                      
                               )
                              # column(12,
                              #        fluidRow(plotlyOutput("RMALineChartDistrict"))
                                      
                               #)
                             )
                        ),
                    tabPanel(title = "Quality Improvement Area",
                             fluidRow(
                               column(10,fluidRow(plotlyOutput("TotCostPlot")))
                              ),
                             fluidRow(
                              column(10,fluidRow(plotlyOutput("CostPlotVsCausecode")))
                             )
                             
                    )
                    
                  )
  

))
