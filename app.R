library(readxl)
library(dplyr)
library(ggplot2)
library(leaflet)
source("makeMap.R")
source("makePlots.R")

ui <- navbarPage("Sun Prairie Community Schools",
  tabPanel("Westside Elementary",
           fluidRow(column(6,leafletOutput("map.ws")),
                    column(6,"Census block(s): 011505-2, 011506-2")),
           br(),h4("Attendance rate"),
           tabsetPanel(
             tabPanel("By day",fluidRow(column(6,plotOutput("p.att.day.ws")))),
             tabPanel("By month",fluidRow(column(6,plotOutput("p.att.month.ws")))),
             tabPanel("By race/ethnicity",fluidRow(column(6,plotOutput("p.att.raceeth.ws")))),
             tabPanel("By grade",fluidRow(column(6,plotOutput("p.att.grade.ws"))))
           ),
           br(),h4("Population statistics"),
           tabsetPanel(
             navbarMenu("By race/ethnicity",
                        tabPanel("census block group view",
                                 fluidRow(column(6,plotOutput("p.pop.raceeth.ws1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.raceeth.ws2"))))
             ),
             navbarMenu("By age bracket",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.agecat.ws1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.agecat.ws2"))))
             ),
             navbarMenu("By household type",
                        tabPanel("census block group view",fluidRow(column(7,plotOutput("p.pop.hhcat.ws1")),
                                                       column(5,"Household => (i) 1-person, or (ii) >=2-person",
                                                              br(),"Household => (i) family, or (ii) non-family",
                                                              br(), "Family household => (i) married-couple family, or (ii) not married-couple family",
                                                              br(), "* Color scheme here is unrelated to the color scheme for schools!"))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.hhcat.ws2"))))
             )
           ),
           br(),h4("Housing statistics"),
           tabsetPanel(
             navbarMenu("By gross rent bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.rent.ws1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.rent.ws2"))))
                        ),
             navbarMenu("By owner costs bracket (w/o mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswom.ws1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.costswom.ws2"))))
                        ),
             navbarMenu("By owner costs bracket (w/ mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswm.ws1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.costswm.ws2"))))
                        )
           ),
           br(),h4("Income statistics"),
           tabsetPanel(
             navbarMenu("By household income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.hhinc.ws1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.inc.hhinc.ws2"))))
             ),
             navbarMenu("By family income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.faminc.ws1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.inc.faminc.ws2"))))
             )
           ),
           br(),h4("Crime statistics"),
           tabsetPanel(
             tabPanel("Substance-related crimes",fluidRow(column(6,plotOutput("p.crime.subs.ws")))),
             tabPanel("Property-related crimes",fluidRow(column(6,plotOutput("p.crime.poss.ws")))),
             tabPanel("Violence-related crimes",fluidRow(column(6,plotOutput("p.crime.viol.ws"))))
             )
           ),
  tabPanel("Northside Elementary",
           fluidRow(column(6,leafletOutput("map.ns")),
                    column(6,"Census block(s): 011505-1, 011505-3, 011506-3")),
           br(),h4("Attendance rate"),
           tabsetPanel(
             tabPanel("By day",fluidRow(column(6,plotOutput("p.att.day.ns")))),
             tabPanel("By month",fluidRow(column(6,plotOutput("p.att.month.ns")))),
             tabPanel("By race/ethnicity",fluidRow(column(6,plotOutput("p.att.raceeth.ns")))),
             tabPanel("By grade",fluidRow(column(6,plotOutput("p.att.grade.ns"))))
           ),
           br(),h4("Population statistics"),
           tabsetPanel(
             navbarMenu("By race/ethnicity",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.raceeth.ns1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.raceeth.ns2"))))
             ),
             navbarMenu("By age bracket",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.agecat.ns1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.agecat.ns2"))))
             ),
             navbarMenu("By household type",
                        tabPanel("census block group view",fluidRow(column(8,plotOutput("p.pop.hhcat.ns1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.hhcat.ns2"))))
             )
           ),
           br(),h4("Housing statistics"),
           tabsetPanel(
             navbarMenu("By gross rent bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.rent.ns1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.rent.ns2"))))
             ),
             navbarMenu("By owner costs bracket (w/o mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswom.ns1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.costswom.ns2"))))
             ),
             navbarMenu("By owner costs bracket (w/ mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswm.ns1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.costswm.ns2"))))
             )
           ),
           br(),h4("Income statistics"),
           tabsetPanel(
             navbarMenu("By household income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.hhinc.ns1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.inc.hhinc.ns2"))))
             ),
             navbarMenu("By family income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.faminc.ns1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.inc.faminc.ns2"))))
             )
           ),
           br(),h4("Crime statistics"),
           tabsetPanel(
             tabPanel("Substance-related crimes",fluidRow(column(6,plotOutput("p.crime.subs.ns")))),
             tabPanel("Property-related crimes",fluidRow(column(6,plotOutput("p.crime.poss.ns")))),
             tabPanel("Violence-related crimes",fluidRow(column(6,plotOutput("p.crime.viol.ns"))))
             )
           ),
  tabPanel("CH Bird Elementary",
           fluidRow(column(6,leafletOutput("map.ch")),
                    column(6,"Census block(s): 011504-1")),
           br(),h4("Attendance rate"),
           tabsetPanel(
             tabPanel("By day",fluidRow(column(6,plotOutput("p.att.day.ch")))),
             tabPanel("By month",fluidRow(column(6,plotOutput("p.att.month.ch")))),
             tabPanel("By race/ethnicity",fluidRow(column(6,plotOutput("p.att.raceeth.ch")))),
             tabPanel("By grade",fluidRow(column(6,plotOutput("p.att.grade.ch"))))
           ),
           br(),h4("Population statistics"),
           tabsetPanel(
             navbarMenu("By race/ethnicity",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.raceeth.ch"))))
             ),
             navbarMenu("By age bracket",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.agecat.ch"))))
             ),
             navbarMenu("By household type",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.hhcat.ch"))))
             )
           ),
           br(),h4("Housing statistics"),
           tabsetPanel(
             navbarMenu("By gross rent bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.rent.ch"))))
             ),
             navbarMenu("By owner costs bracket (w/o mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswom.ch"))))
             ),
             navbarMenu("By owner costs bracket (w/ mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswm.ch"))))
             )
           ),
           br(),h4("Income statistics"),
           tabsetPanel(
             navbarMenu("By household income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.hhinc.ch"))))
             ),
             navbarMenu("By family income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.faminc.ch"))))
             )
           ),
           br(),h4("Crime statistics"),
           tabsetPanel(
             tabPanel("Substance-related crimes",fluidRow(column(6,plotOutput("p.crime.subs.ch")))),
             tabPanel("Property-related crimes",fluidRow(column(6,plotOutput("p.crime.poss.ch")))),
             tabPanel("Violence-related crimes",fluidRow(column(6,plotOutput("p.crime.viol.ch"))))
             )
           ),
  tabPanel("Patrick Marsh Middle",
           fluidRow(column(6,leafletOutput("map.pm")),
                    column(6,"Census block(s): 011600-2, 011700-2, 011506-1", br(), 
                           "Shared w/ Northside: 011505-1, 011505-3, 011506-3", br(),
                           "Shared w/ CH Bird: 011504-1")),
           br(),h4("Attendance rate"),
           tabsetPanel(
             tabPanel("By day",fluidRow(column(6,plotOutput("p.att.day.pm")))),
             tabPanel("By month",fluidRow(column(6,plotOutput("p.att.month.pm")))),
             tabPanel("By race/ethnicity",fluidRow(column(6,plotOutput("p.att.raceeth.pm")))),
             tabPanel("By grade",fluidRow(column(6,plotOutput("p.att.grade.pm"))))
           ),
           br(),h4("Population statistics"),
           tabsetPanel(
             navbarMenu("By race/ethnicity",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.raceeth.pm1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.raceeth.pm2"))))
             ),
             navbarMenu("By age bracket",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.agecat.pm1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.agecat.pm2"))))
             ),
             navbarMenu("By household type",
                        tabPanel("census block group view",fluidRow(column(8,plotOutput("p.pop.hhcat.pm1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.hhcat.pm2"))))
             )
           ),
           br(),h4("Housing statistics"),
           tabsetPanel(
             navbarMenu("By gross rent bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.rent.pm1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.rent.pm2"))))
             ),
             navbarMenu("By owner costs bracket (w/o mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswom.pm1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.costswom.pm2"))))
             ),
             navbarMenu("By owner costs bracket (w/ mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswm.pm1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.costswm.pm2"))))
             )
           ),
           br(),h4("Income statistics"),
           tabsetPanel(
             navbarMenu("By household income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.hhinc.pm1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.inc.hhinc.pm2"))))
             ),
             navbarMenu("By family income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.faminc.pm1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.inc.faminc.pm2"))))
             )
           ),
           br(),h4("Crime statistics"),
           tabsetPanel(
             tabPanel("Substance-related crimes",fluidRow(column(6,plotOutput("p.crime.subs.pm")))),
             tabPanel("Property-related crimes",fluidRow(column(6,plotOutput("p.crime.poss.pm")))),
             tabPanel("Violence-related crimes",fluidRow(column(6,plotOutput("p.crime.viol.pm"))))
             )
           ),
  tabPanel("Combined",
           fluidRow(column(6,leafletOutput("map.combined"))),
           br(),h4("Attendance rate"),
           tabsetPanel(
             tabPanel("By day",fluidRow(column(6,plotOutput("p.att.day.all")))),
             tabPanel("By month",fluidRow(column(6,plotOutput("p.att.month.all")))),
             tabPanel("By race/ethnicity",fluidRow(column(6,plotOutput("p.att.raceeth.all")))),
             tabPanel("By grade",fluidRow(column(6,plotOutput("p.att.grade.all"))))
           ),
           br(),h4("Population statistics"),
           tabsetPanel(
             navbarMenu("By race/ethnicity",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.raceeth.all1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.raceeth.all2"))))
             ),
             navbarMenu("By age bracket",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.agecat.all1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.agecat.all2"))))
             ),
             navbarMenu("By household type",
                        tabPanel("census block group view",fluidRow(column(6,plotOutput("p.pop.hhcat.all1")))),
                        tabPanel("combined view",fluidRow(column(6,plotOutput("p.pop.hhcat.all2"))))
             )
           ),
           br(),h4("Housing statistics"),
           tabsetPanel(
             navbarMenu("By gross rent bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.rent.all1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.rent.all2"))))
             ),
             navbarMenu("By owner costs bracket (w/o mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswom.all1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.costswom.all2"))))
             ),
             navbarMenu("By owner costs bracket (w/ mortage)",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.hous.costswm.all1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.hous.costswm.all2"))))
             )
           ),
           br(),h4("Income statistics"),
           tabsetPanel(
             navbarMenu("By household income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.hhinc.all1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.inc.hhinc.all2"))))
             ),
             navbarMenu("By family income bracket",
                        tabPanel("Block view",fluidRow(column(6,plotOutput("p.inc.faminc.all1")))),
                        tabPanel("Combined view",fluidRow(column(6,plotOutput("p.inc.faminc.all2"))))
             )
           ),
           br(),h4("Crime statistics"),
           tabsetPanel(
             tabPanel("Substance-related crimes",fluidRow(column(6,plotOutput("p.crime.subs.all")))),
             tabPanel("Property-related crimes",fluidRow(column(6,plotOutput("p.crime.poss.all")))),
             tabPanel("Violence-related crimes",fluidRow(column(6,plotOutput("p.crime.viol.all"))))
             )
           )
)

server <- function(input,output) {
  ## sunprairie map
  output$map.ws <- renderLeaflet({m.ws})
  output$map.ns <- renderLeaflet({m.ns})
  output$map.ch <- renderLeaflet({m.ch})
  output$map.pm <- renderLeaflet({m.pm})
  output$map.combined <- renderLeaflet({m})
  
  ## attendance plots
  output$p.att.day.ws <- renderPlot({p.att.day.ws})
  output$p.att.day.ns <- renderPlot({p.att.day.ns})
  output$p.att.day.ch <- renderPlot({p.att.day.ch})
  output$p.att.day.pm <- renderPlot({p.att.day.pm})
  output$p.att.day.all <- renderPlot({p.att.day.all})
  
  output$p.att.month.ws <- renderPlot({p.att.month.ws})
  output$p.att.month.ns <- renderPlot({p.att.month.ns})
  output$p.att.month.ch <- renderPlot({p.att.month.ch})
  output$p.att.month.pm <- renderPlot({p.att.month.pm})
  output$p.att.month.all <- renderPlot({p.att.month.all})
  
  output$p.att.raceeth.ws <- renderPlot({p.att.raceeth.ws})
  output$p.att.raceeth.ns <- renderPlot({p.att.raceeth.ns})
  output$p.att.raceeth.ch <- renderPlot({p.att.raceeth.ch})
  output$p.att.raceeth.pm <- renderPlot({p.att.raceeth.pm})
  output$p.att.raceeth.all <- renderPlot({p.att.raceeth.all})
  
  output$p.att.grade.ws <- renderPlot({p.att.grade.ws})
  output$p.att.grade.ns <- renderPlot({p.att.grade.ns})
  output$p.att.grade.ch <- renderPlot({p.att.grade.ch})
  output$p.att.grade.pm <- renderPlot({p.att.grade.pm})
  output$p.att.grade.all <- renderPlot({p.att.grade.all})
  
  ## population plots
  output$p.pop.raceeth.ws1 <- renderPlot({p.pop.raceeth.ws1})
  output$p.pop.raceeth.ws2 <- renderPlot({p.pop.raceeth.ws2})
  output$p.pop.raceeth.ns1 <- renderPlot({p.pop.raceeth.ns1})
  output$p.pop.raceeth.ns2 <- renderPlot({p.pop.raceeth.ns2})
  output$p.pop.raceeth.ch <- renderPlot({p.pop.raceeth.ch})
  output$p.pop.raceeth.pm1 <- renderPlot({p.pop.raceeth.pm1})
  output$p.pop.raceeth.pm2 <- renderPlot({p.pop.raceeth.pm2})
  output$p.pop.raceeth.all1 <- renderPlot({p.pop.raceeth.all1})
  output$p.pop.raceeth.all2 <- renderPlot({p.pop.raceeth.all2})
  
  output$p.pop.agecat.ws1 <- renderPlot({p.pop.agecat.ws1})
  output$p.pop.agecat.ws2 <- renderPlot({p.pop.agecat.ws2})
  output$p.pop.agecat.ns1 <- renderPlot({p.pop.agecat.ns1})
  output$p.pop.agecat.ns2 <- renderPlot({p.pop.agecat.ns2})
  output$p.pop.agecat.ch <- renderPlot({p.pop.agecat.ch})
  output$p.pop.agecat.pm1 <- renderPlot({p.pop.agecat.pm1})
  output$p.pop.agecat.pm2 <- renderPlot({p.pop.agecat.pm2})
  output$p.pop.agecat.all1 <- renderPlot({p.pop.agecat.all1})
  output$p.pop.agecat.all2 <- renderPlot({p.pop.agecat.all2})
  
  output$p.pop.hhcat.ws1 <- renderPlot({p.pop.hhcat.ws1})
  output$p.pop.hhcat.ws2 <- renderPlot({p.pop.hhcat.ws2})
  output$p.pop.hhcat.ns1 <- renderPlot({p.pop.hhcat.ns1})
  output$p.pop.hhcat.ns2 <- renderPlot({p.pop.hhcat.ns2})
  output$p.pop.hhcat.ch <- renderPlot({p.pop.hhcat.ch})
  output$p.pop.hhcat.pm1 <- renderPlot({p.pop.hhcat.pm1})
  output$p.pop.hhcat.pm2 <- renderPlot({p.pop.hhcat.pm2})
  output$p.pop.hhcat.all1 <- renderPlot({p.pop.hhcat.all1})
  output$p.pop.hhcat.all2 <- renderPlot({p.pop.hhcat.all2})
  
  ## income plots
  output$p.inc.hhinc.ws1 <- renderPlot({p.inc.hhinc.ws1})
  output$p.inc.hhinc.ws2 <- renderPlot({p.inc.hhinc.ws2})
  output$p.inc.hhinc.ns1 <- renderPlot({p.inc.hhinc.ns1})
  output$p.inc.hhinc.ns2 <- renderPlot({p.inc.hhinc.ns2})
  output$p.inc.hhinc.ch <- renderPlot({p.inc.hhinc.ch})
  output$p.inc.hhinc.pm1 <- renderPlot({p.inc.hhinc.pm1})
  output$p.inc.hhinc.pm2 <- renderPlot({p.inc.hhinc.pm2})
  output$p.inc.hhinc.all1 <- renderPlot({p.inc.hhinc.all1})
  output$p.inc.hhinc.all2 <- renderPlot({p.inc.hhinc.all2})
  
  output$p.inc.faminc.ws1 <- renderPlot({p.inc.faminc.ws1})
  output$p.inc.faminc.ws2 <- renderPlot({p.inc.faminc.ws2})
  output$p.inc.faminc.ns1 <- renderPlot({p.inc.faminc.ns1})
  output$p.inc.faminc.ns2 <- renderPlot({p.inc.faminc.ns2})
  output$p.inc.faminc.ch <- renderPlot({p.inc.faminc.ch})
  output$p.inc.faminc.pm1 <- renderPlot({p.inc.faminc.pm1})
  output$p.inc.faminc.pm2 <- renderPlot({p.inc.faminc.pm2})
  output$p.inc.faminc.all1 <- renderPlot({p.inc.faminc.all1})
  output$p.inc.faminc.all2 <- renderPlot({p.inc.faminc.all2})
  
  ## housing plots
  output$p.hous.rent.ws1 <- renderPlot({p.hous.rent.ws1})
  output$p.hous.rent.ws2 <- renderPlot({p.hous.rent.ws2})
  output$p.hous.costswom.ws1 <- renderPlot({p.hous.costswom.ws1})
  output$p.hous.costswom.ws2 <- renderPlot({p.hous.costswom.ws2})
  output$p.hous.costswm.ws1 <- renderPlot({p.hous.costswm.ws1})
  output$p.hous.costswm.ws2 <- renderPlot({p.hous.costswm.ws2})
  
  output$p.hous.rent.ns1 <- renderPlot({p.hous.rent.ns1})
  output$p.hous.rent.ns2 <- renderPlot({p.hous.rent.ns2})
  output$p.hous.costswom.ns1 <- renderPlot({p.hous.costswom.ns1})
  output$p.hous.costswom.ns2 <- renderPlot({p.hous.costswom.ns2})
  output$p.hous.costswm.ns1 <- renderPlot({p.hous.costswm.ns1})
  output$p.hous.costswm.ns2 <- renderPlot({p.hous.costswm.ns2})
  
  output$p.hous.rent.ch <- renderPlot({p.hous.rent.ch})
  output$p.hous.costswom.ch <- renderPlot({p.hous.costswom.ch})
  output$p.hous.costswm.ch <- renderPlot({p.hous.costswm.ch})
  
  output$p.hous.rent.pm1 <- renderPlot({p.hous.rent.pm1})
  output$p.hous.rent.pm2 <- renderPlot({p.hous.rent.pm2})
  output$p.hous.costswom.pm1 <- renderPlot({p.hous.costswom.pm1})
  output$p.hous.costswom.pm2 <- renderPlot({p.hous.costswom.pm2})
  output$p.hous.costswm.pm1 <- renderPlot({p.hous.costswm.pm1})
  output$p.hous.costswm.pm2 <- renderPlot({p.hous.costswm.pm2})
  
  output$p.hous.rent.all1 <- renderPlot({p.hous.rent.all1})
  output$p.hous.rent.all2 <- renderPlot({p.hous.rent.all2})
  output$p.hous.costswom.all1 <- renderPlot({p.hous.costswom.all1})
  output$p.hous.costswom.all2 <- renderPlot({p.hous.costswom.all2})
  output$p.hous.costswm.all1 <- renderPlot({p.hous.costswm.all1})
  output$p.hous.costswm.all2 <- renderPlot({p.hous.costswm.all2})
  
  ## crime plots
  output$p.crime.subs.ws <- renderPlot({p.crime.subs.ws})
  output$p.crime.subs.ns <- renderPlot({p.crime.subs.ns})
  output$p.crime.subs.ch <- renderPlot({p.crime.subs.ch})
  output$p.crime.subs.pm <- renderPlot({p.crime.subs.pm})
  output$p.crime.subs.all <- renderPlot({p.crime.subs.all})
  
  output$p.crime.poss.ws <- renderPlot({p.crime.poss.ws})
  output$p.crime.poss.ns <- renderPlot({p.crime.poss.ns})
  output$p.crime.poss.ch <- renderPlot({p.crime.poss.ch})
  output$p.crime.poss.pm <- renderPlot({p.crime.poss.pm})
  output$p.crime.poss.all <- renderPlot({p.crime.poss.all})
  
  output$p.crime.viol.ws <- renderPlot({p.crime.viol.ws})
  output$p.crime.viol.ns <- renderPlot({p.crime.viol.ns})
  output$p.crime.viol.ch <- renderPlot({p.crime.viol.ch})
  output$p.crime.viol.pm <- renderPlot({p.crime.viol.pm})
  output$p.crime.viol.all <- renderPlot({p.crime.viol.all})
}

shinyApp(ui,server)