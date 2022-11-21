getwd()
setwd("D:/Mba/ATDSV/Assign")
library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)  
library(dplyr)
library(shinyalert)



dashboardPage(skin = "green",
  dashboardHeader(title = "Crop Production in INDIA", titleWidth = 700,
                  tags$li(class="dropdown", tags$a(href="https://www.kaggle.com/code/suchetanaroy/indian-crop-production-analysis-1997-2014/data",
                                                   icon("th"),"DataSet Link"))),
  
  dashboardSidebar(collapsed = F,width = 350,
    sidebarMenu(
      id= "sidebar", 
      menuItem("About Data", tabName = "S1", icon = icon("database")),
      menuItem("Data variables", tabName = "s11", icon = icon("table"),
               menuSubItem("Total Variables",
                           tabName = "sss0",icon = icon("table")),
               menuSubItem("State",
                           tabName = "sss1",icon = icon("table")),
               menuSubItem("District",
                           tabName = "sss2",icon = icon("table")),
               menuSubItem("Crop",
                           tabName = "sss3",icon = icon("table")),
               menuSubItem("Season",
                           tabName = "sss4",icon = icon("table")),
               menuSubItem("Year",
                           tabName = "sss5",icon = icon("table"))),
      menuItem(text = "Visualization", tabName = "S2", icon = icon("chart-line"),
               menuSubItem("Maharashtra state district vs production",
                           tabName = "ss1",icon = icon("line-chart")),
               menuSubItem("State vs production",
                           tabName = "ss2",icon = icon("line-chart")),
               menuSubItem("Crop vs production",
                           tabName = "ss3",icon = icon("line-chart")),
               menuSubItem("Season vs production",
                           tabName = "ss4",icon = icon("line-chart")),
               menuSubItem("Area vs production",
                           tabName = "ss5",icon = icon("line-chart"))),
      menuItem(text ="Top 5 production State plot", tabName = "s4", icon = icon("refresh")),
      menuItem(text ="Least 5 production State plot", tabName = "s5", icon = icon("refresh")),
     
      
      menuItem(text ="About me", tabName = "S3", icon = icon("image"))
      
      
      
  )),
   
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "S1",
              tabBox(id="t1", width= 15, 
                     tabPanel("About Crop Production in INDIA data", icon = icon("address-card"),
                              
                              fluidRow(
                                column(width = 8,tags$img(src="cp.jpeg",width= 500, height= 400)),
                
                               
                                column(width= 4, tags$br(),
                                       tags$p("This is dataset of india crop production in india in which 
                                              we have data of state, district, season and crop. This data 
                                              shows the in how much area you  get the production in each
                                              year. This data is from 1997 to 2015. In this data we can 
                                              easily know that in which crop have more production in 
                                              which year with respect to season. also we can know that 
                                              what are the crop which have less production and more
                                              production so we can focus on that crop to cater the demand 
                                              of increasing population."))
                              )),
                     
                     tabPanel("Crop Production Data Table", icon = icon("table"),
                              dataTableOutput("dt"))
              )
              ),
      tabItem(tabName = "sss0",
              tabBox(id="s11t",width=15,
                     tabPanel("Data Variables", icon = icon("table"),
                              fluidRow(infoBox(title="State",value="33",color= "green",width = 6)),
                              fluidRow(infoBox(title="District",value="646",color= "green",width = 6)),
                              fluidRow(infoBox(title="Season",value="6",color= "green",width = 6)),
                              fluidRow(infoBox(title="Crop",value="124",color= "green",width = 6)),
                              fluidRow(infoBox(title="Production",value="141176116767",color= "green",width = 6)),
                              fluidRow(infoBox(title="Area",value="2948906741",color= "green",width = 6)))
                     
                              
                     
                     )),
      tabItem(tabName = "sss1",
              tabBox(id="s10t", width = 15,
                     tabPanel(title="State wise Production & Area ",icon = icon("table"),dataTableOutput("dtst")))
      ),
      tabItem(tabName = "sss2",
              tabBox(id="s12t", width = 15,
                     tabPanel(title="District wise Production & Area",icon = icon("table"),dataTableOutput("dtd")))
      ),tabItem(tabName = "sss3",
                tabBox(id="s13t", width = 15,
                       tabPanel(title="Crop wise Production & Area",icon = icon("table"),dataTableOutput("dtc")))
      ),tabItem(tabName = "sss4",
                tabBox(id="s14t", width = 15,
                       tabPanel(title="Season wise Production & Area",icon = icon("table"),dataTableOutput("dtse")))
      ),tabItem(tabName = "sss5",
                tabBox(id="s15t", width = 15,
                       tabPanel(title="Year wise Production & Area",icon = icon("table"),dataTableOutput("dty")))
      ),
    
      tabItem(tabName = "ss1",
              tabBox(id="t3", width = 15,
                     tabPanel(title="Maharashtra state district vs production",plotlyOutput("vp1"),textOutput("t1")))
              ),
      tabItem(tabName = "ss2",
              tabBox(id="t4", width = 15,
                     tabPanel(title="State vs production",plotlyOutput("vp2"),textOutput("t2")))
      ),
      tabItem(tabName = "ss3",
              tabBox(id="t5", width = 15,
                     tabPanel(title="Crop vs production",plotlyOutput("vp3"),textOutput("t3")))
      ),
      tabItem(tabName = "ss4",
              tabBox(id="t6", width = 15,
                     tabPanel(title="Season vs production",plotlyOutput("vp4"),textOutput("t4")))
      ),
      tabItem(tabName = "ss5",
              tabBox(id="t7", width = 15,
                     tabPanel(title="Area vs production",plotlyOutput("vp5"),textOutput("t5")))
      ),
      
      tabItem(tabName = "S3",  fluidRow(
        column(width = 4,tags$img(src="me.jpeg",width= 250, height= 300)),
        
        column(width= 3, tags$br(),
               tags$p("Vaibhav Jaydeo Khobragade"),tags$p("Roll No 2141021"),tags$p("MBA-TEM"),tags$p("Subject: ATDSV"))
      )), 
      tabItem(tabName = "S3",  fluidRow(
        column(width = 4,tags$img(src="me.jpeg",width= 250, height= 300)),
        
        column(width= 3, tags$br(),
               tags$p("Vaibhav Jaydeo Khobragade"),tags$p("Roll No 2141021"),tags$p("MBA-TEM"),tags$p("Subject: ATDSV"))
      )),
      tabItem(tabName = "s4",
              tabBox(id="t170", width = 15,
                     tabPanel(title="Kerala",plotlyOutput("vker")),
                     tabPanel(title= "Andhra Pradesh",plotlyOutput("vanp")),
                     tabPanel(title="Tamil Nadu",plotlyOutput("vtn")),
                     tabPanel(title="Uttar Pradesh",plotlyOutput("vup")),
                     tabPanel(title="Assam",plotlyOutput("vasm"))
                     
                     )),
      
      tabItem(tabName = "s5",
              tabBox(id="t171", width = 15,
                     tabPanel(title="Chandigarh",plotlyOutput("vcha")),
                     tabPanel(title= "Mizoram",plotlyOutput("vmiz")),
                     tabPanel(title="Dadra and Nagar Haveli",plotlyOutput("vdnh")),
                     tabPanel(title="Sikkim",plotlyOutput("vsik")),
                     tabPanel(title="Manipur",plotlyOutput("vman"))
              ))
      
      
  
)))
