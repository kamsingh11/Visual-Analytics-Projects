library(plotly)
library(shiny)
library(shinydashboard)
shinyUI(
  
  dashboardPage(title = "India Trade", skin = "yellow",
                
    dashboardHeader(title = "India - Export & Import", titleWidth = 800, 
                    tags$li(class="dropdown", tags$a(href="https://github.com/kamsingh11/Visual-Analytics-Projects",icon("github"),"Source Code",target="_blank"))),
                    #title = span(tagList(img(src="github.jfif", width = 60), tags$a(href="https://github.com/kamsingh11/datascience")))),
    dashboardSidebar(
      width = 200,
      sidebarMenu(id = 'sidebarmenu',
                  # first menu item
                 # menuItem("Dashboard", tabName = "Dashboard", icon = icon("dashboard")),
                  
                  # second menu item with 2 sub menus
                  menuItem('Presentation', tabName = "Presentation", icon = icon("about")
                           # menuSubItem('Report',
                           #             tabName = 'repo'),
                           # menuSubItem('Presentation',
                           #             tabName = 'ppt')
                           #             
      ),
      # second menu item with 2 sub menus
      menuItem("Dataset", tabName = "Data", icon = icon("data")),
      menuItem("Imports & Exports", 
               menuSubItem('Visual 1',
                           tabName = 'Expo', icon = icon("export")),
               menuSubItem('Visual 2',
                           tabName = 'Impo', icon = icon("import"))
               
          ),
      menuItem("Analysis", 
               menuSubItem('Country and Year',
                           tabName = 'line', icon = icon("export")),
               menuSubItem('Commodity and Deficit',
                           tabName = 'commodity', icon = icon("import"))),
      menuItem("About",  tabName = "About", icon = icon("about"))

    )),
    
    
    dashboardBody(


      tabItems(
        tabItem(
          tabName = "Presentation",
          fluidRow(
            tags$iframe(
              style="height:800px; width:100%; scrolling=yes", 
              src = "Group_Project_India_Trade.pdf"
              
            ))
        ),
        
        ## Using box to display plots
         tabItem(tabName = "line",
                 fluidRow(
                   valueBox(Total_Exports, "Total Exports", icon = icon("export")),
                   valueBox(Total_Imports, "Total Imports", icon = icon("import")),
                   valueBox(Overall_Profit, "Overall Profit", icon = icon("thumbs-down"), color = "yellow")
                   
                 ),
                 fluidRow(box(title = "Year", status = "info", solidHeader = T, plotlyOutput("line_year"), width = 12)
                         ),
         
                 fluidRow( box(title = "Country", status = "info", solidHeader = T, plotlyOutput("coun_comp"), width = 12)
                         #    box(title = "Commodity", status = "info", solidHeader = T, plotlyOutput("comm_comp"))
                          #box(title = "Import/Export & deficit", status = "info", solidHeader = T, plotlyOutput("deficit")),
                          

                )),
         tabItem(tabName = "commodity",
                 fluidRow(
                   valueBox(Total_Exports, "Total Exports", icon = icon("export")),
                   valueBox(Total_Imports, "Total Imports", icon = icon("import")),
                   valueBox(Overall_Profit, "Overall Profit", icon = icon("thumbs-down"), color = "yellow")
                   
                 ),
                 fluidRow(box(title = "Commodity", status = "info", solidHeader = T, plotlyOutput("comm_comp"), width = 12)
                 ),
                 
                 fluidRow(box(title = "Import/Export & deficit", status = "info", solidHeader = T, plotlyOutput("deficit"), width = 12)
       
                 )),
        ## Using box to display data
        tabItem(tabName = "Data",
                # First Row
                fluidRow(
                        selectInput("dataset","Select the dataset", choices=c("Export Dataset" = "expo", "Import Dataset" = "impo"))
                ),
                fluidRow(box(title = "Exports/Imports Dataset", width = 6, status = "info", solidHeader = T, DT::dataTableOutput("table")),
                         box(title = "Exports/Imports Summary Statistics", width = 6, status = "info", solidHeader = T, verbatimTextOutput("summary"))
  
                )),
        ## Display Imports & Exports
        tabItem(tabName = "Expo",
                fluidRow(
                  column(3,
                  sliderInput("Topchoose","Slide to see N Top Import/Export values",min=1,max=100,value = 5)),
                  column(3,
                  selectInput("plot_reg","Import/Export by Region", choices=c("Export by Region" = "expo_reg", "Import by Region" = "impo_reg"))),
                  column(3,
                         selectInput("plot_comm","Top 10 Commodities", choices=c("Export by Commodity" = "expo_comm", "Import by Commodity" = "impo_comm"))),
                  column(3,
                         selectInput("plot_coun","Top 10 Countries", choices=c("Export by Country" = "expo_coun", "Import by Country" = "impo_coun")))
                ),

                fluidRow(box(title = "Imports/Exports by Commodity", status = "info", solidHeader = T, plotlyOutput("bar_comm")),
                        #box(title = "Exports by Commodity", status = "info", solidHeader = T, plotlyOutput("Topexp")),
                        box(title = "Imports/Exports by Country", status = "info", solidHeader = T, plotlyOutput("bar_coun"))
                         #box(title = "Exports by Country", status = "info", solidHeader = T, plotlyOutput("TopCountriesexp"))
                         
                ),
        fluidRow(#box(title = "Exports by Country", status = "info", solidHeader = T, plotOutput("TreeMapexp")),
                 box(title = "Imports/Exports by Country", status = "info", solidHeader = T, plotOutput("tree_reg"), width = 12),
                 #box(title = "Exports by Map", status = "info", solidHeader = T, plotlyOutput("exp_plot"))
                 )
 
        
      ),
      tabItem(
        tabName = "About",
        fluidRow(
          tags$iframe(
            style="height:800px; width:100%; scrolling=yes", 
            src = "about.pdf"
            
          ))
      ),
      
      ## Display Imports
      tabItem(tabName = "Impo",
              fluidRow(
                column(3,
                sliderInput("Topchoose1","Slide to see N Top Import/Export values",min=1,max=100,value = 5)),
                column(3,
                       selectInput("plot_HS","Import/Export by Region", choices=c("Export by HS Code" = "expo_HS", "Import by HS Code" = "impo_HS"))),
                column(3,
                       selectInput("plot_bub","Import/Export by Country", choices=c("Export by Country" = "expo_bub", "Import by Country" = "impo_bub"))),
                column(3,
                       selectInput("plot_map","Import/Export by Map", choices=c("Export by Map" = "expo_map", "Import by Map" = "impo_map")))
              ),
              # First Row
              fluidRow(box(title = "Import/Export by HS Code over the years", status = "info", solidHeader = T, plotlyOutput("bar_HS")),
                       box(title = "Import/Export by Country", status = "info", solidHeader = T, plotlyOutput("bub_coun"))
                       
              ),
              fluidRow(box(title = "Imports/Exports by Map", status = "info", solidHeader = T, plotlyOutput("map_reg"), width = 12)
              )
              
              
      ),
        #tabItem("Dashboard", h4("this is the Dashboard tab page")),
        tabItem("repo", h4("this is the chart1 tab page")),
        tabItem("ppt", h4("this is the chart2 tab page")),
        tabItem("export", h4("this is the Dashboard tab page")),
        tabItem("import", h4("this is the chart1 tab page")),
        tabItem("commodity", h4("this is the Dashboard tab page")),
        tabItem("country", h4("this is the chart1 tab page")),
        tabItem("time", h4("this is the chart2 tab page"))
              )
                )
          )
)
