
library(shinydashboard)
library(shiny)
ui <- dashboardPage(
  
    dashboardHeader(title = "Fire predictor"),
  dashboardSidebar(
     sidebarMenu(
       menuItem("Theory",tabName = "Theory",icon = icon("th")),
       menuItem("set dates", tabName = "dates", icon = icon("calendar")),
       menuItem("Variables",tabName = "var",icon = icon("dashboard")),
       menuItem("codes", tabName = "codes", icon = icon("th")),
       menuItem("FIRE PLOTS",tabName = "plots",icon = icon("cog")),
       menuItem("Predicted_plots",tabName = "Predicted_Plots",icon = icon("th"))
     )
   ),
   dashboardBody(
     tabItems(
       
       tabItem(tabName = "Theory",
               "WELCOME"),
       tabItem(tabName = "dates",
               fluidRow(
                 box(title="Set Dates",status="success",solidHeader=TRUE,
                     textInput("Spin_bio0","Spin_bio",value = "2000-4-1"),
                 textInput("Spin_date","Spin_date",value = "2005-4-1"),
                 textInput("Sim_start_date","Sim_start_date",value = "2007-1-1"),
                 textInput("Sim_end_date","Sim_end_date",value = "2007-12-31"))
                 
               )
       ),
       tabItem(tabName = "var",
               textInput("lon","longitude",value = 60),
               textInput("lat","latitude",value = 60),
               textInput("batch_size","batch_size",value = 10000),
               textInput("n_steps","n_steps",value = 10000),
               textInput("learn_rate","learn_rate",value = 0.001),
               selectInput("spin","spin",choices = list("on","off"))),
      
       tabItem(tabName = "codes",
              box(title="BACK-END",status="success",solidHeader=TRUE,hr(),actionButton("simulate",code(strong("RUN"))),br(),hr(),
                  div(code(verbatimTextOutput("code1")),style='overflow-y:Scroll;width:475px;height:240px')),
              box(title="TENSORFLOW",status="success",solidHeader=TRUE,hr(),actionButton("TF_RUN",code(strong("RUN"))),br(),hr(),
                  div(code(verbatimTextOutput("code2")),style='overflow-y:Scroll;width:475px;height:240px')),
              box(title="FORWARD-PROP",status="success",solidHeader=TRUE,hr(),actionButton("analysis",code(strong("RUN"))),br(),hr(),
                  div(verbatimTextOutput("code3"),style='overflow-y:Scroll;width:475px;height:240px'))
       ),
       
       tabItem(tabName = "plots", 
               br(),
             div(h4( textOutput("selected_dates")),style='color:red'),
              br(),
              fluidRow(
                
                 tabBox(
                  
                   id = "tabs", height = "200px",width = "150px",
                   
                   
                   tabPanel("WINTER",value="W",br(),div(code(strong("MONTHS- December, January, February")),style='color:green'),br(),hr(),
                            box(title = "DECEMBER_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("december_plot",width="400px",height ="400px")),
                            box(title = "DECEMBER_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("december_predic_plot",width="400px",height ="400px")),
                            box(title = "JANUARY_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("january_plot",width="400px",height ="400px")),
                            box(title = "JANUARY_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("january_predic_plot",width="400px",height ="400px")),
                            box(title = "FEBRUARY_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("february_plot",width="400px",height ="400px")),
                            box(title = "FEBRUARY_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("february_predic_plot",width="400px",height ="400px"))),
                            
                   tabPanel("SUMMER",VALUE="S",br(),div(code(strong("MONTHS- March, April, May")),style='color:green'),br(),hr(),
                            box(title = "MARCH_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("march_plot",width="400px",height ="400px")),
                            box(title = "MARCH_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("march_predic_plot",width="400px",height ="400px")),
                            box(title = "APRIL_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("april_plot",width="400px",height ="400px")),
                            box(title = "APRIL_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("april_predic_plot",width="400px",height ="400px")),
                            box(title = "MAY_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("may_plot",width="400px",height ="400px")),
                            box(title = "MAY_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("may_predic_plot",width="400px",height ="400px"))),
                   
                   tabPanel("MONSOON",value="MONSOON",br(),div(code(strong("MONTHS- June, July, August, Septmeber")),style='color:green'),br(),hr(),
                            box(title = "JUNE_OBSERVED",status = "success",solidHeader = TRUE, plotOutput("june_plot",width="400px",height ="400px")),
                            box(title = "JUNE_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("june_predic_plot",width="400px",height ="400px")),
                            box(title = "JULY_OBSERVED",status = "success",solidHeader = TRUE, plotOutput("july_plot",width="400px",height ="400px")),
                            box(title = "JULY_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("july_predic_plot",width="400px",height ="400px")),
                            box(title = "AUGUST_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("august_plot",width="400px",height ="400px")),
                            box(title = "AUGUST_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("august_predic_plot",width="400px",height ="400px")),
                            box(title = "SEPTEMBER_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("september_plot",width="400px",height ="400px")),
                            box(title = "SEPTEMBER_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("september_predic_plot",width="400px",height ="400px"))),
                            
                   tabPanel("POST MONSOON",br(),div(code(strong("MONTHS- October, November")),style='color:green'),br(),hr(),
                            box(title = "OCTOBER_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("october_plot",width="400px",height ="400px")),
                            box(title = "OCTOBER_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("october_predic_plot",width="400px",height ="400px")),
                            box(title = "NOVEMBER_OBSERVED",status = "success",solidHeader = TRUE,plotOutput("november_plot",width="400px",height ="400px")),
                            box(title = "NOVEMBER_PREDICTED",status = "success",solidHeader = TRUE,plotOutput("november_predic_plot",width="400px",height ="400px")))
       
                 
                 ))
                 ),
  
       tabItem(tabName = "Predicted_Plots",plotOutput("predic_NLE", width = "450px", height = "450px",click = "plot_click"),
                                           plotOutput("predic_BLE", width = "450px", height = "450px",click = "plot_click"),
                                           plotOutput("predic_MD", width = "450px", height = "450px",click = "plot_click"),
                                           plotOutput("predic_DD", width = "450px", height = "450px",click = "plot_click"),
                                           plotOutput("predic_GR", width = "450px", height = "450px",click = "plot_click"),
                                           plotOutput("predic_SC", width = "450px", height = "450px",click = "plot_click"))
              
               
               
               
               
      
       
               
     )
   )
)

