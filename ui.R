require(shiny)
require(shinydashboard)
require(tidyverse)
require(stringr)
require(scales)
require(grid)
require(ggrepel)
require(readxl)
#require(DT)

# common features --------------------------------------------------------------

CAList <- list("Scotland", "Aberdeen City", "Aberdeenshire", "Angus",
              "Argyll and Bute", "City of Edinburgh",
              "Clackmannanshire", "Dumfries and Galloway",
              "Dundee City", "East Ayrshire", "East Dunbartonshire",
              "East Lothian", "East Renfrewshire", "Falkirk", "Fife",
              "Glasgow City", "Highland", "Inverclyde", "Midlothian",
              "Moray", "Na h-Eileanan Siar", "North Ayrshire",
              "North Lanarkshire", "Orkney Islands", "Perth and Kinross",
              "Renfrewshire", "Scottish Borders", "Shetland Islands",
              "South Ayrshire", "South Lanarkshire", "Stirling",
              "West Dunbartonshire", "West Lothian")

popStartYr <- 1998
popEndYr <- 2018



# ------------------------------------------------------------------------------
ui <- dashboardPage(title="Population Estimates of Scotland - National Records of Scotland",


      dashboardHeader(
        title = tags$a(href='http://www.nrscotland.gov.uk',
                       tags$img(height="45", alt="NRS", src="logo.png"),
                       tags$script(
                         HTML(paste0('$(document).ready(function() {
                                      $("header").find("nav").append(\'<span class="myClass">',
                          'Mid-Year Population Estimates for Scotland, 2018',
                                        '</span>\');
                                        })' ))),
                        tags$head(
                          tags$style(
                            HTML('.myClass {
                                 font-size: 18px;
                                 line-height: 50px;
                                 text-align: left;
                                 padding: 0 15px;
                                 overflow: hidden;
                                 color: white;
                                 font-family: "Roboto", sans-serif !important; font-weight:400;
                                 }')),

                          HTML('<link rel="stylesheet" href="http://fonts.googleapis.com/css?family=Roboto: 100,200,300,400,500,900">'))
                        )
        ),

        dashboardSidebar(
          sidebarMenu(
            menuItem("Introduction", tabName = "tab_Intro", icon = icon("th")),
            menuItem("Population", tabName = "tab_Pop", icon = icon("line-chart")),  #here add name of the menu items
            menuItem("Components of change", tabName = "tab_CompCh", icon = icon("circle")),
            menuItem("Population Structure", tabName = "tab_Struct", icon = icon("area-chart")),
            menuItem("Population by age", tabName = "tab_Age", icon = icon("bar-chart")),
            menuItem("% change by council", tabName = "tab_ChCA", icon = icon("bar-chart")),
            menuItem("% change timeseries", tabName = "tab_ChCALine", icon = icon("line-chart")),
            #menuSubItem("Sub-item 1", tabName = "tab1"), # as an argument within menuItem
            #menuSubItem("Sub-item 2", tabName = "subitem2"),
            #menuSubItem("Sub-item 3", tabName = "subitem3")
            menuItem(HTML("More information"), tabName = "tab_MoreInfo", icon = icon("info")))
        ),


# ------------------------------------------------------------------------------
  dashboardBody(
    
    HTML("<script src='https://cc.cdn.civiccomputing.com/8/cookieControl-8.x.min.js'></script>"),
    HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-91956629-1'></script>"),
    tags$script(src = "cookie_control_config.js"),
    
  #adding css file
    tags$head(

      includeCSS("style.css")

    ),

    tabItems(
    # - Tab 0 ----------------------------------------------------------------------
      tabItem(tabName = "tab_Intro",
        fluidPage(
          titlePanel("Introduction"),

                    br(),

                    h4("This interactive visualisation shows the", (strong("mid-year
                       population estimates for Scotland")), "from 1998 to 2018. The
                       estimates and change over time can also be broken down by age and
                       council area."),


                    h4("The latest estimate of Scotland's population (on 30 June 2018) is",
                        strong("5,438,100"),"the highest ever and an increase of 13,300
                        people (0.2%) over the last year."),


                    h4("Since 1998, Scotland's population has increased by 7%."),

                    br()


         )
         ),



# - Tab 1 - Population time series -----------------------------------------------------------
tabItem(tabName = "tab_Pop",
        fluidPage(
          titlePanel("Population"),

          fluidRow(

            # - Side menu ----------------------------------------------------------
            column(4,
                   wellPanel(
                     helpText("Please select options for chart") ,
                     br(),
                     selectInput("Area_Pop", label="Area:", selected="Scotland",
                                choices=CAList),
                     br(),
                     sliderInput("Years_Pop", "Years:",
                                 min = popStartYr, max = popEndYr,
                                 value = c(popStartYr, popEndYr),sep="")
                     )
            ),
            # - Plot ---------------------------------------------------------------
            column(8,
                   wellPanel(
                     plotOutput("plot_Pop"))
            ))
        )),



# - Tab 2 - Components of change --------------------------------------------------------
tabItem(tabName = "tab_CompCh",

        fluidPage(
          titlePanel("Components of Change on year"),

          fluidRow(

            # - Side menu ----------------------------------------------------------
            column(4,
                   wellPanel(
                     helpText("Please select options for chart") ,
                     br(),
                     selectInput("Area_CompCh", label="Area:", selected="Scotland",
                                 choices=CAList)
                   )
            ),
            # - Plot ---------------------------------------------------------------
            column(8,
                   wellPanel(
                     plotOutput("plot_CompCh"))
            ))
        )),

# - Tab 3 - Population structure -------------------------------------------------
tabItem(tabName = "tab_Struct",
      fluidPage(
        titlePanel("Population Structure"),

        fluidRow(

          # - Side menu ----------------------------------------------------------
          column(4,
                 wellPanel(
                   helpText("Please select options for chart") ,
                   br(),
                   selectInput("Area_Struct", label="Area:", selected="Scotland",
                               choices=CAList),
                   br(),
                   sliderInput("Years_Struct", "Years:",
                               min = popStartYr, max = popEndYr,
                               value = c(popStartYr, popEndYr),sep="")
                 )
          ),
          # - Plot ---------------------------------------------------------------
          column(8,
                 wellPanel(
                   plotOutput("plot_Struct"))
          ))
      )),

# - Tab 4 - Population by age -------------------------------------------------
tabItem(tabName = "tab_Age",
        fluidPage(
          titlePanel("Population by age"),

          fluidRow(

            # - Side menu ----------------------------------------------------------
            column(4,
                   wellPanel(
                     helpText("Please select options for chart") ,
                     br(),
                     selectInput("Area_Age", label="Area:", selected="Scotland",
                                 choices=CAList),
                     br(),
                     sliderInput("Years_Age", "Years:",
                                 min = popStartYr, max = popEndYr,
                                 value = c(popStartYr, popEndYr),sep="")
                   )
            ),
            # - Plot ---------------------------------------------------------------
            #dynamic version (hides difference graph if years identical)
            column(8,
                   wellPanel(
                     plotOutput("plot_Age")),
                   #only show difference if there is more than one year selected
                   conditionalPanel(
                     condition="output.Years_Age_Different",
                     wellPanel(
                       plotOutput("plot_ChAge"))
                   )

            ))
        )),




# - Tab 5 - % change by CA -------------------------------------------------
tabItem(tabName = "tab_ChCA",
        fluidPage(
          titlePanel("Percentage change by council"),

          fluidRow(

            # - Side menu ----------------------------------------------------------
            column(4,
                   wellPanel(
                     helpText("Please select options for chart") ,
                     br(),
                     selectInput("Area_ChCA", label="Area:", selected="Scotland",
                                 choices=CAList),
                     br(),
                     sliderInput("Years_ChCA", "Years:",
                                 min = popStartYr, max = popEndYr,
                                 value = c(popStartYr, popEndYr),sep="")
                   )
            ),
            # - Plot ---------------------------------------------------------------
            column(8,
                   wellPanel(
                     plotOutput("plot_ChCA",height="500px"))
            ))
        )),

# - Tab 5 - % annual change by CA -------------------------------------------------
tabItem(tabName = "tab_ChCALine",
        fluidPage(
          titlePanel("Percentage change by council - timeseries"),

          fluidRow(

            # - Side menu ----------------------------------------------------------
            column(4,
                   wellPanel(
                     helpText("Please select options for chart") ,
                     br(),
                     selectInput("Area_ChCALine", label="Area:", selected="Scotland",
                                 choices=CAList),
                     br(),
                     sliderInput("Years_ChCALine", "Years:",
                                 min = popStartYr, max = popEndYr,
                                 value = c(popStartYr, popEndYr),sep="")
                   )
            ),
            # - Plot ---------------------------------------------------------------
            column(8,
                   wellPanel(
                     plotOutput("plot_ChCALine",height="500px"))
            ))
        )),

# - More information----------------------------------------------------------------
tabItem(tabName = "tab_MoreInfo",
  fluidPage(
    titlePanel("More information"),

    fluidRow(
      column(12,

        br(),

        p("Mid-year population estimates are based on the Census and are updated
          annually to account for population change in the period
          from 1 July to 30 June. Therefore, the period covered by the
          latest mid-2018 statistics (1 July 2017 to 30 June 2018) is
          two years after the referendum on European Union membership (which took place on 23
          June 2016)."),

        p("The two main contributors to population change are natural change (births minus
          deaths) and net migration (the difference between long-term moves into and out
          of Scotland or local areas)."),

        p("The estimated population of an area includes all those usually resident there,
        whatever their nationality. Population figures relate to 30 June of the year shown
          and ages relate to age at last birthday. Long-term international
          migrants are included, but short-term migrants are
          excluded. A long-term migrant is defined by the United Nations (UN)
            as someone who changes country of residence for 12 months or more."),

        p("Population estimates are used for a variety of purposes including resource
            allocation and planning of services such as education and health. They are
            also used for calculating rates and performance measures, informing local
            and national policy, weighting surveys and in modelling the economy."),

        p("Mid-year population estimates for 2019 will be published in spring 2020."),


        p("More information about the estimates, their uses and their
          limitations can be found on the",
          a("Mid-Year Population Estimates",
            href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates",
          target="_blank"),
          "page on the NRS website. More detailed information is available in the Mid-year Population Estimates for Scotland ",
          HTML(paste0(a("Methodology Guide",

            href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2018",
            target="_blank"),
          "."))),

            br(),

        fluidRow(
          column(4,
                 h6(strong("More information")),
                 h6("Data: ", a("Mid-Year Population Estimates, Mid-2018 (csv)",

                                href="https://www.nrscotland.gov.uk/files//statistics/nrs-visual/mid-18-pop-est/mid-year-pop-est-18-data-vis.xlsx",
                                target="_blank")),
                 h6("Publication: ", a("Mid-Year Population Estimates, Mid-2018",

                                        href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/population/population-estimates/mid-year-population-estimates/mid-2018",
                                        target="_blank"))),
          column(4,
                 br(),
                 h6("Follow us on Twitter - ",
                    a("@NatRecordsScot",
                      href="https://twitter.com/NatRecordsScot",
                      target="_blank")),
                 h6("See more ",
                    a("Infographics & Visualisations",
                      href="http://www.nrscotland.gov.uk/statistics-and-data/statistics/stats-at-a-glance/infographics-and-visualisations",
                      target="_blank"))),
          column(4,
                 br(),
                 h6(a("National Records of Scotland", href="http://www.nrscotland.gov.uk", target="_blank")),
                 h6("© Crown Copyright 2019 - ",
                    a("Copyright conditions",
                      href="http://www.nationalarchives.gov.uk/doc/open-government-licence/version/3/",
                      target="_blank")))
        ),
        br(),

      p("Any feedback about this visualisation?",
        a("Get in touch!", href="mailto:statisticscustomerservices@nrscotland.gov.uk?subject=Mid-year%20Population%20Estimates%20of%20Scotland%20visualisation"))
           ) #end column
           ) #end fluidRow
        ) #end fluidPage
        ) #tab4 end

    ) #tabItems
) #dashboardBody

) #dashboardPage

