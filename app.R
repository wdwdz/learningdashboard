library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(dplyr)
library(ggplot2)
library(lubridate)
data("dat")
dat.log <- dat$appdata.log %>%
  mutate(week = week(day))
dat.log$netid[dat.log$netid == "NANA"] <- 'admin'
dat.group <- dat$appdata.group
dat.user <- dat$appdata.user
dat.user$netid[dat.user$username == "admin"] <- 'admin'

dat.survey <- dat$appdata.survey
dat.survey[,'result'] <- round(dat.survey[,'result'],1)
colnames(dat.survey)[2] <- 'netid'
colnames(dat.survey)[1] <- 'day'
dat.survey$batch <- ifelse(dat.survey$time == "t1","1",ifelse(dat.survey$time == "t2","2","3"))
dat.survey$time <- ifelse(dat.survey$time == "t1","2022-10-01",ifelse(dat.survey$time == "t2","2022-10-22","2022-11-15"))
dat.survey$time <- as.Date(dat.survey$time)

event_mappings <- c("message" = 1, "logout" = 2, "signup" = 6, "login" = 7, "main-signup" = 8,
                    "current-create" = 9,"updateprofile" = 10,"friendlist" = 12,"post" = 13,"community-sort" = 14,
                    "like" = 15,"cancelcool" = 16,"funny" = 17,"useful" = 18,"cool" = 19,
                    "cancelfunny" = 20,"canceluseful" = 21,"comment" = 22,"addfriend" = 23,"cancellike" = 25,
                    "navi-current" = 26,"navi-create" = 27,"navi-community" = 28,"community-filter" = 29,"community-next" = 30,"community-previous" = 31)

dat.log$event <- names(event_mappings)[match(dat.log$event, event_mappings)]

dat.post<- dat$appdata.post%>%
  mutate(week = week(day))

dat.comment <- dat$appdata.comment%>%
  mutate(week = week(day))


ui <- dashboardPage(

  dashboardHeader(title = "DEA1500APP Insight"),
  dashboardSidebar(
    tags$head(
      tags$style(HTML("
      div.dataTables_wrapper div.dataTables_filter {
        display: none;
      }
      .dataTables_filter, .dataTables_length { display: none; }

    "))
    ),

    sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
                      label = "Search Userid..."),
    h5("To access the User Profile, you must Login dea1500.xyz", align="center"),
    uiOutput("usernametab"),
    uiOutput("linktab"),
    h4("Profile", align="center"),
    DT::dataTableOutput("userinfo"),
    h4("Log Summary", align="center"),
    DT::dataTableOutput("userloginfo"),
    h4("Survey Summary", align="center"),
    DT::dataTableOutput("usersurveyinfo")
  ),
  dashboardBody(
    tags$head(tags$style(HTML('
        /* logo */
        .skin-blue .main-header .logo {
                              background-color: #6aa84f;
                              }

        /* logo when hovered */
        .skin-blue .main-header .logo:hover {
                              background-color: #d9ead3;
                              }

        /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #8fce00;
                              }

        /* main sidebar */
        .skin-blue .main-sidebar {
                              background-color: #6aa84f;
                              }

        /* active selected tab in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                              background-color: #ff0000;
                              }

        /* other links in the sidebarmenu */
        .skin-blue .main-sidebar .sidebar .sidebar-menu a{
                              background-color: #00ff00;
                              color: #000000;
                              }

        /* other links in the sidebarmenu when hovered */
         .skin-blue .main-sidebar .sidebar .sidebar-menu a:hover{
                              background-color: #ff69b4;
                              }
        /* toggle button when hovered  */
         .skin-blue .main-header .navbar .sidebar-toggle:hover{
                              background-color: #ff69b4;
                              }
                              '))),
    tabsetPanel(id = "tab",
                tabPanel("Log",

                         box(solidHeader = TRUE,collapsible = TRUE,width = 13,
                             valueBoxOutput("userBox"),
                             valueBoxOutput("postBox"),
                             valueBoxOutput("commentBox")
                         ),
                         fluidRow(
                           box(width = 4,
                               sliderInput("time", "Week", 1, max(dat.log$week) - 34, max(dat.log$week) - 34),
                               selectInput("group", "User group",c('All',names(dat.group)) ),
                               selectInput("event", "Log event",c('All',names(event_mappings)) ),
                               DT::dataTableOutput("plot_log_info")
                           ),

                           box(width = 8,
                               plotOutput("plot_log", click = "plot_log_click")

                           )
                         )
                ),
                tabPanel("Survey",
                         box(solidHeader = TRUE,collapsible = TRUE,width = 13,
                             valueBoxOutput("socbeBox"),
                             valueBoxOutput("cogBox"),
                             valueBoxOutput("emoBox")

                         ),
                         fluidRow(
                           box(width = 4,
                               sliderInput("time2", "Survey time", 1, 3, 3),
                               selectInput("group2", "User group",c('All',names(dat.group)) ),
                               selectInput("eng", "Engagement",c('All',unique(dat.survey$eng)) ),
                               DT::dataTableOutput("plot_survey_info")
                           ),

                           box(width = 8,
                               plotOutput("plot_survey", click = "plot_survey_click")

                           )
                         )
                ),

    ),

  )
)



server <- function(input, output, session) {
  userinfo <- eventReactive(input$searchButton,{
    dat.user %>%
      filter(netid == input$searchText) %>%
      mutate(link = paste0("<a href='",link,"' style='color:#cfe2f3;'>","CLICK","</a>")) %>%
      select(netid, username, link)
  })

  userloginfo <- eventReactive(input$searchButton,{
    dat.log %>%
      filter(netid == input$searchText) %>%
      group_by(event) %>%
      summarise(totallog = sum(freq))
  })

  usersurveyinfo <- eventReactive(input$searchButton,{
    dat.survey %>%
      filter(netid == input$searchText) %>%
      select(eng, result, time)
  })

  output$userinfo <- renderDataTable( userinfo(),
                                      options = list(info = FALSE, lengthChange = FALSE, searching = FALSE, paging=FALSE),escape = FALSE,rownames = FALSE )
  output$userloginfo <- renderDataTable( userloginfo(),
                                         options = list(pageLength = 5))
  output$usersurveyinfo <- renderDataTable( usersurveyinfo(),
                                            options = list(pageLength = 5) )

  #log plot
  log_click_info <- reactiveVal()

  observeEvent(input$plot_log_click,
               log_click_info(nearPoints(dat.log %>%
                                           filter(if(input$event == "All") TRUE else event == input$event) %>%
                                           filter(if(input$group == "All") TRUE else netid %in% dat.group[[input$group]]) %>%
                                           select(netid, event, day, freq),
                                         input$plot_log_click))
  )

  output$plot_log <- renderPlot({
    dat.log %>%
      filter(if(input$event == "All") TRUE else event == input$event) %>%
      filter(if(input$group == "All") TRUE else netid %in% dat.group[[input$group]]) %>%
      ggplot()+
      geom_point(aes(day,freq),position = position_dodge(0.02), size=0.5)+
      geom_line(data = dat.log %>%
                  filter(if(input$event == "All") TRUE else event == input$event) %>%
                  filter(if(input$group == "All") TRUE else netid %in% dat.group[[input$group]]) %>%
                  filter(netid %in% log_click_info()$netid),aes(day,freq,group = netid, colour = factor(netid)),position = position_dodge(0.02))+
      scale_fill_continuous(low="thistle2", high="darkred",
                            guide="colorbar",na.value="white")+
      geom_line(data = dat.log %>%
                  filter(if(input$event == "All") TRUE else event == input$event) %>%
                  filter(if(input$group == "All") TRUE else netid %in% dat.group[[input$group]]) %>%
                  group_by(day) %>%
                  summarise(y = sum(freq)/10), aes(day, y),colour="red",linetype = "dashed" )+
      ggthemes::theme_tufte()+
      labs(title=paste(input$event,'log frequency by', input$group,"users"),
           subtitle = "red dash lines = total frequency / 10")+
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 10, color = "red"),
            axis.line.x = element_line(color="black", size = 0.5))
  }, res=96)

  output$plot_log_info <- renderDataTable({
    log_click_info()
  }, rownames = FALSE)

  #log box
  user <- reactive({
    dat.post %>%
      filter(week == as.numeric(input$time) + 34) %>%
      summarize(y = n_distinct(netid))
  })

  post <- reactive({
    nrow(dat.post %>%
           filter(week == as.numeric(input$time) + 34))
  })

  comment <- reactive({
    nrow(dat.comment %>%
           filter(week == as.numeric(input$time) + 34))
  })

  output$userBox <- renderValueBox({
    valueBox(
      user()$y, "#Users", icon = icon("user"),
      color = "blue"
    )
  })

  output$postBox <- renderValueBox({
    valueBox(
      post(), "#Posts", icon = icon("image"),
      color = "purple"
    )
  })

  output$commentBox <- renderValueBox({
    valueBox(
      comment(), "#Comments", icon = icon("comment"),
      color = "yellow"
    )
  })

  #survey plot
  survey_click_info <- reactiveVal()

  observeEvent(input$plot_survey_click,
               survey_click_info(nearPoints(dat.survey %>%
                                              filter(if(input$eng == "All") TRUE else eng == input$eng) %>%
                                              filter(if(input$group2 == "All") TRUE else netid %in% dat.group[[input$group2]]) %>%
                                              select(netid, eng, day, result),
                                            input$plot_survey_click))
  )

  output$plot_survey <- renderPlot({
    dat.survey %>%
      filter(if(input$eng == "All") TRUE else eng == input$eng) %>%
      filter(if(input$group2 == "All") TRUE else netid %in% dat.group[[input$group2]]) %>%
      ggplot()+
      geom_point(aes(day,result),position = position_dodge(0.02), size=0.5)+
      geom_line(data = dat.survey %>%
                  filter(if(input$eng == "All") TRUE else eng == input$eng) %>%
                  filter(if(input$group2 == "All") TRUE else netid %in% dat.group[[input$group2]]) %>%
                  filter(netid %in% survey_click_info()$netid),aes(day,result,group = netid, colour = factor(netid)),position = position_dodge(0.02))+
      scale_fill_continuous(low="thistle2", high="darkred",
                            guide="colorbar",na.value="white")+
      geom_line(data = dat.survey %>%
                  filter(if(input$eng == "All") TRUE else eng == input$eng) %>%
                  filter(if(input$group2 == "All") TRUE else netid %in% dat.group[[input$group2]]) %>%
                  group_by(time) %>%
                  summarise(y = mean(result)), aes(time, y),colour="red",linetype = "dashed" )+
      ggthemes::theme_tufte()+
      labs(title=paste(input$eng,'engagement mean by', input$group2,"users"),
           subtitle = "red dash lines = engagement mean")+
      theme(plot.title = element_text(size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 10, color = "red"),
            axis.line.x = element_line(color="black", size = 0.5))
  }, res=96)

  output$plot_survey_info <- renderDataTable({
    survey_click_info()
  }, rownames = FALSE)

  # survey box
  socbe <- reactive({
    dat.survey %>%
      filter(batch == input$time2) %>%
      filter(eng == "socbe") %>%
      summarize(y = round(mean(result),2))
  })

  emo <- reactive({
    dat.survey %>%
      filter(batch == input$time2) %>%
      filter(eng == "emo") %>%
      summarize(y = round(mean(result),2))
  })

  cog <- reactive({
    dat.survey %>%
      filter(batch == input$time2) %>%
      filter(eng == "cog") %>%
      summarize(y = round(mean(result),2))
  })

  output$socbeBox <- renderValueBox({
    valueBox(
      socbe()$y, "Social-behaviral engagement", icon = icon("b"),
      color = "blue"
    )
  })

  output$cogBox <- renderValueBox({
    valueBox(
      cog()$y, "Cognitive engagement", icon = icon("c"),
      color = "purple"
    )
  })

  output$emoBox <- renderValueBox({
    valueBox(
      emo()$y, "Emotional engagement", icon = icon("e"),
      color = "yellow"
    )
  })


}

shinyApp(ui, server)
