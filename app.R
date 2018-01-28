
######################################### GLOBAL ###################################################
library(shiny)
library(data.table)
library(stringr)
library(googleAuthR)
library(shinyjs)
library(googleID)
library(googlesheets)
library(rsconnect)
library(wordcloud) 
library(RColorBrewer)
library(twitteR)
library(ROAuth)
setup_twitter_oauth("RQFBpm2xJDw5cmn3ReAXJjq4h", "Za52ScGdOkHPM4ICRrIoXGvkKk9XlC8zPk1UNmoRTrBff2fzQg", "957274986422480896-vHz4bM3tZvHDcwRCzw7vpEDswpYIYon", "V8p2elmhQOkRFOFX3EV5x5UXagwuSwCn1w12YpiqDRVYH")

# Google Login #
options(googleAuthR.scopes.selected = c("https://www.googleapis.com/auth/userinfo.email",
                                        "https://www.googleapis.com/auth/userinfo.profile"))
options("googleAuthR.webapp.client_id" = "692003841483-53rksd8d0482g0u02dcuj4odkbkt4g2b.apps.googleusercontent.com")
options("googleAuthR.webapp.client_secret" = "aft7kpL3iIHx2XvCksEuA4Jq")

# Reading from Google Sheets #
gs_ls()
mygooglesheet <- gs_title("Moddo_Database")
responses <- gs_read(ss=mygooglesheet, ws="Form Responses 1")
responses <- data.frame(responses, stringsAsFactors = FALSE)
# clean dataframe
responses <- responses[2:nrow(responses),4:70]

# Module List #
modulelist <- as.vector(data.table(read.csv("modulelist.csv"))[[2]])

ui <- fluidPage(
  uiOutput("mainui"),
  
  ################################################ STYLES ########################################################
  tags$head(tags$style(
    HTML('
         body {
         background-image: url("utown3.jpg");
         background-size: 2000px;
         colour: white;
         }
         .table>tbody>tr>td {
         border: 1px solid transparent;
         }
         .table>thead>tr>th {
         border: 1px solid transparent;
         }
         a {
         color: #ef6304;
         border-color: #ef6304;
         }
         .btn-primary {
         background-color: #4CAF50; /* Green */
         border: none;
         color: white;
         padding: 16px 32px;
         text-align: center;
         text-decoration: none;
         display: inline-block;
         font-size: 16px;
         margin: 4px 2px;
         -webkit-transition-duration: 0.4s; /* Safari */
         transition-duration: 0.4s;
         cursor: pointer;
         }
         .btn-primary:hover {
         background-color: #4CAF50;
         color: white;
         }
         #yestosemmsg {
          font-size: 20px;
         }
         #personalmsg {
          font-size: 14px;
         }
         p {
          font-size: 14px;
         }
         #mymsg {
          font-size: 20px;
         }
         #explainmsg {
          font-size: 18px;
         }
         #invalidmsg {
          font-size: 20px;
         }
         #condmsg {
          font-size: 20px;
         }

         ')
    )),
  tags$head(tags$style(HTML("
                            #s1topresults {border: 1px solid transparent;}
                            body {color: white;}
                            @import url('https://fonts.googleapis.com/css?family=Lato');
                            @import url('https://fonts.googleapis.com/css?family=Source+Sans+Pro:300');
                            body {font-family: 'Source Sans Pro', sans-serif;};
                            "))),
  
  tags$head(
    tags$style(HTML("
                    @import url('https://fonts.googleapis.com/css?family=Lato');
                    @import url('https://fonts.googleapis.com/css?family=Source+Sans+Pro:300');
                    body {font-family: 'Source Sans Pro', sans-serif;};
                    .form-control {
                    margin-bottom: 0px;
                    margin-top: 0px;
                    padding-bottom: 0px;
                    padding-top: 0px;
                    border: 0px;
                    width: auto;
                    background-color: rgb(223, 105, 26);
                    }
                    .form-control {
                    padding: 0;
                    margin-bottom: 0;
                    color: #fff;
                    text-align: center;
                    background-color: #4E5D6C;
                    border-radius: 25px;
                    }
                    # h4 {padding-left: 1em;}
                    
                    "))
    )
    )

################################################ SERVER ###############################################################
server <- function(input, output, session) {
  
  #################### 0. Common in every page ########################
  #Display selected majors
  output$selectedmajor <- renderText({"Major:"})
  output$selectedmajors1 <- renderText(input$major[2])
  output$selectedmajors2 <- renderText(if(length(input$major) == 3) {input$major[3]})
  
  ##################### 1. Welcome Page ###############################################
  output$mainui <- renderUI({
    tagList(
      absolutePanel(
        h1("Moddo", align="centre"),
        h3("Ever experienced a situation where you're unsure of what modules to take, and what combination will make an ideal one? Moddo aims to solve this dilemma."),
        h2("Let's get started."),
        googleAuthUI("gauth_login"),
        textOutput("display_username"),
        hr(),
        p("Personal Data Protection Act"),
        p("Purpose of Data Collection"),
        p("Logging in with Google allows us to obtain personal data from your Google Account. We will only use your Gmail account name to remove duplicating entries that you submit."),
        p("Consent to Provide Personal Data"),
        p("By pressing on the Login button, you are indicating consent to let Moddo use your personal data for the purpose stated above. All personal information will be kept confidential and used for the purpose stated only."),
        p("Withdrawal of Consent"),
        p("Should you wish to withdraw your consent, send an email to jasonyip184@gmail.com and your personal information will be removed from the database."),
        left = "15%",
        right = "15%",
        top = "10%",
        bottom = "10%"
      )
    )
  })
  
  ### Google Login ###
  rv <- reactiveValues(login = FALSE)
  accessToken <- callModule(googleAuth, "gauth_login")
  userDetails <- reactive({
    validate(
      need(accessToken(), "not logged in")
    )
    rv$login <- TRUE
    with_shiny(get_user_info, shiny_access_token = accessToken())
  })
  output$display_username <- renderText({
    validate(
      need(userDetails(), "getting user details")
    )
    userDetails()$givenName
  })
  observe({
    if(rv$login) {
      shinyjs::onclick("gauth_login-googleAuthUi", 
                       shinyjs::runjs("window.location.href = 'https://moddo.shinyapps.io/moddo';"))
    }
  })
  
  #################### 2. Specify no. of sems taken ##########################
  output$selectmajor <- renderText("Select Your Major")
  output$personalmsg <- renderText(paste("Hey there ", userDetails()$name$givenName, ", what is your major?"))
  
  #links
  url <- a("GitHub", href="https://github.com/jasonyip184/Moddo")
  url1 <- a("Gmail", href="https://jasonyip184@gmail.com")
  output$link1 <- renderUI({tagList("Code", url, icon("github"))})
  output$link2 <- renderUI({tagList("Contact", a("Gmail", href = "mailto:jasonyip184@gmail.com"), icon("google"))})
  
  
  wrappingfunc <- function() {observeEvent(accessToken(), {
    #if back button is pressed, reset everything
    output$major <- renderText("")
    output$invalidmsg <- renderText("")
    output$invalidmodmsg <- renderText("")
    output$condmsg <- renderText("")
    output$basedonmsg <- renderText("")
    output$basedonmajors <- renderText("")
    output$mymsg <- renderText("")
    output$s1header <- renderText("")
    output$s2header <- renderText("")
    output$s3header <- renderText("")
    output$s4header <- renderText("")
    output$s5header <- renderText("")
    output$s6header <- renderText("")
    output$s7header <- renderText("")
    output$s8header <- renderText("")
    output$s1mods <- renderText("")
    output$s2mods <- renderText("")
    output$s3mods <- renderText("")
    output$s4mods <- renderText("")
    output$s5mods <- renderText("")
    output$s6mods <- renderText("")
    output$s7mods <- renderText("")
    output$s8mods <- renderText("")
    output$s1taken <- renderText("")
    output$s2taken <- renderText("")
    output$s3taken <- renderText("")
    output$s4taken <- renderText("")
    output$s5taken <- renderText("")
    output$s6taken <- renderText("")
    output$s7taken <- renderText("")
    output$s8taken <- renderText("")
    output$s1results <- renderText("")
    output$s2results <- renderText("")
    output$s3results <- renderText("")
    output$s4results <- renderText("")
    output$s5results <- renderText("")
    output$s6results <- renderText("")
    output$s7results <- renderText("")
    output$s8results <- renderText("")
    output$s1topresults <- renderText("")
    output$s2topresults <- renderText("")
    output$s3topresults <- renderText("")
    output$s4topresults <- renderText("")
    output$s5topresults <- renderText("")
    output$s6topresults <- renderText("")
    output$s7topresults <- renderText("")
    output$s8topresults <- renderText("")
    output$s1topheader <- renderText("")
    output$s2topheader <- renderText("")
    output$s3topheader <- renderText("")
    output$s4topheader <- renderText("")
    output$s5topheader <- renderText("")
    output$s6topheader <- renderText("")
    output$s7topheader <- renderText("")
    output$s8topheader <- renderText("")
    
    tagList(
      output$mainui <- renderUI({
        absolutePanel(
          top = "0",
          bottom = "0",
          left = "20%",
          right = "20%",
          h3("About Moddo", align="center"),
          p("Moddo uses the modules you have taken for each semester to find you the closest-matching record in our database. This record is a previously submitted combination of modules that you can use to gauge your modules for the following semesters.", align="center"),
          hr(),
          textOutput("personalmsg"),
          selectizeInput('major', '', choices = list(
            "Computing" = c("Business Analytics", "Computer Science", "Information Security", "Information Systems", "Computer Engineering"),
            "Arts & Social Sciences" = c("Chinese Language", "Chinese Studies", "Japanese Studies", "Malay Studies", "South Asian Studies", "Southeast Asian Studies", "English Language", "English Literature", "History", "Philosophy", "Theatre Studies", "Communications & New Media", "Economics", "Geography", "Political Science", "Psychology", "Social Work", "Sociology", "Environmental Studies in Geography", "European Studies", "Global Studies"),
            "Business & Accountancy" = c("Business Administration (Accountancy)", "Business Administration"),
            "Dentistry" = c("","Dentistry"),
            "Design & Environment" = c("Architecture","Industrial Design","Project & Facilities Management","Real Estate"),
            "Engineering" = c("Engineering","Biomedical Engineering","Chemical Engineering","Civil Engineering","Engineering Science","Environmental Engineering","Electrical Engineering","Industrial and Systems Engineering","Material Science & Engineering","Mechanical Engineering","Computer Engineering"),
            "Law" = c("","Law"),
            "Medicine" = c("","Medicine"),
            "Nursing" = c("","Nursing"),
            "Music" = c("","Music"),
            "Science" = c("Applied Mathematics","Chemistry","Computational Biology","Data Science and Analytics","Food Science and Technology","Life Sciences","Mathematics","Physics","Quantitative Finance","Statistics","Environmental Studies in Biology"),
            "Pharmacy" = c("","Pharmacy")
          ), multiple = TRUE, selected = "", options = list(maxItems = 3)),
          p("Is this your first semester in NUS?"),
          actionButton("yfirstsem", "Yes", 
                       style="color: #fff; background-color: #0a7508; border-color: #0a7508"),
          actionButton("nfirstsem", "No", 
                       style="color: #fff; background-color: #ba290d; border-color: #ba290d"),
          hr(),
          p("Moddo is limited by the number of records in our database and your honest inputs and sharing of this application with the NUS community is greatly appreciated :)"),
          uiOutput("link1"),
          uiOutput("link2")
          
        )
      })
    )
  })
  }
  #to allow for back button
  wrappingfunc()
  observeEvent(input$back1, {wrappingfunc()})
  observeEvent(input$back2, {wrappingfunc()})
  
  ###################### 3. Yes to First sem in NUS ##############################
  observeEvent(input$major, {
    output$yestosemmsg <- renderText(paste("Welcome to NUS ", userDetails()$name$givenName, ", this is the most popular combination in the first semester for your major!"))
  })
  
  observeEvent(input$yfirstsem, {
    mostpop <- function(responses){
      DT <- data.table(responses)
      DT[,.N, by = names(DT)]
    }
    
    #filter rows by selected major/majors
    if(length(input$major) == 1) {
      responses <- subset(responses, Major.1 %in% input$major[1] | Major.2 %in% input$major[1])
    }
    else {
      checker <- subset(responses, (Major.1 %in% input$major[1] & Major.2 %in% input$major[2]) |
                          (Major.1 %in% input$major[2] & Major.2 %in% input$major[1]))
      #find records that have only 1 major and match either first/second input major
      if(nrow(checker) == 0) {
        responses <- subset(responses, (Major.1 %in% input$major[1] & is.na(Major.2)) | 
                              (Major.1 %in% input$major[2] & is.na(Major.2))) 
      }
      else {
        responses <- checker
      }
    }
    
    #if responses not empty, then execute below
    if(nrow(responses) > 0) {
      #find highest scoring row
      pop <- mostpop(responses)
      pop <- as.data.frame(pop)
      index <- which.max(pop[,68])
      result <- pop[index,]
      names(result) <- NULL
      
      #show major of the record you are recommending
      output$basedonmsg <- renderText("Based on the following majors:")
      output$basedonmajors <- renderTable(result[1:2])
      
      output$s1topresults <- renderTable(result[3:10])
    }
    else {
      output$yestosemmsg <- renderText(paste("Sorry ", userDetails()$name$givenName, ", your major has not been recorded in our database yet :( You're a pioneer!"))
      output$basedonmsg <- renderText("")
      output$basedonmajors <- renderText("")
      output$invalidmsg <- renderText("")
      output$mymsg <- renderText("")
    }
    #########
    tagList(
      output$mainui <- renderUI({
        absolutePanel(id="thirdpage",
                      top = 0,
                      bottom = 0,
                      left = 0,
                      right = 0,
                      absolutePanel(
                        top = "25%",
                        bottom = "25%",
                        left = "3%",
                        right = "25%",
                        
                        textOutput("yestosemmsg"),
                        br(),
                        br(),
                        textOutput("basedonmsg"),
                        tableOutput("basedonmajors"),
                        tableOutput("s1topresults"),
                        br(),
                        actionButton("back1", "Back")
                      )
        )
      })
    )
  })
  
  ##################### 4. Mods input page sem 1 ###################################
  observeEvent(input$nfirstsem, {
    tagList(
      output$mainui <- renderUI({
        absolutePanel(id="thirdpage",
                      top = 0,
                      bottom = 0,
                      left = 0,
                      right = 0,
                      absolutePanel(
                        top = 0,
                        bottom = 0,
                        left = "30%",
                        right = "30%",
                        h4("Enter your modules"),
                        selectizeInput('s1mods', 'Semester 1', choices = modulelist, multiple = TRUE, selected = "", options = list(maxItems = 8)),
                        selectizeInput('s2mods', 'Semester 2', choices = modulelist, multiple = TRUE, selected = "", options = list(maxItems = 8)),
                        selectizeInput('s3mods', 'Semester 3', choices = modulelist, multiple = TRUE, selected = "", options = list(maxItems = 8)),
                        selectizeInput('s4mods', 'Semester 4', choices = modulelist, multiple = TRUE, selected = "", options = list(maxItems = 8)),
                        selectizeInput('s5mods', 'Semester 5', choices = modulelist, multiple = TRUE, selected = "", options = list(maxItems = 8)),
                        selectizeInput('s6mods', 'Semester 6', choices = modulelist, multiple = TRUE, selected = "", options = list(maxItems = 8)),
                        selectizeInput('s7mods', 'Semester 7', choices = modulelist, multiple = TRUE, selected = "", options = list(maxItems = 8)),
                        selectizeInput('s8mods', 'Semester 8', choices = modulelist, multiple = TRUE, selected = "", options = list(maxItems = 8)),
                        
                        actionButton("submitbutton1", "Recommend Me!"),
                        br(),
                        br(),
                        actionButton("back2", "Back")
                      )
        )
      })
    )
  })
  
  ##################### 4. Results ###################################
  observeEvent(input$submitbutton1, {
    sufficientfunction()
    tagList(
      output$mainui <- renderUI({
        absolutePanel(id="thirdpage",
                      top = 0,
                      bottom = 0,
                      left = 0,
                      right = 0,
                      absolutePanel(
                        top = "25%",
                        bottom = "25%",
                        left = "3%",
                        right = "25%",
                        textOutput("invalidmodmsg"),
                        textOutput("invalidmsg"),
                        textOutput("condmsg"),
                        br(),
                        textOutput("basedonmsg"),
                        tableOutput("basedonmajors"),
                        br(),
                        textOutput("mymsg"),
                        textOutput("s2topheader"),
                        tableOutput("s2topresults"),
                        textOutput("s3topheader"),
                        tableOutput("s3topresults"),
                        textOutput("s4topheader"),
                        tableOutput("s4topresults"),
                        textOutput("s5topheader"),
                        tableOutput("s5topresults"),
                        textOutput("s6topheader"),
                        tableOutput("s6topresults"),
                        textOutput("s7topheader"),
                        tableOutput("s7topresults"),
                        textOutput("s8topheader"),
                        tableOutput("s8topresults"),
                        
                        h4("Listen to what people have to say about the recommended mods!"),
                        actionButton("twittercrawl", "Let's Go"),
                        br(),
                        br(),
                        br(),
                        br(),
                        textOutput("explainmsg"),
                        textOutput("s1header"),
                        tableOutput("s1results"),
                        textOutput("s1taken"),
                        tableOutput("s1mods"),
                        
                        textOutput("s2header"),
                        tableOutput("s2results"),
                        textOutput("s2taken"),
                        tableOutput("s2mods"),
                        
                        textOutput("s3header"),
                        tableOutput("s3results"),
                        textOutput("s3taken"),
                        tableOutput("s3mods"),
                        
                        textOutput("s4header"),
                        tableOutput("s4results"),
                        textOutput("s4taken"),
                        tableOutput("s4mods"),
                        
                        textOutput("s5header"),
                        tableOutput("s5results"),
                        textOutput("s5taken"),
                        tableOutput("s5mods"),
                        
                        textOutput("s6header"),
                        tableOutput("s6results"),
                        textOutput("s6taken"),
                        tableOutput("s6mods"),
                        
                        textOutput("s7header"),
                        tableOutput("s7results"),
                        textOutput("s7taken"),
                        tableOutput("s7mods"),
                        
                        textOutput("s8header"),
                        tableOutput("s8results"),
                        textOutput("s8taken"),
                        tableOutput("s8mods"),
                        br(),
                        br(),
                        actionButton("back2", "Back")
                      )
        )
      })
    )
  })
  
  ######################## 5. Twitter Crawl ###############################
  
  observeEvent(input$twittercrawl, {
    tagList(
      output$mainui <- renderUI({
        absolutePanel(
                      top = 0,
                      bottom = 0,
                      left = 0,
                      right = 0,
                      absolutePanel(
                        top = 0,
                        bottom = 0,
                        left = "20%",
                        right = "20%",
                        h3("Here's what people from Twitter think", align="center"),
                        hr(),
                        plotOutput("twitterresult")
                      )
        )
      })
    )
  })

  s1mods <- reactive({sort(input$s1mods)})
  s2mods <- reactive({sort(input$s2mods)})
  s3mods <- reactive({sort(input$s3mods)})
  s4mods <- reactive({sort(input$s4mods)})
  s5mods <- reactive({sort(input$s5mods)})
  s6mods <- reactive({sort(input$s6mods)})
  s7mods <- reactive({sort(input$s7mods)})
  s8mods <- reactive({sort(input$s8mods)})
  
  sufficientfunction <- function() {
    if(length(s8mods()) > 1) {responses <- subset(responses, X70 != "")}
    if(length(s7mods()) > 1) {responses <- subset(responses, X62 != "")}
    if(length(s6mods()) > 1) {responses <- subset(responses, X54 != "")}
    if(length(s5mods()) > 1) {responses <- subset(responses, X46 != "")}
    if(length(s4mods()) > 1) {responses <- subset(responses, X38 != "")}
    if(length(s3mods()) > 1) {responses <- subset(responses, X30 != "")}
    if(length(s2mods()) > 1) {responses <- subset(responses, X22 != "")}
    if(length(s1mods()) > 1) {responses <- subset(responses, X14 != "")}
    
    s1mods <- s1mods()
    s2mods <- s2mods()
    s3mods <- s3mods()
    s4mods <- s4mods()
    s5mods <- s5mods()
    s6mods <- s6mods()
    s7mods <- s7mods()
    s8mods <- s8mods()
    
    ### Scoring Algo
    if(nrow(responses) == 0) {output$invalidmsg <- renderText(paste("Sorry ", userDetails()$name$givenName, ", we do not have a record in your major that has taken more semesters than you did yet :("))}
    else {
      #init scores matrix to find highest scoring record to return
      scores <- c()
      #Loop through every record except last if not count yourself
      for(i in 1:nrow(responses)) {
        count <- 0
        record <- responses[i,]
        
        #for each record, and for each input mod, if mod is in a matching sem, increment count
        for(j in 3:10) { if((record[,j] %in% s1mods()) & (record[,j] != "")) {count <- count + 1} }
        for(j in 11:18){ if((record[,j] %in% s2mods()) & (record[,j] != "")) {count <- count + 1} }
        for(j in 19:26){ if((record[,j] %in% s3mods()) & (record[,j] != "")) {count <- count + 1} }
        for(j in 27:34){ if((record[,j] %in% s4mods()) & (record[,j] != "")) {count <- count + 1} }
        for(j in 35:42){ if((record[,j] %in% s5mods()) & (record[,j] != "")) {count <- count + 1} }
        for(j in 43:50){ if((record[,j] %in% s6mods()) & (record[,j] != "")) {count <- count + 1} }
        for(j in 51:58){ if((record[,j] %in% s7mods()) & (record[,j] != "")) {count <- count + 1} }
        for(j in 59:66){ if((record[,j] %in% s8mods()) & (record[,j] != "")) {count <- count + 1} }
        
        #add (position, score) to scores matrix if count is not 0
        #can only find by highest match score if majors are exactly the same, eitherwise must find most popular
        if(count != 0) {
          if(length(input$major) == 1 & (record[,1] %in% input$major[1] | record[,2] %in% input$major[1])) 
          {scores <- rbind(scores, c(i,count))}
          else {
            if(length(input$major) == 2) {
              if((record[,1] %in% input$major[1] & record[,2] %in% input$major[2]) |
                 (record[,1] %in% input$major[2] & record[,2] %in% input$major[1])) 
              {scores <- rbind(scores, c(i,count))}
            }
          }
        }
      }
      
      ### recommends only the sem that you need at the top, the rest at bottom  
      innerfunction <- function() {
        output$mymsg <- renderText(paste("Hey ", userDetails()$name$givenName, ", this is the combination which matches yours the most!"))
        output$explainmsg <- renderText("How we arrived at this recommendation")
        if(length(s1mods) > 0) {
          output$s2topheader <- renderText("Recommendation for Semester 2:")
          output$s2topresults <- renderTable(result[11:18])
          output$s1header <- renderText("Semester 1")
          while(length(s1mods) != 8) {s1mods <- c(s1mods, "")}
          output$s1results <- renderTable(cbind(c("Mods You've Entered",s1mods), c("Moddo's Match for You",t(result[3:10]))), colnames = FALSE)
        }
        if(length(s2mods) > 0) {
          output$s3topheader <- renderText("Recommendation for Semester 3:")
          output$s3topresults <- renderTable(result[19:26])
          output$s2header <- renderText("Semester 2")
          while(length(s2mods) != 8) {s2mods <- c(s2mods, "")}
          output$s2results <- renderTable(cbind(c("Mods You've Entered",s2mods), c("Moddo's Match for You",t(result[11:18]))), colnames = FALSE)
          output$s2topheader <- renderText("")
          output$s2topresults <- renderText("")
        }
        if(length(s3mods) > 0) {
          output$s4topheader <- renderText("Recommendation for Semester 4:")
          output$s4topresults <- renderTable(result[27:34])
          output$s3header <- renderText("Semester 3")
          while(length(s3mods) != 8) {s3mods <- c(s3mods, "")}
          output$s3results <- renderTable(cbind(c("Mods You've Entered",s3mods), c("Moddo's Match for You",t(result[19:26]))), colnames = FALSE)
          output$s3topheader <- renderText("")
          output$s3topresults <- renderText("")
        }
        if(length(s4mods) > 0) {
          output$s5topheader <- renderText("Recommendation for Semester 5:")
          output$s5topresults <- renderTable(result[35:42])
          output$s4header <- renderText("Semester 4")
          while(length(s4mods) != 8) {s4mods <- c(s4mods, "")}
          output$s4results <- renderTable(cbind(c("Mods You've Entered",s4mods), c("Moddo's Match for You",t(result[27:34]))), colnames = FALSE)
          output$s4topheader <- renderText("")
          output$s4topresults <- renderText("")
        }
        if(length(s5mods) > 0) {
          output$s6topheader <- renderText("Recommendation for Semester 6:")
          output$s6topresults <- renderTable(result[43:50])
          output$s5header <- renderText("Closest match for Semester 5:")
          while(length(s5mods) != 8) {s5mods <- c(s5mods, "")}
          output$s5results <- renderTable(cbind(c("Mods You've Entered",s5mods), c("Moddo's Match for You",t(result[35:42]))), colnames = FALSE)
          output$s5topheader <- renderText("")
          output$s5topresults <- renderText("")
        }
        if(length(s6mods) > 0) {
          output$s7topheader <- renderText("Recommendation for Semester 7:")
          output$s7topresults <- renderTable(result[51:58])
          output$s6header <- renderText("Closest match for Semester 6:")
          while(length(s6mods) != 8) {s6mods <- c(s6mods, "")}
          output$s6results <- renderTable(cbind(c("Mods You've Entered",s6mods), c("Moddo's Match for You",t(result[43:50]))), colnames = FALSE)
          output$s6topheader <- renderText("")
          output$s6topresults <- renderText("")
        }
        if(length(s7mods) > 0) {
          output$s8topheader <- renderText("Recommendation for Semester 8:")
          output$s8topresults <- renderTable(result[59:66])
          output$s7header <- renderText("Closest match for Semester 7:")
          while(length(s7mods) != 8) {s7mods <- c(s7mods, "")}
          output$s7results <- renderTable(cbind(c("Mods You've Entered",s7mods), c("Moddo's Match for You",t(result[51:58]))), colnames = FALSE)
          output$s7topheader <- renderText("")
          output$s7topresults <- renderText("")
        }
        while(length(s8mods) != 8) {s8mods <- c(s8mods, "")}
      }
      
      ### Most popular function
      if(is.null(scores) & input$submitbutton1) {
        mostpop <- function(responses){
          DT <- data.table(responses)
          DT[,.N, by = names(DT)]
        }
        
        #filter rows by selected major/majors
        if(length(input$major) == 1) {
          responses <- subset(responses, Major.1 %in% input$major[1] | Major.2 %in% input$major[1])
          output$invalidmsg <- renderText(paste("Sorry ", userDetails()$name$givenName, ", you have been unable to find a match because none of your mods are taken by anyone with your major in the database yet. Based on your major, the most popular combination has been suggested to you instead!"))
        }
        else {
          checker <- subset(responses, (Major.1 %in% input$major[1] & Major.2 %in% input$major[2]) |
                              (Major.1 %in% input$major[2] & Major.2 %in% input$major[1]))
          #find records that have only 1 major and match either first/second input major
          if(nrow(checker) == 0) {
            responses <- subset(responses, (Major.1 %in% input$major[1] & is.na(Major.2)) | 
                                  (Major.1 %in% input$major[2] & is.na(Major.2))) 
            output$invalidmsg <- renderText(paste("Sorry ", userDetails()$name$givenName, ", you have been unable to find a match because none of your mods are taken by anyone in the database yet. Based on one of your majors, the most popular combination has been suggested to you instead!"))
          }
          else {
            responses <- checker
            output$invalidmsg <- renderText(paste("Sorry ", userDetails()$name$givenName, ", you have been unable to find a match because none of your mods are taken by anyone in the database yet. Based on both of your majors, the most popular combination has been suggested to you instead!"))
          }
        }
        
        #if responses not empty, then execute below
        if(nrow(responses) > 0) {
          #find highest scoring row
          pop <- mostpop(responses)
          pop <- as.data.frame(pop)
          index <- which.max(pop[,68])
          result <- pop[index,]
          names(result) <- NULL
          
          #show major of the record you are recommending
          output$basedonmsg <- renderText("Based on the following majors:")
          output$basedonmajors <- renderTable(result[1:2])
          
          innerfunction()
          output$mymsg <- renderText("")
        }
        else {
          output$condmsg <- renderText(paste("Sorry ", userDetails()$name$givenName, ", your major has not been recorded in our database yet :( You're a pioneer!"))
          output$basedonmsg <- renderText("")
          output$basedonmajors <- renderText("")
          output$invalidmsg <- renderText("")
          output$mymsg <- renderText("")
        }
        
      }
      ### highest scoring function
      else {
        if(input$submitbutton1) {
          output$invalidmsg <- renderText("")
          index <- which.max(scores[,2])
          result <- responses[index,]
          names(result) <- NULL
          
          innerfunction()
        }
      }
    }
    ### Write to sheets
    gs_add_row(ss=mygooglesheet, ws="Form Responses 1",
               input=c("",userDetails()$emails$value,userDetails()$displayName,input$major[1], if(length(input$major) == 1) {""} else {if(length(input$major) == 2) {input$major[2]}},
                       s1mods,s2mods,s3mods,s4mods,s5mods,s6mods,s7mods,s8mods))
    
    ### Fetch tweets
    tweets <- searchTwitter("ger1000", n = 5, lang = "en")
    tweets <-twListToDF(tweets)
    tb<-table(tweets$text)
    output$twitterresult <- renderPlot(wordcloud(names(tb),as.numeric(tb), random.order=TRUE, scale=c(1,.2), colors=brewer.pal(9, "Dark2"), rot.per=.1))
    wordcloud(names(tb),as.numeric(tb),random.order=TRUE, random.color=TRUE, scale=c(1,.2), colors=brewer.pal(4, "Dark2"), ordered.colors=TRUE)
    
  }  
}

shinyApp(ui = ui, server = server)

###myvector <- c("Business Analytics", "Computer Science", "Information Security", "Information Systems", "Computer Engineering", "Chinese Language", "Chinese Studies", "Japanese Studies", "Malay Studies", "South Asian Studies", "Southeast Asian Studies", "English Language", "English Literature", "History", "Philosophy", "Theatre Studies", "Communications & New Media", "Economics", "Geography", "Political Science", "Psychology", "Social Work", "Sociology", "Environmental Studies in Geography", "European Studies", "Global Studies", "Business Administration (Accountancy)", "Business Administration", "Dentistry", "Architecture","Industrial Design","Project & Facilities Management","Real Estate", "Engineering","Biomedical Engineering","Chemical Engineering","Civil Engineering","Engineering Science","Environmental Engineering","Electrical Engineering","Industrial and Systems Engineering","Material Science & Engineering","Mechanical Engineering","Computer Engineering","Law","Medicine", "Nursing","Music", "Applied Mathematics","Chemistry","Computational Biology","Data Science and Analytics","Food Science and Technology","Life Sciences","Mathematics","Physics","Quantitative Finance","Statistics","Environmental Studies in Biology", "Pharmacy")
#sort(myvector)