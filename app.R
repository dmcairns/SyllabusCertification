## Syllabus Certification Tool for 2026A
# This is app.R
# A robust Shiny application demonstrating Google Authentication via googleAuthR
# using the gar_shiny_ui/gar_shiny_auth functions for server deployment.

library(shiny)
library(httr)
library(dplyr)
library(dbplyr)
library(jsonlite) # Added for JSON parsing
library(RSQLite)
library(dbplyr)
library(DT)
library(gt)
library(DBFunctionsTAMU)
library(RMySQL)
library(crayon)
library(qpdf)



# OAuth setup --------------------------------------------------------

# Most OAuth applications require that you redirect to a fixed and known
# set of URLs. Many only allow you to redirect to a single URL: if this
# is the case for, you'll need to create an app for testing with a localhost
# url, and an app for your deployed app.


if (interactive()) {
  jsonInfo <- fromJSON("Fozzie_localhost.json")
  # testing url
  theShinyPort <- 1221  #DMC added to match Google API for project called TestProject
  options(shiny.port = theShinyPort) #DMC added
  APP_URL <- paste0("http://localhost:", theShinyPort,"/") #DMC added
  app <- oauth_app("shinygoogle",
                   key = jsonInfo$web$client_id,
                   secret = jsonInfo$web$client_secret,
                   redirect_uri = APP_URL
  )
} else {
  jsonInfo <- fromJSON("Fozzie_localhost.json")
  app <- oauth_app("shinygoogle",
                   key = jsonInfo$web$client_id,
                   secret = jsonInfo$web$client_secret,
                   redirect_uri = APP_URL
  )
  # deployed URL
  APP_URL <- "https://shiny.artsci.tamu.edu/Fozzie"
}

# Here I'm using a canned endpoint, but you can create with oauth_endpoint()
api <- oauth_endpoints("google")

# Always request the minimal scope needed.
# Added 'openid' to ensure the 'sub' (user ID) claim is available from the userinfo endpoint.
scope <- "https://www.googleapis.com/auth/userinfo.email openid"

# Shiny -------------------------------------------------------------------

ui <- bs4Dash::dashboardPage(
    #freshTheme = geogDashboardThemeFresh,
    options = NULL,
    header = bs4Dash::dashboardHeader(
      title = bs4Dash::dashboardBrand(
        title = "",
        image = "GeographyBannerSM.png",
        color = "primary",
        opacity = 1
      ),
      rightUI = uiOutput("dropdownMenu"),
      status = "primary",
      skin = "light",
      sidebarIcon = shiny::icon("bars"),
        tags$script(HTML("
      $(document).on('click', '#syllabiGrid td', function() {
        // 1. Get the text value of the clicked cell
        var cellValue = $(this).text();

        // 2. Get the row index (adding 1 for R's 1-based indexing)
        var rowIndex = $(this).closest('tr').index() + 1;

        // 3. Send both values to Shiny as a single list object
        Shiny.setInputValue('table_click_data', {
          value: cellValue,
          row: rowIndex
        }, {priority: 'event'});
      });
    "))
    ),
    sidebar = bs4Dash::bs4DashSidebar(
      tagList(h4("something")),
      disable = TRUE,
      minified=FALSE
    ),
    body = bs4Dash::dashboardBody(
      id="dashboardBody",
      tagList(
        verbatimTextOutput("userInfo"),
        bs4Dash::box(
          title = "Created Boxes",
          collapsible = FALSE,
          status = "primary",
          width = 12,
          gt_output("syllabiGrid")
        ),
        div(id = "placeholderInBody")
      )
      , title = "CLAT Syllabus Review"
      , dark = NULL
      , help = NULL
    )
  )
  #h3("Syllabus Review Tracking"),
  # Changed to display the user ID from the userinfo call
  #verbatimTextOutput("google_user_id"),

# A little-known feature of Shiny is that the UI can be a function, not just
# objects. You can use this to dynamically render the UI based on the request.
# We're going to pass this uiFunc, not ui, to shinyApp(). If you're using
# ui.R/server.R style files, that's fine too--just make this function the last
# expression in your ui.R file.
uiFunc <- function(req) {
  if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
    url <- oauth2.0_authorize_url(api, app, scope = scope)
    redirect <- sprintf("location.replace(\"%s\");", url)
    tags$script(HTML(redirect))
  } else {
    ui
  }
}

server <- function(input, output, session) {
  params <- parseQueryString(isolate(session$clientData$url_search))
  if (!has_auth_code(params)) {
    return()
  }

  # Manually create a token
  token <- oauth2.0_token(
    app = app,
    endpoint = api,
    credentials = oauth2.0_access_token(api, app, params$code),
    cache = FALSE
  )

  # --- MODIFIED SECTION ---

  # 1. Use the correct Google user info endpoint
  user_info_url <- "https://www.googleapis.com/oauth2/v3/userinfo"

  # 2. Make the GET request to the Google user info endpoint
  resp <- GET(user_info_url, config(token = token))

  # TODO: check for success/failure here

  # 3. Parse the JSON content and extract the 'sub' field (Google user ID)
  user_data <- content(resp, "text") %>% fromJSON()
  # google_user_id <- user_data$sub

  # 4. Display the extracted Google User ID
  # output$google_user_id <- renderText({
  #   # Check if the user ID was successfully retrieved
  #   if (!is.null(google_user_id) && nchar(google_user_id) > 0) {
  #     paste("Google User ID (sub):", google_user_id)
  #   } else {
  #     "Error: Could not retrieve Google User ID."
  #   }
  # })

  # The original output is modified to show the email if available (e.g., if scope included email)
  user_email <- user_data$email
  output$userInfo <- renderText({
    if (!is.null(user_email) && nchar(user_email) > 0) {
      glue::glue("Logged in email: {user_email}")
    } else {
      "Email not available."
    }
  })
  output$dropdownMenu <- renderUI({
    #Create list of course prefixes.
    semCode <- "202611"
    selectedPrefixes <- c("GEOG", "GEOS")
    selectedPrefixes <- c("GEOG")
    dbConn <- DBFunctionsTAMU::createDBConnection()
    theTables <- dbGetQuery(dbConn, "SHOW TABLES")
    DBFunctionsTAMU::closeAllDBConnections()
    thePrefixes <- theTables %>%
      select("prefix"="Tables_in_DH_Admin_Data") %>%
      filter(stringr::str_detect(prefix, semCode)) %>%
      mutate(course=substr(prefix,1,4))  %>%
      mutate(prefix=stringr::str_sub(course, 1, 4)) %>%
      pull(prefix) %>%
      sort() %>%
      unique()

    selectInput("prefixInput", label="Course Prefixes", choices=thePrefixes, selected=selectedPrefixes, multiple=TRUE)
  })


  rvSyllabiTable <- reactiveVal(data.frame(mtcars))

  output$syllabiGrid <- render_gt({

    rvSyllabiTable() %>%
          gt() %>%
          text_transform(
            locations = cells_body(columns = c(syllabus)),
            fn = function(x) {
              # x is the vector of values in the cell
              case_when(
                x == "TRUE"  ~ as.character(htmltools::tagList(
                  htmltools::tags$i(class = "fa fa-check", style = "color: green;")
                )),
                x == "FALSE" ~ as.character(htmltools::tagList(
                  htmltools::tags$i(class = "fa fa-times", style = "color: red;")
                )),
                TRUE ~ x
              )
            }
          ) %>%
      text_transform(
        locations = cells_body(columns = starts_with("Q")),
        fn = function(x) {
          # x is the vector of values in the cell
          case_when(
            x == "NA"  ~ as.character(htmltools::tagList(
              htmltools::tags$i(class = "fa-regular fa-circle", style = "color: #500000;")
            )),
            x == "1"  ~ as.character(htmltools::tagList(
              htmltools::tags$i(class = "fa-solid fa-y", style = "color: green;")
            )),
            x == "0" ~ as.character(htmltools::tagList(
              htmltools::tags$i(class = "fa-solid fa-n", style = "color: red;")
            )),
            TRUE ~ x
          )
        }
      ) %>%
      text_transform(
        locations = cells_body(columns = c(Certify)),
        fn = function(x) {
          # x is the vector of values in the cell
          case_when(
            x == "NA"  ~ as.character(htmltools::tagList(
              htmltools::tags$i(class = "fa-regular fa-circle", style = "color: #500000;")
            )),
            x == "1"  ~ as.character(htmltools::tagList(
              htmltools::tags$i(class = "fa-regular fa-thumbs-up", style = "color: green;")
            )),
            x == "0" ~ as.character(htmltools::tagList(
              htmltools::tags$i(class = "fa-solid fa-n", style = "color: red;")
            )),
            TRUE ~ x
          )
        }
      ) %>%
      text_transform(
        locations = cells_body(columns = c(Date)),
        fn = function(x) {
          # x is the vector of values in the cell
          case_when(
            is.na(x)  ~ as.character(htmltools::tagList(
              htmltools::tags$i(class = "fa-regular fa-circle", style = "color: #500000;")
            )),
            x == "1"  ~ as.character(htmltools::tagList(
              htmltools::tags$i(class = "fa-regular fa-thumbs-up", style = "color: green;")
            )),
            x == "0" ~ as.character(htmltools::tagList(
              htmltools::tags$i(class = "fa-solid fa-n", style = "color: red;")
            )),
            TRUE ~ x
          )
        }
      )
  })
  observeEvent(input$prefixInput, {
    semDesignation <- "202611"
    if(!is.null(input$prefixInput)){
      fileName <- paste0(input$prefixInput, semDesignation)
      dbConn <- DBFunctionsTAMU::createDBConnection()
      theTable <- tbl(dbConn, fileName)
      #useData <- dbGetQuery(dbConn, paste0("SELECT * FROM ", fileName))
      useData <- theTable %>%
        #filter(timeStamp == max(timeStamp, na.rm = TRUE)) %>%
        collect()
      latestDate <- max(useData$timeStamp, na.rm=TRUE)
      useData <- useData %>%
        filter(timeStamp == latestDate) %>%
        group_by(course.designation1) %>%
        summarize(numEnrolled=sum(numEnrolled))
      DBFunctionsTAMU::closeAllDBConnections()
      dbConn <- DBFunctionsTAMU::createDBConnection()
      theTables <- dbGetQuery(dbConn, "SHOW TABLES")
      DBFunctionsTAMU::closeAllDBConnections()
      theTables <- theTables %>%
        filter(Tables_in_DH_Admin_Data == "SyllabusTableGEOG202611")
      if(nrow(theTables)==0){
        # Table does not exist.  Create it!
        cat(blue("Creating SyllabusTableGEOG202611"))
        newTable <- useData %>%
          mutate(syllabus=FALSE)
        dbConn <- DBFunctionsTAMU::createDBConnection()
        dbWriteTable(dbConn, "SyllabusTableGEOG202611", newTable)
        DBFunctionsTAMU::closeAllDBConnections()
        useData <- newTable

      } else {
        cat(red("Table does exist.  Join info with useData"))
        dbConn <- DBFunctionsTAMU::createDBConnection()
        theTable <- tbl(dbConn, "SyllabusTableGEOG202611")
        statusTable <- theTable %>%
          collect() %>%
          select("course.designation1", "syllabus", starts_with("Q"), "Certify", "Date") %>%
          mutate(syllabus=as.logical(syllabus))
        outTable <- useData %>%
          left_join(statusTable)
      }} else {
        outTable <- mtcars
      }
    rvSyllabiTable(outTable)

    showNotification(paste("the prefix is:", input$prefixInput, "."), type = "message")

  })
  observeEvent(input$table_click_data, {

    #cat("Row", input$clicked_row, "clicked.\n")
    # open a modal window to upload a file.
    # alter syllabus availablity to TRUE
    showModal(modalDialog(
      title = "File Actions",

      # Modal Content
      tagList(
        fileInput("pdfUpload", "Choose a file"),
        br(),
        h3("Certification Questions"),
        radioButtons("Q1", label="Are Course Materials and Topics Aligned with Course Description and Learning Outcomes?",
                     choices = c("Yes", "No"),
                     selected = character(0)),
        radioButtons("Q2", label="Multi-Section Course? ",
                     choices = c("Yes", "No"),
                     selected = character(0)),
        radioButtons("Q3", label="Multi-Section Objectives Aligned (and Coordinator Appointed)?",
                     choices = c("Yes", "No"),
                     selected = character(0)),
        radioButtons("Q4", label="Does the course content include material that relates to race ideology, gender ideology, sexual orientation, or gender identity? ",
                     choices = c("Yes", "No"),
                     selected = character(0)),
        conditionalPanel(
          condition = "input.Q4 == 'Yes'",
          radioButtons("Q5", label="Are topics (especially controversial ones) covered in the course essential for students to be prepared to enter their profession or discipline? Would someone in that field need or be expected to know that information?",
                       choices = c("Yes", "No"),
                       selected = character(0)),
          radioButtons("Q6", label="Are topics covered in the course consistent with the expected body of knowledge in the discipline?",
                       choices = c("Yes", "No"),
                       selected = character(0)),
          radioButtons("Q7", label="Are controversial topics presented that have no relation to the approved course description and learning outcomes? ",
                       choices = c("Yes", "No"),
                       selected = character(0)),
          radioButtons("Q8", label="Do any of the assignments require students to hold certain beliefs that have no relation to the approved course description and learning outcomes on the syllabus to get a particular grade?  ?",
                       choices = c("Yes", "No"),
                       selected = character(0)),
          radioButtons("Q9", label="Request Exception",
                       choices = c("Yes", "No"),
                       selected = character(0))
        ),
        radioButtons("Certify", label="Certify?",
                     choices = c("Yes", "No"),
                     selected = character(0)),

      ),

      footer = tagList(
        modalButton("Cancel"),
        actionButton("save_settings", "Save Changes", class = "btn-success")
      ),
      easyClose = TRUE, # Allows clicking outside the modal to close it
      fade = TRUE       # Smooth transition animation
    ))

  })
  observeEvent(input$pdfUpload, {
    req(input$pdfUpload)

    # Define the destination path
    # We use the original filename from the user's computer
    dest_path <- file.path("syllabi", paste0(input$table_click_data$value, ".pdf"))

    # Copy file from temp location to target folder
    success <- file.copy(input$pdfUpload$datapath, dest_path, overwrite = TRUE)
    # syllabiTable <- syllabiTable() %>%
    #   mutate(syllabus = case_when(course.designation1==input$table_click_data$value ~ TRUE,
    #                               TRUE ~ syllabus))

    if (success) {
      showNotification(paste("Successfully saved:", input$pdfUpload$name), type = "message")
          dbConn <- DBFunctionsTAMU::createDBConnection()
          midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")
          midTable <- midTable %>%
            mutate(syllabus = case_when(course.designation1==input$table_click_data$value ~ 1,
                                        TRUE ~ syllabus))
          displayTable <- midTable %>%
            select(-"row_names") %>%
            mutate(syllabus=as.logical(syllabus))

          rvSyllabiTable(displayTable)
          midTable <- midTable %>%
            select(-"row_names")
          dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
          DBFunctionsTAMU::closeAllDBConnections()
    } else {
      showNotification("Error: Could not save file.", type = "error")
    }
  })
  observeEvent(input$Q1, {

    value <- switch(
      input$Q1,
      "Yes" = 1,
      "No" = 0
    )

    #cat(paste(input$table_click_data$value, "Q1 pushed", "Value:", value, "\n"))
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Q1 = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Q1))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()
  })
  observeEvent(input$Q2, {
    value <- switch(
      input$Q2,
      "Yes" = 1,
      "No" = 0
    )
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Q2 = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Q2))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()
  })
  observeEvent(input$Q3, {
    value <- switch(
      input$Q3,
      "Yes" = 1,
      "No" = 0
    )
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Q3 = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Q3))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()
  })
  observeEvent(input$Q4, {
    value <- switch(
      input$Q4,
      "Yes" = 1,
      "No" = 0
    )
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Q4 = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Q4))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()
  })
  observeEvent(input$Q5, {
    value <- switch(
      input$Q5,
      "Yes" = 1,
      "No" = 0
    )
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Q5 = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Q5))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()
  })
  observeEvent(input$Q6, {
    value <- switch(
      input$Q6,
      "Yes" = 1,
      "No" = 0
    )
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Q6 = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Q6))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()
  })
  observeEvent(input$Q7, {
    value <- switch(
      input$Q7,
      "Yes" = 1,
      "No" = 0
    )
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Q7 = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Q7))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()
  })
  observeEvent(input$Q8, {
    value <- switch(
      input$Q8,
      "Yes" = 1,
      "No" = 0
    )
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Q8 = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Q8))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()
  })
  observeEvent(input$Q9, {
    value <- switch(
      input$Q9,
      "Yes" = 1,
      "No" = 0
    )
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Q9 = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Q9))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()
  })
  observeEvent(input$Certify, {
    value <- switch(
      input$Certify,
      "Yes" = 1,
      "No" = 0
    )
    dbConn <- DBFunctionsTAMU::createDBConnection()
    midTable <- dbGetQuery(dbConn, "SELECT * FROM SyllabusTableGEOG202611")

    midTable <- midTable %>%
      mutate(Certify = case_when(course.designation1==input$table_click_data$value ~ value,
                            TRUE ~ Certify)) %>%
      mutate(Date=as.Date.character(Date)) %>%
      mutate(Date=case_when(course.designation1==input$table_click_data$value ~ lubridate::now(),
                            TRUE ~ Date)) %>%
      mutate(Date=stringr::str_remove(Date, "\\..*"))
    displayTable <- midTable %>%
      select(-"row_names") %>%
      mutate(syllabus=as.logical(syllabus))

    #Add Date and Time Stamp to displayTable and midTable

    rvSyllabiTable(displayTable)
    midTable <- midTable %>%
      select(-"row_names")
    dbWriteTable(dbConn, "SyllabusTableGEOG202611", midTable, overwrite=TRUE)
    DBFunctionsTAMU::closeAllDBConnections()

    # Apply stamp to the syllabus
    submittedSyllabus <- paste0("syllabi/", input$table_click_data$value, ".pdf")
    approvedSyllabus <- paste0("syllabi/Approved/", input$table_click_data$value, "_Approved.pdf")
    qpdf::pdf_overlay_stamp(submittedSyllabus, "www/approved.pdf", output=approvedSyllabus)
  })

}
# Note that we're using uiFunc, not ui!
shinyApp(uiFunc, server)
