## Syllabus Certification Tool for 2026A
# This is app.R
# A robust Shiny application demonstrating Google Authentication via googleAuthR
# using the gar_shiny_ui/gar_shiny_auth functions for server deployment.

library(shiny)
library(httr)
library(dplyr)
library(jsonlite) # Added for JSON parsing

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

ui <- fluidPage(
  # Your regular UI goes here, for when everything is properly auth'd
  h3("Fozzie"),
  # Changed to display the user ID from the userinfo call
  verbatimTextOutput("google_user_id"),
  verbatimTextOutput("userInfo")
)

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
  google_user_id <- user_data$sub

  # 4. Display the extracted Google User ID
  output$google_user_id <- renderText({
    # Check if the user ID was successfully retrieved
    if (!is.null(google_user_id) && nchar(google_user_id) > 0) {
      paste("Google User ID (sub):", google_user_id)
    } else {
      "Error: Could not retrieve Google User ID."
    }
  })

  # The original output is modified to show the email if available (e.g., if scope included email)
  user_email <- user_data$email
  output$userInfo <- renderText({
    if (!is.null(user_email) && nchar(user_email) > 0) {
      glue::glue("Logged in email: {user_email}")
    } else {
      "Email not available."
    }
  })
}

# Note that we're using uiFunc, not ui!
shinyApp(uiFunc, server)
