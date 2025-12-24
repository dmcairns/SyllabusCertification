my_auth_function <- function(jsonFile){
  # Most OAuth applications require that you redirect to a fixed and known
  # set of URLs. Many only allow you to redirect to a single URL: if this
  # is the case for, you'll need to create an app for testing with a localhost
  # url, and an app for your deployed app.

  oauthInfo <- fromJSON(jsonFile)

  if (interactive()) {
    # testing url
    theShinyPort <- 1221  #DMC added to match Google API for project called TestProject
    options(shiny.port = theShinyPort) #DMC added
    APP_URL <- paste0("http://localhost:", theShinyPort,"/") #DMC added
    oauthInfo$web$redirect_uris <- str_replace(oauthInfo$web$redirect_uris, "https", "http")
    app <- oauth_app("shinygoogle",
                     key = oauthInfo$web$client_id,
                     #key = "569494582622-3jnl6sdietjv47v603tq0l3jbik5mj9m.apps.googleusercontent.com",
                     secret = oauthInfo$web$client_secret,
                     #secret = "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",
                     redirect_uri = oauthInfo$web$redirect_uris
                     #redirect_uri = APP_URL
    )
  } else {
    app <- oauth_app("shinygoogle",
                     key = "569494582622-lek8ofqnthl0bvmicaran0q0sv4baooe.apps.googleusercontent.com",
                     secret = "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",
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

  outData <- list(app=app, APP_URL=APP_URL, api=api, scope=scope)
} #Eventually add to a package

has_auth_code <- function(params) {
  # params is a list object containing the parsed URL parameters. Return TRUE if
  # based on these parameters, it looks like auth codes are present that we can
  # use to get an access token. If not, it means we need to go through the OAuth
  # flow.
  return(!is.null(params$code))
} #Eventually add to a package
#
# uiFunc <- function(req) {
#
#   if (!has_auth_code(parseQueryString(req$QUERY_STRING))) {
#     url <- oauth2.0_authorize_url(api, app, scope = scope)
#     redirect <- sprintf("location.replace(\"%s\");", url)
#     tags$script(HTML(redirect))
#   } else {
#     ui
#   }
# } #Eventually add to a package
#
f1 <- function(session, scope, api, APP_URL, app){
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
  return(user_data)
} #Eventually add to a package

