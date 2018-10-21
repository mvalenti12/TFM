source("scripts/libraries_functions.R")
### azure
### the key is not valid anymore. 
end.point <- "westeurope.api.cognitive.microsoft.com"
key1 <- "e4c029dc2b344e3ab86ad9c79b2b054c"

get_azure_response <- function(x){
  res <- POST(url = "https://westeurope.api.cognitive.microsoft.com/face/v1.0/detect?returnFaceId=false&returnFaceLandmarks=false&returnFaceAttributes=emotion",
              body = paste0('{"url":"',x,'"}'),
              add_headers(.headers = c("Ocp-Apim-Subscription-Key" = key1)))
  if(length(httr::content(res))>0){
    print(unlist(httr::content(res)[[1]]$faceAttributes$emotion,
                 use.names=FALSE))
    return(unlist(httr::content(res)[[1]]$faceAttributes$emotion,
                  use.names=FALSE))  
  } else {
    print("warning! no results (azure)")
    return(rep(NA,8))
  }
}  



### google
# Reads a JSON file with the credentials.
# The credentials can be downloaded on Google's Cloud Shell.
creds = fromJSON('credentials/credentials.json')

options("googleAuthR.client_id" = "") #put keys here
options("googleAuthR.client_secret" = "")

# read from the JSON data
options("googleAuthR.client_id" = creds$installed$client_id) 
options("googleAuthR.client_secret" = creds$installed$client_secret)

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))

# Authorizes googleAuthR to access your Google user data
googleAuthR::gar_auth()

# Creates a custom function to retrieve the data
get_google_response <- function(x){
  dt_fun <- getGoogleVisionResponse(x,
                                feature = "FACE_DETECTION") [1,]
  if(dt_fun=="No features detected!"){
    print("warning! no results (google); No feats detected")
    return(rep(NA,9))
  } else if (ncol(dt_fun)<4){
    print("warning! no results (google); num col <4")
    return(rep(NA,9))
  } else {
    res <- c(dt_fun$detectionConfidence,
             dt_fun$landmarkingConfidence,
             dt_fun$joyLikelihood,
             dt_fun$sorrowLikelihood,
             dt_fun$angerLikelihood,
             dt_fun$surpriseLikelihood,
             dt_fun$underExposedLikelihood,
             dt_fun$blurredLikelihood,
             dt_fun$headwearLikelihood)
    print(res)
    return(res)    
  }
}

get_image_link <- function(x){
  url <- paste0("https://www.kiva.org/lend/",x) 
  link <- read_html(url) %>%
    html_nodes(xpath='//*[@id="main"]') %>%
      html_nodes('div') %>%
      html_nodes('div') %>%
      html_nodes('div') %>%
      html_nodes('figure') %>%
      as.character() %>%
      str_extract(pattern = "https://.*.jpg") 
  if(!identical(link,character(0))){
    return(link)
  } else {
    return(NA)
  }
}
