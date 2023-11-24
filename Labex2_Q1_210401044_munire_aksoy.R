library(httr)

# SPOTIFY_ID ve SPOTIFY_SECRET environment variable'lar??n?? ayarlamak i??in
Sys.setenv(SPOTIFY_ID = "d054ae7ca3ae4ab88147925c5ab49681")
Sys.setenv(SPOTIFY_SECRET = "84e3bf85a82249d1adba69b872e1aa27")

# Ard??ndan, fonksiyonu ??a????rabilirsiniz
library(httr)

spotify_token <- function() {
  # Spotify API token alma URL
  token_url <- "https://accounts.spotify.com/api/token"
  
  # Spotify uygulama bilgileri environment variable'lardan al??n??yor
  client_id <- Sys.getenv("SPOTIFY_ID")
  client_secret <- Sys.getenv("SPOTIFY_SECRET")
  
  if (client_id == "" || client_secret == "") {
    stop("SPOTIFY_ID ve SPOTIFY_SECRET environment variable'lar??n?? ayarlay??n.")
  }
  
  # Token alma iste??i i??in body
  body <- list(
    grant_type = "client_credentials",
    client_id = client_id,
    client_secret = client_secret
  )
  
  # Token alma iste??i g??nderme
  response <- POST(
    url = token_url,
    body = body,
    encode = "form",
    add_headers("Content-Type" = "application/x-www-form-urlencoded")
  )
  
  # HTTP status code'u alma
  status_code <- status_code(response)
  
  # Token de??erini alma
  token <- content(response)$access_token
  
  # Bearer token stringini olu??turma
  bearer_token <- paste("Bearer", token)
  
  # Sonu??lar?? liste olarak d??nd??rme
  result <- list(
    status_code = status_code,
    token = bearer_token
  )
  
  return(result)
}

# Kullan??m ??rne??i
token_result <- spotify_token()
print(token_result)
#??zellik 2:

spotify_search_artist <- function(artist_name) {
  if (!is.character(artist_name)) stop("Artist name must be character type.")
  
  token <- spotify_token()
  search_url <- paste0(
    "https://api.spotify.com/v1/search?q=", URLencode(artist_name),
    "&type=artist&limit=", 5
  )
  
  response <- httr::GET(
    url = search_url,
    add_headers("Authorization" = token[[2]])
  )
  
  search_result <- httr::content(response, type = "application/json")
  status_code <- status_code(response)
  
  # ??lk num_results sanat????s??n?? al
  artists <- search_result$artists$items[seq_len(5)]
  
  search_results <- data.frame(
    artist = sapply(artists, function(x) x$name),
    id = sapply(artists, function(x) x$id)
  )
  
  result <- list(
    status_code = status_code,
    search_results = search_results
  )
  
  return(result)
}

print(spotify_search_artist("The Doors"))

