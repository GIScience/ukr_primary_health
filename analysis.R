
install.packages(c("tidyverse","sf","remotes"))
install.packages("tictoc")
remotes::install_github("GIScience/openrouteservice-r")


library(tidyverse)
library(sf)
library(geojsonsf)
library(httr)
library(openrouteservice)
library(tictoc)


api_key <- readLines("config.txt", n = 1)

ukr_health <- read.csv("ukr_health.csv")

ukr_health |> names()


ukr_health <- ukr_health |> st_as_sf(coords = c("lon","lat"), crs=4326)



ukr_health <- ukr_health |> filter(region=="VINNYTSKA")

ukr_health_doctors <- ukr_health |> filter(is_doctor == TRUE)



chunk_size <- 4
num_rows <- nrow(ukr_health_doctors)
settlements_error <- c()

#for (i in seq(1, num_rows, by = chunk_size )) {
for (i in 1:nrow(ukr_health_doctors)) {


  #end_index <- min(i + chunk_size - 1, num_rows)

  #tic(glue::glue("chunk: {i} - {end_index} / {num_rows}"))
  tic(glue::glue("run: {i} / {nrow(ukr_health_doctors)}"))


  #current_chunk <- ukr_health_doctors[i:end_index, ]
  current_chunk <- ukr_health_doctors[i, ]



  coords <- current_chunk$geometry |> st_coordinates()
  coords <- lapply(1:nrow(coords), function(m) coords[m, ])

  body <- list(locations=coords,
               range_type="time",
               range=c(300, 600,1200,1800,2400,3000,3600)
               #time=format(times[t], format = "%Y-%m-%dT%H:%M:%S")
  ) |>
    jsonlite::toJSON(auto_unbox = T)


  headers = c(
    'Authorization' = api_key,
    'Content-Type' = 'application/json'
  )
  res <- VERB("POST", url = "https://api.openrouteservice.org/v2/isochrones/driving-car", body = body, add_headers(headers))

  if (res$status_code!=200){
    settlements_error <- c(settlements_error, current_chunk$level4)
    print("error")
    toc()
    next
  } else{
    isochrones <- res$content |> rawToChar() |> geojson_sf()

    isochrones$settlement <- current_chunk$settlement |> rep(each=7)
    isochrones$settlement_population <- current_chunk$settlement_population |> rep(each=7)
    isochrones$is_doctor <- current_chunk$is_doctor |> rep(each=7)
    isochrones$level4 <- current_chunk$level4 |> rep(each=7)



    if(i ==1 ) {
      all_isochrones <- isochrones
    } else {
      all_isochrones <- rbind(all_isochrones, isochrones)
    }

  }


  toc()

}


union_isochrones <- all_isochrones |> st_make_valid() |>
  group_by(value) |>
  summarise(value = first(value), geometry = st_union(geometry))

st_write(all_isochrones, "isochrones_vinnytska.gpkg", append=F)
st_write(union_isochrones, "isochrones_union_vinnytska.gpkg", append=F)

ukr_health$travel_time <- NA
ukr_health$travel_distance <- NA

# loop over settlements
for (i in 1:nrow(ukr_health)) {
  tic(glue::glue("run: {i} / {nrow(ukr_health)}"))
  current_chunk <- ukr_health[i, ]
  ukr_health_remove <- ukr_health[-i, ]

  #calc distance to all settlements
  ukr_health_remove$sf_distance <-st_distance(
    ukr_health_remove |> st_transform(25836),
    current_chunk |> st_transform(25836)
  )

  # find 10 closest settlements /w doctor(s)
  closest_10 <- ukr_health_remove |>
    arrange(sf_distance) %>%
    head(10)

  # create one df
  input <- rbind(current_chunk |> st_coordinates(), closest_10 |> st_coordinates())

  # request distances between idx 0 and 1-10
  matrix_out <- ors_matrix(input, metrics=c("duration", "distance"),
                           api_key=api_key,
                           output="parsed",
                           sources=0, destinations=1:10)

  # matrix_out$durations |> c()
  # matrix_out$distances |> c()
  # closest_10$sf_distance |> c()
  # closest_10$level4 |> c()

  ukr_health[i, ]$travel_time <- matrix_out$durations |> min()
  ukr_health[i, ]$travel_distance <- matrix_out$distances |> min()
  toc()
}

st_write(ukr_health, "places_vinnytska.gpkg" , append=F)
