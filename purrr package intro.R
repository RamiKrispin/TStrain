# Introduction to the purrr package
# https://www.youtube.com/watch?v=7UlWJWfZO9M&t=922s
library(tidyverse)
library(repurrrsive)

data(sw_people)
luke <- sw_people[[1]] 

names(luke)
luke$name
luke$starships
luke$homeworld

map(sw_people, ~ length(.x$starship))


planet_lookup <- map_chr(sw_planets, "name") %>%
  set_names(map_chr(sw_planets, "url"))
planet_lookup %>% str

planet_lookup[luke$homeworld]
map(.x = sw_people, .f = ~ planet_lookup[.x$homeworld])


sw_people <- sw_people %>% set_names(map_chr(sw_people, "name"))
str(sw_people, max.level = 1)

#How many statships has each character been in ?
map(sw_people, ~ length(.x[["starships"]]))
map_int(sw_people, ~ length(.x[["starships"]]))

# What color is each character's hair?
map(sw_people, ~ .x[["hair_color"]])
map_chr(sw_people, ~ .x[["hair_color"]])

# Is the character male?
map(sw_people, ~ .x[["gender"]] == "male")
map_lgl(sw_people, ~ .x[["gender"]] == "male")

# How heavy is each character?
map(sw_people, ~.x[["mass"]])
map_dbl(sw_people, ~as.numeric(.x[["mass"]]))

map(sw_people, "starships") %>% map_int(length)



