# install.packages("tidyverse")
# install.packages("rvest")
# install.packages("stringr")
# install.packages("ggplot2")

library(tidyverse)
library(rvest)
library(stringr)
library(ggplot2)


link1 <- "https://m.imdb.com/search/title/?title_type=feature&release_date=2010-01-01,2023-12-25&num_votes=2499,&country_of_origin=TR&count=250"
link2 <- "https://m.imdb.com/search/title/?title_type=feature&release_date=,2009-12-31&num_votes=2499,&country_of_origin=TR&count=250"

page1 <- read_html(link1)
page2 <- read_html(link2)


titles1 <- page1 %>% html_nodes(".ipc-title__text") %>% html_text()
titles2 <- page2 %>% html_nodes(".ipc-title__text") %>% html_text()



# -----------------------------------------------------------------------------

titles1 <- page1 %>% html_nodes(".dli-title") %>% html_text()
titles2 <- page2 %>% html_nodes(".dli-title") %>% html_text()

combined_titles <- c(titles1, titles2)
print(combined_titles)

# -----------------------------------------------------------------------------

years1 <- page1 %>% html_nodes(".dli-title-metadata-item:nth-child(1)") %>% html_text()
years2 <- page2 %>% html_nodes(".dli-title-metadata-item:nth-child(1)") %>% html_text()

combined_years <- c(years1, years2)
print(combined_years)

# -----------------------------------------------------------------------------

durations1 <- page1 %>% html_nodes(".dli-title-metadata-item:nth-child(2)") %>% html_text()
durations2 <- page2 %>% html_nodes(".dli-title-metadata-item:nth-child(2)") %>% html_text()

combined_durations <- c(durations1, durations2)
print(combined_durations)

# -----------------------------------------------------------------------------

ratings1 <- page1 %>% html_nodes(".ratingGroup--imdb-rating") %>% html_text()
ratings2 <- page2 %>% html_nodes(".ratingGroup--imdb-rating") %>% html_text()

combined_ratings <- c(ratings1, ratings2)
print(combined_ratings)

# -----------------------------------------------------------------------------

votes1 <- page1 %>% html_nodes(".ipc-rating-star--voteCount") %>% html_text()
votes2 <- page2 %>% html_nodes(".ipc-rating-star--voteCount") %>% html_text()

combined_votes <- c(votes1, votes2)
print(combined_votes)

# -----------------------------------------------------------------------------

title_names1 <- page1 |> html_nodes('.ipc-title__text')
title_names1 <- html_text(title_names1)
title_names1 <- tail(head(title_names1,-1),-1)
title_names1 <- str_split(title_names1, " ", n=2)
title_names1 <- unlist(lapply(title_names1, function(x) {x[2]}))

title_names2 <- page2 |> html_nodes('.ipc-title__text')
title_names2 <- html_text(title_names2)
title_names2 <- tail(head(title_names2,-1),-1)
title_names2 <- str_split(title_names2, " ", n=2)
title_names2 <- unlist(lapply(title_names2, function(x) {x[2]}))

combined_title_names <- c(title_names1, title_names2)
print(combined_title_names)

# -----------------------------------------------------------------------------
# We dont need do any process at years
print(combined_years)

# -----------------------------------------------------------------------------


# For Durations - 1
# Saat ve daikaları ayırıp tanımlıyorum :
hours1 <- as.integer(str_extract(durations1, "\\d+(?=h)"))  # For Hours
minutes1 <- as.integer(str_extract(durations1, "\\d+(?=m)"))  # For Minutes

hours1[is.na(hours1)] <- as.integer("0")     # sadece dakika olan var ise
minutes1[is.na(minutes1)] <- as.integer("0") # sadece saat olan var ise

# Süreleri dakika cinsine dönüştürmek için :
total_duration_minutes1 <- (hours1 * 60) + minutes1



# For Durations - 2
hours2 <- as.integer(str_extract(durations2, "\\d+(?=h)"))
minutes2 <- as.integer(str_extract(durations2, "\\d+(?=m)"))

hours2[is.na(hours2)] <- as.integer("0")
minutes2[is.na(minutes2)] <- as.integer("0")

total_duration_minutes2 <- (hours2 * 60) + minutes2


combined_durations <- c(total_duration_minutes1 , total_duration_minutes2)
print(combined_durations)


# -----------------------------------------------------------------------------

votes1 <- page1 %>% html_nodes(".ipc-rating-star--voteCount") %>% html_text()
votes2 <- page2 %>% html_nodes(".ipc-rating-star--voteCount") %>% html_text()

# From ChatGPT :
votes1 <- str_replace_all(votes1, "[()]", "")
votes2 <- str_replace_all(votes2, "[()]", "")

# From ChatGPT :
votes1 <- str_remove_all(votes1, "[^0-9.K]")
votes2 <- str_remove_all(votes2, "[^0-9.K]")


votes_numeric1 <- str_remove_all(votes1, "K") %>% as.numeric() * 1000
votes_numeric2 <- str_remove_all(votes2, "K") %>% as.numeric() * 1000

combined_votes <- c(votes_numeric1 , votes_numeric2)
print(combined_votes)


# -----------------------------------------------------------------------------

combined_ratings <- c(ratings1, ratings2)
print(combined_ratings)

# Parantezleri tamamen kaldırma
rating_no_parant1 <- str_replace_all(combined_ratings, "\\(.*\\)", "")
rating_no_parant_no_space1 <- str_trim(str_extract(rating_no_parant1, "^\\d+\\.?\\d*"))

# Simdi sayıları double tipine çevirelim : 
combined_ratings <- as.double(rating_no_parant_no_space)

class(combined_ratings)
print(combined_ratings)

# -----------------------------------------------------------------------------

# DATA FRAME'I OLUŞTURABILIRIM ARTIK :

imdb_movies_data <- data.frame(Title = combined_title_names, 
                               Year = combined_years, 
                               Duration = combined_durations,
                               Rate = combined_ratings,
                               Vote = combined_votes)

print(imdb_movies_data)
head(imdb_movies_data)


# -----------------------------------------------------------------------------


imdb_movies_data_vote_decreasing <- imdb_movies_data %>%
  arrange(desc(Rate))

# En iyi ve en kotu 5 film :
print("En iyi oy oranına sahip 5 film:")
print(head(imdb_movies_data_vote_decreasing, 5))

# En alttaki 5 satırı yazdır
print("En kötü oy oranına sahip 5 film:")
print(tail(imdb_movies_data_vote_decreasing, 5))


# -----------------------------------------------------------------------------


search_film <- function(name_of_the_film) {
  result <- imdb_movies_data[imdb_movies_data$Title == name_of_the_film, ]
  return(result)
}

search_film("Senden Bana Kalan")
search_film("Aykut Eniste")
search_film("Kolonya Cumhuriyeti")
search_film("Recep Ivedik 2")

# my_favourite_films <- c("Senden Bana Kalan", "Aykut Eniste","Kolonya Cumhuriyeti", "Recep Ivedik 2")
# search_film(my_favourite_films)
# BUNU NASIL YAPABİLİRİM ?



updated_imdb_movies_data <- imdb_movies_data %>% 
  group_by(Year) %>%
  mutate(Mean_Rate = mean(Rate), Total_Film_Numbers = n())



# MEAN RATE--------------------------------------------------------------

plot_mean_rate_per_year_col <- updated_imdb_movies_data %>% 
  ggplot(aes(x = Year, y = Mean_Rate)) +
  geom_col()

print(plot_mean_rate_per_year_col)



plot_mean_rate_per_year_point <- updated_imdb_movies_data %>% 
  ggplot(aes(x = Year, y = Mean_Rate)) +
  geom_point()

print(plot_mean_rate_per_year_point)

# RATE---------------------------------------------------------------

plot_rate_per_year_box <- updated_imdb_movies_data %>% 
  ggplot(aes(x = Year, y = Rate)) +
  geom_boxplot()

print(plot_rate_per_year_box)

# FILM NUMBER---------------------------------------------------------------

plot_total_film_number_per_year_point <- updated_imdb_movies_data %>% 
  ggplot(aes(x = Year, y = Total_Film_Numbers)) +
  geom_point()

print(plot_total_film_number_per_year_point)

# ---------------------------------------------------------------








