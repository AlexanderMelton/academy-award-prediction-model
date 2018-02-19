################################################################################
###            05_current_data_collection - Oscar Prediction Model           ###
################################################################################

url <- getURL("https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture",.opts = list(ssl.verifypeer = FALSE))
tables <- readHTMLTable(url)
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
df <- ldply(tables, data.frame)
df[] <- lapply(df, as.character)
df$V1 <- ifelse(is.na(df$V1), as.character(df$Year), as.character(df$V1))

out <- as.character(df$V1)

generator_3 <- function(i, vector, stop_index){
    year <- as.numeric(substr(out[[i]],1,4))
    films <- out[(i+1):(stop_index-1)]
    out <- data.frame(film = films)
    out$year <- year
    out
}

current_year <- generator_3(633,out, 642)

current_year <- merge(current_year, generator2(90))

url <- "https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture"
pg <- read_html(url)
links <- html_attr(html_nodes(pg, "a"), "href")
links_df <- unique(data.frame(link = links, film = links, stringsAsFactors = F)) 

links_df$link <- paste0("https://en.wikipedia.org", links_df$link)
links_df$film <- gsub("_", " ", links_df$film)
links_df$film <- gsub("/wiki/", "", links_df$film)
links_df$film <- gsub("(film)", "", links_df$film, fixed = TRUE)
links_df$film <- gsub("[0-9]{4} film", "", links_df$film)
links_df$film <- gsub("()", "", links_df$film, fixed = TRUE)
links_df$film <- gsub("%27", "'", links_df$film, fixed = TRUE)
links_df$film <- gsub("%26", "&", links_df$film, fixed = TRUE)
links_df$film <- gsub("Les Mis%C3%A9rables", "Les Misérables", links_df$film, fixed = TRUE)
links_df$film <- gsub("Birdman", "Birdman or (The Unexpected Virtue of Ignorance)", links_df$film, fixed = TRUE)
links_df$film <- gsub("Loud and", "Loud &", links_df$film, fixed = TRUE)
links_df$film <- trimws(links_df$film)
links_df <- subset(links_df, film %in% current_year$film)
links_df <- subset(links_df, !(duplicated(links_df$film)))

current_year <- join(current_year, links_df, by = 'film')

current_year$length <- as.numeric(lapply(current_year$link, getLength))
current_year$budget <- as.numeric(lapply(current_year$link, getBudget))

director_guild <- rbind(htmltab("https://en.wikipedia.org/wiki/Directors_Guild_of_America_Award_for_Outstanding_Directing_%E2%80%93_Feature_Film",1), htmltab("https://en.wikipedia.org/wiki/Directors_Guild_of_America_Award_for_Outstanding_Directing_%E2%80%93_Feature_Film",2),htmltab("https://en.wikipedia.org/wiki/Directors_Guild_of_America_Award_for_Outstanding_Directing_%E2%80%93_Feature_Film",3))
rownames(director_guild) <- NULL

director_guild$dg_win = 0
for(i in 1:130){
  if(i == 1){count = 0}
  if(count == 0){director_guild$dg_win[i] = 1}
  count = count + 1
  if(count == 5){count = 0}
}

director_guild <- director_guild %>%
  select(Film,dg_win) %>%
  rename(film = Film) %>%
  mutate(dg_nom = 1)

current_year <- left_join(current_year, director_guild) %>%
  mutate_each(funs(replace(., which(is.na(.)), 0)))
current_year$dg_win <- ifelse(current_year$film == "The Shape of Water", 1, 0)
rm(director_guild)

screenplay_noms <- htmltab('https://en.wikipedia.org/wiki/Academy_Award_for_Best_Original_Screenplay',3) %>%
  select(Film) %>% rename(film = Film)
current_year$screenplay_nom <- ifelse(current_year$film %in% screenplay_noms$film, 1,0)
rm(screenplay_noms)

page <- read_html('https://en.wikipedia.org/wiki/Academy_Award_for_Best_Visual_Effects')
tables <- html_nodes(page, "table")
visual_effects <- rbind(as.data.frame(html_table(tables[10], fill = TRUE)),
                        as.data.frame(html_table(tables[11], fill = TRUE)),
                        as.data.frame(html_table(tables[12], fill = TRUE)))
current_year$visual_effects_nom <- ifelse(current_year$film %in% visual_effects$Film, 1,0)

director_noms <- htmltab('https://en.wikipedia.org/wiki/Academy_Award_for_Best_Director',12) %>%
  select(Film) %>% rename(film = Film)
current_year$director_nom <- ifelse(current_year$film %in% director_noms$film, 1,0)
rm(director_noms)

page <- read_html('https://en.wikipedia.org/wiki/List_of_foreign-language_films_nominated_for_Academy_Awards#Best_Picture')
tables <- html_nodes(page, "table")
foreign <- as.data.frame(html_table(tables[1], fill = T))

current_year$foreign <- ifelse(current_year$film %in% foreign$Film.title.used.in.nomination, 1, 0)
rm(foreign, page, tables)

current_year$rt_score <- as.numeric(lapply(current_year$link, getRT))/100

############################
current_year$adj_budget <- current_year$budget
predictionModel(master, current_year)

