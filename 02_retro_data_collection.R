################################################################################
###         02_retro_data_collection - Oscar Prediction Model                ###
################################################################################

################### Generates Film name, year, and win/lose#####################

generator1 <- function(i){
  browser()
  tmp <- htmltab("https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture",i)
  tmp <- tmp %>% select(Film) %>% rename(film = Film)
  tmp$year = 1990 + i - 65
  tmp$winner <- 0
  tmp$winner[1] <- 1 
  tmp
}

master <- ldply(65:90, generator1)
master$film <- gsub("Les Misérables", "Les Mis?rables", master$film)

tmp <- read_html('https://en.wikipedia.org/wiki/Academy_Award_for_Best_Picture') %>% html_nodes('table.wikitable')
html_table(tmp[3], fill = TRUE)
################### Generates number of total nominations ######################  

generator2 <- function(i){
  ord = i %% 10
  ord_ind <- ""
  if(ord == 1){
    ord_ind = "st"
  }else if(ord == 2){
    ord_ind = "nd"
  }else if(ord == 3){
    ord_ind = "rd"
  }else{
    ord_ind = "th"
  }
  url <- paste0("https://en.wikipedia.org/wiki/",i, ord_ind, "_Academy_Awards")
  file<-read_html(url)
  tables<-html_nodes(file, "table")
  tmp_1 <- as.data.frame(html_table(tables[3], fill = TRUE))
  tmp_2 <- as.data.frame(html_table(tables[4], fill = TRUE))
  if(c("Nominations") %in% colnames(tmp_1)){
    tmp = tmp_1
    }
     else{
       tmp = tmp_2
     }
  tmp %>%
    rename(nominations = Nominations,
           film = Film) %>%
    mutate(year = 1990 + i - 63)
}

master <- merge(master, ldply(63:88, generator2))

##################### Generating Wikipedia Links ###############################

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
links_df$film <- gsub("Les Mis%C3%A9rables", "Les Mis?rables", links_df$film, fixed = TRUE)
links_df$film <- gsub("Birdman", "Birdman or (The Unexpected Virtue of Ignorance)", links_df$film, fixed = TRUE)
links_df$film <- gsub("Loud and", "Loud &", links_df$film, fixed = TRUE)
links_df$film <- trimws(links_df$film)
links_df <- subset(links_df, film %in% master$film)
links_df <- subset(links_df, !(duplicated(links_df$film)))

master <- join(master, links_df, by = 'film')
master$link <- ifelse(master$film == "Les Mis?rables", 'https://en.wikipedia.org/wiki/Les_Mis%C3%A9rables_(2012_film)', master$link)
rm(links_df, url, pg, links)
########################### Getting Length of Films ############################

getLength <- function(link){
  link <- read_html(link)
  page <- link %>%
    html_node(".vevent") %>%
    html_text()
  return(as.numeric(str_extract(str_extract(page, "[0-9]{2,3} [minutes]"),
                                "[0-9]{2,3}")))
} 

master$length <- as.numeric(lapply(master$link, getLength))
####################### Generating Budget of Films #############################

getBudget <- function(link){
  link <- read_html(link)
  page <- link %>%
    html_node(".vevent") %>%
    html_text()
  tmp <- str_extract_all(page, "Budget\\\n\\$[0-9,.]{1,5}")
  tmp <- gsub("Budget\n$","", tmp, fixed = TRUE)
  tmp <- gsub("m","", tmp, fixed = TRUE)
  tmp <- as.numeric(tmp)
  if(isTRUE(tmp > 1)){
    return(as.numeric(tmp))
  }else{
    tmp <- str_extract_all(page, "Budget\\\n\\str[:punct:][0-9,.]{1,5} million")
    tmp <- gsub("Budget\n$","", tmp, fixed = TRUE)
    tmp <- gsub("million","", tmp, fixed = TRUE)
    tmp <- trimws(tmp)
    tmp <- gsub("[^\\w]", ",", tmp, perl = TRUE)
    tmp <- as.integer(str_split(tmp,",")[[1]])
    tmp <- mean(tmp)
    if(isTRUE(tmp > 1)){
      return(tmp)
    }else{
      tmp <- str_extract_all(page, "Budget\\\nUS\\$[0-9,.]{1,5}")
      tmp <- gsub("Budget\nUS$","", tmp, fixed = TRUE)
      tmp <- gsub("million","", tmp, fixed = TRUE)
      tmp <- as.numeric(tmp)
      if(isTRUE(tmp > 1)){
        return(tmp)
      }else{
        tmp <- str_extract_all(page, "Budget\\\n\\?[0-9,.]{1,5} million \\(\\$[.0-9,]{1,5} million")
        tmp <- gsub("Budget\\\n\\?[.0-9,]{1,5} million","", tmp)
        tmp <- gsub("million","", tmp, fixed = TRUE)
        tmp <- gsub("($","", tmp, fixed = TRUE)
        tmp <- as.numeric(trimws(tmp))
        if(isTRUE(tmp > 1)){
          return(tmp)
        }else{
          return("FIX")
        }
      }
      }
    }
}

master$budget <- lapply(master$link, getBudget)

missing = c("Four Weddings and a Funeral", "The Grand Budapest Hotel", 
            "The Crying Game", "The Queen")
missing_budget = c(4.4, 25, 4.6, 15)

for(i in 1:length(missing)){
  master$budget <- ifelse(master$film == missing[i], missing_budget[i], master$budget)
}

rm(missing, missing_budget)    
master$budget <- as.numeric(master$budget)

####################### Getting Director Guild Data ############################

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

master <- left_join(master, director_guild) %>%
            mutate_each(funs(replace(., which(is.na(.)), 0)))
master$df_nom <- ifelse(master$film == "Les Mis?rables", 1, master$director_nom)
rm(director_guild)

################## Nomination for Best Original Screenplay #####################

screenplay_noms <- htmltab('https://en.wikipedia.org/wiki/Academy_Award_for_Best_Original_Screenplay',3) %>%
  select(Film) %>% rename(film = Film)
master$screenplay_nom <- ifelse(master$film %in% screenplay_noms$film, 1,0)
rm(screenplay_noms)

################## Nomination for Best Visual Effects ##########################

page <- read_html('https://en.wikipedia.org/wiki/Academy_Award_for_Best_Visual_Effects')
tables <- html_nodes(page, "table")
visual_effects <- rbind(as.data.frame(html_table(tables[5], fill = TRUE)),
                        as.data.frame(html_table(tables[6], fill = TRUE)),
                        as.data.frame(html_table(tables[7], fill = TRUE)))
master$visual_effects_nom <- ifelse(master$film %in% visual_effects$Film, 1,0)

###################### Nomination for Best Director ############################
director_noms <- htmltab('https://en.wikipedia.org/wiki/Academy_Award_for_Best_Director',3) %>%
  select(Film) %>% rename(film = Film)
master$director_nom <- ifelse(master$film %in% director_noms$film, 1,0)
rm(director_noms)

#################### Nomination for Best Foreign Film ##########################
page <- read_html('https://en.wikipedia.org/wiki/List_of_foreign-language_films_nominated_for_Academy_Awards#Best_Picture')
tables <- html_nodes(page, "table")
foreign <- as.data.frame(html_table(tables[1], fill = T))

master$foreign <- ifelse(master$film %in% foreign$Film.title.used.in.nomination, 1, 0)
rm(foreign, page, tables)

###################### Getting Rotten Tomatoes Score ###########################

getRT <- function(link){
  link <- read_html(link)
  page <- link %>%
    html_node("#content") %>%
    html_text()
  score <- as.numeric(gsub("%","",(str_extract(strtrim(str_extract(page, "Rotten Tomatoes[^\n]+[0-9]{2,3}%"), 150), "[0-9]{2}%"))))
  if(!is.na(score)){return(score)}else{
    return(as.numeric(str_extract(str_extract(page, "[0-9]{2,3}% [^\n]+ Rotten Tomatoes"),
                                  "[0-9]{2,3}")))
  }
} 

master$rt_score <- as.numeric(lapply(master$link, getRT))/100

missing = c("American Beauty", "Finding Neverland", "Sense and Sensibility",
            "The Prince of Tides")
missing_rt = c(0.88, 0.83, 0.98, 0.73)

for(i in 1:length(missing)){
  master$rt_score <- ifelse(master$film == missing[i], missing_rt[i], master$rt_score)
}
rm(missing, missing_rt)
######################## Adjusting for Inflation ###################################

cpi <- read.table("https://fred.stlouisfed.org/data/CPIAUCSL.txt",
             skip = 53, header = TRUE)
cpi$year <- year(cpi$DATE)
yearly_cpi <- cpi %>% group_by(year) %>% summarize(cpi = mean(VALUE)) 
current_cpi <- as.vector(as.numeric(yearly_cpi[which(yearly_cpi$year == 2016),"cpi"]))
yearly_cpi$multiplier <- current_cpi/yearly_cpi$cpi

master <- left_join(master, yearly_cpi)
master$adj_budget <- master$budget * master$multiplier
master$cpi <- NULL
master$multiplier <- NULL

rm(cpi, yearly_cpi, current_cpi)

















