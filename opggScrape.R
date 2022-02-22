# This is the web scrapper for the op.gg game history.
# The code is inspired partially by https://www.linkedin.com/pulse/how-scrape-data-from-dynamic-website-r-benjamin-aboagye



library(rvest)
library(RSelenium)
library(binman)
library(stringr)
library(xlsx)


# RSelenium packages for automating web scrapping (drop down menus)
if(require("RSelenium"))print("Package `RSelenium` loaded successfully")

rD <- rsDriver()
remDr <- rD$client
# Issue due to firewall 
driver <- rsDriver(browser = c('chrome'), chromever = '99.0.4844.17', port = 444L)



# OP.GG link
my_hist <- read_html("https://na.op.gg/summoners/na/xelin%20k")


# First look at all the class type nodes in the source.
class_nodes <- my_hist %>%
  html_nodes("*") %>%
  html_attr("class") %>%
  unique()


# Set up 'global' variables
LOST_NODES = 'div.css-utzuox'
WIN_NODES = 'div.css-7uadie'

GAME_LENGTH = 'div.game-length'
GAME_RESULT = 'div.game-result'
GAME_CHAMP = 'div.champion'
NAME = 'div.name'


# Get a lists of nodes for interests
test_lists <- html_nodes(my_hist, '.css-ja2wlz')


## Get initial variables that are needed.
# Game result
# Champion names
# KDA
# Rank
# Game elapse time
# Control Score (Minions Farmed)
# Tower TD
# Inhibitor TD
# Baron Nash TD
# Dragon TD
# Herald TD
# Team Gold
# Team kills
# Team Death


# Due to the different interpretation of "WIN" and "LOST" games, 
# I first scrape all the games that are "LOST", then all the games 
# that are "WIN". 
# I can use "WIN" and "LOSE" to do a binomial distribution analysis. (log regre)




test_lists[1] %>%
  # "class css-utzuox" is the nodes which store the info for games that are lost.
  html_nodes('div.css-utzuox') %>%
  html_nodes('div.kda') %>%
  html_nodes('div.k-d-a') %>%
  html_text()



test <- test_lists[1] %>%
  # "class css-utzuox" is the nodes which store the info for games that are lost.
  html_nodes('div.css-utzuox') %>%
  html_nodes('div.kda') %>%
  html_nodes('div.k-d-a') %>%
  html_text()



# Main function to get game history information
get.Game.result <- function(game_list_nodes, nodes.code){
  
  if(length(game_list_nodes %>% html_nodes(nodes.code))) {
    
    print("WDNMD")
    
    interm.nodes <- game_list_nodes %>%
      html_nodes(nodes.code) 
    
    game_res <- interm.nodes %>%
      html_nodes(GAME_RESULT) %>%
      html_text()
    
    champions_names <- interm.nodes %>%
      html_nodes(GAME_CHAMP) %>%
      html_nodes(NAME) %>%
      html_text()
    
    
    game_time <- interm.nodes %>%
      html_nodes(GAME_LENGTH) %>%
      html_text()
    
    control_score <- interm.nodes %>%
      html_nodes('div.stats') %>%
      html_nodes('div.cs') %>%
      html_text()
    
    kda <- interm.nodes %>%
      html_nodes('div.kda') %>%
      html_nodes('div.k-d-a') %>%
      html_text()
    
    # Consider 10s of kills and assists!!! 
    kills <- str_extract(kda, pattern = '^\\d+')
    death <- str_extract(kda, pattern = ' \\d+ ') %>% str_remove_all(' ')
    
    assists <- str_extract(kda, pattern = '\\d+$')
    
  }
  
  return(data.frame(cbind(game_res, champions_names, game_time, 
                          control_score, kills, death, assists)))
}



# Export dataframe to csv


win.result <- get.Game.result(test_lists, WIN_NODES)
lost.result <- get.Game.result(test_lists, LOST_NODES)

total.result <- rbind(win.result, lost.result)

write.csv(total.result, 'LOLGameHistoryInfo.csv', row.names = F)



## TODO: Write automation web scrapping code using RSelenium for "drop down" information
## So far this scraper only gets the basic infor for game history.






















