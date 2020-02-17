# Web Scraping Assignment
# DATA 900 - Professor Gyory
# Jacob Mannix

#Process
# -P1- Access Wikipedia Hot 100 Era top singles by year (available years: 1958-2019)
# -P1- Get a list of the top singles for a particular year
# -P2- Access Genius lyrics website and get lyrics for each of the songs in the list
# -P2- Put all lyrics in a list and make some visuals on the words used in that year

# Loading Libraries
library(rvest)
library(RSelenium)
library(tidyverse)
library(stringr)

# Loading Variables back into Global environment
load("/Users/jacobmannix/Box Sync/M.S. Analytics/Analytics Fall/DATA 900/Web Scrapping/Assignment/Web Scraping Assignment Data Variables/Songs1980_2015.RData") # Songs
load("/Users/jacobmannix/Box Sync/M.S. Analytics/Analytics Fall/DATA 900/Web Scrapping/Assignment/Web Scraping Assignment Data Variables/lyrics1980_2015.RData") # Lyrics
load("/Users/jacobmannix/Box Sync/M.S. Analytics/Analytics Fall/DATA 900/Web Scrapping/Assignment/Web Scraping Assignment Data Variables/docs1980_2015.RData") # docs

#P1----- Scraping Billboard Hot 100 Era top singles from Wikipedia by year -------

# Reading page for Billboard "Hot 100 Era" number one singles
billboard_singles <- read_html("https://en.wikipedia.org/wiki/List_of_Billboard_number-one_singles")

# Getting list of all years where Billboard had a "Hot 100 Era" list
hot100_years_full <- billboard_singles %>%
  html_nodes("tbody") %>%
  html_nodes("tr") %>%
  html_nodes("td") %>%
  html_nodes("a") %>%
  html_text()

# Trimming the list above to years of "Hot 100 Era" (1958-2019)
hot100_years <- hot100_years_full[c(58:62)] #(1:62) #trimming the list of years
hot100_years <- "1980" # Here you can choose a specific year to look at

#P2----- Iterating through the list of years to get all of the Hot 100 Era Tables -----
hot100_list <- c()  # Creating an empty list for the songs

for(i in hot100_years){
  hot100_session <- html_session("https://en.wikipedia.org/wiki/List_of_Billboard_number-one_singles") # Initializing HTML Session
  
  hot100_link <- hot100_session %>% 
    follow_link(i)
  
  hot100_chart <- hot100_link %>% # Get the song charts for a specific year
    # html_nodes("table.wikitable.plainrowheaders") %>%
    html_nodes(xpath = "/html/body/div[3]/div[3]/div[4]/div/table[2]") %>%
    html_table(fill = TRUE, header = 1)
  
  hot100_list <- append(hot100_list, hot100_chart) # Appending the table/songs to the overall list
  #Sys.sleep(5) # optional system sleep between iterations
}

# Converting list to a dataframe and getting unique list of songs and then back into list
hot100_df <- data.frame(hot100_list)[3:4]
hot100_df$Song.Artist <- paste(hot100_df$Song, hot100_df$Artist.s.)

hot100_songs_df <- unique(data.frame(hot100_df)[3])

hot100_songs_list <- c()

for (i in hot100_songs_df){
  hot100_songs_list <- str_replace_all(i, "[:punct:]", '')
  # hot100_songs_list <- str_replace_all(i, '"', '')
}

Songs1980 <- hot100_songs_list

#Take Dataframe and pass song name into genius.com, Using RSelenium to access Genius.com lyrics for each song
driver <- rsDriver(browser = c("firefox"))
remote_driver <- driver[["client"]]
remote_driver$open()

# Looping through each song name and getting lyrics for each song
# Initialize empty lists
lyrics_list <- c()
lyrics_out <- c()
hot100_songs_list <- Songs1980
for(i in 1:length(hot100_songs_list)){
  remote_driver$navigate("https://genius.com")
  
  remote_driver$refresh() #Refresh to home page
  Sys.sleep(2)
  address_element <- remote_driver$findElement(using = 'xpath', value = '/html/body/div/div/div[1]/form/input')
  address_element$sendKeysToElement(list(hot100_songs_list[i]))
  Sys.sleep(2)
  
  button_element <- remote_driver$findElement(using = 'xpath', value = "/html/body/div/div/div[1]/form/div[2]")
  button_element$clickElement()
  Sys.sleep(2)
  button_element2 <- remote_driver$findElement(using = 'class', value = "mini_card")
  button_element2$clickElement()
  Sys.sleep(2)
  
  lyrics_out <- remote_driver$findElement(using = "xpath", value="/html/body/routable-page/ng-outlet/song-page/div/div/div[2]/div[1]/div/defer-compile[1]/lyrics/div/div/section")
  Sys.sleep(2)
  
  lyrics_list_text <- lyrics_out$getElementText()
  lyrics_list <- append(lyrics_list, lyrics_list_text)
  #lyrics_list <- lyrics_list[-c(1)]
}

#Write Lyrics to CSV or text file
# write.csv(lyrics_list, file = "test1980lyrics.csv")
# lyrics1980 <- lyrics_list
# lyrics1980 <- lyrics1980[-c(6,7)]
# lyrics1985 <- lyrics1985[-c(8,16)]
# lyrics1990 <- lyrics1990[-c(3,12)]
# lyrics2015 <- lyrics2015[-c(2)]

# ----------------------------------------------------------------------------------------

# Add Lyrics to Song information 

#mydf <- data.frame(street_names, lat_long_column) %>%
#  mutate(lat_long = str_remove_all(lat_long, "\\(|\\)")) %>%        # Remove the parentheses
  # from the lat long string
#  separate(lat_long, into = c("latitude", "longitude"), sep = ",")

# driver$server$stop()  # Drops the connection to the server

# Beginning to visualize data with wordclouds
# Load Libraries for wordcloud
library("tm") # text mining
library("wordcloud") # world cloud
library("RColorBrewer") # color palettes
library("SnowballC") # text stemming

# text <- read.csv(file = '/Users/jacobmannix/Desktop/test1980lyrics.csv')
docyearALL <- c(docs1980, docs1985, docs1990, docs1995, docs2000, docs2005, docs2010, docs2015)

docs <- Corpus(VectorSource(docyearALL)) # lyrics_list
# inspect(docs)

# Cleaning up the docs
toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "\\|")

# Cleaning up the docs further
docs <- tm_map(docs, content_transformer(tolower)) #to lower case
docs <- tm_map(docs, removeNumbers) # Remove numbers
docs <- tm_map(docs, removeWords, stopwords("english")) # Remove english common stopwords
docs <- tm_map(docs, removePunctuation) # Remove punctuations
docs <- tm_map(docs, stripWhitespace) # Eliminate extra white spaces
docs <- tm_map(docs, removeWords, c("chorus", "verse"))
# docs <- tm_map(docs, removeWords, c("niggas", "fuckin", "bitch")) # Remove your own stop word

# Creating a Term Document Matrix to display most frequently used words
dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v) # 
head(d, 20)

for (i in docyear){
  dtm <- TermDocumentMatrix(i)
  m <- as.matrix(dtm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  headd <- head(d, 15)
  
  docshead <- append(docshead, headd)
}





# Creating a Word Cloud
set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=50, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#exploring frequent terms and there associations
# findFreqTerms(dtm, lowfreq = 4)
# findAssocs(dtm, terms = "psycho", corlimit = 0.3)
# head(d, 10)

# Plotting word frequencies as Barplot
barplot(d[1:10,]$freq, las = 2, names.arg = d[1:10,]$word,
        col ="lightblue", main ="Most frequent words 2015",
        ylab = "Word frequency")

# SENTIMENT ANALYSIS
#install.packages('sentimentr')
library(sentimentr)
sentiment(dtm)

# maybe try for distinct words in songs? for different years/ decades?
# install.packages("tidytext")
# library(tidytext)
# get_sentiments("afinn")





