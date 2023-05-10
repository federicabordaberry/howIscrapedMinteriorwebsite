#Here is my step by step of how I got to my results
#I found a pattern in my URL (https://www.minterior.gub.uy/index.php/unicom/noticias)
#Each page (there are 210 of them) contained 25 news in it
#The URL attached this at the end "/noticias?start=25" on the second page, and the number kept going from 25 to 25 for all the pages
#I generated a Google Sheets where I generated the 210 URLs
#In a new sheet I transposed all of my rows into columns with the function "TRANSPOSE"
#I copied that sheet and created a scraper using the "IMPORTXML" function. My XPath query was the URL and the HTML tag "//td[@class="list-title"]//@href"
#It looked something like this: =IMPORTXML(URL,//td[@class="list-title"]//@href)
#That gave me all the URLs to the news in each page
#Because there were thousands of scraping formulas working at the same time, I had to paste "only values" on a new sheet as not to collapse Google Sheets
#After that, I ended up with tons of 27 row columns filled with al the URLs from news 
#Instead of finding a way of getting all the 210 columns into one, I looked at the URLs and found a pattern
#The base URL ("https://www.minterior.gub.uy/index.php/unicom/noticias/") had attached at the end a number that always added 1, started in 4625 and ended in 10778
#Using a CONCAT funcion in sheets I adhered all those numbers to a new base link 
#The problem, which I didn't realise at the time, was that not all of the URLs generated existed (I would encounter a problem later)
#Having more than 6,000 rows of URLs I decided I had to clean up and work only with the URLs that were directly linked to raided items by getting the title of the news
#Here is when R Studio Cloud comes in (I find the online version goes quicker than the one in my computer)
#I created a function that fetched the "//h2" HTML tag on a given URL

library(rvest)

all_url_dataset <- read_xlsx("Unicom links for R.xlsx")

find_title <- function(URL) {
  title <- rvest::read_html(URL) %>%
    html_element("h2") %>%
    html_text2()
  return(title)
}

#That worked with individual links, but then I needed the function to go all through my rows and leave the result in a new column
#I used this as a first option:

apply(all_url_dataset, 1, find_title) %>%
  Sys.sleep(1)
  
#When the scraper started working on each row, I immediatelly got a "404 HTML" error
#That, of course, happened because I hadn't realised that many of my URLs did not exist
#I tried another way, with the following code:

ind_title(all_url_dataset$URL[1])
titlevector <- c()
for (i in all_url_dataset$URL){
foundtitle <- find_title(i)
 titlevector <- c(titlevector,foundtitle)
Sys.sleep(1)
print()
}

#Again, I received all the times, the "404 HTML" error
#That's when I tried using Google Sheets, another scraper
#The function I used to scrape was IMPORTXML and my XPath had each URL and the //h2 tag
#That took time, because I had to paste results as only values, so Google Sheets wouldn't collapse again
#But, by doing that, I saw that some URLs were giving me N/A values
#I clicked on those and that's how I realised that the N/A ones were the invalid URLs (I painted the cells yellow to distinguish them and to be able to erase the scraping formula to allow other formula to complete)
#Using a filter, I sent all my N/A URLs to the end and erased them
#Once again, I needed to scrape all the titles in the URLs, but now I had no invalid ones, so I gave R a try once again
#I uploaded my new dataset and decided to ask questions to the Artificial Intelligence Chat GPT
#I asked for a scraper that looked for all "//h2" tags in a given URL, that that formula repeated itself all through the rows and that the result was sent to a new column matching the URLs
#After trying some options it gave me, I edited the code myself, using Chat GPT one´s as a base
#The used code was this:

library(readxl)
library(dplyr)
library(rvest)
library(tidyverse)


selected_urls <- read_excel("Selected URL titles.xlsx")

scrape_h2 <- function(URL) {
  webpage <- read_html(URL)
  h2_tags <- html_nodes(webpage, "h2")
  h2_text <- html_text(h2_tags)
  return(h2_text)
}

selected_urls <- selected_urls %>%
  mutate(h2_text = sapply(URL, scrape_h2))
  
#Have into account that the column that had all my URLs was called "URL"
#Then, I started filtering which titles were appropriate for news that had raided items in it
#For that, I used a word counter website and inserted the column that had all the titles, as text only
#Then, I received the most common used words and used that as an inspiration to find regular expressions in news titles that had raided items in them
#Using the REGEXEXTRACT function in Google Sheets, I searched for regular expressions in titles
#An example: =REGEXEXTRACT(B2,"[Aa]llan[A-Za-z]+") -B2 being the cell with the title-
#After using that formula for all the regular expressions I thought of (52 of them) I selected the URLs that had titles which had any of those regular expressions
#I then reduced my quantity of rows to a bit more than 2,500 against the first more than 6,000
#After a long journalistic process, I realized that it was not representative enough to know how many of the news of the Ministry of Interior were related to raided goods
#I decided I only needed to know how many of them where related to raided goods in the frontier with Argentina or Brazil.
#Once again, I used a REGEXEXTRACT formula.
#For Argentina, the formula was this: =REGEXEXTRACT(B2,"[Ar]rgen[A-Za-z]+")
#For Brazil, it was this: =REGEXEXTRACT(B2,"[Bb]rasi[A-Za-z]+")
#They ended up being something like 49 titles with news from Brazil, against 10 with news from Argentina

