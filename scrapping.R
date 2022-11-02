library(rvest)
library(RSelenium)

### Start the server
rD <- rsDriver(browser = c("firefox"), port = 4450L)
remDr <- rD[["client"]]
###

url <- "https://www.linkedin.com/jobs/search?keywords=Data%20Analyst&location=Adelaide%2C%20South%20Australia%2C%20Australia&geoId=107042567&trk=public_jobs_jobs-search-bar_search-submit&position=1&pageNum=0"

remDr$navigate(url)

for(i in 1:50) { 
  print(i)
  remDr$findElement("css", "body")$sendKeysToElement(list(key = "end"))
  remDr$findElement("css", "body")$sendKeysToElement(list(key = "up_arrow"))
  Sys.sleep(2)
  
  tryCatch(expr = {
    remDr$findElement(using = 'class name', 
                      value = 'infinite-scroller__show-more-button--visible')$clickElement()
    print("has button")
    Sys.sleep(2)
  },
  error = function(e) {
    print("sem botão")
  })
  
  end_test <- !grepl(
    "hidden",
    remDr$findElement(using = 'class name', 
                      value = 'inline-notification')$getElementAttribute("class")[[1]],
    fixed = TRUE)
  
  if(end_test) {
    print("chegou ao fim!!!")
    break
  }
}

jobs <- remDr$getPageSource()[[1]] %>% 
  read_html()

### Stop the server
remDr$close()
rD[["server"]]$stop()
###

n_jobs <- jobs %>% 
  html_element(".results-context-header__job-count") %>% 
  html_text()

# jobs_list <- jobs %>% 
#   html_element(".jobs-search__results-list") %>% 
#   html_elements("li")

### Only getting jobs with links
jobs_list <- jobs %>% 
  html_element(".jobs-search__results-list") %>% 
  html_elements(".base-card__full-link")

saveRDS(jobs_list, "jobs_list.RDS")
jobs_list <- readRDS("jobs_list.RDS")

# links <- jobs_list %>% 
#   html_elements(".base-card__full-link") %>% 
#   html_attr("href")

links <- jobs_list %>% 
  html_attr("href")

titles <- c()
locations <- c()
date_groups <- c()

get_metadata <- function(job_html) {
  job_metadata <- job %>% 
    html_element(".top-card-layout__entity-info")
  
  title <- job_metadata %>%
    html_element("h1") %>%
    html_text(trim = TRUE)
  
  location <- job_metadata %>%
    html_element("h4") %>%
    html_element(".topcard__flavor--bullet") %>%
    html_text(trim = TRUE)
  
  date_group <- job_metadata %>%
    html_element(".posted-time-ago__text") %>%
    html_text(trim = TRUE)
  
  list(title = title, location = location, date_group = date_group)
}

for (link in links[1:5]) {
  remDr$navigate(link)
  Sys.sleep(3)
  
  remDr$findElement(using = 'class name',
                    value = 'show-more-less-html__button--more')$clickElement()
  Sys.sleep(1)
  
  job <- remDr$getPageSource()[[1]] %>% 
    read_html()
  
  test_jobs <- append(test_jobs, list(job))
  
  # job_metadata <- get_metadata(job)
  # titles <- append(titles, job_metadata$title)
  # locations <- append(locations, job_metadata$location)
  # date_groups <- append(date_groups, job_metadata$date_group)
}

test_jobs <- list()

for (job in test_jobs) {
  test_html <- job %>% 
    html_element(".show-more-less-html__markup") %>% 
    html_text2()
  test_words <- c("excel", "power bi", "powerbi", "analytics", "analysis", 
                  " r ", ",r ", "\\Q r.\\E", "python", "sql", "tableau", 
                  "dashboard", "visualization", "storytelling", "QLIK", 
                  "SQL server", "SAS", "etl", "Sponsorship", "spreadsheet")
  
  test_retorno <- sapply(test_words, grepl, test_html, ignore.case = TRUE)
  
  print(length(names(test_retorno[test_retorno])))
  print(names(test_retorno[test_retorno]))
}

# titles <- jobs_list %>% 
#   html_elements(".base-search-card__title") %>% 
#   html_text(trim = TRUE)
# 
# locations <- jobs_list %>% 
#   html_elements(".job-search-card__location") %>% 
#   html_text(trim = TRUE)
# 
# date_groups <- jobs_list %>% 
#   html_elements(".job-search-card__listdate") %>% 
#   html_text(trim = TRUE)
# 
# dates <- jobs_list %>% 
#   html_elements(".job-search-card__listdate") %>% 
#   html_attr("datetime")


jobs_df <- data.frame(titles, locations, dates, date_groups, links)
