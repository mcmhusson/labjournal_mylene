library(tidyverse)
library(scholar)
library(openalexR)
library(rvest)
library(jsonlite)
library(httr)
library(rvest)
library(reshape2)
library(xml2)


#load the functions you need from the packages
fpackage.check <- function(packages) {
  lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

fsave <- function(x, file = NULL, location = "./data/processed/") {
  ifelse(!dir.exists("data"), dir.create("data"), FALSE)
  ifelse(!dir.exists("data/processed"), dir.create("data/processed"), FALSE)
  if (is.null(file))
    file = deparse(substitute(x))
  datename <- substr(gsub("[:-]", "", Sys.time()), 1, 8)
  totalname <- paste(location, datename, file, ".rda", sep = "")
  save(x, file = totalname)  #need to fix if file is reloaded as input name, not as x. 
}

fload <- function(filename) {
  load(filename)
  get(ls()[ls() != "filename"])
}

fshowdf <- function(x, ...) {
  knitr::kable(x, digits = 2, "html", ...) %>%
    kableExtra::kable_styling(bootstrap_options = c("striped", "hover")) %>%
    kableExtra::scroll_box(width = "100%", height = "300px")
}


################################################################################

# Jochem name

# link your email
options(openalexR.mailto = "mylene.husson@ru.nl") 

url <- "https://api.openalex.org/authors?search=Jochem Tolsma"

# based on what you have learned so far, you would probably first try:
jt <- read_html("https://api.openalex.org/authors?search=Jochem+Tolsma") %>%
  html_text2()

substr(jt, 1, 100)

jt_json <- fromJSON("https://api.openalex.org/authors?search=Jochem+Tolsma", simplifyVector = FALSE)
#glimpse(jt_json, max.level = 1)

jt_json[["results"]][[1]][["display_name"]]
jt_json$results[[1]]$display_name

List_names <- c()
List_names <- c(List_names, jt_json$results[[1]]$display_name)

################################################################################
List_citationindex <- c()
List_citationindex <- c(List_citationindex, jt_json$results[[1]]$cited_by_count)

List_citationindex

################################################################################
# GET THE COLLABORATOR'S NAMES

jt_json$results[[1]]$works_api_url
jt_publi <- fromJSON(jt_json$results[[1]]$works_api_url, simplifyVector = FALSE)


jt_publi$results[[1]]$authorships[[i]]$raw_author_name


# Let's try to make a loop: get all of the people Jochem works together with

List_publi <- c()

for (j in 1:length(jt_publi$results)){
  for (i in 1:length(jt_publi$results[[j]]$authorships)) {
    List_publi <- c(List_publi, jt_publi$results[[j]]$authorships[[i]]$raw_author_name)
  }
}

List_publi

# But you don't just want to know the people, you want to know the collaborations
# So we will make a list of authors per paper, and add that list to a list of all people


List_colabs <- list()
for (j in 1:length(jt_publi$results)){
  List_authors <- c()
  for (i in 1:length(jt_publi$results[[j]]$authorships)) {
    List_authors <- c(List_authors, jt_publi$results[[j]]$authorships[[i]]$raw_author_name)
  }
  List_colabs[[j]] <- List_authors  # assign as list element
}

View(List_colabs)

################################################################################
# now loop over various people: DOES NOT WORK

names <- list()

links <- c("https://api.openalex.org/authors?search=Jochem+Tolsma", 
          "https://api.openalex.org/authors?search=Bas+Hofstra", 
          "https://api.openalex.org/authors?search=Saskia+Glas")

for (link in links){
  data <- fromJSON(link, simplifyVector= FALSE)
  publi <- fromJSON(data$results[[1]]$works_api_url, simplifyVector = FALSE)
  for (j in 1:length(jt_publi$results)){
    List_authors <- c()
    for (i in 1:length(jt_publi$results[[j]]$authorships)) {
      List_authors <- c(List_authors, jt_publi$results[[j]]$authorships[[i]]$raw_author_name)
    }
    List_colabs[[j]] <- List_authors  # assign as list element
  }
}

View(List_colabs)

  
################################################################################
# NOW TRY WITH EXCEL FILE AND OPEN ALEX DATA

soc_pol <- readxl::read_excel("/Users/mylenehusson/Desktop/SocialNetworks/labjournal_mylene/data/20240419Scholarid_soc_pol.xlsx")

mail <- "mylene.husson@ru.nl"

author_list <- soc_pol$Naam[1:3]
View(author_list)

all_author_data <- list()
all_colabs <- list()

for (author in author_list) {
  search <- tryCatch({
    oa_fetch(entity = "authors", 
             search = author, 
             mailto = mail)
  }, error = function(e) {
    message("Error with author: ", author)
    return(NULL)
  })
  
  if (is.null(search) || nrow(search) == 0) {
    next
  }
  
  # store the first result in the list
  all_author_data[[author]] <- search[1, ]
}

View(all_author_data)

# From all_author_data we can now get the works_api_url
# and use the for loops we made before to get data on who they worked together with.

for (author in names(all_author_data)) {
  
  # get the works_api_url
  works_url <- all_author_data[[author]]$works_api_url
  
  # fetch the publications JSON from the API
  publi <- fromJSON(content(GET(works_url), as = "text"), simplifyVector = FALSE)
  
  List_colabs <- list()  # store co-authors per publication for this author
  
  # loop over each publication
  for (j in seq_along(publi$results)) {
    List_authors <- c()
    
    # loop over authorships in this publication
    for (i in seq_along(publi$results[[j]]$authorships)) {
      List_authors <- c(List_authors, 
                        publi$results[[j]]$authorships[[i]]$raw_author_name)
    }
    
    List_colabs[[j]] <- List_authors
  }
  
  # assign this author's collaborations to the main list
  all_colabs[[author]] <- List_colabs
}

View(all_colabs)

# We could also get information on number of works counts
work_count <- list()

for (author in names(all_author_data)) {
  work_count[[author]] <- all_author_data[[author]]$works_count
}

View(work_count)



