library(tidyverse)
library(xml2)
library(glue)
library(parallel)

n_cores <- parallel::detectCores() - 2

here::i_am("R/999_search_for_bills_in_headings.R")

infiles <- list.files(here::here("scrapedxml/debates"),
                      pattern = "debates20*",
                      full.names = TRUE)

### ParlParse will occasionally store duplicates of date entries
### Pick the last of each date (i.e., if we have a, b, and c, pick c
date <- sub("scrapedxml/lordspages//daylord", "", infiles, fixed = TRUE)
date <- gsub("[a-z]", "", date)

infiles <- by(infiles, list(date = date), FUN = tail, n = 1)
infiles <- unlist(unique(infiles))

find_term <- function(file, term) {
    require(xml2)
    require(tidyverse)
    require(glue)
    page <- read_xml(file)
    
### find all headings that contain the term in upper- or lower-case
    xpath <- "//major-heading[contains(., '{term}')]"
    xpath <- glue::glue(xpath)
    
    movements <- page |> xml_find_all(xpath)

    if (length(movements) == 0) {
        xpath <- "//minor-heading[contains(., '{term}')]"
        xpath <- glue::glue(xpath)
        movements <- page |> xml_find_all(xpath)
    }

### Return early if there's nothing there
    if (length(movements) == 0) {
        return(NULL)
    }

    holder <- vector(mode = "list", length = length(movements))
    i <- 1

    ### for each such major heading, get the following speech
    ## For each such speech, get the preceding major or minor heading,
    ## and the subsequent major of minor heading
    for (m in movements) {
        ### Get the ID for the first speech
        first <- m |> xml_attr("id")
        heading_text <- m |> xml_text(trim = TRUE) |>
            str_squish()
        ### Get the relevant heading
        xpath <- "following-sibling::speech"
        res <- m |> xml_find_first(xpath)
        first_speech <- res |> xml_attr("id")
### We want to return a data frame with the search term
### the file itself
### the ID of the heading
### the ID of the speech

        holder[[i]] <- data.frame(heading = heading_text,
                                  first = first,
                                  first_speech, 
                                  file = file)
        i <- i + 1
    }
    holder <- bind_rows(holder)
    return(holder)
}



cl <- makeCluster(n_cores)
clusterExport(cl, "find_term")

term <- "Wreck Removal Convention Bill"

res <- parLapply(cl, infiles, find_term, term)
res <- bind_rows(res) |>
    arrange(file)

res


res |> filter(grepl("2000", file))
