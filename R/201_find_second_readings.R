library(tidyverse)
library(xml2)
library(glue)
library(parallel)

n_cores <- parallel::detectCores() - 2

here::i_am("R/201_find_second_readings.R")

### We're going to operate on files in the 2000s
infiles <- list.files(here::here("scrapedxml/lordspages"),
                      pattern = "daylord20*",
                      full.names = TRUE)

### ParlParse will occasionally store duplicates of date entries
### Pick the last of each date (i.e., if we have a, b, and c, pick c
date <- sub("scrapedxml/lordspages//daylord", "", infiles, fixed = TRUE)
date <- gsub("[a-z]", "", date)

infiles <- by(infiles, list(date = date), FUN = tail, n = 1)
infiles <- unlist(unique(infiles))
length(infiles)

### Shuffle the files
infiles <- sample(infiles, length(infiles), replace = FALSE)

### Let's pick an example file
eg <- grep("2021-11-02a", infiles, value = TRUE)

parse_file <- function(file) {
    require(xml2)
    require(tidyverse)
    page <- read_xml(file)
### Find all the speeches that contain the magic words
    xpath <- "//speech[contains(., 'That the Bill be now read a second time.')]"

    xpath <- "//speech[contains(., 'That the Bill be now read a second time.')] |
//speech[contains(., 'That the Bill be read a second time.')]"
    movements <- page |> xml_find_all(xpath)
    holder <- vector(mode = "list", length = length(movements))
    i <- 1
    ## For each such speech, get the preceding major or minor heading,
    ## and the subsequent major of minor heading
    for (m in movements) {
        ### Get the ID for the first speech
        first <- m |> xml_attr("id")

        ### Get the relevant heading
        xpath <- "preceding-sibling::major-heading[1] | preceding-sibling::minor-heading[1]"
        res <- m |> xml_find_all(xpath)
        ### We find the last in document order (i.e., the last preceding)
        res <- res[[length(res)]]
        heading <- res |> xml_text() |> str_squish()
        heading <- sub("Orders of the Day — ", "", heading)

        ### Get the speech preceding the next heading
        xpath <- ".//following-sibling::major-heading[1] | following-sibling::minor-heading[1]"
        res <- m |> xml_find_first(xpath)
        if (length(res) == 0) {
            res <- page |>
                xml_find_all("//speech")
            res <- res[[length(res)]]
            last <- res |> xml_attr("id")
            
        } else { 
            last <- res |> xml_attr("id")
        }
        
        holder[[i]] <- data.frame(heading = heading,
                                  first = first,
                                  last = last,
                                  file = file)
        i <- i + 1
    }
    bind_rows(holder)
}

cl <- makeCluster(n_cores)
clusterExport(cl, "parse_file")

res <- parLapply(cl, infiles, parse_file)
res <- bind_rows(res)

stopCluster(cl)

### Title case everything
res$heading <- str_to_title(str_to_lower(res$heading))
write.csv(res,
          file = here::here("working", "second_readings_to_be_checked.csv"),
          row.names = FALSE)


### Incorporate stuff from what's already been checked
if (FALSE) {
    aux <- read.csv(here::here("working",
                               "second_readings_checked.csv"))
    aux$already_checked <- 1
    res <- left_join(res,
                     aux,
                     by = join_by(heading, first, last, file))
    write.csv(res,
              here::here("working", "second_readings_to_be_checked.csv"))
                     
}
