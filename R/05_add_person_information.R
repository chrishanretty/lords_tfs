library(tidyverse)
library(jsonlite)
library(stringr)
here::i_am("R/05_add_person_information.R")

### Read in the corpus
dat <- readRDS(here::here("outputs", "daily_lord_by_heading.rds"))

### Read in the membership files
ppl <- fromJSON(here::here("twfy_ids", "people.json"))

### Information on memberships (one person => mult. memberships)
members <- ppl$membership |>
    dplyr::select(id, person_id)

ppl <- ppl$persons |>
    dplyr::select(id, identifiers) |>
    unnest(identifiers) |>
    filter(scheme == "wikidata") |>
    dplyr::select(-scheme)

dat <- left_join(dat, members,
                 by = join_by(speaker == id),
                 suffix = c("", ".bak")) 

dat <- dat |>
    mutate(person_id = ifelse(is.na(person_id),
                              as.character(person),
                              as.character(person_id)))


dat <- dat |>
    filter(!is.na(person_id)) |>
    filter(person_id != "unknown") |>
    filter(!grepl("royal", speaker))


### Work out who was previously an MP
twfy2wikidata <- read.csv(here::here("data",
                                     "all_postwar_mps_in_twfy_all_wikidata_ids.csv")) |>
    mutate(item_short = sub(".*Q", "Q", item))

### Add on the identifiers for the previous MPs
dat <- left_join(dat,
                 twfy2wikidata |> dplyr::select(person_id, item_short),
                 by = join_by(person_id))

dat <- dat |>
    mutate(former_pol = !is.na(item_short))

### How many people have identifiers?
chk <- dat |>
    group_by(person_id) |>
    summarize(has_id = any(!is.na(item_short)),
              .groups = "drop")

### Add on the identifiers for others
dat <- left_join(dat,
                 ppl,
                 by = join_by(person_id == id),
                 relationship = "many-to-one")

dat <- dat |>
    mutate(item_short = ifelse(is.na(item_short),
                               as.character(identifier),
                               as.character(item_short)))


chk2 <- dat |>
    group_by(person_id) |>
    summarize(has_id = any(!is.na(item_short)),
              .groups = "drop")

### What would the wikidata query look like?


qry <- "
# Members of HoL 
SELECT DISTINCT ?item ?itemLabel ?dob ?twfyid ?sexLabel {
 ?item p:P39 ?positionStatement .
 ?positionStatement ps:P39 wd:Q18952564 .
 OPTIONAL { ?positionStatement pq:P580 ?start . }
 OPTIONAL { ?item wdt:P569 ?dob .
           BIND(year(now()) - year(?dob) AS ?age)
 }
 OPTIONAL { ?item wdt:P2171 ?twfyid . } 
 OPTIONAL { ?item wdt:P21 ?sex . }
 SERVICE wikibase:label { bd:serviceParam wikibase:language 'en' }
}
ORDER BY ?start
"

lu <- read.csv(here::here("data",
                          "wikidata_hol_query.csv")) |>
    mutate(item_short = sub(".*Q", "Q", item)) |>
    filter(!is.na(twfyid)) |>
    mutate(person_id = paste0("uk.org.publicwhip/person/", twfyid)) |>
    distinct(person_id, item_short, itemLabel) 

### What if we have multiple matches
lu[which(lu$person_id == "uk.org.publicwhip/person/12902"),]
dat <- left_join(dat,
                 lu,
                 by = join_by(person_id),
                 suffix = c("", ".alt"),
                 relationship = "many-to-one")

dat <- dat |>
    mutate(item_short = ifelse(is.na(item_short),
                               as.character(item_short.alt),
                               as.character(item_short))) |>
    dplyr::select(-item_short.alt)



chk3 <- dat |>
    group_by(person_id) |>
    summarize(has_id = any(!is.na(item_short)),
              .groups = "drop")


### Add on the final lot
odds <- read.csv(here::here("data", "misc_persons.csv")) |>
    dplyr::select(person_id, item_short)

dat <- left_join(dat,
                 odds,
                 by = join_by(person_id),
                 suffix = c("", ".alt"))

dat <- dat |>
    mutate(item_short = ifelse(is.na(item_short),
                               as.character(item_short.alt),
                               as.character(item_short))) |>
    dplyr::select(-item_short.alt, -identifier)

### Now probably best to delete all the personal information regarding dob and re-merge

aux <- read.csv(here::here("data",
                           "wikidata_hol_query.csv")) |>
    mutate(dob = as.Date(substr(dob, 0, 10))) |>
    mutate(item_short = sub(".*entity/Q", "Q", item)) |>
    group_by(item_short, itemLabel) |>
    summarize(dob = min(dob, na.rm = TRUE),
              sexLabel = unique(sexLabel)[1],
              .groups = "drop") |>
    mutate(dob = ifelse(!is.finite(dob),
                        NA,
                        as.character(dob)),
           dob = as.Date(dob))

dat <- left_join(dat, aux,
                 by = join_by(item_short, itemLabel))

### Now, how do we deal with party?
saveRDS(dat,
        file = here::here("outputs", "garnished_lord_by_heading.rds"))
