library(tidyverse)
library(arrow)

here::i_am("R/402_aggregate_third_readings.R")

third <- read.csv(here::here("working",
                              "third_readings_checked.csv"))

## aux <- read.csv(here::here("working",
##                            "third_readings_manual_garnished.csv"))

## third <- rbind(third, aux)

### Subset to things with a leg_title
third <- third |>
    filter(!is.na(leg_title))

### For each entry, go in and get the corresponding RDS file
third <- third |>
    mutate(parquet_loc = sub("uk.org.publicwhip/lords/", "daylord", first),
           parquet_loc = sub("\\..*", ".parquet", parquet_loc),
           parquet_loc = here::here("working/distilled",
                                    parquet_loc))

third <- third |>
    mutate(first = sub("uk.org.publicwhip/lords/",
                       "",
                       first,
                       fixed = TRUE),
           last = sub("uk.org.publicwhip/lords/",
                       "",
                      last,
                      fixed = TRUE)
           ) |>
    separate_wider_delim(first,
                         names = c("date", "first_major", "first_minor"),
                         delim = ".",
                         cols_remove = FALSE) |>
    mutate(first_num = as.numeric(paste(first_major,
                                        str_pad(first_minor, width = 2, pad = "0"),
                                        sep = "."))) |>
    separate_wider_delim(last,
                         names = c("date2", "last_major", "last_minor"),
                         delim = ".",
                         cols_remove = FALSE) |>
    mutate(last_num = as.numeric(paste(last_major,
                                        str_pad(last_minor, width = 2, pad = "0"),
                                       sep = ".")))

### function to iterate over the elements of this data frame
res <- apply(third, 1, function(x) {
    file <- x["parquet_loc"]
    df <- read_parquet(file)
###
    df <- df |>
        mutate(speech_id = sub("uk.org.publicwhip/lords/",
                               "",
                               speech_id)) |>
        separate_wider_delim(speech_id,
                             names = c("date2", "speech_major", "speech_minor"),
                             delim = ".",
                             cols_remove = FALSE) |>
        mutate(speech_num = as.numeric(paste(speech_major,
                                            str_pad(speech_minor, width = 2, pad = "0"),
                                            sep = "."))) |>
        mutate(date = gsub("[a-zA-Z]", "", date2),
               date = as.Date(date, format = "%Y-%m-%d"))


### Filter to those entries between first_num and last_num
    matching <- which(df$speech_num >= as.numeric(x["first_num"]) &
                      df$speech_num <= as.numeric(x["last_num"]))

    df <- df[matching,] |>
        mutate(nchars = nchar(sents))|>
        group_by(speaker, person, date) |>
        summarize(Future = weighted.mean(Future, nchars),
                  Present = weighted.mean(Present, nchars),
                  Past = weighted.mean(Past, nchars),
                  nchars = sum(nchars),
                  .groups = "drop") |>
        mutate(leg_title = x["leg_title"],
               speaker = as.character(speaker),
               person = as.character(person))
    return(df)  
})

res <- bind_rows(res)

res <- res |>
    mutate(chamber = "Lords")

### How do we add on person information?

saveRDS(res, file = here::here("outputs", "agg_third_readings.rds"))
