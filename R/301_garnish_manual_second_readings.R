### Purpose of this code: to read in the manual list of second readings
### Find, for each speech, the preceding major or minor heading 
### and the last speech which is a sibling preceding a major or minor heading

library(tidyverse)
library(xml2)
library(glue)

here::i_am("R/301_garnish_manual_second_readings.R")

infile <- here::here("working",
                     "second_readings_manual.csv")

dat <- read.csv(infile)


for (i in 1:nrow(dat)) {
### Read the corresponding XML file
    xml_file <- dat$file[i]
    doc <- read_xml(xml_file)
    xpath <- "//speech[@id='{first}']"
    first <- dat$first[i]
    xpath <- glue::glue(xpath)
    speech <- doc |> xml_find_first(xpath = xpath)
    following_headings <- speech |> xml_find_first("(following-sibling::major-heading|following-sibling::minor-heading)")
    if (length(following_headings) == 0) {
        ### Just take the last speech in the document
        last_speech <- speech |> xml_find_all("//speech")
    } else { 
        last_speech <- following_headings |> xml_find_all("preceding-sibling::speech")
    }
    
    last_speech <- last_speech[[length(last_speech)]]
    last <- last_speech |> xml_attr("id")
    dat$last[i] <- last
    
}

write.csv(dat,
          file = here::here("working",
                                 "second_readings_manual_garnished.csv"),
          row.names = FALSE)
