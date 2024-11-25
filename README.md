This folder contains materials to produce measures of future focus for
speakers in the Lords. These measures are produced at the level of the
day speaker, across all types of legislative speech, and for
legislative bills specifically.

The code in this folder is split across code written in R and code
written in Python. Code written in R (files `R/01_download_files.R`,
`R/02_tidy_files.R`) downloads the source files from the publicwhip
site, processes the XML, and splits the text into sentences ready for
processing. Code written in Python (file
`python/03_predict_distilbert.py`) predicts class membership
(past/present/future) using an existing pre-trained model hosted on
Huggingface. Subsequent R code aggregates this information in
different ways.

The shell command `./doall.sh` wil run the main workflow. In order to
generate measures of temporal focus for "bill talk" alone, run
`R/401_aggregate_second_readings.R` and
`R/402_aggregate_third_readings`. These will pull in information about
the specific XML files and XPath queries contained in working. 

The code in files `201_find_second_readings.R` and
`301_garnish_manual_second_readings.R`, and the equivalents for third
readings, should not be run unless you want to manually find the XML
files and XPath queries once again. These files are included so that
you can understand how the files in working were machine generated
before being hand-coded.

The output of this process is contained in outputs/, with legislators
recorded by their wikidata ID.

A manifest of the main files is reported below. 

.
├── data
│   ├── all_postwar_mps_in_twfy_all_wikidata_ids.csv
│   ├── odds_and_sods.csv
│   └── wikidata_hol_query.csv
├── doall.sh
├── outputs
├── python
│   └── 03_predict_distilbert.py
├── R
│   ├── 00_helpers.R
│   ├── 01_download_files.R
│   ├── 02_tidy_files.R
│   ├── 04_aggregate.R
│   ├── 05_add_person_information.R
│   ├── 201_find_second_readings.R
│   ├── 202_find_third_readings.R
│   ├── 301_garnish_manual_second_readings.R
│   ├── 401_aggregate_second_readings.R
│   ├── 402_aggregate_third_readings.R
│   ├── 999_missing_readings.R
│   └── 999_search_for_bills_in_headings.R
├── README.md
├── scrapedxml
│   └── lordspages
├── twfy_ids
│   ├── lords_entries_no_speaker_only_person_id.csv
│   ├── people.json
│   ├── twfy2wikidata.csv
│   ├── twfy2wikidata_lu.csv
│   ├── twfy2wikidata_lu_manual.csv
│   └── wd2twfy_const_lu.csv
└── working
    ├── distilled
    ├── lordspages
    ├── reverse_map.R
    ├── second_readings_checked.csv
    ├── second_readings_manual.csv
    ├── second_readings_manual_garnished.csv
    ├── second_readings_to_be_checked.csv
    ├── third_readings_checked.csv
    ├── third_readings_manual.csv
    └── third_readings_to_be_checked.csv

