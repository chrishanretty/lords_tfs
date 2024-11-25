#!/usr/bin/bash
R CMD BATCH --no-save --no-restore R/01_download_files.R
R CMD BATCH --no-save --no-restore R/02_tidy_files.R
python python/03_predict_distilbert.py
R CMD BATCH --no-save --no-restore R/04_aggregate.R
R CMD BATCH --no-save --no-restore R/05_add_person_information.R
