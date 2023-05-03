library(usethis)

## code to prepare `DATASET` dataset goes here

# # EXAMPLE CODE:
# page <- read_html("https://www.ssa.gov/oact/babynames/numberUSbirths.html")
#
# ssa <- page %>% html_nodes("table") %>% .[[2]] %>% html_table() %>% tbl_df()
# names(ssa) <- c("year", "M", "F", "total")
# ssa$total <- NULL
#
# ssa$M <- parse_number(ssa$M)
# ssa$F <- parse_number(ssa$F)
#
# applicants <- ssa %>%
#   gather(sex, n_all, M:F) %>%
#   arrange(year, sex) %>%
#   mutate(n_all = as.integer(n_all)) %>%
#   arrange(year, sex)
#
# write_csv(applicants, "data-raw/applicants.csv")
# usethis::use_data(applicants, overwrite = TRUE)


# DO NOT DELETE THE LAST LINE, replace 'Dataset' with the dataset name
# Rename this file to the name of the dataset
write_csv(DATASET, "data-raw/DATASET.csv")
usethis::use_data(DATASET, overwrite = TRUE)
