library(tidyverse)
library(janitor)
library(lubridate)
library(readxl)

# source: https://open.canada.ca/data/en/dataset/2e4e5626-3185-4c8d-932a-7e161355fb96
pop_by_dep_ex_level <- read_csv("https://www.canada.ca/content/dam/tbs-sct/documents/innovation/human-resources-statistics/ssa-pop6-eng.csv") %>%
  clean_names %>%
  rename(
    organization = departments_and_agencies,
    ex_type = fps_executive
  ) %>%
  mutate(date = as_date(paste0(
    substr(date, 1, 4),
    "-",
    substr(date, 5, 6),
    "-01"
  ))) %>%
  mutate(date = date + months(1) - days(1)) # align with fiscal year dates (end of month)

# source: https://hrdatahub-centrededonneesrh.tbs-sct.gc.ca/?GoCTemplateCulture=en-CA
pop_by_classification_by_organization <- tibble(file = fs::dir_ls("data/source/hrdatahub-centrededonneesrh.tbs-sct.gc.ca/", glob = "*.xlsx")) %>%
  mutate(
    organization = map_chr(file, ~read_excel(.x, range = "A1", col_names = c("applied_filters")) %>% pull()),
    counts = map(file, function(x) read_excel(x, skip = 1))
  ) %>%
  unnest(counts) %>%
  clean_names %>%
  mutate(organization = str_remove(organization, fixed("Applied filters:\nOrganization (select one) is "))) %>%
  mutate(organization = str_remove(organization, fixed("\nYear (March 31) is 2017, 2018, 2019, 2020, 2021, or 2016"))) %>%
  select(-file, fy_end = march_31)
