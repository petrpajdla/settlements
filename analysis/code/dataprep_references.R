# Project "Settlements"
# Script nr. ?
# PREPARE REFERENCES
# author: Petr Pajdla
# Corrected references are added to base database

library(tidyverse)
library(here)
library(httr)

# References
refs <- readxl::read_excel(here("analysis/data/raw_data", "references.xlsx")) %>%
  select(id, references) %>%
  separate(references, into = str_c("r", 1:15), sep = ";") %>%
  pivot_longer(-id) %>%
  filter(!is.na(value)) %>%
  mutate(value = str_trim(value)) %>%
  select(id, reference = value)

# PIAN to Akce
pian <- refs %>%
  mutate(pian = if_else(str_detect(reference, "^P-\\d+"), TRUE, FALSE)) %>%
  filter(pian) %>%
  select(-pian) %>%
  mutate(pian1 = str_extract(reference, "^P-\\d+"),
         pian2 = str_extract(reference, "\\d{5}$"),
         pian = str_c(pian1, "-0", pian2)) %>%
  select(-pian1, -pian2)

# ask for password somwhere
password <- rstudioapi::askForPassword()


# query API ---------------------------------------------------------------

#'@param child Child to select from the XML tree
query_amcr_api <- function(amcr_ident, username, child) {

  api_url <- "https://api.aiscr.cz/dapro/"
  verb <- "oai?verb=GetRecord"
  ident <- paste0("&identifier=https://api.aiscr.cz/id/", amcr_ident)
  prefix <- "&metadataPrefix=oai_amcr"

  resp <- GET(paste0(api_url, verb, ident, prefix),
              authenticate(username, password))

  res <- content(resp, "parsed", encoding = "UTF-8") %>%
    rvest::html_elements(xpath = child) %>%
    rvest::html_text()

  return(res)
}

#'@param x vector of AMCR identifiers
query_amcr_api_loop <- function(x, username, child) {

  res <- vector("list", length(x))
  names(res) <- x

  for (i in seq_along(x)) {
    Sys.sleep(0.1)
    res[[i]] <- query_amcr_api(x[i], username, child)
    print(i)
  }

  return(res)

}


# Queries -----------------------------------------------------------------

# get dokumentační jednotky
dok_jednotky <- query_amcr_api_loop(pian$pian, "pajdla@arub.cz",
                                    child = "//child_dok_jednotka")

dok_jednotky_clean <- dok_jednotky %>%
  map(as_tibble) %>%
  bind_rows(.id = "pian") %>%
  rename(dok_jednotka = value)

# get akce/lokality
akce_raw <- query_amcr_api_loop(dok_jednotky_clean$dok_jednotka, "pajdla@arub.cz",
                    child = "//parent_akce | //parent_lokalita")

akce <- akce_raw %>%
  map(as_tibble) %>%
  bind_rows(.id = "dok_jednotka") %>%
  rename(akce = value)

# bind together to get akce for pian
# I decided to cite only the PIAN entity, because I cannot be sure which of
# its parent nodes should in fact be cited, also some of cited PIANs do not
# exist in the database anymore, because PIAN is not a persistent identifier/entity.
# = Mistake in data collection, the collector did not have understanding of the
# AMČR data model...
pian_refs <- pian %>%
  left_join(dok_jednotky_clean) %>%
  left_join(akce) %>%
  mutate(new_reference = if_else(!is.na(dok_jednotka),
                                 str_c("AMČR: Záznam ",
                                       pian,
                                       " [cit. 2021-04-14]. Archeologická mapa České republiky. Dostupné z: https://digiarchiv.aiscr.cz/id/",
                                       pian,
                                       "."),
                                 NA_character_)) %>%
  select(id, reference, new_reference) %>%
  distinct()

refs_output <- refs %>%
  left_join(pian_refs) %>%
  mutate(reference = if_else(is.na(new_reference), reference, new_reference)) %>%
  select(-new_reference)

# output
write_csv(refs_output,
          here("analysis/data/derived_data/output_manual_edit/", "references.csv"))
