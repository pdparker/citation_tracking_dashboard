# Setup ####
# My API key is saved in my .Renviron
# Need to be on ACU VPN to work
library(rscopus)
library(rAltmetric)
library(tidyverse)
library(magrittr)
library(data.table)
library(lubridate)
# Extract Scopus Data ####
if(have_api_key()){
  scopus <- author_df(au_id = "23470098500")
  }else{
  cat("Set API Key")
  }
# Extract Altmetric Data ####
#Create a new function to pass to possibly
altmetrics2 <- function(x) altmetrics(doi = x) %>%altmetric_data()
# Safe version of almtrics
alm <- possibly(altmetrics2,otherwise = NULL)
# Map safe function to DOIs
alt <- pmap(list(scopus$`prism:doi`), alm)
# Bind into a data.frame
alt_clean <- rbindlist(alt %>% compact(), fill=TRUE)
# Extract PlumX Data ####
#plumX metrics
plumx2 <- function(x) plumx_metrics(x, "doi")
plumx2_safe <- possibly(plumx2,otherwise = NULL)
plum <- pmap(list(scopus$`prism:doi`), plumx2_safe)


# Combine the metrics ####
metrics <- left_join(scopus, alt_clean, by = c("prism:doi"= "doi")) %>%
  mutate(date = as.Date(`prism:coverDate`),
         year = year(date))

metrics %>%
  mutate(citations = as.numeric(`citedby-count`),
         score = as.numeric(score)) %>%
  ggplot(aes(citations)) +
  geom_histogram()

metrics %>%
  mutate(citations = as.numeric(`citedby-count`),
         score = as.numeric(score)) %>%
  ggplot(aes(score)) +
  geom_histogram()

metrics %>%
  mutate(citations = as.numeric(`citedby-count`),
         score = as.numeric(score)) %>%
  group_by(year) %>%
  summarise(., correlation = cor(citations, score, use = "pairwise.complete.obs"),
            n = n()) %>%
  mutate(year = as.integer(year)) %>%
  ggplot(aes(x=year, y = correlation, label = n)) +
  geom_line() +
  geom_point(aes(size = n)) +
  geom_text(hjust = 0, nudge_x = .5, nudge_y = .01) +
  scale_fill_continuous(name="Number of Publications")


