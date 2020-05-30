# Setup ####
# My API key is saved in my .Renviron
# Need to be on ACU VPN to work
library(rscopus) # Extract scopus and plum metrics
library(rAltmetric) # Extract altmetrics
library(tidyverse) # Data manipulations
library(magrittr) #
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
altmetrics2 <- function(x) altmetrics(doi = x) %>% altmetric_data()
# Safe version of almtrics
alm <- possibly(altmetrics2,otherwise = NULL)
# Map safe function to DOIs
alt <- pmap(list(scopus$`prism:doi`), alm)
# Bind into a data.frame
alt_clean <- rbindlist(alt %>% compact(), fill=TRUE)
# Extract PlumX Data ####
#plumX metrics
plumx2 <- function(x) plumx_metrics(x, "doi")
# Safe extraction
plumx2_safe <- possibly(plumx2,otherwise = NULL)
# Extract plum metrics
plum <- pmap(list(scopus$`prism:doi`), plumx2_safe)
# Function to dive into deep plum list
pluck_fun <- function(x){
  tmp <- pluck(x,"content") %>%
    unlist() 
  titles <- tmp[c(T,F)] 
  values <- tmp[c(F,T)]
  out <- data.frame(values = values)
  rownames(out) <- titles
  out <- out %>%
    rownames_to_column() %>%
    gather(var, value, -rowname) %>% 
    spread(rowname, value)
  return(out)
}
# Export to data frame
plum_clean  <-map(plum %>% compact, pluck_fun) %>%
  rbindlist(fill=TRUE) %>%
  select(-`404`, -`Item not found.`, -var)
# Combine the metrics ####
alts <- full_join(plum_clean, alt_clean)
metrics <- left_join(scopus, alts, by = c("prism:doi"= "doi")) %>%
  mutate(date = as.Date(`prism:coverDate`),
         year = year(date)) %>%
  setNames(tolower(names(.)))


save(metrics, file = "metrics.RData")
