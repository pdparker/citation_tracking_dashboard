# Setup ####
# My API key is saved in my .Renviron
# If one a mac:
#   1. Open a terminal and type: cd ~
#   2. type: touch .Renviron
#   3. type: open .Renviron
#   4. In the document type: Elsevier_API = "<<your Scopus API key>>"
#   5. Close and save
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

# Extract Journal Information
issns <- metrics$issns %>% unique %>% na.omit()

retrieve_metrics <- function(issn, token=NULL, sleep = .1){
  require(rscopus)
  nms = c("status", "year")
  front = "http://api.elsevier.com/content/serial/title?issn="
  end   = "&field=SJR,SNIP&view=STANDARD&apiKey="
  if(is.null(token)){
    if(rscopus::have_api_key()==TRUE){
      token = Sys.getenv("Elsevier_API")
    }else{cat("No API Key")}
  }else{token = token}
  query = paste0(front, issn, end, token)
  
  tmp = GET(query)
  tmp = content(tmp, "text")
  tmp = fromJSON(tmp, flatten = TRUE)
  SNIP = data.frame(tmp$`serial-metadata-response`$entry$SNIPList.SNIP)
  names(SNIP) <- c(nms, "SNIP")
  SJR = data.frame(tmp$`serial-metadata-response`$entry$SJRList.SJR)
  names(SJR) <- c(nms, "SJR")
  out = c(issn = issn, SNIP = SNIP$SNIP,SJR = SJR$SJR)
  return(out)
  Sys.sleep(sleep)
}
retrieve_metrics_safely <- possibly(retrieve_metrics,otherwise = NULL)

j_metrics <- map(issns, retrieve_metrics_safely)

j_metrics  <- j_metrics %>% 
  compact() %>% rbind_list()


metrics <- left_join(metrics, j_metrics, by = c("issns"= "issn"))

save(metrics, file = "data/metrics.RData")

metrics_display <-
  metrics %>%
  select(Title = `dc:title`,
         SJR = SJR,
         SNIP = SNIP,
         DOI = `prism:doi`,
         Year = year,
         Citations = `citedby-count`,
         Alt = score,
         Views = `abstract_views`,
         Downloads = `download_count`,
         Social = socialmedia) %>%
  mutate_at(vars(Citations:Social), as.numeric) %>%
  mutate(Year = as.integer(Year)) %>%
  mutate_at(vars(Citations:Social), .funs = list(~replace(.,is.na(.),"0")))

save(metrics_display, file = "data/metrics_display.RData")




#shinyAppDir("R")



