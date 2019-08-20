################################################################################
# Preamble and libraries
################################################################################
rm(list=ls())

library(robotstxt)
library(rvest)
library(selectr)
library(xml2)
library(dplyr)
library(stringr)
library(forcats)
library(magrittr)
library(tidyr)
library(ggplot2)
library(lubridate)
library(tibble)
library(purrr)
library(stargazer)

################################################################################
# Approved ballot measures
################################################################################
ballot2020approved = read_html("https://ballotpedia.org/2020_ballot_measures")

# Scrape data from individual state tables
# Ideally we would use state name prior to table as identifier
alabama1 = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(3) %>% mutate(state = "Alabama", date = "March 3, 2020")
  
alabama2 = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(4) %>% mutate(state = "Alabama", date = "November 3, 2020")

arkansas = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(5) %>% mutate(state = "Arkansas", date = "November 3, 2020")

california = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(6) %>% mutate(state = "California", date = "November 3, 2020")

colorado = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(7) %>% mutate(state = "Colorado", date = "November 3, 2020")

illinois = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(8) %>% mutate(state = "Illinois", date = "November 3, 2020")

iowa = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(9) %>% mutate(state = "Iowa", date = "November 3, 2020")

louisiana = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(10) %>% mutate(state = "Louisiana", date = "November 3, 2020")

michigan = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(11) %>% mutate(state = "Michigan", date = "November 3, 2020")

missouri = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(12) %>% mutate(state = "Missouri", date = "November 3, 2020")

montana = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(13) %>% mutate(state = "Montana", date = "November 3, 2020")

nebraska = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(14) %>% mutate(state = "Nebraska", date = "November 3, 2020")

nevada = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(15) %>% mutate(state = "Nevada", date = "November 3, 2020")

newmexico = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(16) %>% mutate(state = "New Mexico", date = "November 3, 2020")

northdakota = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(17) %>% mutate(state = "North Dakota", date = "November 3, 2020")

utah = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(18) %>% mutate(state = "Utah", date = "November 3, 2020")

wisconsin = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(19) %>% mutate(state = "Wisonsin", date = "November 3, 2020")

wyoming = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(20) %>% mutate(state = "Wyoming", date = "November 3, 2020")

oregon = ballot2020approved %>% 
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(21) %>% mutate(state = "Oregon", date = "November 3, 2020")

# Combine states
approved = bind_rows(alabama1, alabama2, arkansas, california, colorado, illinois, 
                 iowa, louisiana, michigan, missouri, montana, nebraska, 
                 nevada, newmexico, northdakota, oregon, utah, wisconsin, wyoming)

# Clean dataframe
approved = approved %>%
  janitor::clean_names() %>%
  select(state, date, subject, title, description, type, bill_number)

# Export as LaTeX table
approved_latex = approved %>% 
  select(state, title) %>%
  rename(State = state, Title = title)

stargazer(approved_latex, 
          header=FALSE, 
          summary = FALSE,
          font.size = "scriptsize",
          column.sep.width = "0pt",
          title = 'All currently approved 2020 ballot initiatives by state',
          label = "tab: state_initiatives",
          out = "tables/state_initiatives_approved.tex")

################################################################################
# Potential ballot measures
################################################################################
# Define URL paths
ballot2020potential = read_html("https://ballotpedia.org/Potential_2020_ballot_measures")

# Extract potential ballot measures by state
alaska = ballot2020potential %>% html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(1) %>%
  mutate(state = "Alaska")

arizona = ballot2020potential %>% html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(2) %>%
  mutate(state = "Arizona")

arkansas = ballot2020potential %>% html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(3) %>%
  mutate(state = "Arkansas")

california = ballot2020potential %>% html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE) %>%
  extract2(3) %>%
  mutate(state = "California")










arizona = ballot2020potential %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "headerSort", " " ))] | //*[(@id = "Arizona")]') %>%
  html_nodes("table") %>%
  html_table(fill = TRUE, header = TRUE)

arizona = ballot2020potential %>% 
  html_nodes("#Arizona") %>%  
  html_nodes("table") %>% 
  html_table(fill = TRUE)

oregon = ballot2020potential %>% 
  html_nodes("#Oregon") %>%  
  html_nodes("table") %>% 
  html_table(fill = TRUE)









create_data <- function(container, p){

 # Read html,
 temp <- container[p] %>% read_html()
 donor_funds <- temp %>% html_nodes("#donor_fund") %>% html_text() }

pages <- c(0:99999)
prefix <- "https://amp-mei.net/aim/viewActivityPreview.do~public=true~pageId=2~activityId="
container_pages <- paste0(prefix, pages)

