library(dplyr)
library(stringr)
library(httr)
library(jsonlite)

# Setup --------------------------------------------------------------------
token <- # your token here
version <- # your API version here
act <- # your creation act here

# Put the first part of the URL together
Credentials <- paste0('https://graph.facebook.com/',version,'/act_',act,'/delivery_estimate?access_token=',token,'&include_headers=false&method=get&optimization_goal=REACH&pretty=0&suppress_http_code=1')

# DEMOGRAPHIC SPECIFICATIONS ----------------------------------------------

# Age groups
age_groups <- tibble(
  age1 = c(13, seq(15, 65, by = 10)), # starts of age groups
  age2 = c(seq(14, 65, by = 10), 65) # ends of age groups
)

# Names of provinces and territories
provinces <- list(
  "Alberta", 
  "British Columbia",
  "Manitoba",
  "New Brunswick",
  "Newfoundland",
  "Northwest Territories", 
  "Nova Scotia", 
  "Nunavut",  
  "Ontario", 
  "Prince Edward Island",
  "Quebec",
  "Saskatchewan",
  "Yukon"
)

# MORE SETUP ---------------------------------------------------------

# Get the location IDs of provinces and territories from Facebook
id_df <- plyr::ldply(provinces, function(x) {
  GET(
    "https://graph.facebook.com/v4.0/search",
    query=list(
      location_types = "region",
      type = "adgeolocation",
      q = x, # use province name here
      access_token = token, 
      limit = 3
    )
  ) %>% 
    content(as="text", encoding = "UTF-8") %>% 
    fromJSON %>% 
    .[[1]]
})

# Get the list of origin countries
origin_df <- GET(
  "https://graph.facebook.com/v4.0/search",
  query=list(
    type='adTargetingCategory',
    class='behaviors',
    access_token=token,
    limit=300
  )) %>%
  content(as="text")%>%
  fromJSON %>%
  .[[1]] %>%
  filter(str_detect(name, "Lived in"))


# QUERY FOR EACH ORIGIN COUNTRY  ------------------------------------------------------

# Initialize data frame to store results
estimate_df <- data.frame()

for (g in 1:2) { # iterate over both genders
  
  for (i in 1:nrow(origin_df)) { # iterate over all origin countries
    
    # Construct the part of the query that specifies origin country 
    origin <- paste0(',"flexible_spec":[{"', origin_df[i, "type"],'":[{"id":"', origin_df[i, "id"],'","name":"', origin_df[i, "name"],'"}]}]')  
    
    for (j in 1:nrow(id_df)) { # iterate over provinces and territories
      
      region_id <- id_df[j, "key"]
      region_name <- id_df[j, "name"]
      
      # Do a separate query for each age group
      estimate_df <- rbind(estimate_df, plyr::adply(age_groups, 1, function(age_row) {
        
        # Construct the query
        query <- paste0(Credentials,'&
                  targeting_spec={"age_min":', age_row$age1,',
                  "age_max":',age_row$age2,',
                  "genders":[',g,']',
                  origin,',
                  "geo_locations":{"regions":[ {"key":"', region_id,' "}],"location_types":["home"]},
                  "facebook_positions":["feed","instant_article","instream_video","marketplace"],
                  "device_platforms":["mobile","desktop"],
                  "publisher_platforms":["facebook","messenger"],
                  "messenger_positions":["messenger_home"]}')
        
        # Remove line breaks and whitespace
        query <- str_replace_all(query, "[\n]", "") %>%
          str_replace_all(" ", "")
        
        # Perform the query
        url(query) %>% fromJSON %>% .$data %>% transmute(sex = g, region = region_name, from = origin_df[i, "name"], daily = estimate_dau, monthly = estimate_mau)
      })
      )
    }
    # Now query for all of Canada
    estimate_df <- rbind(estimate_df, plyr::adply(age_groups, 1, function(age_row) {
      query <- paste0(Credentials,'&
                  targeting_spec={"age_min":', age_row$age1,',
                  "age_max":',age_row$age2,',
                  "genders":[',g,']',
                  origin,',
                  "geo_locations":{"countries":["CA"],"location_types":["home"]},
                  "facebook_positions":["feed","instant_article","instream_video","marketplace"],
                  "device_platforms":["mobile","desktop"],
                  "publisher_platforms":["facebook","messenger"],
                  "messenger_positions":["messenger_home"]}')
      
      query <- str_replace_all(query, "[\n]", "") %>%
        str_replace_all(" ", "")
      
      url(query) %>% fromJSON %>% .$data %>% transmute(sex = g, region = "Canada", from = origin_df[i, "name"], daily = estimate_dau, monthly = estimate_mau)
    })
    )
    
  }
}

# Save to .csv file
readr::write_csv(estimate_df, path = str_c("data/raw/", Sys.time() %>% stringr::str_replace(" ", "_"), "_expat.csv"))

# QUERY FOR POPULATION -------------------------------
pop_df <- data.frame()
x
for (g in 1:2) {

  for (j in 1:nrow(id_df)) { # iterate over provinces and territories
    
    region_id <- id_df[j, "key"]
    region_name <- id_df[j, "name"]
    
    pop_df <- rbind(pop_df, plyr::adply(age_groups, 1, function(age_row) {
      query <- paste0(Credentials,'&
                    targeting_spec={"age_min":', age_row$age1,',
                    "age_max":',age_row$age2,',
                    "genders":[',g,'],
                    "geo_locations":{"regions":[ {"key":"', region_id,' "}],"location_types":["home"]},
                    "facebook_positions":["feed","instant_article","instream_video","marketplace"],
                    "device_platforms":["mobile","desktop"],
                    "publisher_platforms":["facebook","messenger"],
                    "messenger_positions":["messenger_home"]}')
      query <- str_replace_all(query, "[\n]", "") %>%
        str_replace_all(" ", "")
      
      url(query) %>% fromJSON %>% .$data %>% transmute(sex = g, region = region_name, daily = estimate_dau, monthly = estimate_mau)
    }))
  }
}

# Save to .csv file
readr::write_csv(pop_df, path = str_c("data/raw/", Sys.time() %>% stringr::str_replace(" ", "_"), "_pop.csv"))