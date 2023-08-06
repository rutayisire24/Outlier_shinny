

pacman::p_load(rio ,      #for importing data
               here ,      #(for relative filepaths)
               janitor,  #(for cleaning data)
               lubridate,  #(for date cleaning)
               epikit,     #(for creating age categories)
               tidyverse,   #(for data management and visualization)
               httr,
               jsonlite,
               readxl,
               janitor
)


## --- Function to pull specific Files

fetch_data_hmis <- function(url) {
  data <- url %>%
    GET(authenticate(username, password)) %>%
    content("text") %>%
    fromJSON(flatten = T) %>%
    as.data.frame()
  # read_csv(show_col_types = FALSE)
  
  return(data)
}


#-- get credentials from Renviron
username <- "moh-comm-m_e.rmeddie"
password <- "Basketball@24"


# HMIS Org Units
# ===============================================================================
# Read HMIS Org unit hierarchy
hmis_ou_long_link <- "https://hmis.health.go.ug/api/organisationUnits.csv?paging=false" # reads a long format of OUs
hmis_ou_long <- hmis_ou_long_link %>%
  GET(authenticate(username, password)) %>%
  content("text") %>%
  read_csv(show_col_types = FALSE)

# reads a tabular format for showing which sites belong to what ou levels
hmis_ou_link <- "https://hmis.health.go.ug/api/organisationUnits.csv?filter=level:eq:5&paging=false&fields=id,level,name,path,coordinates%5bid,name,parent%5d"

HMIS_Org_Units <- hmis_ou_link %>%
  GET(authenticate(username, password)) %>%
  content("text") %>%
  read_csv(show_col_types = FALSE) %>%
  separate(path,
           into = c(
             "empty", "country_uid", "region_uid",
             "district_uid", "subcounty_uid", "facility_uid"
           ),
           sep = "/"
  ) %>%
  left_join(hmis_ou_long, by = c("region_uid" = "id")) %>%
  left_join(hmis_ou_long, by = c("district_uid" = "id")) %>%
  left_join(hmis_ou_long, by = c("subcounty_uid" = "id")) %>%
  rename(
    "region" = displayName.x,
    "district" = displayName.y,
    "subcounty" = displayName
  )

# HMIS Data Elements
hmis_de_Link <- "https://hmis.health.go.ug/api/dataElements.csv?paging=false"

hmis_de <- hmis_de_Link %>%
  GET(authenticate(username, password)) %>%
  content("text") %>%
  read_csv(show_col_types = FALSE)

# HMIS Data Elements
hmis_ds_Link <- "https://hmis.health.go.ug/api/dataSets.csv?paging=false"

hmis_ds <- hmis_ds_Link %>%
  GET(authenticate(username, password)) %>%
  content("text") %>%
  read_csv(show_col_types = FALSE)

hmis_di_Link <- "https://hmis.health.go.ug/api/indicators.csv?paging=false"

hmis_di <- hmis_di_Link %>%
  GET(authenticate(username, password)) %>%
  content("text") %>%
  read_csv(show_col_types = FALSE)

hmis_coc_link <- "https://hmis.health.go.ug/api/categoryOptionCombos.csv?paging=false"

hmis_coc <- hmis_coc_link %>%
  GET(authenticate(username, password)) %>%
  content("text") %>%
  read_csv(show_col_types = FALSE)


# Data Pull 

facility_link <- "https://hmis.health.go.ug/api/analytics/dataValueSet.json?dimension=dx%3AD9A0afrTYPw%3BllcQzAlLAWc%3BhCRvkCwYdvt%3BDM0iRlEZHKC%3BhCED8FhuM7S%3BWIJ0fbjLIJh%3BmeW1T5n8XV0%3BwPHNUK3jrFJ%3BCGWRNOSVEsk&dimension=pe%3A202201%3B202202%3B202203%3B202204%3B202205%3B202206%3B202207%3B202208%3B202209%3B202210%3B202211%3B202212%3B202301%3B202302%3B202303%3B202304%3B202305%3B202306&dimension=ou%3AakV6429SUqu%3BLEVEL-Sg9YZ6o7bCQ&showHierarchy=false&hierarchyMeta=false&includeMetadataDetails=true&includeNumDen=true&skipRounding=false&completedOnly=false"

facility_data <- fetch_data_hmis(facility_link)



data_i <- facility_data %>%
  dplyr::select(dataValues.dataElement, dataValues.period, dataValues.orgUnit,dataValues.value) %>%
  left_join(hmis_de, by = c("dataValues.dataElement" = "id")) %>%
  left_join(HMIS_Org_Units, by = c("dataValues.orgUnit" = "facility_uid")) %>%
  # left_join(hmis_coc, by = c("dataValues.categoryOptionCombo" = "id")) %>%
  clean_names() %>%
  select(c(display_name,name,data_values_period,region,district,data_values_value))

names(data_i) <- c("variable","facility","periodname","region","district","values")
saveRDS(data_i,"impute_i.rds")


