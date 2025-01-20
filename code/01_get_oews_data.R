# Clean inputs data



# Clean OEWS 2023 data
# link <- 'https://www.bls.gov/oes/special.requests/oes_research_2023_allsectors.xlsx'

# download.file(link, here('input/oes_research_2023_allsectors.xlsx'))

oews_raw <- read_excel('input/oes_research_2023_allsectors.xlsx')


# Revised version 1/16/2025
oews_clean <- oews_raw |> 
  clean_names() |> 
  select(state=area_title, naics, naics_title, occ_code, occ_title,
         tot_emp, h_mean, a_mean, h_median, a_median) |> 
  filter(
    # Construction laborers in Construction Industry (NAICS 23)
    (naics == "23" & occ_code == "47-2061") |
      
      # Heavy and Tractor-Trailer Truck Drivers in Transportation and Warehousing (NAICS 48-49)
      (naics == "48-49" & occ_code == "53-3032") |
      
      # Light Truck Drivers in Transportation and Warehousing (NAICS 48-49)
      (naics == "48-49" & occ_code == "53-3033") |
      
      # Janitors, Maids, Landscaping, Customer Service, Security in Admin & Support, Waste Mgmt & Remediation (NAICS 56)
      (naics == "56" & occ_code == "37-2011") |
      (naics == "56" & occ_code == "37-2012") |
      (naics == "56" & occ_code == "37-3011") |
      (naics == "56" & occ_code == "43-4051") |
      (naics == "56" & occ_code == "33-9032") |
      
      # Manicurists and Pedicurists in Other Services (except Public Administration) (NAICS 81)
      (naics == "81" & occ_code == "39-5092") |
      
      # Retail Salespersons in Retail Trade (NAICS 44-45)
      (naics == "44-45" & occ_code == "41-2031") |
      
      # Home Health and Personal Care Aides in Nursing and Residential Care Facilities (NAICS 623000)
      (naics == "623000" & occ_code == "31-1120")
  ) |> 
  mutate(a_median = as.numeric(a_median))

# Construction: 23
# Transportation and warehouse: 48-49
# Administrative and Support and Waste Management and Remediation Services 56
# Other Services (except Public Administration) 81
# Retail trade 44-45
# Nursing and Residential Care Facilities 623000


# Define Census division xwalk
# Source: https://www2.census.gov/geo/docs/maps-data/maps/reg_div.txt
census_division_crosswalk <- tibble::tribble(
  ~census_division, ~state,
  # New England
  "New England", "Connecticut",
  "New England", "Maine",
  "New England", "Massachusetts",
  "New England", "New Hampshire",
  "New England", "Rhode Island",
  "New England", "Vermont",
  # Middle Atlantic
  "Middle Atlantic", "New Jersey",
  "Middle Atlantic", "New York",
  "Middle Atlantic", "Pennsylvania",
  # East North Central
  "East North Central", "Illinois",
  "East North Central", "Indiana",
  "East North Central", "Michigan",
  "East North Central", "Ohio",
  "East North Central", "Wisconsin",
  # West North Central
  "West North Central", "Iowa",
  "West North Central", "Kansas",
  "West North Central", "Minnesota",
  "West North Central", "Missouri",
  "West North Central", "Nebraska",
  "West North Central", "North Dakota",
  "West North Central", "South Dakota",
  # South Atlantic
  "South Atlantic", "Delaware",
  "South Atlantic", "Florida",
  "South Atlantic", "Georgia",
  "South Atlantic", "Maryland",
  "South Atlantic", "North Carolina",
  "South Atlantic", "South Carolina",
  "South Atlantic", "Virginia",
  "South Atlantic", "West Virginia",
  "South Atlantic", "District of Columbia",
  # East South Central
  "East South Central", "Alabama",
  "East South Central", "Kentucky",
  "East South Central", "Mississippi",
  "East South Central", "Tennessee",
  # West South Central
  "West South Central", "Arkansas",
  "West South Central", "Louisiana",
  "West South Central", "Oklahoma",
  "West South Central", "Texas",
  # Mountain
  "Mountain", "Arizona",
  "Mountain", "Colorado",
  "Mountain", "Idaho",
  "Mountain", "Montana",
  "Mountain", "Nevada",
  "Mountain", "New Mexico",
  "Mountain", "Utah",
  "Mountain", "Wyoming",
  # Pacific
  "Pacific", "Alaska",
  "Pacific", "California",
  "Pacific", "Hawaii",
  "Pacific", "Oregon",
  "Pacific", "Washington"
)

