# Script to join OEWS and ECEC

# Create a crosswalk between ECEC and OEWS using industry_code and naics variables
industry_naics_crosswalk <- tibble::tribble(
  ~industry_code, ~industry_title, ~naics,   ~naics_title,
  "230000",       "Construction",          "23",     "Construction",
  "412000",       "Retail Trade Industry", "44-45",  "Retail Trade",
  "430000",       "Transportation and Warehousing", "48-49", "Transportation and Warehousing",
  "560000",       "Administrative and Waste Services Industry", "56", "Administrative and Support and Waste Management and Remediation Services",
  "623000",       "Nursing and Residential Care Facilities Industry", "623000", "Nursing and Residential Care Facilities",
  "810000",       "Other Services Industry", "81", "Other Services (except Public Administration)"
)

# Join the crosswalk to ind_ecec_clean
ecec_industry_xwalk <- regional_industry_ecec |>
  left_join(industry_naics_crosswalk, by = c("industry_title"))  
  
oews_state_xwalk <- census_division_crosswalk |> 
  left_join(oews_clean, by = c('state'), relationship='many-to-many')

ecec_oews_merge <- ecec_industry_xwalk |> 
# Join oews_clean by naics and naics_title 
  left_join(oews_state_xwalk, by = c("naics", "naics_title", 'census_division'), relationship = 'many-to-many') |> 
  arrange(census_division, state, occ_title) |> 
  select(state, census_division, industry = naics_title, occupation=occ_title, compensation_component, ratio_of_pay_final, a_median) |> 
  mutate(share_of_pay = ratio_of_pay_final * a_median)
  
