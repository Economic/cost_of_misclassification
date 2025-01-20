
misclass_wb <- createWorkbook()

#Header and percent format
pct = createStyle(numFmt = '0.0%')
acct = createStyle(numFmt = '#,#0' )
curr = createStyle(numFmt = '$#,#0')

hs1 <- createStyle(fgFill = "#A31538", halign = "CENTER", textDecoration = "Bold",
                   border = "Bottom", fontColour = "white")

addWorksheet(misclass_wb, sheetName = "Cost to workers")
addWorksheet(misclass_wb, sheetName = "Cost to social insur")
addWorksheet(misclass_wb, sheetName = "Compensation profile")
addWorksheet(misclass_wb, sheetName = "ECEC methodology")

addWorksheet(misclass_wb, sheetName = "ECEC OEWS merge")
addWorksheet(misclass_wb, sheetName = "W2 and IC comparison")
addWorksheet(misclass_wb, sheetName = "ECEC OEWS industry xwalk")


writeData(misclass_wb, x=cost_to_social_insurance, headerStyle = hs1, sheet = "Cost to social insur", startCol = 1, startRow = 1, colNames = TRUE)
writeData(misclass_wb, x=cost_to_employees, headerStyle = hs1, sheet = "Cost to workers", startCol = 1, startRow = 1, colNames = TRUE)
writeData(misclass_wb, x=regional_industry_ecec, headerStyle = hs1, sheet = "Compensation profile", startCol = 1, startRow = 1, colNames = TRUE)
writeData(misclass_wb, x=ecec_methodology, headerStyle = hs1, sheet = "ECEC methodology", startCol = 1, startRow = 1, colNames = TRUE)

writeData(misclass_wb, x=ecec_oews_merge, headerStyle = hs1, sheet = "ECEC OEWS merge", startCol = 1, startRow = 1, colNames = TRUE)
writeData(misclass_wb, x=w2_ic_comparison, headerStyle = hs1, sheet = "W2 and IC comparison", startCol = 1, startRow = 1, colNames = TRUE)

# Include ECEC OEWS crosswalk key
export_xwalk_key <- industry_naics_crosswalk |> 
  select(`ECEC industry code` = industry_code, `ECEC industry title` = industry_title,
         `OEWS naics code` = naics, `OEWS naics title` = naics_title)

writeData(misclass_wb, x=export_xwalk_key, headerStyle = hs1, sheet = "ECEC OEWS industry xwalk", startCol = 1, startRow = 1, colNames = TRUE)


# Add notes and sources

writeData(misclass_wb, sheet="Cost to social insur", "Source: EPI analysis of data from the Bureau of Labor Statistics' Employer Cost for Employee Compensation (ECEC) 2024Q2 and Occupational Employment and Wage Statistics (OEWS) Research Estimates by State and Industry May 2023 data.", startCol=1, startRow=nrow(cost_to_social_insurance)+4)
writeData(misclass_wb, sheet="Cost to workers", "Source: EPI analysis of data from the Bureau of Labor Statistics' Employer Cost for Employee Compensation (ECEC) 2024Q2 and Occupational Employment and Wage Statistics (OEWS) Research Estimates by State and Industry May 2023 data.", startCol=1, startRow=nrow(cost_to_employees)+4)
writeData(misclass_wb, sheet="ECEC methodology", "Source: EPI analysis of data from the Bureau of Labor Statistics' Employer Cost for Employee Compensation (ECEC) 2024Q2 and Occupational Employment and Wage Statistics (OEWS) Research Estimates by State and Industry May 2023 data.", startCol=1, startRow=nrow(ecec_methodology)+3)
writeData(misclass_wb, sheet="ECEC OEWS merge", "Source: EPI analysis of data from the Bureau of Labor Statistics' Employer Cost for Employee Compensation (ECEC) 2024Q2 and Occupational Employment and Wage Statistics (OEWS) Research Estimates by State and Industry May 2023 data.", startCol=1, startRow=nrow(ecec_oews_merge)+4)

# Format numbers

addStyle(misclass_wb, "Cost to workers", style=curr, cols=c(3:8), rows=2:(nrow(cost_to_employees)+1), gridExpand=TRUE)
addStyle(misclass_wb, "Cost to workers", style=pct, cols=c(9:10), rows=2:(nrow(cost_to_employees)+1), gridExpand=TRUE)

addStyle(misclass_wb, "Cost to social insur", style=curr, cols=c(3:7), rows=2:(nrow(cost_to_social_insurance)+1), gridExpand=TRUE)
addStyle(misclass_wb, "Cost to social insur", style=pct, cols=c(8:9), rows=2:(nrow(cost_to_social_insurance)+1), gridExpand=TRUE)

addStyle(misclass_wb, "ECEC methodology", style=curr, cols=c(4, 7, 9), rows=2:(nrow(ecec_methodology)+1), gridExpand=TRUE)
addStyle(misclass_wb, "ECEC methodology", style=pct, cols=c(5, 8, 10, 11, 12, 13), rows=2:(nrow(ecec_methodology)+1), gridExpand=TRUE)

addStyle(misclass_wb, "Compensation profile", style=pct, cols=c(4), rows=2:(nrow(regional_industry_ecec)+1), gridExpand=TRUE)

addStyle(misclass_wb, "ECEC OEWS merge", style=curr, cols=c(6,7), rows=2:(nrow(ecec_oews_merge)+1), gridExpand=TRUE)
addStyle(misclass_wb, "ECEC OEWS merge", style=pct, cols=c(5), rows=2:(nrow(ecec_oews_merge)+1), gridExpand=TRUE)

addStyle(misclass_wb, "W2 and IC comparison", style=curr, cols=c(3:16), rows=2:(nrow(w2_ic_comparison)+1), gridExpand=TRUE)


saveWorkbook(misclass_wb, here(paste0("output/State misclassification estimates ", format(Sys.time(), "%d-%b-%Y %H.%M"), ".xlsx")), overwrite = TRUE)

