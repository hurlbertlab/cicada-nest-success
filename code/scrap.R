# I only have county code, I want county name

county_codes <- read.csv("https://www2.census.gov/geo/docs/reference/codes2020/national_county2020.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE, strip.white = TRUE) #county df from US Census)

#State FIPS and County FIPS are separate columns, need to merge to match cicada_county 

county_codes$STATEFP <- sprintf("%02d", as.integer(county_codes$STATEFP)) #make sure its 2 digits
county_codes$COUNTYFP <- sprintf("%03d", as.integer(county_codes$COUNTYFP)) #make sure its 3 digits
county_codes$ST_CNTY_CODE <- paste0(county_codes$STATEFP, county_codes$COUNTYFP) #combine

# Now merge to create cicada county with both code and name
cicada_county <- left_join(cicada_county, county_codes, by = "ST_CNTY_CODE")
cicada_county <- subset(cicada_county, select = -c(STATE, STATEFP, COUNTYFP, COUNTYNS, CLASSFP, FUNCSTAT))


