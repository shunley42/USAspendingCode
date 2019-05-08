#
#
#
# Creating API Functions for USAspending
#
#
#

######
# This function pulls federal obligations for a specific FY and funding agency
#####
federal_obligations_api <- function(year, id) {
  url <- paste0("https://api.usaspending.gov/api/v2/federal_obligations?fiscal_year=",
  year, 
  "&",
  "funding_agency_id=", id)
  
  resp <- GET(url)
  if (http_type(resp) != "application/json") {
    stop("API did not return json", call. = FALSE)
  }
  
  parsed <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
  
  if (http_error(resp)) {
    stop(
      sprintf(
        "API request failed [%s]\n%s\n<%s>", 
        status_code(resp),
        parsed$message,
        parsed$documentation_url
      ),
      call. = FALSE
    )
  }
  
  structure(
    list(
      content = parsed,
      path = url,
      response = resp
    )
  )
}

# Example pull
pull <- federal_obligations_api(year = "2018", id = "1068")


