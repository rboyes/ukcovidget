#' Queries the UK governments covid REST API and extracts case numbers by region.
#'
#' @param filters      API filters. See the API documentations for
#'                     additional information: https://coronavirus.data.gov.uk/developers-guide
#'
#' @param structure    Structure parameter. See the API documentations
#'                     for additional information: https://coronavirus.data.gov.uk/developers-guide
#'
#' @param endpoint     Endpoint URL for the UK government data REST API.
#'
#' @return data.frame  Comprehensive dataframe containing all
#'                     the data for the given ``filter`` and ``structure`.`
#'
#' @examples
#' query_filters <- c("areaType=ltla")
#' query_structure <- list(
#'   date       = "date",
#'   name       = "areaName",
#'   code       = "areaCode",
#'   daily      = "newCasesBySpecimenDate",
#'   cumulative = "cumCasesBySpecimenDate"
#' )
#' df_cases <- get_data(query_filters, query_structure)
#'
#' query_filters <- c("areaType=nation", "areaName=england", "date=2020-04-01")
#' query_structure <- list(cumulative = "cumCasesBySpecimenDate")
#' df_cases <- get_data(query_filters, query_structure)
get_data <- function (filters, structure, endpoint = "https://api.coronavirus.data.gov.uk/v1/data") {

  results      <- data.frame()
  current_page <- 1

  repeat {
    httr::GET(
      url   = endpoint,
      query = list(
        filters   = paste(filters, collapse = ";"),
        structure = jsonlite::toJSON(structure, auto_unbox = TRUE),
        page      = current_page
      ),
      httr::timeout(60)
    ) -> response

    # Handle errors:
    if ( response$status_code >= 400 ) {
      err_msg = httr::http_status(response)
      stop(err_msg)
    } else if ( response$status_code == 204 ) {
      break
    }

    # Convert response from binary to JSON:
    json_text <- httr::content(response, "text")
    dt        <- jsonlite::fromJSON(json_text)
    results   <- rbind(results, dt$data)

    if ( is.null( dt$pagination$`next` ) ){
      break
    }

    current_page <- current_page + 1;

  }

  return(results)

}
