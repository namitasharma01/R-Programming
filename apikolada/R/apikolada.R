#' Advanced Programming in R - Lab 5
#' Package to connect with the web API Kolada

#' @name api.kolada
#' @aliases api.kolada
#' @title Class to interface with web API Kolada
#' @description This class is implemented to interface with API Kolada and
#'     get KPI (Key Performance Indicator) data of different municipalities
#'     and organizational unis and plot some graphs for further analysis.
#'     Please refer the methods section of documentation for information
#'     on each method
#' @return Returns the data from Kolada API and few summary and plot functions
#' @examples
#' \dontrun{
#' obj = api.kolada()
#' obj$get.muni()
#' obj$get.ou.muni("Stockholm")
#' obj$get.kpi.member("Befolkning")
#' obj$get.muni.kpi("Befolkning", "Linköping", "Invånare 65-79 år, antal", "T")
#' obj$plot.muni.kpi(muni.kpi$period, muni.kpi$kpi, "Invånare 65-79 år, antal")
#' }
#' @importFrom methods new setRefClass
#' @export api.kolada


# Class for processing data from kolada API
api.kolada <- setRefClass(
  "api.kolada",

  fields = list(
    muni.all        = "data.frame",
    ou.all          = "data.frame",
    kpi.group.all   = "data.frame",
    muni.kpi        = "data.frame",
    muni.kpi.filter = "data.frame"
  ),


  methods = list(
    initialize = function() {
      muni.all        <<- data.frame()
      ou.all          <<- data.frame()
      kpi.group.all   <<- data.frame()
      muni.kpi        <<- data.frame()
      muni.kpi.filter <<- data.frame()
    },


    getdata.api = function(url) {
      "Get relevant data from API by passing the url"

      response    <- httr::GET(url = url)
      raw.content <- rawToChar(response$content)
      # Set encoding of the raw content to UTF-8 to preserve swedish characters in the data
      Encoding(raw.content) <- "UTF-8"
      # Return data as a dataframe
      content.df = (jsonlite::fromJSON(raw.content))$values
      return(content.df)
    },


    get.muni = function() {
      "Get all the municipality codes from API"

      muni.all <<- getdata.api("http://api.kolada.se/v2/municipality")
      return(muni.all)
    },


    get.muni.id = function(muni) {
      "Get the municipality code for the given municipality title"

      if (nrow(muni.all) == 0) {
        muni.all <<- get.muni()
      }
      muni.id  <- muni.all[which(tolower(muni.all$title) == tolower(muni)), "id"]
      return(muni.id)
    },


    get.ou.muni = function(muni) {
      "Get all the organizational units in a municipality from API"

      ou.all <<- getdata.api(paste0("http://api.kolada.se/v2/ou?municipality=", get.muni.id(muni)))
      return(ou.all)
    },


    get.ou.id = function(muni, ou) {
      "Get the organizational unit ID for a given organizational unit from API"

      if (nrow(ou.all) == 0) {
        ou.all <<- get.ou.muni(muni)
      }
      ou.id  <- ou.all[which(tolower(ou.all$title) == tolower(ou)), "id"]
      return(ou.id)
    },


    get.kpi.group = function() {
      "Get all KPI groups from API"

      kpi.group.all <<- getdata.api("http://api.kolada.se/v2/kpi_groups")
      return(kpi.group.all)
    },


    get.kpi.member = function(kpigroup) {
      "Get the KPI indicators within the selected KPI group"

      if (nrow(kpi.group.all) == 0) {
        kpi.group.all   <<- get.kpi.group()
      }
      kpi.member.list <- kpi.group.all[which(tolower(kpi.group.all$title) == tolower(kpigroup)), "members"]
      kpi.members     <- do.call(what = rbind, args = kpi.member.list)
      return(unique(kpi.members))
    },


    get.kpi.id = function(kpigroup, kpi) {
      "Get the KPI ID for a given KPI title from API"

      kpi.members <- get.kpi.member(kpigroup)
      kpi.id      <- kpi.members[which(tolower(kpi.members$member_title) == tolower(kpi)), "member_id"]
      return(kpi.id)
    },


    get.muni.kpi = function(kpigroup, muni, kpi, gender) {
      "Get KPI data for a municipality based on the chosen KPI indicator from API"

      tryCatch(
        {
          # Build a URL to get municipality data for KPI indicator chosen
          url <- paste0("http://api.kolada.se/v2/data/kpi/", get.kpi.id(kpigroup, kpi),
                        "/municipality/", get.muni.id(muni))

          # Get KPI data for a municipality based on the chosen KPI indicator from API
          muni.kpi.nested <- getdata.api(url)
          # Flatten out the nested dataframe having KPI values along with its period data
          i <- 1
          muni.kpi.flat <- list()
          for (x in muni.kpi.nested$values){
            muni.kpi.flat[[i]] <- cbind.data.frame(count  = x$count,
                                                   gender = x$gender,
                                                   value  = x$value,
                                                   period = muni.kpi.nested[i, "period"])
            i <- i + 1
          }
          muni.kpi   <<- do.call(what = rbind, args = muni.kpi.flat)
          muni.kpi.filter <<- muni.kpi[which(muni.kpi$gender == gender), ]
          return(muni.kpi.filter)
        },
        error = function(e) {
          stop("Sorry! Data not available for the requested input from API")
        })
    },


    plot.muni.kpi = function(period_val, kpi_val, kpi_label) {
      "Plot KPI values of the municipality over the years"

      tryCatch(
        {
          scatter.smooth(x    = period_val,
                         y    = kpi_val,
                         xlab = "Period (in years)",
                         ylab = kpi_label)
        },
        error = function(e) {
          stop("Not enough values to plot a graph!")
        })
    },

    summary.kpi = function(kpi_df) {
      "Summary statistics of municipality KPI values"
      return(summary(kpi_df))
    }
  )
)

# 1. flatten out nested df
# 2. try catch
