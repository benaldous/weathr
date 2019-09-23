#' Get List of Stations
#' 
#' @import readr dplyr stringr
#' @importFrom curl curl_fetch_memory
#' 
#' @export

get_stations = function(wmo = TRUE) {
  col_names = c("ID","Lat","Long","Elev","State","Name","GSNFlag","HCNFlag","WMOID")
  url = "ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/ghcnd-stations.txt"
  response = curl_fetch_memory(url)
  status = response[["status_code"]]
  if(status != 226) stop(status)
  content = response[["content"]]
  data = content %>%
    read_fwf(col_positions = fwf_widths(c(12,9,9,7,3,31,4,4,5),col_names),
             col_types = "cdddcccci") %>%
    mutate_at(vars(ID,State:HCNFlag),str_squish) %>%
    mutate(Elev = na_if(Elev,-999.9)*3.281,
           GSNFlag = case_when(GSNFlag == "GSN" ~ TRUE,
                               is.na(GSNFlag) ~ FALSE))
  if(wmo) data = filter(data,! is.na(WMOID))
  return(data)
}
