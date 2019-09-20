#' Get Weather Data
#' 
#' @param stationid GHCN station ID
#' 
#' @import stringr readr dplyr tidyr
#' @importFrom curl curl_fetch_memory
#' @importFrom lubridate ymd
#' 
#' @export

get_weather = function(stationid) {
  col_names = c("ID","Year","Month","Element",str_c(c("Value","MFlag","QFlag","SFlag"),rep(1:31,each = 4)))
  url = str_glue("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/{stationid}.dly")
  response = curl::curl_fetch_memory(url)
  status = response[["status_code"]]
  if(status != 226) stop(status)
  content = response[["content"]]
  data = content %>%
    read_fwf(col_positions = fwf_widths(c(11,4,2,4,rep(c(5,1,1,1),31)),col_names),
             col_types = cols(.default = "c")) %>%
    filter(Element %in% c("TMAX","TMIN","PRCP","SNOW")) %>%
    gather(Key,Value,-ID,-Year,-Month,-Element) %>%
    mutate(Key = str_replace(Key,"(?<=[a-z])(?=[0-9])"," ")) %>%
    separate(Key,c("Key","Day"),sep = " ") %>%
    spread(Key,Value) %>%
    drop_na(SFlag) %>%
    transmute(StationID = ID,
              Date = ymd(str_c(Year,Month,Day,sep = "-")),
              Element,
              Value = as.numeric(Value)) %>%
    spread(Element,Value) %>%
    mutate(TMAX = round((TMAX/10)*(9/5) + 32),
           TMIN = round((TMIN/10)*(9/5) + 32),
           PRCP = (PRCP/100)/2.54,
           SNOW = (SNOW/10)/2.54) %>%
    mutate_at(vars(TMAX,TMIN),as.integer) %>%
    select(StationID,Date,TMAX,TMIN,PRCP,SNOW)
  return(data)
}