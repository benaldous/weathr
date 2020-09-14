#' Get Weather Data
#' 
#' @param stationid GHCN station ID
#' 
#' @import stringr readr dplyr tidyr
#' @importFrom curl curl_fetch_memory
#' @importFrom lubridate ymd
#' 
#' @export

get_daily = function(stationid) {
  col_names = c("ID","Year","Month","Element",str_c(c("Value","MFlag","QFlag","SFlag"),rep(1:31,each = 4)))
  url = str_glue("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/all/{stationid}.dly")
  response = curl_fetch_memory(url)
  status = response[["status_code"]]
  if(status != 226) stop(status)
  content = response[["content"]]
  data = content %>%
    read_fwf(col_positions = fwf_widths(c(11,4,2,4,rep(c(5,1,1,1),31)),col_names),
             col_types = cols(.default = "c")) %>%
    filter(Element %in% c("TMAX","TMIN","PRCP","SNOW")) %>%
    pivot_longer(c(-ID,-Year,-Month,-Element)) %>%
    separate(name,c("Key","Day"),sep = "(?<=[a-z])(?=[0-9])") %>%
    pivot_wider(names_from = Key,values_from = value) %>%
    drop_na(SFlag) %>%
    mutate_at(vars(Value),as.integer) %>%
    mutate(Value = case_when(Element %in% c("TMAX","TMIN") ~ round((Value/10)*(9/5) + 32),
                             Element == "PRCP" ~ (Value/100)/2.54,
                             Element == "SNOW" ~ (Value/10)/2.54)) %>%
    mutate(Date = str_c(Year,Month,Day,sep = "-") %>% ymd()) %>%
    select(Date,Element,Value) %>%
    pivot_wider(names_from = Element,values_from = Value) %>%
    mutate_at(vars(matches("TMAX"),matches("TMIN")),as.integer)
  return(data)
}

#' @describeIn get_daily
#' 
#' @param stationid USAF-WBAN station ID
#' @param year Year
#' 
#' @importFrom curl curl_fetch_disk
#' @importFrom lubridate ymd_hm
#' 
#' @export

get_hourly = function(stationid,year) {
  url = str_glue("ftp://ftp.ncei.noaa.gov/pub/data/noaa/{year}/{stationid}-{year}.gz")
  file = curl_fetch_disk(url,tempfile())$content
  hourly = gzfile(file) %>%
    read.fwf(c(15,12,60,5,1,5,1)) %>%
    as_tibble() %>%
    filter(V4 != 9999,
           V6 != 9999) %>%
    transmute(Timestamp = ymd_hm(V2),
              Temperature = V4/10*9/5 + 32,
              Dewpoint = V6/10*9/5 + 32)
  unlink(file)
  return(hourly)
}
