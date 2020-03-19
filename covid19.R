
# library imports ----------------------------------------------------------

sapply( c("magrittr","dplyr","readr","reshape2","skimr","ggplot2"),
        function(x){
          suppressPackageStartupMessages(library(x, character.only = TRUE))
          x
        },USE.NAMES = FALSE)

# setting up repository and/or updating -----------------------------------

setwd("/work/git")
if(!dir.exists("COVID-19")){
  system("git clone https://github.com/CSSEGISandData/COVID-19.git")
}
system("cd COVID-19 && git pull ")


# data import -------------------------------------------------------------

data <- list.files("COVID-19/csse_covid_19_data/csse_covid_19_time_series",full.names = TRUE) %>%
  grep("csv$", x = ., value = TRUE) %>% lapply(function(x){
    read_csv(x) %>% 
      melt(data = . , id.vars = colnames(.)[1:4]) %>%
      transmute(State = `Province/State`,
                Country = `Country/Region`,
                Lat , Long,
                Date = as.Date(variable, "%m/%d/%y"),
                Status = gsub(".*-|.csv","",x),
                Count = value)
  }) %>% Reduce(f = rbind, x = .) %>%
  skim_tee()

# function definitions ----------------------------------------------------

plot_country_total <- function(country, scale = NA, skim = TRUE){
  if(is.na(scale)){
    scale <- ifelse(max(data$Count[data$Country == country]) < 1000,"linear","log")
  }
  if(skim){
    data %>% 
      filter(Country == country) %>% skim() %>% print()
  }
  plot <- data %>% 
    filter(Country == country) %>% group_by(Status, Date) %>% summarise(Count = sum(Count)) %>%
    ggplot(aes(x= Date , y = Count , color = Status )) + 
    geom_path() +
    theme_bw() +
    scale_x_date(date_breaks = "2 days" , date_labels = "%d/%m/'%y")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(country, subtitle = "Total Cases")
  if(scale == "log"){
    return(plot + scale_y_log10())
  }else{
    return(plot + scale_y_continuous(breaks = seq(0, max(data$Count[data$Country == country])+10,20)))
  }
}

plot_country_new <- function(country, scale = NA, skim = TRUE){
  if(is.na(scale)){
    scale <- ifelse(max(data$Count[data$Country == country]) < 1000,"linear","log")
  }
  if(skim){
    data %>% 
      filter(Country == country) %>% skim() %>% print()
  }
  plot <-  data %>% 
    filter(Country == country) %>%
    group_by(Status, Date) %>% 
    summarise(Count = sum(Count)) %>%
    group_by(Status) %>% 
    mutate(new_cases = Count - lag(Count, default = 0, order_by = Date)) %>%
    ggplot(aes(x= Date , y = new_cases , color = Status )) + 
    geom_path() +
    theme_bw() +
    scale_x_date(date_breaks = "2 days" , date_labels = "%d/%m/'%y")+
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle(country, subtitle = "New Cases")
  if(scale == "log"){
    return(plot + scale_y_log10())
  }else{
    return(plot + scale_y_continuous(breaks = seq(0, max(data$Count[data$Country == country])+10,20)))
  }
}


# testing  ----------------------------------------------------------------


unique(data$Country) %>% sort() %>% print()

plot_country_new("Oman")

plot_country_total("India")

