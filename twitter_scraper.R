# Please make sure to install Python before executing this script

required_packages <- c("dplyr","jsonlite","stringr","tidyr", "lubridate", "ISOweek", "writexl", "svMisc", "rstudioapi")
for (packge in required_packages) {
        if (packge %in% rownames(installed.packages())) {
                require(packge, character.only = TRUE)
        } else {
                install.packages(packge)
                require(packge, character.only = TRUE)
        }
}

seq(as.Date("2014-04-01"), as.Date("2019-05-31"), by = "quarter") -> d1
d1[length(d1)+1] <- as.Date("2019-05-31")
# d2 <- d1 + 1

unlink("tweets_json", recursive = T) # deletes the tweets_json folder, if any
# Checks and creates the "tweets_json" folder
if(!dir.exists("tweets_json")){
        dir.create("tweets_json")
}

keywords <- c("micro_strategy", "powerbi", "tableau", "alteryx", "sap_analytics", "ibm_analytics", "sisense", "qlik", "oracle_analytics", "sas_software")
h <- c("MicroStrategy", "PowerBI", "Tableau", "Alteryx", "SAPAnalytics", "IBMAnalytics", "Sisense", "Qlik", "OracleAnalytics", "SASsoftware")
hashtags <- paste0("#", h)

termId <- terminalExecute("pip install twitterscraper")
while(terminalBusy(termId)){
        Sys.sleep(1)
}
terminalKill(termId)

for (j in seq_along(keywords)){
        for (i in seq_along(d1)[-length(d1)]) {
                from = d1[i]
                to = d1[i+1]
                if(!dir.exists(paste0("./tweets_json/", keywords[j]))){
                        dir.create(paste0("./tweets_json/", keywords[j]))
                }
                termId <- terminalExecute(paste0("twitterscraper ", hashtags[j], " -bd ", from, " -ed ", to, " -o ./tweets_json/", keywords[j], "/", keywords[j], "_tweets_",i,".json"))
                while(terminalBusy(termId)){
                        Sys.sleep(1)
                }
                terminalKill(termId)
        }
}

# Reads in the JSON files, converts them to dataframes and stores them in list "df_json"
df_json <- list()
for(i in seq_along(keywords)){
        tweet_files <- list.files(paste0("./tweets_json/", keywords[i]))
        df_file <- list()
        for (j in seq_along(tweet_files)){
                df_file[[j]] <- fromJSON(paste0("tweets_json/", keywords[i], "/", tweet_files[j]), flatten = T)
                
        }
        df_keyword <- do.call("rbind", df_file)
        df_keyword <- df_keyword[!duplicated(df_keyword),]
        df_json[[keywords[i]]] <- df_keyword %>% arrange(timestamp)
        svMisc::progress(i, length(keywords))
}

# Function that generates dates from week number
date_in_week <- function(year, week, weekday){
        w <- paste0(year, "-W", sprintf("%02d", week), "-", weekday)
        ISOweek2date(w)
}

# Function that gets all the count work done
count_weekly_tweets <- function(tweets_json = df_json){
        json_names <- names(tweets_json)
        df_list <- list()
        for(keyword in seq_along(tweets_json)){
                # Slice timestamp to date and time
                df <- tweets_json[[keyword]] %>%
                        separate(timestamp, c("date", "time"), "T")
                
                df$iso_week <- date2ISOweek(df$date)
                df$week <- df$iso_week %>% str_split_fixed(., "-",3)%>% .[,2] %>% gsub("W","",.) %>% as.numeric()
                df$year <- df$iso_week %>% str_split_fixed(., "-",3)%>% .[,1] %>% as.numeric()
                
                total_weekly_tweets <- c()
                week_date <- c()
                count = 0
                for(i in 2014:2019){
                        for(j in 1:53){
                                count = count + 1
                                total_weekly_tweets[count] = df %>% filter(week == j, year == i) %>% nrow()
                                week_start <- date_in_week(year = i, week = j, weekday = 1)
                                week_end <- date_in_week(year = i, week = j, weekday = 7)
                                week_date[count] <- strftime(week_start, format = "%d-%m-%y")
                        }
                }
                
                count_start <- which(total_weekly_tweets > 0)[1]
                count_end <- which(total_weekly_tweets > 0) %>% .[length(.)]
                total_duration <- count_start:count_end
                
                df_list[[json_names[keyword]]] <- cbind.data.frame(week = week_date[total_duration], number_of_tweets = total_weekly_tweets[total_duration])
                svMisc::progress(keyword, length(tweets_json))
        }
        return(df_list)
}

df_list <- count_weekly_tweets() # stores all the data into the list df_list

write_xlsx(df_list, "count_tweets.xlsx") # outputs the data into Excel


