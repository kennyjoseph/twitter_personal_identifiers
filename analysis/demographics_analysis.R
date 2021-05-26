source("util.R")

panel_1 <- get_all_info("panel_7_7_20")

##############
# Load in all panel members
panel_info <- panel_1[['panel']]

#############
# Load in demographic data and subset to folks with a description
panel_dem <- fread("demog_v4.tsv")
setnames(panel_dem, c("userid", "num_tweets", "age", "gender", "race", 
                      "party", "state", "cd", "zip", "county_fips", "partisan_score",
                      "registration_status", "first_tweet", "last_tweet", "statuses_count",
                      "favourites_count", "followers_count", "friends_count", "user_created_at", "verified"))
panel_dem <- panel_dem[userid %in% panel_info$uid]
# Link to census data for urban/rural comparison
census <- fread("https://raw.githubusercontent.com/MEDSL/2018-elections-unoffical/master/election-context-2018.csv")
census <- census[, .(fips,rural_pct,trump16,clinton16,white_pct,black_pct,hispanic_pct)]
census[, census_fips := as.character(fips)]
st <- unique(data.table(state.fips)[,.(abb,fips)])
setnames(st, c("state","state_fips"))
panel_dem <- merge(panel_dem, st,by="state",all.x=T)
panel_dem[, census_fips := paste0(state_fips,str_pad(panel_dem$county_fips,3,side="left", pad="0"))]
panel_dem <- merge(panel_dem, census, by="census_fips",all.x=T)
# summary stats on demographics
paste0("Gender & ",
       1-nrow(panel_dem[gender %in% c("Male","Female")])/nrow(panel_dem)," & ",
       nrow(panel_dem[gender %in% c("Female")])/nrow(panel_dem[gender %in% c("Male","Female")]), " \\")

paste0("Pol & ",
       1-nrow(panel_dem[party %in% c("Democrat","Republican")])/nrow(panel_dem)," & ",
       nrow(panel_dem[party %in% c("Democrat")])/nrow(panel_dem[party %in% c("Democrat","Republican")]), " \\")

paste0("Race & ",
       1-nrow(panel_dem[race %in% c("Asian","Caucasian","African-American","Hispanic")])/nrow(panel_dem)," & ",
       nrow(panel_dem[race %in% c("Asian")])/nrow(panel_dem[race %in% c("Asian","Caucasian","African-American","Hispanic")])," & ",
       nrow(panel_dem[race %in% c("Caucasian")])/nrow(panel_dem[race %in% c("Asian","Caucasian","African-American","Hispanic")])," & ",
       nrow(panel_dem[race %in% c("African-American")])/nrow(panel_dem[race %in% c("Asian","Caucasian","African-American","Hispanic")])," & ",
       nrow(panel_dem[race %in% c("Hispanic")])/nrow(panel_dem[race %in% c("Asian","Caucasian","African-American","Hispanic")])," & ",
       " \\")

paste0("Age & ",
       1-nrow(panel_dem[!is.na(age)])/nrow(panel_dem)," & ",
       mean(panel_dem[!is.na(age)]$age), " & ",
       median(panel_dem[!is.na(age)]$age),
       " \\")

paste0("Rural PC & ",
       1-nrow(panel_dem[!is.na(rural_pct)])/nrow(panel_dem)," & ",
       mean(panel_dem[!is.na(rural_pct)]$rural_pct), " & ",
       median(panel_dem[!is.na(rural_pct)]$rural_pct),
       " \\")


######### Demographics ############
library(lubridate)

mg <- merge(panel_dem, panel_1[['output']], by.x="userid", by.y="uid")
mg[, createdat_date := as.POSIXct(user_created_at, format="%a %b %d %H:%M:%S +0000 %Y", tz="GMT")]
mg[, account_age :=as.integer((mg$createdat_date-min(mg$createdat_date))/(60*60*24))]
mg <- merge(mg, panel_1[['output']][,.N, by=term], by="term")

# Merge in discrete columns 
sp_socdem <- merge( spread(mg[party %in% c("Democrat","Republican"), 
                              .N, by=.(party,term)], party, N, fill = 0),
                    spread(mg[gender %in% c("Male","Female"), 
                              .N, by=.(gender,term)], gender, N, fill = 0),
                    by="term")
sp_socdem <- merge(sp_socdem, spread(mg[race %in% c("Caucasian","African-American","Hispanic", "Asian"),
                                        .N, by=.(race,term)], race, N, fill = 0),
                   by="term")
sp_socdem <- merge(sp_socdem, spread(mg[,.N, by=.(verified,term)], verified, N, fill = 0),
                   by="term")
setnames(sp_socdem, c("FALSE","TRUE"), c("not_verified","verified"))
sp_socdem <- merge(sp_socdem, df[,.N, by=term], by="term")

# Compute raw and normalized log-odds values
sp_socdem[, lo_republican := compute_logodds(Republican,Democrat,1,1)$v]
sp_socdem[, lo_republican_d := compute_logodds(Republican,Democrat,1,1)$delta]
sp_socdem[, lo_female := compute_logodds(Female,Male,1,1)$v]
sp_socdem[, lo_female_d := compute_logodds(Female,Male,1,1)$delta]
sp_socdem[, lo_white := compute_logodds(Caucasian,`African-American`+Asian+Hispanic,1,1)$v]
sp_socdem[, lo_white_d := compute_logodds(Caucasian,`African-American`+Asian+Hispanic,1,1)$delta]
sp_socdem[, lo_black := compute_logodds(`African-American`,Caucasian,1,1)$v]
sp_socdem[, lo_black_d := compute_logodds(`African-American`,Caucasian,1,1)$delta]
sp_socdem[, lo_asian := compute_logodds(Asian,Caucasian,1,1)$v]
sp_socdem[, lo_asian_d := compute_logodds(Asian,Caucasian,1,1)$delta]
sp_socdem[, lo_hispanic := compute_logodds(Hispanic,Caucasian,1,1)$v]
sp_socdem[, lo_hispanic_d := compute_logodds(Hispanic,Caucasian,1,1)$delta]

sp_socdem[, lo_verified := compute_logodds(verified,not_verified,1,1)$v]
sp_socdem[, lo_verified_d := compute_logodds(verified,not_verified,1,1)$delta]

# Compute continuous, remonving things with < MIN_N observations
MIN_N = 10

sp_socdem <- merge(sp_socdem, mg[!is.na(age) & N > MIN_N, 
                                 list(mean_age=mean(age), 
                                      mean_age_d=mean(age)), 
                                 by=term],
                   all.x=T)


sp_socdem <- merge(sp_socdem, mg[!is.na(rural_pct) & N > MIN_N, 
                                 list(mean_rural=mean(rural_pct),
                                      mean_rural_d=mean(rural_pct)), by=term],
                   all.x=T)

sp_socdem <- merge(sp_socdem, mg[!is.na(account_age) & N > MIN_N, 
                                 list(mean_account_age=mean(account_age), 
                                      mean_account_age_d=mean(account_age)), by=term],
                   all.x=T)
 
sp_socdem <- merge(sp_socdem, mg[!is.na(followers_count) & N > MIN_N, 
                                 list(median_followerscount=median(followers_count), 
                                      median_followerscount_d=median(followers_count)), 
                                  by=term],
                   all.x=T)
 
sp_socdem <- merge(sp_socdem, mg[!is.na(friends_count), 
                                 list(median_friendscount=median(friends_count), 
                                      median_friendscount_d=median(followers_count)), 
                                  by=term],
                   all.x=T)

sp_socdem <- merge(sp_socdem, mg[!is.na(friends_count) & !is.na(followers_count) & N > MIN_N, 
                                 list(mean_folfriendratio=mean(log((followers_count+1)/(1+friends_count))), 
                                      mean_folfriendratio_d=mean(log((followers_count+1)/(1+friends_count)))),
                                 by=term],
                   all.x=T)


vars_comp_socdem <- c("lo_republican",
                      "lo_female",
                      "lo_white",
                      "lo_black",
                      "lo_asian",
                      "lo_hispanic",
                      "lo_verified",
                      "mean_folfriendratio",
                      "mean_rural",
                      "mean_age")


d <- data.table()
N_SHOWN <- 10
for(v in vars_comp_socdem){
  z <- rbind(sp_socdem[!is.na(get(v))][order(get(v))][1:N_SHOWN ][,c("term",paste0(v,"_d")),with=F],
             sp_socdem[!is.na(get(v))][order(-get(v))][1:N_SHOWN ][,c("term",paste0(v,"_d")),with=F])
  z$t <- v
  z$k <- "bottom"
  z[1:N_SHOWN ]$k <- "top"
  
  #z[1:10]$term <- paste0("<div style='color:orange' align='left'>",z[1:10]$term,"</div>")
  #z[11:20]$term <- paste0("<p style='color:#29BF12'>",z[11:20]$term,"</p>")
  setnames(z,paste0(v,"_d"),"value")
  d <- rbind(d,z)
}

d$label <- factor(d$t, levels=c("lo_female","lo_republican","mean_age","mean_rural",
                                "lo_asian","lo_black","lo_hispanic","lo_white",
                                "lo_verified",
                                "mean_folfriendratio"
                                ),
                  labels=c("<strong style='color:#FFBF00'>Male </strong> - <strong style='color:#29BF12'>Female</strong>",
                           "<strong style='color:#FFBF00'>Dem.</strong> - <strong style='color:#29BF12'>Rep.</strong>",
                           "<strong style='color:#FFBF00'>Young</strong> - <strong style='color:#29BF12'>Old</strong>",
                           "<strong style='color:#FFBF00'>Urban</strong> - <strong style='color:#29BF12'>Rural</strong>",
                           "<strong style='color:#FFBF00'>White</strong> - <strong style='color:#29BF12'>Asian</strong>",
                           "<strong style='color:#FFBF00'>White</strong> - <strong style='color:#29BF12'>Black</strong>",
                           "<strong style='color:#FFBF00'>White</strong> - <strong style='color:#29BF12'>Hispanic</strong>",
                           "<strong style='color:#FFBF00'>(Not)</strong> <strong style='color:#29BF12'>White</strong>",
                           "<strong style='color:#FFBF00'>(Not)</strong> <strong style='color:#29BF12'>Verified</strong>",
                           "<strong style='color:#FFBF00'>Low</strong> - <strong style='color:#29BF12'>High</strong> Status"))

p <- ggplot(d[!is.na(label)], aes(value,reorder_within(term,value,t),value,fill=k)) + 
  geom_col() + 
  scale_y_reordered() + 
  facet_wrap(~label, scales="free",nrow=2) + 
  scale_fill_manual(values=c("#29BF12","#FFBF00")) +
  theme_classic(10) + 
  theme(strip.background = element_blank(),
        strip.text=element_textbox(size=13),
        legend.position="none") +
  xlab("") + ylab("")
p
ggsave("img/p1.pdf",p,h=6,w=13)


mg[term %in% retain_for_age_etc, mean(log((followers_count+1)/(friends_count+1))),by=term][order(-V1)][1:20]


#########################


