source("util.R")
library(readxl)
theme_set(theme_bw(20))

# annotation results
ANNOTATOR_FILE <- "../annotated_data/labels_identity.xlsx"

#######################
###### Initial annotation - evaluating term count + number of times observed
########################

# FILE FOR ANNOTATED DATA TASKS 1 and 2
d <- fread("../annotated_data//wordidf.csv", encoding = "UTF-8")

d[, v := ifelse( n == 1, "1",
                 ifelse(n == 2, "2", 
                        ifelse(n < 6, "3-5", 
                               ifelse(n < 11, "5-10", 
                                      ifelse(n < 26, "10-25", 
                                             ifelse(n < 101, "25-100", "100+"))))))]
d[, v := factor(v,
                levels= c("1","2", "3-5", "5-10","10-25", "25-100","100+"))
]
d[, nt := ifelse(n_token < 4, as.character(n_token), "4+")]
d[, nt := factor(nt, c("1","2","3","4+"))]
p1 <- ggplot(d[,.N, by=.(v,nt)], aes(v,nt,fill=log(N),label=N))+
  geom_tile(alpha=.7) +
  scale_fill_gradientn(colors=c("blue","white","red"))+geom_text() +
  ylab("N Tokens in Phrase") + xlab("N Bios Phrase Appeared In") + 
  theme(legend.position="none", panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),)
p1
ggsave("img/init_count.pdf",p1,h=6,w=6)
table(d$v,d$nt)


### sample for vocab filtering
#set.seed(0)
#sampled <- d[, sample(phrase,30), by=.(nt,v)]
#ann_phrases = sample(sampled$V1,replace = F, size = nrow(sampled))
#write.table(ann_phrases,"annotated_data/sampled_words_XXX.txt",row.names=F,quote=F,col.names=F)

round_1 <- get_annotation_data(ANNOTATOR_FILE,"")
round_2 <-get_annotation_data(ANNOTATOR_FILE,"-p2")

get_agreement_stats(round_1)
get_agreement_stats(round_2)

########### Vocab limitation annotation #################
# aggregate responses
disagreements_r2 <- data.table(read_xlsx(ANNOTATOR_FILE, 
                                         sheet="disagreements-p2"))
round_2_final <- finalize_and_resolve_disagreements(round_2,
                                                    disagreements_r2)
table(round_2_final$response)
# Plot results
mg <- merge(d, round_2_final[,.(Phrase,response)], by.x="phrase", by.y="Phrase")
z <- mg[, .N, by = .(v,nt,response)]
sk <- spread(z, response,N, fill=0)
m <- sk[, as.list(binom.agresti.coull(Y, N+Y)), by=.(v,nt)]
m$nt <- paste("N Tokens: ",m$nt)
pl <- ggplot(m, aes(v, mean, ymin=lower,ymax=upper))+ 
  geom_pointrange(position=position_dodge(width=.3)) + 
  facet_wrap(~nt,nrow=1) + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  geom_hline(data=m[,mean(mean),by=nt],
             aes(yintercept = V1),color='red',linetype='dashed') +
  scale_y_continuous("% Labeled\nPersonal Identifiers",
                     labels=percent)+
  xlab("N. Bios Phrase Appeared in")
ggsave("img/init_ann.pdf",pl,h=4,w=10)
sk[, sum(Y)/sum(Y+N), by=nt]




#######################
###### SAmpling code for annotation
########################


# File for final annotation task
final_ann_list <- fread("../annotated_data/dataset_linked_terms_final_annotation.csv")

w <- rbindlist(
  list(
    w_idf1 = get_wordfreq_data("panel_7_7_20"),
    w_idf2 = get_wordfreq_data("panel_9_4_19"),
    w_idf3 = get_wordfreq_data("oct_2019_random")
  )
)
# additional filtering step to avoid sampling too heavily from potentially noisy tail
w <- w[n > 1]

#### sample for evaluation
#w[, prop := n / sum(.SD$n), by=dataset]
#to_annotate_final <- w[, sample(phrase,200,prob=prop),by=dataset]
#shuffled_ann <- sample_n(to_annotate_final,size=nrow(to_annotate_final), replace=F)
#write.csv(shuffled_ann, "annotated_data/dataset_linked_terms_final_annotation_XXX.csv",row.names=F)
#write.table(unique(shuffled_ann$V1),"annotated_data/final_annotation_terms_XXX.txt",row.names=F,quote=F,col.names=F)


###############################################
#################### Annotation outcomes
###############################################
final_ann_list <- fread("../annotated_data/dataset_linked_terms_final_annotation_2.csv")
round_3 <-get_annotation_data(ANNOTATOR_FILE,"-fin2")
round_3 <- merge(round_3, final_ann_list, by.x="Phrase",by.y = "V1")

################## Agreement  #################
get_agreement_stats(round_3[!duplicated(Phrase)])

get_agreement_stats(round_3[dataset == "panel_7_7_20"])
get_agreement_stats(round_3[dataset == "panel_9_4_19"])
get_agreement_stats(round_3[dataset == "oct_2019_random"])


########### Reliability 2 #################
# aggregate responses
round_3_final[, .N, by=.(dataset,response)]
disagreements_r3 <- data.table(read_xlsx(ANNOTATOR_FILE, 
                                         sheet="disagreements-fin2"))
round_3_final <- finalize_and_resolve_disagreements(round_3[, - "dataset",with=F],
                                                    disagreements_r3)
round_3_final <- merge(round_3_final, final_ann_list, by.x="Phrase",by.y = "V1")

# Plot results
r3_fin <- rbind(round_3_final[, as.list(binom.agresti.coull(sum(response =="Y"), .N)), by=dataset][, v:="Yes"],
      round_3_final[, as.list(binom.agresti.coull(sum(response =="N"), .N)), by=dataset][, v:="No"],
      round_3_final[, as.list(binom.agresti.coull(sum(response =="U"), .N)), by=dataset][, v:="Unclear"])

add_dataset_lab(r3_fin)

r3_plt <- ggplot(r3_fin, aes(dataset_lab,
                   mean,
                   ymin=lower,
                   ymax=upper,
                   color=v))+geom_pointrange(size=1.2)+
  scale_color_discrete("Phrase\nContains\nPersonal\nIdentifier?")+
  scale_y_continuous("% Annotated Phrases", labels=percent)+
  xlab("Dataset")
ggsave("img/r3.pdf",r3_plt,h=4,w=8)
survey################# 


# for network analysis
write.table(unique(round_2_final[response=="N"]$Phrase,
                   round_3_final[response =="N"]$Phrase), 
            "annotated_data/annotated_no.txt", 
            row.names=F,quote=F,col.names=F)

########### Other Descriptive Stats #################
# Load in all panel members
panel_1 <- get_all_info("panel_7_7_20")
panel_2 <- get_all_info("panel_9_4_19")
random <- get_all_info("oct_2019_random")

nrow(panel_1[['panel']])


all_user <- rbind(panel_1[['peruser_eng_count']],
                  panel_2[['peruser_eng_count']],
                  random[['peruser_eng_count']])

all_user <- all_user[,list(nv=.N), by=.(N,dataset)][, perc :=nv/sum(nv),by=dataset]
add_dataset_lab(all_user)
sum_1 <- ggplot(all_user, aes(N,perc,fill=dataset_lab)) +
  geom_col(position='dodge')  + 
  scale_y_continuous("% Users", labels=percent)+
  scale_x_continuous("N of Personal\nIdentifiers Extracted", 
                     breaks=c(0,1,3,5,10,20),limits=c(-1,20)) + 
  scale_fill_discrete("Dataset") +
  theme(legend.position=c(.7,.7)); sum_1
ggsave("img/user_cnt.pdf",sum_1,h=5,w=4.2)

top_terms <- rbind(panel_1[['perterm_eng_count']][order(-perc)][1:20],
                   panel_2[['perterm_eng_count']][order(-perc)][1:20],
                   random[['perterm_eng_count']][order(-perc)][1:20])
add_dataset_lab(top_terms)
sum_2 <- ggplot(top_terms, aes(perc,reorder_within(term,perc,dataset)))+ 
  geom_col() + 
  scale_y_reordered("Top 20\nPersonal Identifiers")+  
  scale_x_continuous("Percent of Users Expressing Identifier",labels=percent)  +
  facet_wrap(~dataset_lab,scales="free_y") + theme_classic(16); sum_2
ggsave("img/top_terms.pdf",sum_2,h=5,w=12)


################## Correlation in counts across samples

w <- rbindlist(
  list(
    w_idf1 = get_wordfreq_data("panel_7_7_20"),
    w_idf2 = get_wordfreq_data("panel_9_4_19"),
    w_idf3 = get_wordfreq_data("oct_2019_random")
  )
)
w_sp <- spread(w[,.(n_token,n,phrase,dataset)], dataset,n,fill=0)

cor(log(w_sp$panel_7_7_20+1),log(w_sp$oct_2019_random+1))
cor(log(w_sp$panel_7_7_20+1),log(w_sp$panel_9_4_19+1))


####### Compare to dictionaries

survey_data <- fread("identity_info_embedding_impressions.csv")
survey_data$ty <- "Identity"
survey_data[grepl("^b_", identity)]$ty <- "Behavior"
survey_data[grepl("^m_", identity)]$ty <- "Modifier"
survey_data$identity <- sub("^[bmi]_","",survey_data$identity)
survey_data$identity <- sub("_"," ",survey_data$identity)
survey_data$identity <- stri_replace_all_fixed( tolower(survey_data$identity),"_", " ")

oi_cs <- fread("identity_list_joseph_cscw_paper.txt",header=F,sep="\t")
w[, in_survey_act_id := phrase %in% survey_data[source == "UGA ACT Data" & ty == "Identity"]$identity]
w[, in_survey_act_mod := phrase %in% survey_data[source == "UGA ACT Data" & ty == "Modifier"]$identity]
w[, in_survey_act_beh := phrase %in% survey_data[source == "UGA ACT Data" & ty == "Behavior"]$identity]
w[, in_list_act_cscw := phrase %in% oi_cs$V1]
w[, in_survey_boluk := phrase %in% survey_data[source == "Bolukbasi et al."]$identity]
w[, in_survey_agarwal := phrase %in% survey_data[source == "Agarwal Personality"]$identity]

tst <- melt(w, 
            id=c("phrase","n","dataset"), 
            measure=grep("in_",names(w), value=T))

precision_test <- data.table()
for(i in c(1:5,seq(10,3000,by=100))){
  print(i)
  t <-  tst[, list(nleft=sum(n > i &  value),
                   nvocab=sum(n > i )),by=.(dataset,variable)]
  t$i <- i
  precision_test <- rbind(precision_test,t)
}

precision_test <- merge(precision_test, 
                     data.table(variable=c("in_survey_act_id",
                                           "in_survey_act_mod",
                                           "in_survey_act_beh",
                                           "in_list_act_cscw",
                                           "in_survey_boluk",
                                           "in_survey_agarwal"
                                           ), 
                                tot = c(length(unique(survey_data[source == "UGA ACT Data" & ty == "Identity"]$identity)),
                                        length(unique(survey_data[source == "UGA ACT Data" & ty == "Modifier"]$identity)),
                                        length(unique(survey_data[source == "UGA ACT Data" & ty == "Behavior"]$identity)),
                                        length(unique(oi_cs$V1)),
                                        length(unique(survey_data[source == "Bolukbasi et al."]$identity)),
                                        length(unique(survey_data[source == "Agarwal Personality"]$identity))
                                      )
                                ),
                                by="variable")
add_dataset_lab(precision_test)
precision_test <- precision_test[nvocab > 100]
precision_test[, survey_data := factor(variable,
                                    levels=c("in_survey_act_id",
                                             "in_list_act_cscw",
                                             "in_survey_boluk",
                                             "in_survey_agarwal"),
                                    labels=c("ACT Identities",
                                             "ACT Modifiers",
                                             "ACT Behaviors",
                                             "Identities\nJoseph et al.",
                                             "Identities\nAgarwal et al.",
                                             "Identities\nBolukbasi et al."))]
precision_test[, prec := nleft / nvocab]
precision_plt <- ggplot(precision_test, aes(i,prec,color=dataset_lab))+
  geom_point() + 
  geom_line() + 
  scale_y_continuous("Precision (% Personal\nIdentifiers in Term List)",
       labels=percent) +
  xlab("Threshold for Number of Bios Appearing In") + 
  scale_color_discrete("Dataset") + 
  facet_wrap(~survey_data)
ggsave("img/precision.pdf",precision_plt,h=7,w=12)


survey_data[, in_panel := identity %in% w[dataset =="panel_7_7_20"]$phrase ]
survey_data[ , src := factor(source, levels=c("Agarwal Personality","Bolukbasi et al.", "UGA ACT Data"),
                             labels=c("Personality","","Affect"))]
pan <- ggplot(survey_data[source != "This Paper"], 
       aes(consensus,dimension,color=in_panel))+ stat_summary(fun.data="mean_cl_boot",size=.9,position=position_dodge(.2)) +
  xlab("Scaled Perceived\nPosition on Dimension") +
  ylab("")+
  scale_color_discrete("Term\nAppears in\nPanel 1", labels=c("No","Yes"))+
  facet_grid(src~., space = "free",scales="free_y")
pan
ggsave("img/pan_val.pdf",pan,h=5,w=9)

tst[ , in_any := phrase %in% c(survey_data[source == "UGA ACT Data"]$identity, 
                               oi_cs$V1,survey_data[source == "Bolukbasi et al."]$identity,
                               survey_data[source == "Agarwal Personality"]$identity)]

tst[in_any == F][order(-n)][dataset == "panel_7_7_20" & variable == "in_survey_boluk"]
tst[in_any == F][order(-n)][dataset == "panel_7_7_20" & variable == "in_survey_boluk"][1:10]
