library(tidyr)
library(ggrepel)
library(ggplot2)
library(data.table)
library(bit64)
library(ggplot2)
library(scales)
library(ggtext)
library(Matrix)
library(purrr)
library(rvest)
library(xml2)
library(Hmisc)
library(emojifont)
library(maps)
library(glue)
library(patchwork)
library(readxl)
library(irr)
library(stringr)
library(tidyr)
library(tidytext)
library(cowplot)
library(data.table)
library(ggplot2)
library(tidyverse)
library(stringi)
compute_logodds <- function(c1,c2,a1=1,a2=1){
  c1_s = c1 + a1
  c2_s = c2 + a2
  d <- (log( c1_s / (sum(c1) + a1*length(c1)- c1_s )) - 
          log( c2_s / (sum(c2) + a2*length(c2)- c2_s )))
  sigma = (1./c1_s) + (1./c2_s)
  val = d/sqrt(sigma)
  return(data.frame(delta=d,sigma=sigma,val=val))
}


read_simple_user_info <- function(inFile){
  
  r = readBin(inFile, raw(), file.info(inFile)$size)
  r[r==as.raw(0)] = as.raw(0x20)
  tfile = tempfile(fileext=".txt")
  writeBin(r, tfile)
  rm(r)
  inFile = tfile
  
  return(fread(inFile, sep="\t",col.names =c("uid",
                                             'name',
                                             "screen_name",
                                             'url',
                                             'protected',
                                             'location',
                                             'description',
                                             "followers_count",
                                             "friends_count",
                                             "created_at",
                                             "utc_offset",
                                             'time_zone',
                                             "statuses_count",
                                             "lang",
                                             "status_created_at",
                                             'status_coordinates',
                                             "status_lang",
                                             "profile_image_url_https","verified")))
}




gen_plot <- function(d, dim_levels, positive, negative,n=10){

  df <- merge(data.table(n=names(positive),pos=as.vector(unlist(positive))),
              data.table(n=names(negative),neg=as.vector(unlist(negative))),by="n")
  df$n <- factor(df$n, levels=dim_levels)
  df <- df[order(n)]
  d$label <- factor(d$t, levels=dim_levels,
                    labels=glue_data(df, 
"<p><strong style='color:#29BF12'>{pos}</strong></p><p><strong style='color:#FFBF00'>{neg}</strong></p>"))
  
  p <- ggplot(d, aes(value,reorder_within(term,value,t),value,fill=k)) + 
    geom_col() + 
    scale_y_reordered() + 
    facet_wrap(~label, scales="free",nrow=2) + 
    scale_fill_manual(values=c("#29BF12","#FFBF00")) +
    theme_classic(13) + 
    theme(strip.background = element_blank(),
          strip.text=element_textbox(size=13, hjust=.5,vjust=1, width = unit(1, "npc"),
                                     height = unit(1,"in")),
          legend.position="none") +
    xlab("") + ylab("")
  p
  
}



# function to construct the sparse network
# based on Dianti's methodology (see paper for reference and description)
gen_sparse_network_dianati <- function(df,
                                       item_name){
  ni <- paste0(item_name,"_i")
  nj <- paste0(item_name,"_j")
  
  weight_df <-df[,list(weight=sum(x)),by=ni]
  setnames(weight_df,ni,item_name)
  
  total_degree <- sum(weight_df$weight)
  
  d <- df[get(ni) <= get(nj)]
  d <- merge(d,weight_df,by.x=ni,by.y=item_name)
  setnames(d, "weight","weight_i")
  
  d <- merge(d,weight_df, by.x=nj,by.y=item_name)
  setnames(d, "weight","weight_j")
  
  d[, q := total_degree / 2.0]
  d[, p := weight_i * weight_j / q / q / 2.0]
  d[,pval :=  pbinom(x - 1, round(q), p, lower.tail = FALSE),]
  d[,significance := -log(pval)]
  d <- d[order(-significance)]
  max_sig <- max(d[!is.infinite(significance)]$significance)
  d[is.infinite(significance), significance := max_sig]
  return(d)
}

gen_network <- function(d, nodes_name, items_name){
  # Map people and phenotypes to matrix indices
  
  node_df <- data.table(node=unique(d[,get(nodes_name)]),
                        node_idv=1:length(unique(d[,get(nodes_name)])))
  item_df <- data.table(item=unique(d[,get(items_name)]),
                        item_idv = 1:length(unique(d[,get(items_name)])))
  
  node_item_net <- merge(d,node_df,by.x=nodes_name, by.y="node")
  node_item_net <- merge(node_item_net,item_df,by.x=items_name,by.y="item")
  
  mat <- sparseMatrix(i = node_item_net$node_idv,
                      j = node_item_net$item_idv,
                      x = 1)
  
  # From the person -> phenotype matrix, now compute the phenotype-> phenotype matrix
  r <- t(mat)
  mat <- mat %*% r 
  m <- as (mat, "dgTMatrix")
  df <- data.table(i = m@i + 1, j = m@j + 1, x = m@x)
  df <- merge(df, node_df, by.x="i",by.y="node_idv")
  setnames(df, "node",paste0(nodes_name,"_i"))
  df <- merge(df,node_df, by.x="j",by.y="node_idv")
  setnames(df, "node",paste0(nodes_name,"_j"))
  df[, i:= NULL]
  df[, j:= NULL]
  
  # df is now the matrix
  df <- gen_sparse_network_dianati(df, "term")
  setnames(df, "x","c_ij")
  return(df)
  
}

get_wordfreq_data <- function(dataset,subset_to_vocab = T){
  ############
  # Ok, finally, read in the identity data
  df <- fread(paste0("wordidf_",dataset, ".csv"))
  if(subset_to_vocab){
    vocab <- fread(paste0("vocab_",dataset,".tsv.txt"),header=F,sep="\t")
    df <-df[phrase %in% vocab$V1]
  }
  df$dataset <- dataset
  return(df)
  
}
get_panel_data <- function(dataset){
  print(paste("Dataset: ", dataset))
  panel_info <- read_simple_user_info(paste0(dataset,".tsv"))
  orig_n <- nrow(panel_info)
  print(paste("Pct blank: ",
              nrow(panel_info[description == ""]),
              nrow(panel_info[description == ""])/nrow(panel_info))
  )
  panel_info <- panel_info[description != ""]
  print(paste("Rm for we are: ",
              sum(grepl("we are|not affiliated",panel_info$description)),
              sum(grepl("we are|not affiliated",panel_info$description))/nrow(panel_info)
  ))
  panel_info <- panel_info[! grepl("we are|not affiliated",description)]
  print(paste("Rm for lang: ",
              nrow(panel_info[!status_lang %in% c("en","","und","es")]),
              nrow(panel_info[!status_lang %in% c("en","","und","es")])/nrow(panel_info)
  ))
  panel_info <- panel_info[status_lang %in% c("en","","und","es")]
  
  lang_data <- fread(paste0("lang_",dataset,".tsv"),header=F)
  setnames(lang_data,c("uid","inferred_lang"))
  panel_info <- merge(panel_info,lang_data[!duplicated(uid)],by="uid",all.x=T)
  
  panel_info[, prof_len := str_length(description)]
  print(paste("N: ",nrow(panel_info), nrow(panel_info)/orig_n))
  
  #print(paste("% verified: ", sum(panel_info$verified)))
  print(paste("% verified: ", sum(panel_info$verified)/nrow(panel_info)))
  #print(paste("% verified: ", sum(panel_info$protected)))
  print(paste("% protected: ", sum(panel_info$protected)/nrow(panel_info)))
  print(paste("Median Followers: ", median(panel_info$followers_count)))
  print(paste("Median Friends: ", median(panel_info$friends_count)))
  print(paste("Median Status Count: ", median(panel_info$statuses_count)))
  print(paste("% inferred English: ", sum(panel_info[!is.na(inferred_lang)]$inferred_lang == "en")/nrow(panel)))
  print(paste("Median Profile Length: ", median(panel_info$prof_len)))
  
  return(panel_info)
}



get_single_annotator <- function(filename,annotator,sheet_suffix){
  f <- data.table(read_xlsx(filename, sheet= paste0(annotator,sheet_suffix)))
  setnames(f, "Does this expression contain a personal identifier?", 
           "resp"
  )
  # Count greater than one as an identifier after discussion, but note for
  # discussion about intersectionality
  f[ , r := ifelse(resp=="No - none",1, 
                   ifelse(resp =="Unclear",2, 3)) ]
  setnames(f, "r",paste0("resp_",annotator,"_f"))
  setnames(f, "resp",paste0("resp_",annotator))
  return(f)
}

get_all_info <- function(dataset){
  vocab <- get_wordfreq_data(dataset)
  output <- fread(paste0("output_",dataset,".tsv"))
  output <- output[output$term %in% vocab_info$phrase]
  panel <- get_panel_data(dataset)
  
  peruser_count <- merge(panel[,.(uid)],output[,.N, by=uid], all.x=T)
  peruser_count[is.na(N)]$N <- 0 
  peruser_count$dataset <- dataset
  
  peruser_eng_count <- merge(panel[inferred_lang=="en" & prof_len > 30,.(uid)],output[,.N, by=uid], all.x=T)
  peruser_eng_count[is.na(N)]$N <- 0 
  peruser_eng_count$dataset <- dataset
  
  panel <- merge(panel, peruser_count, by="uid")
  
  perterm_count <- output[,length(unique(uid)),by=term]
  perterm_count[, perc := V1/nrow(panel)]
  perterm_count$dataset <- dataset
  
  perterm_eng_count <- output[uid %in%panel[inferred_lang=="en" & prof_len > 30]$uid,length(unique(uid)),by=term]
  perterm_eng_count[, perc := V1/nrow(panel)]
  perterm_eng_count$dataset <- dataset
  
  return(list(vocab=vocab,
              output=output,
              panel=panel,
              peruser_count = peruser_count,
              peruser_eng_count = peruser_eng_count,
              perterm_count = perterm_count,
              perterm_eng_count = perterm_eng_count))
}


get_annotation_data <- function(filename,
                                sheet_suffix){
  
  a1 <- get_single_annotator(filename,"a1",sheet_suffix)
  a2 <- get_single_annotator(filename,"a2",sheet_suffix)
  a3 <- get_single_annotator(filename,"a3",sheet_suffix)  
  annotations <- merge(a1[,.(Phrase,resp_a1,resp_a1_f)],
                       a2[,.(Phrase,resp_a2,resp_a2_f)],
                       by="Phrase",all = T)
  annotations <- merge(annotations, 
                       a3[,.(Phrase,resp_a3, resp_a3_f)],
                       by="Phrase",all=T)
  annotations[, Phrase := as.character(Phrase)]
  annotations[, Phrase := sub("[.]0$","",Phrase)]
  
}


get_agreement_stats <- function(k){
  k_1 <- kripp.alpha(t(as.matrix(k[, .(resp_a1_f,resp_a2_f,resp_a3_f)])))
  unc <- k$resp_a1 == "Unclear" | k$resp_a2 == "Unclear" | k$resp_a3 == "Unclear"
  k_2 <- kripp.alpha(t(as.matrix(k[ifelse(is.na(unc), T, !unc), .(resp_a1_f,resp_a2_f,resp_a3_f)])))
  return(list(full=k_1, without_unclear = k_2))
  
}

finalize_and_resolve_disagreements <- function(ann_data, disagreements){
  # Resolve disagreements
  disagreements[ , resp_a3_f := ifelse(resp_a3=="No - none",1, ifelse(resp_a3 =="Unclear",2, 3)) ]
  disagreements[ , resp_a1_f := ifelse(resp_a1=="No - none",1, ifelse(resp_a1 =="Unclear",2, 3)) ]
  disagreements[ , resp_a2_f := ifelse(resp_a2=="No - none",1, ifelse(resp_a2 =="Unclear",2, 3)) ]
  agreed_upon_phrases <- ann_data[!Phrase %in% disagreements$Phrase]
  responses <- rbind(agreed_upon_phrases,disagreements)
  
  spread_resp <- spread(melt(responses[,.(Phrase, resp_a1_f, resp_a2_f, resp_a3_f)])[!is.na(value)]
                        [,.N, by=.(Phrase,value)], 
                        value,N,fill=0)
  setnames(spread_resp, c("1","2","3"),c("N","U","Y"))
  
  # Examples of where everyone disagreed. For discussion, but remove for calculation
  spread_resp[ (N < 2) & (Y < 2) & (U <2)]
  spread_resp <- spread_resp[ (N > 1) | (Y > 1) | (U > 1)]
  
  spread_resp$response <- apply(spread_resp[,.(N,U,Y)],1, 
                                function(l){c("N","U","Y")[which.max(l)]})
  return(spread_resp)
}

add_dataset_lab <- function(data,do_rev=F){
  data[, dataset_lab := factor(dataset, levels=c("panel_7_7_20","panel_9_4_19","oct_2019_random"),
                                 c("Panel 1","Panel 2","Random"))]

}
