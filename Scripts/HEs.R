
# HEs
merged <- read.csv('Datasets/Finaldata and Zscores/Finaldata and Zscores.csv')

# Calculate the regressors ---------------------------------------------------
# (B41-45)
attitudes <- merged[c("husb_final_say_num","prioritize_educ_num","husb_provide_inc_num",
                      "husb_justified_yell_num","husb_justified_beat_num")]
# (B62, B64)
violence <- merged[c("dc19_yell_num", "dc19_hit_num")]

# (B78, B81)
knowledge <- merged[c("know_online_valid_noaut_num","know_org_noaut_valid_num", 
                      "know_online_nehad_num", "know_org_nehad_num")]

# (B67-B71)
hypothetical <- merged[c("look_online_num", "contact_org_num")]

# (B80, B83)
reporting2 <- merged[c("dc19_look_online_num", "dc19_look_org_num")]

# z-scores
zscore_at <- scale(rowMeans(scale(attitudes) %*% diag(c(-1,-1,-1,-1,-1))))
zscore_violence <- scale(rowMeans(scale(violence) %*% diag(c(1,1))))
zscore_know <- scale(rowMeans(scale(knowledge)%*% diag(c(1,1,1,1))))
zscore_rep1 <- scale(rowMeans(scale(hypothetical) %*% diag(c(1,1))))
zscore_rep2 <- scale(rowMeans(scale(reporting2)%*% diag(c(1,1))))

merged$x1 <- zscore_at
merged$x2 <- zscore_violence
merged$x3 <- zscore_know
merged$x4 <- zscore_rep1
merged$x5 <- zscore_rep2

lm1 <- lm(FS_TV_zscore ~ pooledF_WI + in_group_10018 + reminder_10018 + 
             x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
             x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
             x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
            x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
            x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
            tv_evening_num + tv_sattelite_num + tv_top3_chan_num + tv_top3_shows_num + sat_show_num +
            factor(block_ids), weights = weight, data = merged)

lm2 <- lm(FS_FW_zscore ~ pooledF_WI + in_group_10018 + reminder_10018 + 
            x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
            x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
            x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
            x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
            x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
             X2mos_socmed_dv_num + X2mos_whatsapp_dv_num + factor(block_ids),
           weights = weight, data = merged)

lm3 <- lm(RF_knowledge2_zscore ~ pooledF_WI + in_group_10018 + reminder_10018 + 
            x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
            x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
            x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
            x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
            x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
            know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
            know_org_noaut_valid_num + know_online_nehad_num + know_org_nehad_num +
            factor(block_ids), 
          weights = weight, 
          data = merged)

lm4 <- lm(RF_att1_zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
            x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
            x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
            x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
            x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
            x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
            husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
            husb_justified_beat_num + future_equal_say_num + future_equal_rights_num +
            factor(block_ids),
          weights = weight, data = merged)

lm5 <- lm(RF_att2_zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
            x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
            x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
            x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
            x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
            x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
            husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
            husb_justified_beat_num + future_equal_say_num + future_equal_rights_num +
            factor(block_ids),
           weights = weight, na.action = na.omit, data = merged)


lm6 <-lm(RF_preference_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
           x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
           x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
           x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
           x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
           x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
           factor(block_ids), 
         weights = weight, 
         data = merged)

lm7 <- lm(RF_dcovid_zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
            x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
            x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
            x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
            x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
            x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
            dcovid_yelled_end_num_refuse + dcovid_hit_end_num_refuse + dcovid_assault_end_num_refuse + 
            x1*dcovid_yelled_end_num_refuse + x1*dcovid_hit_end_num_refuse + x1*dcovid_assault_end_num_refuse + 
            x2*dcovid_yelled_end_num_refuse + x2*dcovid_hit_end_num_refuse + x2*dcovid_assault_end_num_refuse + 
            x3*dcovid_yelled_end_num_refuse + x3*dcovid_hit_end_num_refuse + x3*dcovid_assault_end_num_refuse + 
            x4*dcovid_yelled_end_num_refuse + x4*dcovid_hit_end_num_refuse + x4*dcovid_assault_end_num_refuse + 
            x5*dcovid_yelled_end_num_refuse + x5*dcovid_hit_end_num_refuse + x5*dcovid_assault_end_num_refuse + 
            bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num  + 
            factor(block_ids),
              weights = weight, data = merged)


lm8 <-lm(RF_hypDM2_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
           x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
           x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
           x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
           x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
           x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
           talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
           factor(block_ids), 
           weights = weight, 
           data = merged)

lm9 <- lm(RF_hypSA2_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
            x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
            x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
            x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
            x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
            x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
            talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num +
            factor(block_ids),
            weights = weight,
            data = merged)


lm10 <-lm(RF_dcovid2_zscore ~ pooledF_WI + in_group_10018  + reminder_10018 + 
            x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
            x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
            x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
            x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
            x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
            dcovid_accessonline_end_num_refuse + dcovid_contactorg_end_num_refuse +
            x1*dcovid_accessonline_end_num_refuse + x1*dcovid_contactorg_end_num_refuse +
            x2*dcovid_accessonline_end_num_refuse + x2*dcovid_contactorg_end_num_refuse +
            x3*dcovid_accessonline_end_num_refuse + x3*dcovid_contactorg_end_num_refuse +
            x4*dcovid_accessonline_end_num_refuse + x4*dcovid_contactorg_end_num_refuse +
            x5*dcovid_accessonline_end_num_refuse + x5*dcovid_contactorg_end_num_refuse +
            know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + dc19_look_org_num +
            know_org_noaut_valid_num +know_online_nehad_num + 
            know_org_nehad_num +
               factor(block_ids), 
             weights = weight, 
             data = merged)

lm11 <- lm(RF_fo_zscore ~ pooledF_WI + in_group_10018 +reminder_10018 + 
             x1 + x1*pooledF_WI + x1*in_group_10018 + x1*reminder_10018 + 
             x2 + x2*pooledF_WI + x2*in_group_10018 + x2*reminder_10018 +
             x3 + x3*pooledF_WI + x3*in_group_10018 + x3*reminder_10018 + 
             x4 + x4*pooledF_WI + x4*in_group_10018 + x4*reminder_10018 + 
             x5 + x5*pooledF_WI + x5*in_group_10018 + x5*reminder_10018 + 
             husb_final_say_num + prioritize_educ_num + husb_provide_inc_num + husb_justified_yell_num + 
             husb_justified_beat_num + future_equal_say_num + future_equal_rights_num +
           factor(block_ids),
         weights = weight, data = merged)

covariates <- c( "SM Individual", "SM Group", "TV", 
                 "Attitudes index", "Experienced violence", "Resource Knowledge", "Hypothetical use and contact", "Recent use and contact",
                 "Attitudes x SM Individual", "Attitudes x SM Group", "Attitudes x TV",
                 "Experienced violence x SM Individual", "Experienced violence x SM Group", "Experienced violence x TV",
                 "Resource knowledge x SM Individual", "Resource knowledge x SM Group", "Resource knowledge x TV",
                 "Hypothetical use and contact x SM Individual", "Hypothetical use and contact x SM Group", "Hypothetical use and contact x TV",
                 "Recent use and contact x SM Individual", "Recent use and contact x SM Group", "Recent use and contact x TV")

omit_var <- c("Constant", "block_ids", 
              "tv_evening_num", "tv_sattelite_num", "tv_top3_chan_num", "tv_top3_shows_num", "sat_show_num",
              "X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num", "know_online_valid_noaut_num", "bc19_look_online_num", 
              "dc19_look_online_num", "bc19_look_org_num", "dc19_look_org_num",
              "know_org_noaut_valid_num", "know_online_nehad_num", "know_org_nehad_num", "husb_final_say_num", "prioritize_educ_num", 
              "husb_provide_inc_num", "husb_justified_yell_num","husb_justified_beat_num", "future_equal_say_num", 
              "future_equal_rights_num", "dcovid_yelled_end_num_refuse", "dcovid_hit_end_num_refuse", "dcovid_assault_end_num_refuse", 
              "x1:dcovid_yelled_end_num_refuse", "x1:dcovid_hit_end_num_refuse", "x1:dcovid_assault_end_num_refuse", 
              "x2:dcovid_yelled_end_num_refuse", "x2:dcovid_hit_end_num_refuse", "x2:dcovid_assault_end_num_refuse", 
              "x3:dcovid_yelled_end_num_refuse", "x3:dcovid_hit_end_num_refuse", "x3:dcovid_assault_end_num_refuse",
              "x4:dcovid_yelled_end_num_refuse", "x4:dcovid_hit_end_num_refuse", "x4:dcovid_assault_end_num_refuse",
              "x5:dcovid_yelled_end_num_refuse", "x5:dcovid_hit_end_num_refuse", "x5:dcovid_assault_end_num_refuse",
              "bc19_yell_num", "dc19_yell_num", "bc19_hit_num", "dc19_hit_num", "talk_husband_num", "talk_family_num", 
              "report_authorities_num", "look_online_num", "contact_org_num", "dcovid_accessonline_end_num_refuse", 
              "dcovid_contactorg_end_num_refuse", 
              "x1:dcovid_accessonline_end_num_refuse", "x1:dcovid_contactorg_end_num_refuse",
              "x2:dcovid_accessonline_end_num_refuse", "x2:dcovid_contactorg_end_num_refuse",
              "x3:dcovid_accessonline_end_num_refuse", "x3:dcovid_contactorg_end_num_refuse",
              "x4:dcovid_accessonline_end_num_refuse", "x4:dcovid_contactorg_end_num_refuse",
              "x5:dcovid_accessonline_end_num_refuse", "x5:dcovid_contactorg_end_num_refuse")

dep_var <- c("\\shortstack{Index of \\\\ TV show \\\\ consumption}", 
             "\\shortstack{Index of \\\\ videos of  \\\\women's \\\\empowerment \\\\and support \\\\consumption}", 
             "\\shortstack{Index of \\\\ knowledge  \\\\ about \\\\ treatment \\\\information}", 
             "\\shortstack{Index of \\\\ attitudes  \\\\  toward \\\\ gender and \\\\ marital \\\\equality}", 
             "\\shortstack{Index of \\\\ attitudes on  \\\\ sexual  \\\\ violence}", 
             "\\shortstack{Index of \\\\ donation to  \\\\ organizations  \\\\ supporting  \\\\ women}", 
             "\\shortstack{Index of \\\\ domestic and  \\\\ sexual violence  \\\\ experienced  \\\\ during  \\\\ COVID-19}", 
             "\\shortstack{Index of \\\\ hypothetical use  \\\\ of online  \\\\ resources  \\\\ and contact with  \\\\ an organization  \\\\ when responding  \\\\ to domestic  \\\\ violence}", 
             "\\shortstack{Index of \\\\ hypothetical use  \\\\ of online  \\\\ resources  \\\\ and contact with  \\\\ an organization  \\\\ when responding  \\\\ to sexual  \\\\ violence}", 
             "\\shortstack{Index of \\\\ recent use  \\\\ of online  \\\\ resources and  \\\\ contact with  \\\\ an organization  \\\\ during  \\\\ COVID-19}", 
             "\\shortstack{Index of \\\\ views on  \\\\ women's  \\\\ future outlook  \\\\ toward gender  \\\\ and marital  \\\\ equality}")

HEs <- stargazer(lm1, lm2, lm3, lm4, lm5, lm6, lm7, lm8, lm9, lm10, lm11,
                    header=FALSE,
                    font.size="tiny",
                    dep.var.labels.include = FALSE,
                 label = "tab:HEs1",
                    table.placement = "H",
                 dep.var.caption = "",
                    column.labels=dep_var,
                    covariate.labels= covariates,
                    omit = omit_var, 
                    omit.stat=c("f", "ser","adj.rsq"), 
                    column.sep.width = "0pt",
                    title = "Heterogeneous effects in main outcomes by baseline indexes on attitudes towards gender and marital equality (Attitudes), 
                    domestic violence experienced during COVID-19 (Experienced violence), knowledge on treatment information (Resource knowledge), hypothetical use of online resources and 
                 contact with an organization when responding to domestic violence (Hypothetical use and contact), and recent use of online resources and contact with an organization variables (Recent use and contact)",
                    type = "latex")



note.latex <- "\\multicolumn{12}{l} {\\parbox[t]{20cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
All regressions include controls for all baseline covariates in the outcome family as stated in their corresponding tables 
from Table \\ref{tab:fs_tv} to \\ref{tab:rf_9}. 
The main baseline indexes are attitudes towards gender and marital equality (Attitudes), 
domestic violence experienced during COVID-19 (Experienced violence), 
knowledge on treatment information (Resource knowledge), 
hypothetical use of online resources and contact with an organization when responding to domestic violence (Hypothetical use and contact), 
and recent use of online resources and contact with an organization variables (Recent use and contact). 
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
HEs[grepl("Note", HEs)] <- note.latex


aux <- paste(HEs, collapse = "")
part1 <- sub("Attitudes index.*", "", aux, ignore.case = T)
part2 <- sub(".*Attitudes x SM Individual", "Attitudes x SM Individual ", aux)

modified_HEs <- paste(part1, part2, collapse = "")

cat(alignTable(modified_HEs, 2.25), file = 'Tables/S33.tex')

# HEs for Arab Barometer comparable variables ----------------------------
merged$age_index <- scale(rowMeans(scale(merged$age)))
merged$ba_above_index <- scale(rowMeans(scale(merged$ba_above)))
merged$married_index <- scale(rowMeans(scale(merged$married)))
merged$child_index <- scale(rowMeans(scale(merged$male_children + merged$female_children)))

sm_data <- merged[c("socmed_Facebook", "socmed_Instagram", "socmed_Snapchat", "socmed_Telegram",
                                 "socmed_Twitter", "socmed_whatsapp", "socmed_YouTube")]
merged$social_media_index <- scale(rowMeans(scale(sm_data) %*% diag(c(1,1,1,1,1,1,1))))
merged$hours_soc_med_num_index <- scale(rowMeans(scale(merged$hours_soc_med_num)))

map_agree <- c("Strongly agree" = 1, "Agree" = 2, "Neutral" =3, "Disagree" = 4, "Strongly disagree" = 5)
merged$husbfs <- as.numeric(revalue(merged$husb_final_say, map_agree))
merged$husbfs_index <- scale(rowMeans(scale(merged$husbfs)))

merged$prio_educ <- as.numeric(revalue(merged$prioritize_educ, map_agree))
merged$prio_educ_index <- scale(rowMeans(scale(merged$prio_educ)))


merged$hit_bcovid <- 0
merged$hit_bcovid[which(merged$bc19_hit != "Never")] <- 1
merged$hit_bcovid_index <-  scale(rowMeans(scale(merged$hit_bcovid)))

# support from variable
# relative
indexes_fam <- which(merged$talk_family_num == 4 | merged$talk_family_num == 5)
indexes_husb <- which(merged$talk_husband_num == 4 | merged$talk_husband_num == 5)

index1 <- c(indexes_fam, indexes_husb) # all fam and husb combined
index1 <- unique(index1) # only unique indexes
merged$aux1 <- 0
merged$aux1[index1] <- 1

# police
ind2 <- which(merged$report_authorities_num == 4 | merged$report_authorities_num == 5)
merged$aux2 <- 0
merged$aux2[ind2] <- 1

# Contact Org
ind3 <- which(merged$contact_org_num == 4 | merged$contact_org_num == 5)

merged$aux3 <- 0
merged$aux3[ind3] <- 1

# index
support_data <- merged[c("aux1", "aux2", "aux3")]
merged$support_index <- scale(rowMeans(scale(support_data) %*% diag(c(1,1,1))))

# ---------
merged$z1 <- merged$age_index
merged$z2 <- merged$ba_above_index
merged$z3 <- merged$married_index
merged$z4 <- merged$child_index
merged$z5 <- merged$social_media_index
merged$z6 <- merged$hours_soc_med_num_index
merged$z7 <- merged$husbfs_index
merged$z8 <- merged$prio_educ_index
merged$z9 <- merged$support_index
merged$z10 <- merged$hit_bcovid_index

zscores <- c("FS_TV_zscore",
             "FS_FW_zscore",
             "RF_knowledge2_zscore",
             "RF_att1_zscore",
             "RF_att2_zscore",
             "RF_hypDM2_zscore",
             "RF_hypSA2_zscore",
             "RF_fo_zscore")

cov1 <- c("tv_evening_num", "tv_sattelite_num", "tv_top3_chan_num", "tv_top3_shows_num", "sat_show_num")
cov2 <- c("X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num")
cov3 <- c("know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num", 
          "bc19_look_org_num", "dc19_look_org_num", "know_org_noaut_valid_num", "know_online_nehad_num", 
          "know_org_nehad_num")
cov4 <- c("husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num",
            "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num")
cov7 <- c("talk_husband_num","talk_family_num","report_authorities_num","look_online_num","contact_org_num")

list_controls<- list(cov1, cov2, cov3, cov4, cov4, cov7, cov7, cov4)

count <- 1
for (i in zscores) {
  fmla <- as.formula(paste(i, " ~ ", "pooledF_WI + in_group_10018 + reminder_10018 + ", 
                            "z1 + z1*pooledF_WI + z1*in_group_10018 + z1*reminder_10018 + 
                             z2 + z2*pooledF_WI + z2*in_group_10018 + z2*reminder_10018 +
                             z3 + z3*pooledF_WI + z3*in_group_10018 + z3*reminder_10018 + 
                             z4 + z4*pooledF_WI + z4*in_group_10018 + z4*reminder_10018 + 
                             z5 + z5*pooledF_WI + z5*in_group_10018 + z5*reminder_10018 + 
                             z6 + z6*pooledF_WI + z6*in_group_10018 + z6*reminder_10018 + 
                             z7 + z7*pooledF_WI + z7*in_group_10018 + z7*reminder_10018 +
                             z8 + z8*pooledF_WI + z8*in_group_10018 + z8*reminder_10018 + 
                             z9 + z9*pooledF_WI + z9*in_group_10018 + z9*reminder_10018 + 
                             z10 + z10*pooledF_WI + z10*in_group_10018 + z10*reminder_10018 + ",
                           paste(list_controls[[count]], collapse = " + "), " + ",
                           "factor(block_ids)", sep = ""))
  print(fmla)
  nam <- paste("lm_", i, sep = "")
  assign(nam, lm(fmla, weights = weight, data = merged))
  count <- count + 1
}

# reg 6, preferences
fmla <- as.formula(paste("RF_preference_zscore", " ~ ", "pooledF_WI + in_group_10018 + reminder_10018 + ", 
                         "z1 + z1*pooledF_WI + z1*in_group_10018 + z1*reminder_10018 + 
                             z2 + z2*pooledF_WI + z2*in_group_10018 + z2*reminder_10018 +
                             z3 + z3*pooledF_WI + z3*in_group_10018 + z3*reminder_10018 + 
                             z4 + z4*pooledF_WI + z4*in_group_10018 + z4*reminder_10018 + 
                             z5 + z5*pooledF_WI + z5*in_group_10018 + z5*reminder_10018 + 
                             z6 + z6*pooledF_WI + z6*in_group_10018 + z6*reminder_10018 + 
                             z7 + z7*pooledF_WI + z7*in_group_10018 + z7*reminder_10018 +
                             z8 + z8*pooledF_WI + z8*in_group_10018 + z8*reminder_10018 + 
                             z9 + z9*pooledF_WI + z9*in_group_10018 + z9*reminder_10018 + 
                             z10 + z10*pooledF_WI + z10*in_group_10018 + z10*reminder_10018 + ",
                         "factor(block_ids)", sep = ""))
print(fmla)
nam <- paste("lm_", "RF_preference_zscore", sep = "")
assign(nam, lm(fmla, weights = weight, data = merged))

# reg 7, preferences
fmla <- as.formula(paste("RF_dcovid_zscore", " ~ ", "pooledF_WI + in_group_10018 + reminder_10018 + ", 
                         "z1 + z1*pooledF_WI + z1*in_group_10018 + z1*reminder_10018 + 
                             z2 + z2*pooledF_WI + z2*in_group_10018 + z2*reminder_10018 +
                             z3 + z3*pooledF_WI + z3*in_group_10018 + z3*reminder_10018 + 
                             z4 + z4*pooledF_WI + z4*in_group_10018 + z4*reminder_10018 + 
                             z5 + z5*pooledF_WI + z5*in_group_10018 + z5*reminder_10018 + 
                             z6 + z6*pooledF_WI + z6*in_group_10018 + z6*reminder_10018 + 
                             z7 + z7*pooledF_WI + z7*in_group_10018 + z7*reminder_10018 +
                             z8 + z8*pooledF_WI + z8*in_group_10018 + z8*reminder_10018 + 
                             z9 + z9*pooledF_WI + z9*in_group_10018 + z9*reminder_10018 + 
                             z10 + z10*pooledF_WI + z10*in_group_10018 + z10*reminder_10018 + ",
                         "bc19_yell_num + dc19_yell_num + bc19_hit_num + dc19_hit_num  + 
                         talk_husband_num + talk_family_num + report_authorities_num + look_online_num + contact_org_num + 
                         dcovid_yelled_end_num_refuse + dcovid_hit_end_num_refuse + dcovid_assault_end_num_refuse +",
                         "z1*dcovid_yelled_end_num_refuse + z1*dcovid_hit_end_num_refuse + z1*dcovid_assault_end_num_refuse + 
                             z2*dcovid_yelled_end_num_refuse + z2*dcovid_hit_end_num_refuse + z2*dcovid_assault_end_num_refuse +
                             z3*dcovid_yelled_end_num_refuse + z3*dcovid_hit_end_num_refuse + z3*dcovid_assault_end_num_refuse + 
                             z4*dcovid_yelled_end_num_refuse + z4*dcovid_hit_end_num_refuse + z4*dcovid_assault_end_num_refuse + 
                             z5*dcovid_yelled_end_num_refuse + z5*dcovid_hit_end_num_refuse + z5*dcovid_assault_end_num_refuse + 
                             z6*dcovid_yelled_end_num_refuse + z6*dcovid_hit_end_num_refuse + z6*dcovid_assault_end_num_refuse + 
                             z7*dcovid_yelled_end_num_refuse + z7*dcovid_hit_end_num_refuse + z7*dcovid_assault_end_num_refuse +
                             z8*dcovid_yelled_end_num_refuse + z8*dcovid_hit_end_num_refuse + z8*dcovid_assault_end_num_refuse + 
                             z9*dcovid_yelled_end_num_refuse + z9*dcovid_hit_end_num_refuse + z9*dcovid_assault_end_num_refuse + 
                             z10*dcovid_yelled_end_num_refuse + z10*dcovid_hit_end_num_refuse + z10*dcovid_assault_end_num_refuse + ",
                         "factor(block_ids)", sep = ""))
print(fmla)
nam <- paste("lm_", "RF_dcovid_zscore", sep = "")
assign(nam, lm(fmla, weights = weight, data = merged))

  
# reg 10, preferences
fmla <- as.formula(paste("RF_dcovid2_zscore", " ~ ", "pooledF_WI + in_group_10018 + reminder_10018 + ", 
                         "z1 + z1*pooledF_WI + z1*in_group_10018 + z1*reminder_10018 + 
                             z2 + z2*pooledF_WI + z2*in_group_10018 + z2*reminder_10018 +
                             z3 + z3*pooledF_WI + z3*in_group_10018 + z3*reminder_10018 + 
                             z4 + z4*pooledF_WI + z4*in_group_10018 + z4*reminder_10018 + 
                             z5 + z5*pooledF_WI + z5*in_group_10018 + z5*reminder_10018 + 
                             z6 + z6*pooledF_WI + z6*in_group_10018 + z6*reminder_10018 + 
                             z7 + z7*pooledF_WI + z7*in_group_10018 + z7*reminder_10018 +
                             z8 + z8*pooledF_WI + z8*in_group_10018 + z8*reminder_10018 + 
                             z9 + z9*pooledF_WI + z9*in_group_10018 + z9*reminder_10018 + 
                             z10 + z10*pooledF_WI + z10*in_group_10018 + z10*reminder_10018 + ",
                         "know_online_valid_noaut_num + bc19_look_online_num + dc19_look_online_num + bc19_look_org_num + 
                         dc19_look_org_num + know_org_noaut_valid_num +know_online_nehad_num + know_org_nehad_num + 
                         dcovid_accessonline_end_num_refuse + dcovid_contactorg_end_num_refuse +",
                         "z1*dcovid_accessonline_end_num_refuse + z1*dcovid_contactorg_end_num_refuse +
                             z2*dcovid_accessonline_end_num_refuse + z2*dcovid_contactorg_end_num_refuse + 
                             z3*dcovid_accessonline_end_num_refuse + z3*dcovid_contactorg_end_num_refuse + 
                             z4*dcovid_accessonline_end_num_refuse + z4*dcovid_contactorg_end_num_refuse + 
                             z5*dcovid_accessonline_end_num_refuse + z5*dcovid_contactorg_end_num_refuse +
                             z6*dcovid_accessonline_end_num_refuse + z6*dcovid_contactorg_end_num_refuse + 
                             z7*dcovid_accessonline_end_num_refuse + z7*dcovid_contactorg_end_num_refuse + 
                             z8*dcovid_accessonline_end_num_refuse + z8*dcovid_contactorg_end_num_refuse + 
                             z9*dcovid_accessonline_end_num_refuse + z9*dcovid_contactorg_end_num_refuse + 
                             z10*dcovid_accessonline_end_num_refuse + z10*dcovid_contactorg_end_num_refuse + ",
                         "factor(block_ids)", sep = ""))
print(fmla)
nam <- paste("lm_", "RF_dcovid2_zscore", sep = "")
assign(nam, lm(fmla, weights = weight, data = merged))

labels_cov <- c("Age", "Education above BA", "Married", "Number of children", "Social media use", " Social media hours",
                "Husband final say", "Male education priority", "Seek support", "Experienced violence")

b <- strsplit(paste0(paste(labels_cov, collapse = " x SM Individual,"), " x SM Individual"), ",")[[1]]
c <- strsplit(paste0(paste(labels_cov, collapse = " x SM Group,"), " x SM Group"), ",")[[1]]
d <- strsplit(paste0(paste(labels_cov, collapse = " x TV,"), " x TV"), ",")[[1]]

covariates_names <- c(rbind(b, c, d))

covariates <- c("SM Individual", "SM Group", "TV", 
                labels_cov, covariates_names)

z <- c("z1","z2","z3","z4","z5","z6","z7","z8","z9","z10")

omit_var <- c("Constant", "block_ids", 
              "tv_evening_num", "tv_sattelite_num", "tv_top3_chan_num", "tv_top3_shows_num", "sat_show_num",
              "X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num", "know_online_valid_noaut_num", "bc19_look_online_num", 
              "dc19_look_online_num", "bc19_look_org_num", "dc19_look_org_num",
              "know_org_noaut_valid_num", "know_online_nehad_num", "know_org_nehad_num", "husb_final_say_num", "prioritize_educ_num", 
              "husb_provide_inc_num", "husb_justified_yell_num","husb_justified_beat_num", "future_equal_say_num", 
              "future_equal_rights_num", "dcovid_yelled_end_num_refuse", "dcovid_hit_end_num_refuse", "dcovid_assault_end_num_refuse", 
              strsplit(paste0(paste0(z, collapse = ":dcovid_yelled_end_num_refuse,"), ":dcovid_yelled_end_num_refuse"), ",")[[1]],
              strsplit(paste0(paste0(z, collapse = ":dcovid_hit_end_num_refuse,"), ":dcovid_hit_end_num_refuse"), ",")[[1]],
              strsplit(paste0(paste0(z, collapse = ":dcovid_assault_end_num_refuse,"), ":dcovid_assault_end_num_refuse"), ",")[[1]],
              "bc19_yell_num", "dc19_yell_num", "bc19_hit_num", "dc19_hit_num", "talk_husband_num", "talk_family_num", 
              "report_authorities_num", "look_online_num", "contact_org_num", "dcovid_accessonline_end_num_refuse", 
              "dcovid_contactorg_end_num_refuse", 
              strsplit(paste0(paste0(z, collapse = ":dcovid_accessonline_end_num_refuse,"), ":dcovid_accessonline_end_num_refuse"), ",")[[1]],
              strsplit(paste0(paste0(z, collapse = ":dcovid_contactorg_end_num_refuse,"), ":dcovid_contactorg_end_num_refuse"), ",")[[1]])


HEs2 <- stargazer(lm_FS_TV_zscore, lm_FS_FW_zscore, lm_RF_knowledge2_zscore,
                  lm_RF_att1_zscore, lm_RF_att2_zscore, lm_RF_preference_zscore,
                  lm_RF_dcovid_zscore, lm_RF_hypDM2_zscore, lm_RF_hypSA2_zscore,
                  lm_RF_dcovid2_zscore, lm_RF_fo_zscore,
                 header=FALSE,
                 font.size="tiny",
                 label = "tab:HEs2",
                 dep.var.labels.include = FALSE,
                 table.placement = "H",
                 dep.var.caption = "",
                 column.labels=dep_var,
                 covariate.labels= covariates,
                 omit = omit_var, 
                 omit.stat=c("f", "ser","adj.rsq"), 
                 column.sep.width = "0pt",
                 title = "Heterogeneous effects on main outcomes by comparable variables with the Arab Barometer sample",
                 type = "latex")

note.latex <- "\\multicolumn{12}{l} {\\parbox[t]{20cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
All regressions include controls for all baseline covariates in the outcome family as stated in their corresponding Tables from Table S13 to Table S23.
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\"
HEs2[grepl("Note", HEs2)] <- note.latex


aux <- paste(HEs2, collapse = "")

part1 <- sub("Age.*", "", aux)
part2 <- sub(".*Age x SM Individual", "Age x SM Individual ", aux)

modified_table <- paste(part1, part2, collapse = "")

cat(alignTable(alignTableV(modified_table, 3),2.25), file = 'Tables/S34.tex')



# HEs by distance to endline survey --------------------------------------------
merged$date <- sub(' .*', '', merged$date) # Delete hour from date format

# Create a numeric value for the distance from September 5th to endline survey completion
date <- unique(merged$date)[order(as.Date(unique(merged$date), format = "%Y-%m-%d"))] # Get ordered dates
distance_dates <- c(1:26, 28:(length(date)+1)) + 5 # Convert dates to numeric and add 5 (Intervention ended in September 5)

# Auxiliary data frame to match numeric values to each date
aux_data <- cbind.data.frame(date, distance_dates)
merged$distance_dates <- aux_data$distance_dates[match(merged$date, aux_data$date)]

# Demean date variable
merged$distance_dates_demeaned <- merged$distance_dates - weighted.mean(merged$distance_dates, w = merged$weight)
merged$aux_date <- merged$distance_dates_demeaned

interaction_var <- merged$distance_dates
level <- c("aux_date")
interacted_segment <- c("distance_dates_demeaned")

# Label for interacted variable
interaction_name <- c("Distance")

covariates_separate <- c("SM Individual", "SM Group", "TV")
covariates <- c(covariates_separate, paste(covariates_separate, interaction_name, sep = " x "))

# Treatments and interactions
treatments <- c("pooledF_WI", "in_group_10018", "reminder_10018")

interacted_treatments <- paste0(treatments, ":", interacted_segment, collapse = " + ")

# Calculate means
zscores <- c("FS_TV_zscore",
             "FS_FW_zscore",
             "RF_knowledge2_zscore",
             "RF_att1_zscore",
             "RF_att2_zscore",
             "RF_preference_zscore",
             "RF_dcovid_zscore",
             "RF_hypDM2_zscore",
             "RF_hypSA2_zscore",
             "RF_dcovid2_zscore",
             "RF_fo_zscore")

control1 <- c("tv_evening_num", "tv_sattelite_num", "tv_top3_chan_num", 
              "tv_top3_shows_num", "sat_show_num",
              "age", "educ_aboveBA", "married")

control2 <- c("X2mos_socmed_dv_num", "X2mos_whatsapp_dv_num",
              "age", "educ_aboveBA", "married")

control3 <- c("know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num",
              "bc19_look_org_num","dc19_look_org_num",
              "know_org_noaut_valid_num", "know_online_nehad_num", 
              "know_org_nehad_num","age", "educ_aboveBA", "married")

control4 <- c("husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num", 
              "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num", "age", "educ_aboveBA", "married")

control5 <- c("husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num",
              "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num", "age", "educ_aboveBA", "married")

control6 <- c("age", "educ_aboveBA", "married")

control7 <- c("bc19_yell_num", "dc19_yell_num", "bc19_hit_num", "dc19_hit_num" , 
              "age", "educ_aboveBA", "married")

control8 <- c("talk_husband_num", "talk_family_num", "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")

control9 <- c("talk_husband_num", "talk_family_num", "report_authorities_num", "look_online_num", "contact_org_num",
              "age", "educ_aboveBA", "married")

control10 <- c("know_online_valid_noaut_num", "bc19_look_online_num", "dc19_look_online_num", "bc19_look_org_num", 
              "dc19_look_org_num", "know_org_noaut_valid_num", "know_online_nehad_num", 
              "know_org_nehad_num", "age", "educ_aboveBA", "married")

control11 <- c("husb_final_say_num", "prioritize_educ_num", "husb_provide_inc_num", "husb_justified_yell_num", 
              "husb_justified_beat_num", "future_equal_say_num", "future_equal_rights_num",
              "age", "educ_aboveBA", "married")

refuse7 <- c("dcovid_yelled_end_num_refuse", "dcovid_hit_end_num_refuse", "dcovid_assault_end_num_refuse")

refuse10 <- c("dcovid_accessonline_end_num_refuse", "dcovid_contactorg_end_num_refuse")

# Append controls and refuse 
controls <- list(control1,control2,control3,control4,control5,control6,control7,control8,
              control9,control10,control11)

refuse <- list(NA,NA,NA,NA,NA,NA,refuse7,NA,NA,refuse10,NA)

# Run regressions
lm_list <- list()
count <- 1

for (x in zscores) {
  if (is.na(refuse[count])) {
    fmla <- as.formula(paste0(x, " ~ ",  paste(treatments, collapse = " + "), " + ", 
                              interacted_treatments, " + ",
                              paste0(controls[[count]], collapse = " + "), " + ",
                              level, 
                              " + factor(block_ids)"))
    
  } else {
    fmla <- as.formula(paste0(x, " ~ ",  paste(treatments, collapse = " + "), " + ", 
                              interacted_treatments, " + ",
                              paste0(controls[[count]], collapse = " + "), " + ",
                              level, " + ",
                              paste0(refuse[[count]], collapse = " + "),
                              " + factor(block_ids)"))
  }

  nam <- paste("lm_", count, sep = "")
  assign(nam, lm(fmla, weights = weight,  data = merged))
  
  lm_list[[count]] <- get(nam, envir = globalenv())
  count <- count + 1
}

omit_var <- c("Constant", "block_ids",  unlist(controls), refuse7, refuse10, level)

# Generate one-sided p-values?
HEs3 <- stargazer(lm_list,
                  header = FALSE,
                  font.size = "tiny",
                  label = "tab:HEs3",
                  dep.var.labels.include = FALSE,
                  table.placement = "H",
                  dep.var.caption = "",
                  column.labels = dep_var,
                  covariate.labels = covariates,
                  omit = omit_var, 
                  omit.stat=c("f", "ser","adj.rsq"), 
                  column.sep.width = "0pt",
                  add.lines = list(c("Distance Mean", round(rep(weighted.mean(merged$distance_dates, w = merged$weight), 11), 3))),
                  title = "Heterogeneous effects on main outcomes by distance from endline survey completion and end of intervention",
                  type = "latex")

note.latex <- paste("\\multicolumn{12}{l} {\\parbox[t]{20cm}{ \\textit{Notes:} 
We report estimates from WGLS regressions where the weights are in the inverse probability of treatment 
assignment, including randomization block fixed effects. 
All regressions include controls for all baseline covariates in the outcome family and demographics.
The range of the variable", interaction_name, "is between 6 and 38, to the distance in days between the completion of the endline survey and the end of the intervention.
* denotes p$<$0.1, ** denotes p$<$0.05, and *** denotes p$<$0.01.}} \\\\")
HEs3[grepl("Note", HEs3)] <- note.latex

cat(alignTable(HEs3, 2.25), file = 'Tables/S35.tex')
