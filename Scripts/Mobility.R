# Mobility analysis plots 

# Read dataset
mobility <- read.csv('Datasets/Mobility/2020_EG_Region_Mobility_Report.csv') %>% dplyr::select(-c(1,3:8)) %>% 
  group_by(date, country_region) %>% summarise_all(mean, na.rm = T)

mobility$date_num <- 1:dim(mobility)[1]

# We define ggplot setup -------------------------------------------------------
legend.text.18 <- element_text(color = "black", size = 18, family="Arial")
final.text.14 <- element_text(color = "black", size = 14, hjust = 0.5, family="Arial")
final.text.12 <- element_text(color = "black", size = 12, hjust = 0.5, family="Arial")

# Define colors for all graphs
colors <- c("#0072B2","#D55E00")
th_egypt <- theme(panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(),
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.title.x=element_blank(), 
                  axis.title.y=element_blank(),
                  panel.border = element_blank())

#  -----------------------------------------------------------------------------
names <- names(mobility)[3:8]

maxs <- sapply(mobility[names], function(x) max(x) )  
mins <- sapply(mobility[names], function(x) min(x) )  

# Create a plot for each variable (From July 10, 2020 through September 5, 2020.)

ggplot(mobility, aes(date, retail_and_recreation_percent_change_from_baseline)) +
  geom_point(color = '#0072B2') +
  th_egypt +
  ylab('Percent change from baseline') +
  geom_vline(xintercept = c('2020-07-10', '2020-09-05'), color = '#D55E00') + # 147 and 204
  scale_x_discrete(breaks = c('2020-02-15', '2020-07-10', '2020-09-05', '2020-12-31'),
                   expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = final.text.14, axis.text.y = final.text.14) +
  ylim((mins[1]), (maxs[1] + 5))


list_of_plots <- list()

for (i in 1:6) {
  
  name <- paste("plot_", i, " <-", sep = "")
  
  content <- paste0("ggplot(mobility, aes(date,", names[i], ")) +", 
  "geom_point(color = '#0072B2') +
  th_egypt +
  ylab('Percent change from baseline') +
  geom_vline(xintercept = c('2020-07-10', '2020-09-05'), color = '#D55E00') + 
  scale_x_discrete(breaks = c('2020-02-15', '2020-07-10', '2020-09-05', '2020-12-31'),
  expand = expansion(mult = c(0, 0.1))) +
  theme(axis.text.x = final.text.12, axis.text.y = final.text.14) +
  ylim(", (mins[i]), ",", (maxs[i] + 5), ")")
                    

  eval(parse(text = paste0(name, content)))
  
  nam = substr(name, 1, nchar(name)-3)
  
  list_of_plots[[i]] <- get(nam, envir = globalenv())
  
}

# Save all plots created
save_names <- paste0("mobility_", 1:6, ".pdf")

for (i in 1:6) {
  ggsave(get(paste0("plot_", i), envir = globalenv()) , path = 'Figures/Mobility', filename = save_names[i], device = cairo_pdf, 
         width = 7, height = 5, dpi = 300)
}



# # Example for one plot ---------------------------------------------------------
# ggplot(mobility, aes(date_num, retail_and_recreation_percent_change_from_baseline)) +
#   geom_point(color = "#0072B2") +
#   th_egypt +
#   ylab("") +
#   geom_vline(xintercept = c(155, 204), color = "#D55E00") + # July 18, 2020 through September 5, 2020.
#   xlim(0, 321) +
#   ylim(-75,25)


