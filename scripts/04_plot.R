##############
# LOAD ASDRS #
##############

ASDRs <- read.csv(here::here("out-data", "ASDRs-2019-2024.csv"))

## loop over standard populations
walk(c("2000", "2020"), function(p){

  ## select appropriate ASDRs
  if(p == "2000"){
    
  ASDRs <- 
    ASDRs %>% 
    rename(Mx = Mx.2000)
  
  }else{
    
  ASDRs <- 
    ASDRs %>% 
    rename(Mx = Mx.2020)
    
  }
  
########
# PLOT #
########

## generate data frame for plotting
plot.df <-
  ASDRs %>% 
  ## restrict to combined ASDRS
  filter(Sex == "Total") %>% 
  ## combine all drug-induced deaths and all alcohol-induced deaths
  mutate(Cause = case_when(grepl("drugs only", Cause) | Cause == "Other drug-induced" ~ "Drug-induced",
                           grepl("alcohol only", Cause) | Cause == "Other alcohol-induced" ~ "Alcohol-induced",
                           TRUE ~ Cause)) %>% 
  summarize(Mx = sum(Mx), .by = c(Year, Sex, Cause)) %>% 
  ## add zero rates where causes are missing
  complete(Year, Sex, Cause, fill = list(Mx = 0)) %>% 
  ## generate indicator variables for plotting
  mutate(Indicator2 = case_when(Year %in% c(2019, 2024) & Cause == "COVID-19" ~ "1", 
                                Year %in% 2020:2023 & Cause == "COVID-19" ~ "2",
                                Year %in% c(2019, 2024) & !grepl("COVID-19|Alcohol|Drug|Substances|Residual", Cause, ignore.case = TRUE) ~ "3", 
                                Year %in% 2020:2023 & !grepl("COVID-19|Alcohol|Drug|Substances|Residual", Cause, ignore.case = TRUE) ~ "4",
                                Year %in% c(2019, 2024) & (grepl("Alcohol|Drug|Substances", Cause, ignore.case = TRUE)) ~ "5", 
                                Year %in% 2020:2023 & (grepl("Alcohol|Drug|Substances", Cause, ignore.case = TRUE)) ~ "6",
                                Year %in% c(2019, 2024) & Cause == "Residual" ~ "7", 
                                Year %in% 2020:2023 & Cause == "Residual" ~ "8"),
         Indicator1 = case_when(Indicator2 %in% c("1", "2") ~ "1",
                                Indicator2 %in% c("3", "4") ~ "2",
                                Indicator2 %in% c("5", "6") ~ "3",
                                Indicator2 %in% c("7", "8") ~ "4")) 

## arrange causes of death
## COVID-19 & residual
causes.1 <- c("COVID-19", "Residual")

## external causes
causes.3 <- c("Drug-induced", "Alcohol-induced", "Unintentional injuries, excl. substances", "Suicide, excl. substances", "Other external, excl. substances")

## internal causes, sorted by ASDR in 2019
causes.2 <-
  plot.df[plot.df$Year == 2019 &
            !plot.df$Cause %in% c(causes.1, causes.3),] %>% 
  arrange(-Mx) %>% 
  pull(Cause)

## vector with all causes
causes <- c(causes.1, causes.2, causes.3)

## arrange facets manually
design <- "
  A###B
  CDEFG
  HIJKL
  MNOPQ
"

## change cause-of-death labels
my_labeller = as_labeller(
  c(
    "COVID-19" = "COVID-19^a",
    "Residual" = "Residual",
    "Heart disease" = "Heart~d.^list(b,c)",
    "Cancer" = "Cancer",
    "Neurodegenerative diseases" = "Neurodegenerative~d.^c",
    "Respiratory diseases" = "Respiratory~d.",
    "Stroke" = "Stroke^b",
    "Diabetes" = "Diabetes",
    "Hypertension and arterial diseases" = "Hypertension~and~arterial~d.^b",
    "Kidney disease" = "Kidney~d.",
    "Septicemia" = "Septicemia",
    "Other chronic liver disease and cirrhosis" = "Chronic~liver~d.~and~cirrhosis^c",
    "Drug-induced" = "Drug-induced",
    "Alcohol-induced" = "Alcohol-induced",
    "Unintentional injuries, excl. substances" = "Unintentional~injuries^c",
    "Suicide, excl. substances" = "Suicide^c",
    "Other external, excl. substances" = "Other~external^c"
  ), 
  default = label_parsed
)

## determine y axis range
## all causes except COVID-19
ranges <-
  plot.df %>% 
  mutate(Cause = factor(Cause, levels = causes)) %>% 
  filter(!Cause %in% "COVID-19") %>% 
  summarize(min = min(Mx),
            max = max(Mx), .by = Cause) %>% 
  mutate(min = plyr::round_any(min, f = floor, accuracy = 5),
         max = ifelse(substr(round(max), 
                             nchar(round(max)),
                             nchar(round(max))) < 5,
                             plyr::round_any(max, f = ceiling, accuracy = 10),
                             plyr::round_any(max, f = ceiling, accuracy = 10) + 10)) %>% 
  mutate(diff = max(max - min),
         max = min + diff) %>% 
  arrange(Cause) %>% 
  select(-diff)

## separately for COVID-19
ranges <-
  bind_rows(
    data.frame(Cause = "COVID-19", 
               min = 0, 
               max = ifelse(substr(round(max(plot.df$Mx[plot.df$Cause == "COVID-19"])), 
                                   nchar(round(max(plot.df$Mx[plot.df$Cause == "COVID-19"]))),
                                   nchar(round(max(plot.df$Mx[plot.df$Cause == "COVID-19"])))) < 5,
                            plyr::round_any(max(plot.df$Mx[plot.df$Cause == "COVID-19"]), f = ceiling, accuracy = 20),
                            plyr::round_any(max(plot.df$Mx[plot.df$Cause == "COVID-19"]), f = ceiling, accuracy = 20) + 10)),
    ranges
    )

## pull minimums / maximums for each cause
min <- ranges$min
max <- ranges$max

## plot
plot.1 <-
  ggplot(plot.df %>% mutate(Cause = factor(Cause, levels = causes)), 
         aes(x = Year, y = Mx)) +
  ## add horizontal line at 2019 value
  geom_hline(data = . %>% filter(Year == 2019), 
             aes(yintercept = Mx), 
             linewidth = 1, color = "gray80") +
  geom_line(aes(group = Cause, color = Indicator1), 
            linewidth = 1.5, linetype = "dashed", show.legend = FALSE) +
  geom_point(aes(fill = Indicator2), 
             pch = 21, size = 4, color = "black", show.legend = FALSE) +
  ## add ASDR labels
  geom_text(aes(label = sprintf("%.1f", Mx)), 
            vjust = -1, size = 3.5) +
  ## assign colors to causes of death
  scale_color_manual("", values = c("1" = "#FDBF6F", "2" = "#A6CEE3", "3" = "#FB9A99",  "4" = "#CAB2D6")) + 
  scale_fill_manual("", values = c("1" = "#FF7F00", "2" = "#FDBF6F",  "3" = "#1F78B4", "4" = "#A6CEE3", "5" = "#E31A1C", "6" = "#FB9A99", "7" = "#6A3D9A", "8" = "#CAB2D6")) + 
  theme_bw() +
  theme(strip.background = element_rect(fill = "white", color = "white"),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.line = element_line(color = "black"),
        strip.text = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 14)) +
  ggh4x::facet_manual(vars(Cause), design = design, scale = "free", labeller = my_labeller) +
  ## manually adjust axis range
  ggh4x::facetted_pos_scales(x = list(
    
    Cause == causes[1] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)), 
    Cause == causes[2] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)), 
    Cause == causes[3] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[4] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[5] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[6] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[7] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[8] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[9] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[10] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[11] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[12] ~ scale_x_continuous(breaks = 2019:2024, labels = rep("", 6)),
    Cause == causes[13] ~ scale_x_continuous(breaks = 2019:2024, labels = c("2019", "'20", "'21", "'22", "'23", "2024")),
    Cause == causes[14] ~ scale_x_continuous(breaks = 2019:2024, labels = c("2019", "'20", "'21", "'22", "'23", "2024")),
    Cause == causes[15] ~ scale_x_continuous(breaks = 2019:2024, labels = c("2019", "'20", "'21", "'22", "'23", "2024")),
    Cause == causes[16] ~ scale_x_continuous(breaks = 2019:2024, labels = c("2019", "'20", "'21", "'22", "'23", "2024")),
    Cause == causes[17] ~ scale_x_continuous(breaks = 2019:2024, labels = c("2019", "'20", "'21", "'22", "'23", "2024"))
    
  ),
  
  y = list(
    
    Cause == causes[1] ~ scale_y_continuous(limits = c(min[1], max[1]), breaks = c(min[1], max[1])), 
    Cause == causes[2] ~ scale_y_continuous(limits = c(min[2], max[2]), breaks = c(min[2], max[2])), 
    Cause == causes[3] ~ scale_y_continuous(limits = c(min[3], max[3]), breaks = c(min[3], max[3])),
    Cause == causes[4] ~ scale_y_continuous(limits = c(min[4], max[4]), breaks = c(min[4], max[4])),
    Cause == causes[5] ~ scale_y_continuous(limits = c(min[5], max[5]), breaks = c(min[5], max[5])),
    Cause == causes[6] ~ scale_y_continuous(limits = c(min[6], max[6]), breaks = c(min[6], max[6])),
    Cause == causes[7] ~ scale_y_continuous(limits = c(min[7], max[7]), breaks = c(min[7], max[7])),
    Cause == causes[8] ~ scale_y_continuous(limits = c(min[8], max[8]), breaks = c(min[8], max[8])),
    Cause == causes[9] ~ scale_y_continuous(limits = c(min[9], max[9]), breaks = c(min[9], max[9])),
    Cause == causes[10] ~ scale_y_continuous(limits = c(min[10], max[10]), breaks = c(min[10], max[10])),
    Cause == causes[11] ~ scale_y_continuous(limits = c(min[11], max[11]), breaks = c(min[11], max[11])),
    Cause == causes[12] ~ scale_y_continuous(limits = c(min[12], max[12]), breaks = c(min[12], max[12])),
    Cause == causes[13] ~ scale_y_continuous(limits = c(min[13], max[13]), breaks = c(min[13], max[13])),
    Cause == causes[14] ~ scale_y_continuous(limits = c(min[14], max[14]), breaks = c(min[14], max[14])),
    Cause == causes[15] ~ scale_y_continuous(limits = c(min[15], max[15]), breaks = c(min[15], max[15])),
    Cause == causes[16] ~ scale_y_continuous(limits = c(min[16], max[16]), breaks = c(min[16], max[16])),
    Cause == causes[17] ~ scale_y_continuous(limits = c(min[17], max[17]), breaks = c(min[17], max[17]))
    
    
    )
  ) +
  coord_cartesian(xlim = c(2018.75, 2024.25)) +
  labs(x = "", 
       y = "Deaths per 100,000 U.S. standard population")

#############
# SAVE DATA #
#############

## save plot data
write.csv(plot.df %>% select(-c(Indicator1, Indicator2)), 
          here::here("out-data", paste0("plot-data-", p, ".csv")), row.names = FALSE)

#############
# SAVE PLOT #
#############

ggsave(here::here("out-figures", paste0("plot-1-", p, ".svg")), plot.1, width = 15, height = 9)
ggsave(here::here("out-figures", paste0("plot-1-", p, ".pdf")), plot.1, width = 15, height = 9)

})
