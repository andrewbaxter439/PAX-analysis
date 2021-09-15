library(visNetwork)
library(tidyverse)
library(lubridate)
library(igraph)
library(magrittr)
library(export)

# Function to search countries --------------------------------------------
# countries <- str_split("United Kingdom, Austria, Ireland, Croatia, Slovak, Slovenia, New Zealand, Finland, Estonia", ", ")[[1]]
pax_dat <- read_csv(paste0("Data/all-paxdata.csv"))
country_pax <- function(..., dat = pax_dat){
  
search_string <- paste(c(...), collapse = "|")

# dat <- read_csv(paste0("Data/all-paxdata.csv"))
if ("Ireland" %in% c(...)) {
  dat <- dat %>% mutate_at(vars(-Dat, -GeWom), ~ str_replace_all(., "Northern Ireland", "NI"))
} 

dat_tidy <- dat %>% 
  # mutate(Dat = dmy(Dat)) %>% 
  filter(Dat >= dmy("01/01/2000")) %>% 
  # mutate(PPName = str_replace_all(PPName, "Northern Ireland", "NI"))
  # mutate_all(~ str_replace_all(., "Northern Ireland", "NI")) %>% 
  filter_all(any_vars(str_detect(., search_string)))

dat_tidy %>%
  mutate(ThrdParty = ifelse(is.na(ThrdPart), "none", ThrdPart)) %>% 
  mutate(InName = as.integer(str_detect(.$Agt, search_string)),
         Con = as.integer(str_detect(.$Con, search_string)),
         Part = as.integer(str_detect(.$Part, search_string)),
         ThrdPart= as.integer(str_detect(.$ThrdParty, search_string))) %>% 
  mutate(Country = c(...)[1],
         Contry_of_conflict = Con,
         Party_or_mediator = ifelse(Con==0 & Part ==1, 1, 0),
         Observer_or_3rdparty = ifelse(Con==0 & Part ==0 & ThrdPart == 1, 1, 0),
         Host = ifelse(Con == 0 & Part == 0 & ThrdPart == 0 & InName == 1, 1, 0),
         AgtId = factor(AgtId),
         PP = factor(PP)) %>% 
  select(Country, AgtId, PPName, PP, Dat, Contry_of_conflict, Party_or_mediator, Observer_or_3rdparty, Host, GeWom)

}


# all countries -----------------------------------------------------------

countries_plus <- list(
  c("United Kingdom", "UK", "London", "Britain"),
  c("Ireland", "Irish", "Dublin"),
  c("Estonia", "Tallinn"),
  c("Austria", "Vienna"),
  c("Croatia", "Croat", "Zagreb"),
  c("Slovakia", "Slovak", "Bratislava"),
  c("Slovenia", "Ljubljana"),
  c("Finland", "Finnish", "Helsinki"),
  c("New Zealand", "Wellington"),
  c("Denmark", "Danish", "Copenhagen"),
  c("Norway", "Norwegian", "Oslo"),
  c("Sweden", "Swed", "Stockholm")
)

all_countries_plus <- map(countries_plus, country_pax)

countries_data_tidy_plus <- reduce(all_countries_plus, bind_rows)

countries_data_tidy_plus

countries_data_tidy_plus %>% 
  group_by(Country, GeWom) %>% 
  summarise(
    Agts_con = sum(Contry_of_conflict, na.rm = TRUE),
    Agts_par = sum(Party_or_mediator, na.rm = TRUE),
    Agts_Obs = sum(Observer_or_3rdparty, na.rm = TRUE),
    Agts_host = sum(Host, na.rm = TRUE)
  ) %>% 
  # arrange(Country, GeWom) %>% 
  # mutate(GeWom = fct_inorder(GeWom)) %>% 
  gather("var", "val", 3:6) %>% 
  group_by(Country, var) %>% 
  mutate(lab_y = (val/(sum(val)) + (1-(val/(sum(val))))*(GeWom-1)*-1)/2
         ) %>% 
  mutate(GeWom = factor(GeWom)) %>%
  # mutate(lab_y = 0.5*val) %>% 
  ggplot(aes(x = var, y = val, fill = factor(GeWom))) +
  geom_bar(stat = "identity", position = "fill") +
  coord_flip() +
  theme_minimal(base_family = "IBM Plex Sans") +
  theme(text = element_text(family = "sans", size = 12),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank()) +
  geom_text(aes(label = ifelse(val==0, NA, val), y = lab_y), hjust = 0.5, position = "stack", size = 4) +
  facet_wrap(~ Country, scales = "free") +
  scale_fill_manual(values = c("#FFcc00", "#5566FF"),
                    name = "Reference to Gender\nin agreement",
                    breaks = c(0,1),
                    labels = c("No reference\nto Gender", "Specific Reference\nto Gender")) +
  scale_y_continuous(name = "Number of agreements signed since January 2000", labels = NULL,expand = c(0, 0)) +
  scale_x_discrete(labels = c("As affected\ncountry", "As Party", "As 3rd party/\nObserver", "Host"),
                   breaks = c("Agts_con", "Agts_par", "Agts_Obs", "Agts_host"),
                   limits = c("Agts_host", "Agts_Obs", "Agts_par", "Agts_con"),
                   name = NULL)

# ggsave("pax_graph2.png", units = "mm", width = 320, height = 180)
# export::graph2ppt(last_plot(), "pax_graph.pptx", aspectr = 16/9, width = 12)

# igraph? -----------------------------------------------------------------

pax_net <- countries_data_tidy_plus %>% graph_from_data_frame()
spot_cols <- countries_data_tidy_plus %>% 
  select(AgtId, GeWom, Contry_of_conflict, Party_or_mediator, Observer_or_3rdparty, Host) %>% 
  # unique() %>% 
  mutate(col = ifelse(GeWom == 1,"#5566FF", "#FFcc00"),
         cat = ifelse(Contry_of_conflict == 1, 1,
                       ifelse(Party_or_mediator == 1, 2, 
                              ifelse(Observer_or_3rdparty == 1, 3, 4))) 
         )

V(pax_net)$color[1:12] <- "skyblue"
V(pax_net)$color[13:118] <- spot_cols[which(spot_cols$AgtId %in% V(pax_net)$name[13:118]), "col"]$col
V(pax_net)$frame.color <- NA
V(pax_net)$size[1:12] <- 10
V(pax_net)$size[13:118] <- 5
V(pax_net)$label[1:12] <- gsub(" ", "\n", V(pax_net)$name[1:12])
V(pax_net)$label[13:118] <- NA
V(pax_net)$label.cex <- 0.7
E(pax_net)$arrow.mode <- 0
  

ni_pp <- countries_data_tidy_plus %>% filter(PP == 68) %>% pull(AgtId)
phil_pp <- countries_data_tidy_plus %>% filter(PP == 95) %>% pull(AgtId)
afg_pp <- countries_data_tidy_plus %>% filter(PP == 2) %>% pull(AgtId)
sri_pp <- countries_data_tidy_plus %>% filter(PP == 124) %>% pull(AgtId)

groups_list <- list(which(V(pax_net)$name %in% ni_pp$AgtId),
                    which(V(pax_net)$name %in% phil_pp$AgtId),
                    which(V(pax_net)$name %in% sri_pp$AgtId),
                    which(V(pax_net)$name %in% afg_pp$AgtId),
                    which(V(pax_net)$name %in% ukr_pp$AgtId))

tkplot(pax_net, mark.groups = groups_list, mark.col = NA)
coords <-  tk_coords("1")/500

svg("igraph_plot.svg")
plot(pax_net, mark.groups = groups_list, layout = coords, mark.col = NA,
     rescale = FALSE,
     xlim = c(0,max(coords[,1])),
     ylim = c(0,max(coords[,2])))
dev.off()

svg("igraph_plot_ng.svg")
plot(pax_net, layout = coords,
     rescale = FALSE,
     xlim = c(0,max(coords[,1])),
     ylim = c(0,max(coords[,2])))
dev.off()



# legend ------------------------------------------------------------------

legend_df <- tibble(`Peace Process` = factor(unique(groups$PPName)[c(1,2,4,3,5)]),
       col = rainbow(5),
       y = 1:5,
       x = 1:5)

col_vectors <- legend_df$col
names(col_vectors) <- legend_df$`Peace Process`

legend_df %>% 
  ggplot(aes(x,y,col = `Peace Process`)) +
  geom_line(size = 2) + 
  scale_color_manual(values = col_vectors) + 
  theme_void()

graph2ppt(last_plot(), "igraph_legend.pptx")
# visNetwork --------------------------------------------------------------

ni_pp   <- countries_data_tidy_plus %>% filter(PP == 68)  %>% select(PPName, PP, AgtId)
phil_pp <- countries_data_tidy_plus %>% filter(PP == 95)%>% select(PPName, PP, AgtId)
afg_pp  <- countries_data_tidy_plus %>% filter(PP == 2)  %>% select(PPName, PP, AgtId)
sri_pp  <- countries_data_tidy_plus %>% filter(PP == 124)%>% select(PPName, PP, AgtId)
ukr_pp  <- countries_data_tidy_plus %>% filter(PPName == "Ukraine peace process")%>% select(PPName, PP, AgtId)

groups <- bind_rows(
  ni_pp %>% filter(PPName == "Northern Ireland peace process"),
  phil_pp,
  afg_pp,
  sri_pp,
  ukr_pp
) %>% 
  mutate(id = AgtId)

nodes %>% 
  group_by(title) %>% 
  count() %>% 
  arrange(desc(n))
  

nodes <- rbind(c_nodes, a_nodes) %>% 
  mutate(id = as.character(id),
         font.size = 30,
         group = Type)

visgraph <- visNetwork(nodes = nodes, edges = links %>% mutate_all(as.character),
           height = "900px",
           width = "1200px"
           )

visgraph <- visNetwork(nodes = nodes, edges = links %>% mutate_all(as.character),
           height = "900px",
           width = "1200px"
# ) %>% visGroups(groupname = "Country", shape = "triangle") %>% 
) %>% visGroups(groupname = "Country", shape = "icon", icon = list(code = "f024", size = 100),
                shadow = list(enabled  = TRUE)) %>%
  visGroups(groupname = "Agt", shape = "circle") %>% 
  addFontAwesome()

visSave(visgraph, "networkgraph_2.html")

# summarise data for country ----------------------------------------------

country_summary <- function(Country1 = NA){
  
  countries <- countries_data_tidy_plus %>% 
    select(Country) %>% 
    unique() %>% 
    arrange(Country) %>% 
    mutate(num = 1:nrow(.)) %>% 
    select(num, Country)
  
  if (is.na(Country1)) {
    print(countries)
    num <- readline(prompt = "Please choose a country: ")
    Country1 <- countries[[num,"Country"]]
  }
  
countries_data_tidy_plus %>% 
  filter(Country == Country1) %>% 
  arrange(Dat) %>% 
  group_by(PPName) %>% 
  summarise(nAgg = n()) %>% 
  transmute(text = paste0(PPName, " \U2012 ", nAgg, " peace agreement", ifelse(nAgg>1, "s", ""))) %>% 
  pull(text) %>% 
  paste(collapse = "; ") %>% 
  paste("Peace Processes:", .) %>% 
    print()
  
  countries_data_tidy_plus %>% 
    group_by(Country, GeWom) %>% 
    summarise(
      Agts_con = sum(Contry_of_conflict, na.rm = TRUE),
      Agts_par = sum(Party_or_mediator, na.rm = TRUE),
      Agts_Obs = sum(Observer_or_3rdparty, na.rm = TRUE),
      Agts_host = sum(Host, na.rm = TRUE)
    ) %>% 
    filter(Country == Country1) %>%
    # filter(., GeWom == 1)
    bind_rows(summarise_at(., -1, sum) %>% mutate(count = "Total"), filter(., GeWom == 1) %>% select(-GeWom) %>% mutate(count = "mentions women")) %>% 
    filter(!is.na(count)) %>% 
    select(Country, count, 3:6) %>% 
    print()
}
