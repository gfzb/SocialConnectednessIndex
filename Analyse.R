##########################################################################################
# 
# Facebook Social Connectedness Index
#
# Grünenfelder Zumbach GmbH - Sozialforschung und Beratung, 25.09.2020
#  
# Script      1_Analyse.R      
# Beschrieb   Script zur Analyse und Visualisierung der SCI-Daten
#
##########################################################################################

# Load packages --------------------------------------------------------------------------
pacman::p_load(readr, dplyr, stringr, ISOcodes)

# Load data ------------------------------------------------------------------------------

# country_country_Aug2020
tf_countries <- tempfile(fileext = ".tsv") 

download.file(
  url = "https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/resource/6265373e-c143-4d34-8786-641a13921173/download/country_country_aug2020.tsv",
  destfile = tf_countries
  )

countries <- read_tsv(tf_countries)

# gadm1_nuts3_counties_gadm1_nuts3_counties_Aug20
tf_regions <- tempfile(fileext = ".zip")

download.file(
  url = "https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/resource/3a98c06b-d373-45ed-a954-d93bdb12d5d0/download/gadm1_nuts3_counties_gadm1_nuts3_counties_aug2020.tsv.zip",
  destfile = tf_regions
  )

td <- tempdir()
unzip(tf_regions, exdir = td)

regions <- read_tsv(paste0(td, "/gadm1_nuts3_counties_gadm1_nuts3_counties_Aug2020.tsv"))

# Load metadata --------------------------------------------------------------------------

# gadm1_nuts3_counties_levels
tf_levels <- tempfile(fileext = ".csv")

download.file(
  url = "https://data.humdata.org/dataset/e9988552-74e4-4ff4-943f-c782ac8bca87/resource/53060517-669e-46c0-b723-3b68fa8aad80/download/gadm1_nuts3_counties_levels.csv",
  destfile = tf_levels
)

levels <- read_csv(tf_levels)

# Add metadata ---------------------------------------------------------------------------

# Country names
cnts <- ISO_3166_1 %>% 
  mutate(name = ifelse(!is.na(Common_name), Common_name, Name)) %>% 
  select(Alpha_2, Alpha_3, name) %>% 
  add_row(Alpha_2 = "XK", Alpha_3 = "XKO", name = "Kosovo")

countries <- countries %>% 
  left_join(cnts, by = c("user_loc" = "Alpha_2")) %>% 
  left_join(cnts, by = c("fr_loc" = "Alpha_2"), suffix = c("_user", "_fr"))

# Region names (non-nuts3)
levels1 <- levels %>% 
  filter(!level == "nuts3") %>% 
  mutate(Alpha_3 = str_sub(key, 1, 3)) %>% 
  left_join(cnts)

# Region names (nuts3, non-ch)
levels2 <- levels %>% 
  filter(level == "nuts3") %>% 
  filter(!str_detect(key, "CH[-0]{1,}")) %>% 
  mutate(Alpha_2 = str_sub(key, 1, 2)) %>% 
  left_join(
    cnts %>% 
      mutate(
        Alpha_2 = ifelse(Alpha_2 == "GB", "UK", Alpha_2),
        Alpha_2 = ifelse(Alpha_2 == "GR", "EL", Alpha_2)
      )
  )

# Region names (ch)
levels3 <- levels %>% 
  filter(str_detect(key, "CH[-0]{1,}")) %>%
  mutate(Alpha_2 = "CH", Alpha_3 = "CHE") %>% 
  left_join(
    tibble(
      code = c(
        "CH011", "CH012", "CH013", "CH021", "CH022", "CH023", "CH024", "CH025",
        "CH031", "CH032", "CH033", "CH040", "CH051", "CH052", "CH053", "CH054",
        "CH055", "CH056", "CH057", "CH061", "CH062", "CH063", "CH064", "CH065", 
        "CH066", "CH070"),
      name = c(
        "Vaud", "Valais", "Genève", "Bern", "Freiburg", "Solothurn", "Neuchâtel",
        "Jura", "Basel-Stadt", "Basel-Landschaft", "Aargau", "Zürich", "Glarus",
        "Schaffhausen", "Appenzell Ausserrhoden", "Appenzell Innerrhoden", "St. Gallen", 
        "Graubünden", "Thurgau", "Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden",
        "Zug", "Ticino"
      ),
      name2 = c(
        "Waadt", "Wallis", "Genf", "Bern", "Freiburg", "Solothurn", "Neuenburg",
        "Jura", "Basel-Stadt", "Basel-Landschaft", "Aargau", "Zürich", "Glarus",
        "Schaffhausen", "Appenzell Ausserrhoden", "Appenzell Innerrhoden", "St. Gallen", 
        "Graubünden", "Thurgau", "Luzern", "Uri", "Schwyz", "Obwalden", "Nidwalden",
        "Zug", "Tessin"
      ),
      name3 = c(
        "VD", "VS", "GE", "BE", "FR", "SO", "NE", "JU", "BS", "BL", "AG", "ZH", "GL",
        "SH", "AR", "AI", "SG", "GR", "TG", "LU", "UR", "SZ", "OW", "NW", "ZG", "TI"
      )
    ), by = c("key" = "code")
  )

levels <- bind_rows(levels1, levels2, levels3)

regions <- regions %>% 
  left_join(levels, by = c("user_loc" = "key")) %>% 
  left_join(levels, by = c("fr_loc" = "key"), suffix = c("_user", "_fr"))

# Analysis Country-Level -----------------------------------------------------------------

# Median International Connectedness
countries_overall_median <- countries %>%
  mutate(!name_user == name_fr) %>% 
  group_by(name_user) %>% 
  summarise(scaled_sci_median = median(scaled_sci)) %>% 
  arrange(desc(scaled_sci_median))

# Analysis Region-Level ------------------------------------------------------------------

# CH regions
regions_ch <- regions %>% 
  filter(Alpha_2_user == "CH") %>% 
  filter(Alpha_2_fr == "CH")

# Median Cantonal Connectedness
regions_ch_overall_median <- regions_ch %>%
  mutate(!name_user == name_fr) %>% 
  group_by(name_user) %>% 
  summarise(scaled_sci_median = median(scaled_sci)) %>% 
  arrange(desc(scaled_sci_median))

# Min Cantonal Connectedness
regions_ch_overall_min <- regions_ch %>%
  mutate(!name_user == name_fr) %>% 
  group_by(name_user) %>% 
  summarise(scaled_sci_min = min(scaled_sci)) %>% 
  arrange(desc(scaled_sci_min))

# Min Internatioanl-Cantonal Connectedness
regions_ch_int_median <- regions %>% 
  filter(Alpha_2_user == "CH") %>% 
  filter(!Alpha_2_fr == "CH") %>% 
  group_by(name_user) %>% 
  summarise(scaled_sci_median = median(scaled_sci)) %>% 
  arrange(desc(scaled_sci_median))

# Cantonal Connectedness   
regions_ch2 <- regions_ch %>% 
  select(name2_user, name_fr, name2_fr, scaled_sci) %>% 
  filter(!is.na(name2_fr)) %>% 
  mutate(group = case_when(
    scaled_sci >= 413428.0 ~ 9,
    scaled_sci < 413428.0 & scaled_sci >= 194930.0 ~ 8,
    scaled_sci < 194930.0 & scaled_sci >= 119345.0 ~ 7,
    scaled_sci < 119345.0 & scaled_sci >= 87289.0 ~ 6,
    scaled_sci < 87289.0 & scaled_sci >= 69810.5 ~ 5,
    scaled_sci < 69810.5 & scaled_sci >= 52229.0 ~ 4,
    scaled_sci < 52229.0 & scaled_sci >= 33304.0 ~ 3,
    scaled_sci < 33304.0 & scaled_sci > 24377 ~ 2,
    scaled_sci <= 24377 ~ 1
    )) %>% 
  select(name2_user, name_fr, group, scaled_sci, name2_fr) %>% 
  group_by(name2_user) %>% 
  mutate(Rang = -1 * (rank(scaled_sci) - 26)) %>% 
  mutate(Rang = ifelse(Rang > 0, paste0("<br>Rang: ", Rang, "/25"), "")) %>% 
  mutate(Odds = max(scaled_sci) / scaled_sci) %>% 
  mutate(Odds = ifelse(Odds < 10, round(Odds, 1), round(Odds, 0))) %>% 
  mutate(Odds = ifelse(Odds > 1, paste0(
    "<br><br><em>Die Wahrscheinlichkeit einer Facebook-Freundschaft zwischen den Kantonen ",
    name2_user, " und ", name2_fr, " ist <strong>", Odds, 
    "-mal kleiner</strong> als die einer Verbindung zweier User*innen aus dem Kanton ",
    name2_user, "</em>."), "")
    )