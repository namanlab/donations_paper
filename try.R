library(tidyverse)
library(tidygraph)
library(ggraph)
library(stringr)
library(xml2)
doc <- read_xml("projects.xml")

# View root node
xml_root(doc)
nodes <- xml_children(doc)
df <- nodes %>%
  map_df(function(node) {
    as_list(node) %>%
      map_chr(~ ifelse(is.null(.x), NA, as.character(.x))) %>%
      tibble(!!!.)
  })
write.csv(df, "donations_data.csv", row.names = F)

options(scipen = 999)
df %>% filter(country == "Ukraine") %>% 
  mutate(funding = as.numeric(funding), 
         goal = as.numeric(goal),
         approvedDate = as.POSIXct(approvedDate)) %>%
  ggplot(aes(x = approvedDate, y = funding + 1)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  geom_vline(
    xintercept = as.POSIXct("2022-02-24", tz = "UTC"),
    linetype = "dashed"
  )

df %>% filter(country == "Palestine") %>% 
  mutate(funding = as.numeric(funding), 
         goal = as.numeric(goal),
         approvedDate = as.POSIXct(approvedDate)) %>%
  ggplot(aes(x = approvedDate, y = funding + 1)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  geom_vline(
    xintercept = as.POSIXct("2023-10-07", tz = "UTC"),
    linetype = "dashed"
  )





df %>% filter(country == "Ukraine") %>% 
  mutate(funding = as.numeric(funding), 
         goal = as.numeric(goal),
         approvedDate = as.POSIXct(approvedDate)) %>%
  mutate(week = paste0(year(approvedDate), "-", month(approvedDate))) %>%
  group_by(week)  %>%
  summarise(funding = sum(funding)) %>%
  mutate(week = ym(week)) %>%
  ggplot(aes(x = week, y = funding + 1)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  geom_vline(
    xintercept = ymd("2022-02-24"),
    linetype = "dashed"
  )

df %>% filter(country == "Palestine") %>% 
  mutate(funding = as.numeric(funding), 
         goal = as.numeric(goal),
         approvedDate = as.POSIXct(approvedDate)) %>%
  mutate(week = paste0(year(approvedDate), "-", month(approvedDate))) %>%
  group_by(week)  %>%
  summarise(funding = sum(funding)) %>%
  mutate(week = ym(week)) %>%
  ggplot(aes(x = week, y = funding + 1)) +
  geom_point() +
  theme_bw() +
  scale_y_log10() +
  geom_vline(
    xintercept =ymd("2023-10-07"),
    linetype = "dashed"
  )
  