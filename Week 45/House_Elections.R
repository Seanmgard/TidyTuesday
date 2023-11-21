
# Load Packages -----------------------------------------------------------

pacman::p_load(tidytuesdayR, gridExtra, tidyverse, ggtext, showtext, geofacet)


# Load Tidy Tuesday Data --------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-11-07')

house <- tuesdata$house


# Theme, Fonts, Background Formatting -------------------------------------

line_color <- c("#E81B23", "#00AEF3")
font_family <- "Georgia"
font_color <- "gray30"
bcolor <- "#F8F8F8"
  
theme_set(theme_minimal(base_size = 12, base_family = font_family))
  
Title <- "2020 US House Election Results"
Subtitle <- "Vote breakdown between Democrats, Republicans, and Third Party Candidates. Democrats ultimately maintained control of the United States House of Representatives \n
in the 2020 election, though Republicans made net gains of +14 seats."
Caption <- paste0(
  "Data from <b>MIT Election Data and Science Lab.</b> #TidyTuesday Week 45. Chart by Sean Gardner (seanmgard.github.io)"
)

# Clean Data --------------------------------------------------------------

house_updated <- house |> 
  group_by(state, state_po, year, party) |> 
  mutate(party = if_else(party == "DEMOCRATIC-NPL", "DEMOCRAT", party),
         party = if_else(party %in% c("DEMOCRAT", "REPUBLICAN"), party, "THIRD_PARTY"))

house_updated1 <- house_updated |> 
  summarise(candidatevotes = sum(candidatevotes), totalvotes = sum(totalvotes))

Test <- house_updated1 |> 
  select(state, state_po, year, party, candidatevotes) |>
  filter(year == "2020") |> 
  pivot_wider(names_from = party, values_from = candidatevotes) |> 
  mutate_all(~ replace(., is.na(.), 0)) |> 
  mutate(total_votes = DEMOCRAT + REPUBLICAN + THIRD_PARTY,
         Democrat = DEMOCRAT/total_votes,
         Republican = REPUBLICAN/total_votes,
         "Third Party" = THIRD_PARTY/total_votes)

Test2 <- Test |> 
  pivot_longer(cols = c('Democrat', "Republican", "Third Party"),
               names_to = "party",
               values_to = "vote") |> 
  mutate(state = str_to_title(tolower(state)),
         state = if_else(state == "District Of Columbia", "DC", state),
         vote = as.numeric(vote),
         party = factor(party, levels = c("Third Party", "Republican", "Democrat")))


# Plot the results --------------------------------------------------------

ggplot(Test2, aes(party, vote, fill = party)) +
  geom_col(width=0.75) +
  scale_fill_manual(values = c("#ffd58b", "#B53B2F", "#367BB2")) +
  coord_flip() +
  facet_geo(~state) +
  labs(x = "Percent of Total Vote", title= Title, subtitle = Subtitle, caption = Caption) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 12, margin = margin(r = 10), family = font_family),
    axis.text.y = element_text(size = 8, colour = font_color, family = font_family),
    axis.text.x = element_blank(),
    strip.text = element_text(face = "bold", hjust = 0, size =10),
    text = element_text(size = 12, lineheight = 0.3, colour = font_color, family = font_family),
    plot.background = element_rect(fill = bcolor, colour = '#1E212B'),
    plot.title            = element_text(color=font_color, face="bold", family = font_family, size=34, margin=margin(t=10)),
    plot.subtitle         = element_markdown(color= font_color, size=13, lineheight = 0.1, family = font_family, margin=margin(t = 5, b = 20)),
    plot.margin = margin(b = 30, t = 30, r = 70, l = 70),
    plot.caption = element_markdown(colour = "#1E212B", hjust = 0, family = "lato", margin = margin(t = 20)),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = .3, linetype = "dashed", color = "grey85"),
    legend.position = "bottom",
    legend.title = element_blank())
  
