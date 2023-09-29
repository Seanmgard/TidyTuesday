
# Load Packages -----------------------------------------------------------

pacman::p_load(tidytuesdayR, gridExtra, tidyverse, ggtext, showtext, ggimage, ggrepel, scales, png)

# Load tidytuesday data ---------------------------------------------------

tuesdata <- tidytuesdayR::tt_load('2023-09-26')

richmondway <- tuesdata$richmondway

# Manipulate Data ---------------------------------------------------------

Fuck_Average <- mean(richmondway$F_count_RK)

richmondway <- richmondway %>% 
  mutate(Fuck_Index = (F_count_RK / Fuck_Average)-1)

#Clean wording of Season and episode labels
richmondway$Season_Episode <- gsub("S", "Season ", richmondway$Season_Episode)
richmondway$Season_Episode <- gsub("_e", " Episode ", richmondway$Season_Episode)

# Specify Font -----------------------------------------------------------

#Specify font
font_add_google("Lato", "lato")
showtext_auto()

# Title and Subtitle Text ----------------------------------------------------------------

Title <- "TED LASSO - THE ROY KENT F*** INDEX"
Caption <- "Data from Deepsha Menghani. #TidyTuesday Week 39. Chart by Sean Gardner (seanmgard.github.io)"
subtitle <- 
"Roy Kent swears a lot. Like, A LOT. 
On average, he says the f-word 
~9 times per episode. Some episodes 
he adds to Phoebe's swear jar more 
than others. 

An index of 50% means 
that Roy says the f-word 50% 
more than an average episode.

Season 1 is a bit lighter on the 
f-bombs with only 1 episode (9) 
over-indexing. 

Episodes 5 and 12 of Season 2 had 
the most f-bombs in the series with 
an index of more than 150%"

# Roy Images --------------------------------------------------------------

Happy_Roy <- "C:/Users/seanm/OneDrive/Documents/R Projects/TidyTuesday/Tidy_Tuesday/Roy_Kent_Happy.png"

Angry_Roy <- readPNG("Roy_Kent_Mad.png", native=TRUE)

# Plot Results ---------------------------------------------------------

Roy_Kent_Image <- richmondway %>% 
  ggplot(aes(x=Fuck_Index, y=reorder(Season_Episode, + Fuck_Index), fill = Fuck_Index)) +
  geom_col() +
  annotate("text", x = 1.2, y = 20.0, label = subtitle, color="white", lineheight=1.0, hjust=0, vjust=0.5, family="futura", size=4.2) +
  scale_fill_gradient2(high = '#fd5c63', low = '#78c4e3', midpoint = 0) +
  scale_x_continuous(breaks = seq(-1, 1.75, by = 0.5),
                     limits = c(-1, 1.75),
                     labels = percent) +
  geom_text(aes(label = Season_Episode,
                x = ifelse(Fuck_Index < 0, 0.15, -0.15),
                vjust = 0.5), color="white",
            size = 3) +
  geom_image(aes(image="C:/Users/seanm/OneDrive/Documents/R Projects/TidyTuesday/Tidy_Tuesday/Roy_Kent_Mad_JPG.jpg", x=-0.7, y=27), size=0.4, by="width") +
  geom_image(aes(image="C:/Users/seanm/OneDrive/Documents/R Projects/TidyTuesday/Tidy_Tuesday/Roy_Kent_Happy_JPG.jpg", x=0.7, y=10), size=0.35, by="width") +
  labs(
    x="F Count Rank",
    y= "Episode",
    title = Title,
    caption=Caption) +
  theme_void() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position = 'none',
    axis.title = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_text(size = 8, colour = "white"),
    axis.line.x = element_line(color = "gray"),
    strip.text = element_text(size = 10, color = "white", face = "bold", family = "futura"),
    text = element_text(size = 12, lineheight = 0.3, colour = "white", family = "futura"),
    plot.background = element_rect(fill = 'black', colour = 'black'),
    plot.title= element_text(color="white", face="bold.italic", family = "futura", size=26, vjust=3, hjust=0.8),
    plot.subtitle = element_text(),
    plot.caption = element_markdown(colour = "grey", hjust = 0, family = "futura", margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 20, r = 20, l = 20)
  )
  print(Roy_Kent_Image)
  
ggsave("Roy_Kent_Image.png", Roy_Kent_Image)
