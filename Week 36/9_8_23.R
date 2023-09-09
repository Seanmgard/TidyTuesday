
# Load Packages -----------------------------------------------------------

pacman::p_load(tidytuesdayR, gridExtra, tidyverse, ggtext, showtext)

# Load tidytuesday data ---------------------------------------------------

demographics <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/demographics.csv')
wages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/wages.csv')
states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-09-05/states.csv')

# Fonts and Background Formatting -----------------------------------------

line_color <- c("#cba135", "#87ceeb")

#Specify font
font_add_google("Lato", "lato")
showtext_auto()

# Clean Data --------------------------------------------------------------

wages_long <- wages %>% 
  pivot_longer(cols=c(union_wage, nonunion_wage),
               names_to= "Wage_Type",
               values_to="Wage")

Included_Categories <- c("all wage and salary workers", 
                         "construction", 
                         "private sector: manufacturing", 
                         "private sector: nonagricultural", 
                         "public administration", 
                         "public sector: federal", 
                         "public sector: local government", 
                         "public sector: state government", 
                         "wholesale/retail")

Filtered_wages <- wages_long %>% 
  filter(facet %in% Included_Categories)

value_mapping <- c(
  "all wage and salary workers" = "All Wage and Salary Workers",
    "construction" = "Construction",
  "private sector: manufacturing" = "Manufacturing",
  "private sector: nonagricultural" = "Nonagricultural",
  "public administration" = "Public Administration",
  "public sector: federal" = "Federal Government",
  "public sector: local government" = "Local Government",
  "public sector: state government" = "State Government",
  "wholesale/retail" = "Wholesale/Retail")

Filtered_wages_Renamed <- Filtered_wages %>% 
  mutate(facet = ifelse(facet %in% names(value_mapping), value_mapping[facet], facet))

# Visualize Data ----------------------------------------------------------

#Title and Subtitle Text
Title <- "Union vs. Non-Union Wages Over Time (1970 - 2022)"
Subtitle <- "The raw increase in wages for union and nonunion members over the past 50 years."
Caption <- "Data from Union Membership, Coverage, and Earnings from the CPS. #TidyTuesday Week 36. Chart by Sean Gardner (seanmgard.com)"

# Create a line chart with facets
Union_Membership_Chart <- ggplot(Filtered_wages_Renamed, aes(x = year, y = Wage, color = Wage_Type)) +
  geom_line(linewidth=1.0) +
  labs(x = "Year", y = "Wage", title= Title, subtitle = Subtitle, caption = Caption) +
  facet_wrap(~ facet, scales = "free_y") +
  scale_colour_manual(values = line_color) +
  scale_y_continuous() +
  theme_void() +
  theme(
    plot.title.position   = "plot",
    plot.caption.position = "plot",
    legend.position       = 'plot',
    axis.title = element_blank(),
    axis.text = element_text(size = 8, colour = "white", family = "lato"),
    strip.text = element_text(size = 10, color = "white", face = "bold", family = "lato"),
    text = element_text(size = 12, lineheight = 0.3, colour = "white", family = "lato"),
    plot.background = element_rect(fill = '#1E212B', colour = '#1E212B'),
    plot.title            = element_text(color="white", face="bold", family = "lato", size=26, margin=margin(t=10)),
    plot.subtitle         = element_markdown(color= "white", size=13, family = "lato", margin=margin(t = 5, b = 20)),
    plot.caption = element_markdown(colour = "grey", hjust = 0, family = "lato", margin = margin(t = 20)),
    plot.margin = margin(b = 20, t = 20, r = 20, l = 20)
  )
