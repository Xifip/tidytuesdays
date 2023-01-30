# Get the data
#install.packages("alone") 
library(alone)

# install packages
library(tidyverse)
library(here)
library(cowplot)
library(magick)
library(scales)
library(showtext)

# set showtext options and add fonts
showtext_auto()
showtext_opts(dpi=400) 
font_add_google("Special Elite", family = "special")
font_add_google("Rajdhani", family = "rajdhani")

# set colours for fonts and background
bkgn_col <-  "#F5F5F5FF"
title_text_col <- "#424242FF"
info_text_col <- "#9E9E9EFF"

# Create dataframe for main plot
survivalists_df <- survivalists |>
  mutate(gender_fact = gender |> as.factor(),
         season_fact = season |> as.factor(),
         reason_category_fact = reason_category |> as.factor(),
         label_nudge = ifelse(days_lasted < 6, 0.2, 0),
         label_colour = ifelse(days_lasted < 6, "#9E9E9EFF", "white"),
         square_stroke = ifelse(days_lasted < 6, 2, 4)
  ) 

# Create plot helper showing how to read the main plot
explainer_plot <- ggplot() + 
  annotate("rect", xmin = 0, xmax = 0.4, ymin = 0, ymax = 0.25,
           alpha = 0.35, fill = info_text_col, colour = title_text_col, size = 2, linetype = "dotted") +  
  annotate("rect", xmin = 0.02, xmax = 0.12, ymin = 0.02, ymax = 0.12,
           alpha = 1, fill = "#01AD74FF", colour = "#DB3EB1FF", size = 2) +
  geom_text(aes(x = 0.07, y = 0.07, label = "30"), size = 20, color="White") +
  geom_text(aes(x = 0.02, y = 0.22, label = "Days Survived"), size = 10, color="White", hjust = 0) + 
  geom_text(aes(x = 0.13, y = 0.17, label = "Gender"), size = 10, color="#DB3EB1FF", hjust = 0) +
  geom_text(aes(x = 0.17, y = 0.07, label = "Reason\nTapped Out"), size = 10, color="#01AD74FF", hjust = 0) +
  annotate(geom = "curve",
           x = 0.12, y = 0.155, 
           xend = 0.08, yend = 0.13,
           size = 1.5,
           angle = 0,
           color="#DB3EB1FF",
           arrow = arrow(type = "closed", 
                         length = unit(2.5, 'mm'))) +
  annotate(geom = "curve",
           x = 0.17, y = 0.07, 
           xend = 0.125, yend = 0.07,
           size = 1.5,
           angle = 0,
           color="#01AD74FF",
           arrow = arrow(type = "closed", 
                         length = unit(2.5, 'mm'))) +  
  annotate(geom = "curve",
           x = 0.05, y = 0.19, 
           xend = 0.05, yend = 0.11,
           size = 1.5,
           angle = 0,
           color="White",
           arrow = arrow(type = "closed", 
                         length = unit(2.5, 'mm'))) + 
  xlim(c(0,0.75)) +
  ylim(c(0,0.75)) +
  theme_void() 
  
# Create main plot

survivalists_plot <- survivalists_df |>
  ggplot(aes(x = season_fact,
             y = result
  )) +
  geom_point(aes(color = gender_fact, 
                 fill = reason_category_fact, 
                 stroke = square_stroke, 
                 size = days_lasted),
             
             shape =22
  ) +
  scale_size(range = c(0, 40)) +
  geom_text(aes(label = days_lasted), 
            colour = survivalists_df$label_colour,
            fontface = "bold",
            nudge_x = survivalists_df$label_nudge,
            size = 10
            ) +
  labs(caption = "\nDataviz by @costellosan | #TidyTuesday\nSource: {alone} by @danoehm") +
  theme_minimal() +
  ylab("Contestant Result") +
  xlab("Season") +
  guides(colour = guide_legend(override.aes = list(size = 10, stroke = 2),
                               title="Gender"),
         size = "none",
         fill = guide_legend(override.aes = list(size = 10, stroke = 2),
                             title="Reason\nTapped out")) +
  theme(plot.background = element_rect(fill = bkgn_col, colour = bkgn_col),
        panel.spacing.y=unit(0, "cm"),
        strip.text = element_blank(), 
        axis.text.x = element_text(size = 30, family = "rajdhani"),
        axis.text.y = element_text(size = 30, family = "rajdhani"),
        axis.title = element_text(size = 50, family = "rajdhani"),
        axis.title.x = element_text(margin=margin(t=20)),          
        legend.title = element_text(size = 23, family = "rajdhani"),
        legend.text = element_text(size = 20, family = "rajdhani"),
        plot.margin = margin(8, 2, 2, 24, unit = "cm"),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        plot.caption = element_text(family = "rajdhani", colour = "#543d37",
                                    size = 12,
                                    hjust = 0.9,
                                    lineheight = 1.3)) +
  scale_color_manual(values=c("#DB3EB1FF", "#08B5D3FF")) +
  scale_fill_manual(values=c("#01AD74FF", "#EAAF38FF",  "#BA2F2AFF",  "#08B5D3FF")) +
  scale_y_reverse(labels = label_ordinal(), breaks = seq(1,10,1)) 

# get logo
logo_file <- here::here("data/alone-hex-2048x2048.png")

# Create cowplot combining everything
cowplot::ggdraw() + 
  cowplot::draw_plot(survivalists_plot) +
  cowplot::draw_text(text = paste0('"',episodes[3,c("quote")],'"'), x = 0.3, y = 0.95,
                     hjust = 0, vjust = 1, #halign = 0, valign = 1, 
                     size = 40, family = "special",  color = title_text_col) +  
  cowplot::draw_text(text = episodes[3,c("author")], x = 0.7, y = 0.9,
                     hjust = 0, vjust = 1, #halign = 0, valign = 1, 
                     size = 36, family = "special", fontface = "italic", color = title_text_col) +    
  cowplot::draw_text(text = "Survial by Season and Rank", x = 0.08, y = 0.75,
                     hjust = 0, vjust = 1, #halign = 0, valign = 1, 
                     size = 30, family = "rajdhani", color = title_text_col) +
  cowplot::draw_text(text = "Each contestant is represented\nby a square showing how many\ndays they survived why they\ntapped out and their gender. ", 
                     x = 0.08, y = 0.7,
                     hjust = 0, vjust = 1, #halign = 0, valign = 1, 
                     size = 25, family = "rajdhani", color = title_text_col) +
  cowplot::draw_plot(explainer_plot,
                     x = 0.08, y = 0.45,
                     width = 0.3,
                     height = 0.4,
                     scale = 1) +
  cowplot::draw_text(text = "Tapping out for\n'Medical/Health' reasons", 
                     x = 0.08, y = 0.4,
                     hjust = 0, vjust = 1, #halign = 0, valign = 1, 
                     size = 30, family = "rajdhani", color = title_text_col) +  
  cowplot::draw_text(text = "After Season 5 'Medical/ Health'\nis the most common reason to\ntap out.\n\nAll but two contestants in seasons\n5-9, finishing above 7th place\ntapped out for 'Medical/Health'\nreasons.", 
                     x = 0.08, y = 0.33,
                     hjust = 0, vjust = 1, #halign = 0, valign = 1, 
                     size = 25, family = "rajdhani", color = title_text_col) +
  cowplot::draw_image(logo_file, x = 0.08, y = 0.95, 
                      hjust = 0, vjust = 1, halign = 0, valign = 1,
                      width = 0.1)

# save png
ggsave(filename = file.path("plots/", 
                            "alone.png"), 
       dpi = 400, width = 30, height = 20, bg = "#f9f9f7")