library(tidyverse)
library(showtext)
library(ggtext)
library(glue)
library(emojifont)
library(medicaldata)

# load fonts
font_add_google("Ubuntu")
font_add_google("Archivo Narrow")
font_add_google("Archivo")

# load font awesome icons
sysfonts::font_add(
  family = "Font Awesome 6 Brands",
  regular = system.file("fonts", "fontawesome", "otfs",
    "Font-Awesome-6-Brands-Regular-400.otf",
    package = "nrBrand"
  )
)
showtext_auto()

# load data
data("scurvy")

# colours
bg_col <- "#202A44"
highlight_col <- "#C41E3A"
light_col <- "#ffffff"

# text
social <- "<span style='font-family:\"Font Awesome 6 Brands\";color:#202A44;'>&#xf099;</span><span style='color:#ffffff;'>.</span><span style='font-family:Ubuntu;color:#202A44;'>@nrennie35</span><span style='color:#ffffff;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#202A44;'>&#xf4f6;</span><span style='color:#ffffff;'>.</span><span style='font-family:Ubuntu;color:#202A44;'>fosstodon.org/@nrennie</span><span style='color:#ffffff;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#202A44;'>&#xf08c;</span><span style='color:#ffffff;'>.</span><span style='font-family:Ubuntu;color:#202A44;'>nicola-rennie</span><span style='color:#ffffff;'>..</span><span style='font-family:\"Font Awesome 6 Brands\";color:#202A44;'>&#xf09b;</span><span style='color:#ffffff;'>.</span><span style='font-family:Ubuntu;color:#202A44;'>nrennie</span><span style='color:#ffffff;'>..</span>"
cap <- paste0(
  "**Data**: A Treatise on the Scurvy in Three Parts. James Lind. 1757.<br>
  **Graphic**: ",
  social
)
st <- "In 1757, it was not known that scurvy is a manifestation of vitamin C
deficiency. A variety of remedies had been anecdotally reported, but Lind
was the first to test different regimens of acidic substances (including
citrus fruits) against each other in a randomised controlled trial. This chart
shows how many of the 12 sailors diagnosed with scurvy had moderate or severe
symptoms by day six of their allocated treatment."

# labels
treatment_labs <- scurvy |>
  select(treatment, dosing_regimen_for_scurvy) |>
  mutate(
    treatment_lab = str_replace_all(treatment, "_", " "),
    treatment_lab = str_to_title(treatment_lab),
    dosing_regimen_for_scurvy = str_to_title(
      str_replace_all(dosing_regimen_for_scurvy, "_", " ")
    ),
    treatment_lab = glue::glue(
      "**{treatment_lab}**<br>*{dosing_regimen_for_scurvy}*"
    ),
    # manual line breaks for y axis text
    treatment_lab = str_replace_all(treatment_lab, "Garlic, ", "Garlic,<br>"),
    treatment_lab = str_replace_all(treatment_lab, "Peru, ", "Peru,<br>")
  ) |>
  select(-dosing_regimen_for_scurvy) |>
  distinct()

# relabelling function
scurvy_relabel <- function(x) {
  x_df <- data.frame(treatment = x)
  x_df |>
    left_join(treatment_labs, by = "treatment") |>
    pull(treatment_lab)
}

# number of 12 sailors who had moderate or severe symptoms by day 6 of scurvy.
plot_data <- scurvy |>
  select(-c(study_id, fit_for_duty_d6, dosing_regimen_for_scurvy)) |>
  mutate(across(
    -treatment,
    ~ case_when(
      .x %in% c("2_moderate", "3_severe") ~ "Yes",
      TRUE ~ "No"
    )
  )) |>
  pivot_longer(-treatment, names_to = "symptoms") |>
  group_by(treatment, symptoms, value) |>
  mutate(
    n = n(),
    symptoms = str_to_title(str_replace_all(symptoms, "_", " ")),
    symptoms = str_replace_all(symptoms, " D6", "")
  ) |>
  ungroup() |>
  complete(treatment, symptoms, value) |>
  mutate(n = replace_na(n, 0)) |>
  filter(value == "Yes") |>
  select(-value) |>
  left_join(treatment_labs, by = "treatment") |>
  mutate(treatment = factor(treatment,
    levels = c(
      "citrus", "dilute_sulfuric_acid",
      "cider", "purgative_mixture",
      "vinegar", "sea_water"
    )
  )) |>
  arrange(treatment) |>
  mutate(treatment = forcats::fct_relabel(treatment, scurvy_relabel))

# plot
ggplot(
  data = plot_data,
  mapping = aes(x = symptoms, y = treatment, size = factor(n))
) +
  geom_point(
    data = filter(plot_data, n > 0),
    fill = "black",
    colour = "black",
    pch = 21, alpha = 0.3
  ) +
  scale_x_discrete(
    position = "top",
    labels = function(x) stringr::str_wrap(x, width = 15)
  ) +
  scale_y_discrete(drop = FALSE) +
  labs(
    x = NULL,
    y = NULL,
    title = "The Treatment of Scurvy in 1757",
    subtitle = st,
    caption = cap
  ) +
  theme_minimal(base_size = 28, base_family = "Ubuntu") +
  guides(size = guide_legend(
    nrow = 1, title = "Number of sailors",
    theme(legend.title.position = "left"),
    label.position = "bottom"
  )) +
  theme(
    legend.position.inside = c(-0.3, 1.05),
    legend.position = "inside",
    axis.text.y = element_markdown(lineheight = 0.5),
    axis.text.x = element_text(lineheight = 0.4, vjust = 0),
    panel.grid.major = element_line(linewidth = 0.3),
    plot.background = element_rect(fill = light_col, colour = light_col),
    panel.background = element_rect(fill = light_col, colour = light_col),
    plot.title = element_textbox_simple(
      hjust = 0,
      colour = bg_col,
      face = "bold",
      lineheight = 0.5,
      margin = margin(b = 20)
    ),
    plot.subtitle = element_textbox_simple(
      hjust = 0,
      colour = bg_col,
      lineheight = 0.5,
      margin = margin(b = 20)
    ),
    plot.caption = element_textbox_simple(
      hjust = 0,
      colour = bg_col,
      lineheight = 0.5,
      margin = margin(t = 10)
    ),
    plot.caption.position = "plot",
    plot.title.position = "plot"
  )

# save
ggsave("2024/2024-04-LIDA-Seminar/images/scurvy.png", height = 6, width = 6, units = "in")
