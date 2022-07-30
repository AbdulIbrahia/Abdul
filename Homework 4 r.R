library(tidyverse)
library(ggplot2)
#install.packages('ggridges')
library(ggridges)
library(lubridate)
library(ggrepel)
library(colorspace)


folder_location='C:/Users/abdul/Downloads/Slides12Rscripts_Data.'

cars93 <- MASS::Cars93
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = FALSE, method = "loess", formula = y ~ x, color = "#0072B2") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  )+
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)")



cars93 <- MASS::Cars93
ggplot(cars93, aes(x = Price, y = Fuel.tank.capacity)) +
  geom_point(color = "grey60") +
  geom_smooth(se = TRUE, method = "lm", formula = y ~ x, color = "#8fe388") +
  geom_smooth(se = TRUE, method = "glm", formula = y ~ x, color = "#fe8d6d") +
  geom_smooth(se = TRUE, method = "gam", formula = y ~ x, color = "#7c6bea") +
  scale_x_continuous(
    name = "price (USD)",
    breaks = c(20, 40, 60),
    labels = c("$20,000", "$40,000", "$60,000")
  ) +
  scale_y_continuous(name = "fuel-tank capacity\n(US gallons)") +
  labs(title = "lm") +
  labs(title = "glm") +
  labs(title = "gam") +
  theme(text = element_text(size = 14, color = c("#8fe388", "#fe8d6d", "#7c6bea")))



# Q4
load("./preprint_growth.rda") #please change the path if needed
head(preprint_growth)
preprint_growth %>% filter(archive == "bioRxiv") %>%
  filter(count > 0) -> biorxiv_growth
preprints<-preprint_growth %>% filter(archive %in%
                                        c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")) %>%filter(count > 0) %>%
  mutate(archive = factor(archive, levels = c("bioRxiv", "arXiv q-bio", "PeerJ Preprints")))
preprints_final <- filter(preprints, date == ymd("2017-01-01"))
ggplot(preprints) +
  aes(date, count, color = archive, fill = archive) +
  geom_line(size = 1) +
  scale_y_continuous(
    limits = c(0, 600), expand = c(0, 0),
    name = "preprints / month",
    sec.axis = dup_axis( #this part is for the second y axis
      breaks = preprints_final$count, #and we use the counts to position our labels
      labels = c("arXivq-bio", "PeerJPreprints", "bioRxiv"),
      name = NULL)
  )+
  scale_x_date(name = "year",
               limits = c(min(biorxiv_growth$date), ymd("2017-01-01"))) +
  scale_color_manual(values = c("#0072b2", "#D55E00", "#009e73"),
                     name = NULL) +
  theme(legend.position = "none")
# SOLUTION

preprint_growth %>% drop_na() %>% filter(count > 0, year > 2004) -> preprint_full
preprint_full %>% filter(archive == "bioRxiv" | archive == "F1000Research") -> bio_f1000
ggplot(bio_f1000, aes(x=date, y=count, color="#7c6bea", fill="#7c6bea")) +
  geom_line(size=1) +
  scale_x_date(name = "year",
               limits = c(min(bio_f1000$date), ymd("2017-01-01"))) +
  scale_y_continuous(name = "preprints / month") +
  theme(legend.position = "right") +
  xlim(c(ymd("2014-02-01"),ymd("2017-01-01"))) +
  ggtitle("Preprint Counts")

