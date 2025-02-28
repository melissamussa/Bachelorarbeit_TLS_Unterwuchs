library(ggplot2)
library(e1071)


data <- read.csv(csv_file)

par(mfrow = c(1, 3), mar = c(5, 5, 4, 2))

# Histogramm für Krautige Pflanzen
p1 <- ggplot(data, aes(x = Herbs)) +
  geom_histogram(fill = "lightgreen", color = "black", bins = 20) +
  ggtitle("Krautige Pflanzen") +
  xlab("Prozent (%)") +
  ylab("Häufigkeit") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# Histogramm für Gras
p2 <- ggplot(data, aes(x = Grass)) +
  geom_histogram(fill = "darkgreen", color = "black", bins = 20) +
  ggtitle("Gras") +
  xlab("Prozent (%)") +
  ylab("Häufigkeit") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

# Histogramm für Sträucher
p3 <- ggplot(data, aes(x = Shrub)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 20) +
  ggtitle("Sträucher") +
  xlab("Prozent (%)") +
  ylab("Häufigkeit") +
  theme(plot.title = element_text(hjust = 0.5, size = 14))

library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

skewness(data$Herbs)   
skewness(data$Grass)   
skewness(data$Shrub)   




library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)

data_long <- data %>%
  pivot_longer(cols = c(Herbs, Grass, Shrub), 
               names_to = "Layer", 
               values_to = "Percentage") %>%
  mutate(
    Layer = case_when(
      Layer == "Herbs" ~ "Krautige Pflanzen",
      Layer == "Grass" ~ "Gras",
      Layer == "Shrub" ~ "Sträucher"
    ),
    Layer = fct_relevel(Layer, "Krautige Pflanzen", "Gras", "Sträucher"),  
    Main_Plot = paste0("Plot ", substr(Plot_nb, 5, 6))
  )

# Durchschnittliche Vegetationsbedeckung pro Hauptplot
data_avg <- data_long %>%
  group_by(Main_Plot, Layer) %>%
  summarise(Percentage = mean(Percentage, na.rm = TRUE), .groups = "drop")  

farben <- c("Krautige Pflanzen" = "lightgreen", "Gras" = "darkgreen", "Sträucher" = "lightblue")

# Balkendiagramm für Hauptplots
ggplot(data_avg, aes(x = Layer, y = Percentage, fill = Layer)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~Main_Plot, scales = "fixed") +  
  labs(title = "Durchschnittliche Vegetationsbedeckung pro Plot",
       x = "Schicht",
       y = "Prozent (%)") +
  scale_fill_manual(values = farben) +  
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    legend.position = "bottom"  
  ) +
  scale_y_continuous(limits = c(0, 60))  

