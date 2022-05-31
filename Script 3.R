library(FSA)
library(tidyverse)
library(tidymodels)
library(rcompanion)

#load the data
setwd("C:/Users/Alexandra/Documents/PhD/Results")
attributes <- read_csv("attributes.csv")

#tidy
attributes <- attributes %>%
  filter(circumferential < 100)
attributes$label <- as.character(attributes$label)
attributes$genotype <- replace(attributes$genotype,
                               attributes$genotype == "wt",
                               "1_wt")
attributes$genotype <- replace(attributes$genotype,
                               attributes$genotype == "pxy_two",
                               "2_pxy_two")
attributes$genotype <- replace(attributes$genotype,
                               attributes$genotype == "pxy_four",
                               "3_pxy_four")
attributes$cell_type <- replace(attributes$cell_type,
                                attributes$cell_type == 1,
                                "1_xylem_vessels")
attributes$cell_type <- replace(attributes$cell_type,
                                attributes$cell_type == 2,
                                "2_xylem_parenchyma")
attributes$cell_type <- replace(attributes$cell_type,
                                attributes$cell_type == 3,
                                "3_cambium")
attributes$cell_type <- replace(attributes$cell_type,
                                attributes$cell_type == 4,
                                "4_phloem_fiber")
attributes$cell_type <- replace(attributes$cell_type,
                                attributes$cell_type == 5,
                                "5_phloem_parenchyma")
attributes$cell_type <- replace(attributes$cell_type,
                                attributes$cell_type == 6,
                                "6_periderm")
attributes <- attributes %>%
  arrange(time_point,
          genotype,
          cell_type,
          radial)

#plot cells

wt_3.5_1 <- attributes %>%
  filter(time_point == 3.5) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "a")
wt_3.5_1_plot <- wt_3.5_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_3.5_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_3.5_2 <- attributes %>%
  filter(time_point == 3.5) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "aa")
wt_3.5_2_plot <- wt_3.5_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_3.5_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_3.5_3 <- attributes %>%
  filter(time_point == 3.5) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "ab")
wt_3.5_3_plot <- wt_3.5_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_3.5_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_3.5_1 <- attributes %>%
  filter(time_point == 3.5) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "ac")
pxy_two_3.5_1_plot <- pxy_two_3.5_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_3.5_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_3.5_2 <- attributes %>%
  filter(time_point == 3.5) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "ad")
pxy_two_3.5_2_plot <- pxy_two_3.5_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_3.5_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_3.5_3 <- attributes %>%
  filter(time_point == 3.5) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "ae")
pxy_two_3.5_3_plot <- pxy_two_3.5_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_3.5_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_3.5_1 <- attributes %>%
  filter(time_point == 3.5) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "af")
pxy_four_3.5_1_plot <- pxy_four_3.5_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_3.5_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_3.5_2 <- attributes %>%
  filter(time_point == 3.5) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "ag")
pxy_four_3.5_2_plot <- pxy_four_3.5_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_3.5_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_3.5_3 <- attributes %>%
  filter(time_point == 3.5) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "ah")
pxy_four_3.5_3_plot <- pxy_four_3.5_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_3.5_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_4_1 <- attributes %>%
  filter(time_point == 4) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "ai")
wt_4_1_plot <- wt_4_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_4_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_4_2 <- attributes %>%
  filter(time_point == 4) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "aj")
wt_4_2_plot <- wt_4_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_4_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_4_3 <- attributes %>%
  filter(time_point == 4) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "b")
wt_4_3_plot <- wt_4_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_4_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_4_1 <- attributes %>%
  filter(time_point == 4) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "c")
pxy_two_4_1_plot <- pxy_two_4_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_4_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_4_2 <- attributes %>%
  filter(time_point == 4) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "d")
pxy_two_4_2_plot <- pxy_two_4_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_4_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_4_3 <- attributes %>%
  filter(time_point == 4) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "e")
pxy_two_4_3_plot <- pxy_two_4_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_4_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_4_1 <- attributes %>%
  filter(time_point == 4) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "f")
pxy_four_4_1_plot <- pxy_four_4_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_4_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_4_2 <- attributes %>%
  filter(time_point == 4) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "g")
pxy_four_4_2_plot <- pxy_four_4_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_4_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_4_3 <- attributes %>%
  filter(time_point == 4) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "h")
pxy_four_4_3_plot <- pxy_four_4_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_4_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_4.5_1 <- attributes %>%
  filter(time_point == 4.5) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "i")
wt_4.5_1_plot <- wt_4.5_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_4.5_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_4.5_2 <- attributes %>%
  filter(time_point == 4.5) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "j")
wt_4.5_2_plot <- wt_4.5_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_4.5_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_4.5_3 <- attributes %>%
  filter(time_point == 4.5) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "k")
wt_4.5_3_plot <- wt_4.5_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_4.5_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_4.5_1 <- attributes %>%
  filter(time_point == 4.5) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "l")
pxy_two_4.5_1_plot <- pxy_two_4.5_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_4.5_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_4.5_2 <- attributes %>%
  filter(time_point == 4.5) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "m")
pxy_two_4.5_2_plot <- pxy_two_4.5_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_4.5_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_4.5_3 <- attributes %>%
  filter(time_point == 4.5) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "n")
pxy_two_4.5_3_plot <- pxy_two_4.5_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_4.5_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_4.5_1 <- attributes %>%
  filter(time_point == 4.5) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "o")
pxy_four_4.5_1_plot <- pxy_four_4.5_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_4.5_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_4.5_2 <- attributes %>%
  filter(time_point == 4.5) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "p")
pxy_four_4.5_2_plot <- pxy_four_4.5_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_4.5_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_4.5_3 <- attributes %>%
  filter(time_point == 4.5) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "q")
pxy_four_4.5_3_plot <- pxy_four_4.5_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_4.5_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_5_1 <- attributes %>%
  filter(time_point == 5) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "r")
wt_5_1_plot <- wt_5_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#944491", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_5_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_5_2 <- attributes %>%
  filter(time_point == 5) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "s")
wt_5_2_plot <- wt_5_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#944491", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_5_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

wt_5_3 <- attributes %>%
  filter(time_point == 5) %>%
  filter(genotype == "1_wt") %>%
  filter(plant == "t")
wt_5_3_plot <- wt_5_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#944491", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "wt_5_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_5_1 <- attributes %>%
  filter(time_point == 5) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "u")
pxy_two_5_1_plot <- pxy_two_5_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#944491", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_5_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_5_2 <- attributes %>%
  filter(time_point == 5) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "v")
pxy_two_5_2_plot <- pxy_two_5_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_5_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_two_5_3 <- attributes %>%
  filter(time_point == 5) %>%
  filter(genotype == "2_pxy_two") %>%
  filter(plant == "w")
pxy_two_5_3_plot <- pxy_two_5_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#944491", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_two_5_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_5_1 <- attributes %>%
  filter(time_point == 5) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "x")
pxy_four_5_1_plot <- pxy_four_5_1 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#944491", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_5_1.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_5_2 <- attributes %>%
  filter(time_point == 5) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "y")
pxy_four_5_2_plot <- pxy_four_5_2 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#944491", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_5_2.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

pxy_four_5_3 <- attributes %>%
  filter(time_point == 5) %>%
  filter(genotype == "3_pxy_four") %>%
  filter(plant == "z")
pxy_four_5_3_plot <- pxy_four_5_3 %>%
  ggplot(aes(x = radial,
             y = circumferential,
             color = cell_type)) +
  geom_point(size = 0.1,
             show.legend = FALSE) +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#33B6BC", "#631915")) +
  theme_void()
ggsave(filename = "pxy_four_5_3.pdf",
       device = "pdf",
       width = 3,
       height = 3,
       units = "in")

#count the cells
cell_number <- attributes %>%
  group_by(time_point,
           genotype,
           plant,
           cell_type) %>%
  count() %>%
  drop_na()
cell_number <- cell_number %>%
  ungroup()

cell_number_stats <- tibble(time_point = NA,
                            genotype = NA,
                            cell_type = NA,
                            plant_1 = NA,
                            plant_2 = NA,
                            plant_3 = NA,
                            mean = NA,
                            sd = NA)
for (i in unique(cell_number$time_point)) {
  filtered1 <- cell_number %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      p1 <- filtered3$n[1]
      p2 <- filtered3$n[2]
      p3 <- filtered3$n[3]
      m <- mean(filtered3$n)
      s <- sd(filtered3$n)
      cell_number_stats <- add_row(.data = cell_number_stats,
                                   time_point = i,
                                   genotype = g,
                                   cell_type = t,
                                   plant_1 = p1,
                                   plant_2 = p2,
                                   plant_3 = p3,
                                   mean = m,
                                   sd = s)
    }
  }
}
cell_number_stats <- cell_number_stats[-1, ]

cell_number_for_plot <- cell_number_stats %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "3.5_1_wt",
                                              "01_3.5_wt")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "3.5_2_pxy_two",
                                              "02_3.5_pxy_2")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "3.5_3_pxy_four",
                                              "03_3.5_pxy_4")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "4_1_wt",
                                              "04_4_wt")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "4_2_pxy_two",
                                              "05_4_pxy_2")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "4_3_pxy_four",
                                              "06_4_pxy_4")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "4.5_1_wt",
                                              "07_4.5_wt")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "4.5_2_pxy_two",
                                              "08_4.5_pxy_2")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "4.5_3_pxy_four",
                                              "09_4.5_pxy_4")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "5_1_wt",
                                              "10_5_wt")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "5_2_pxy_two",
                                              "11_5_pxy_2")
cell_number_for_plot$time_genotype <- replace(cell_number_for_plot$time_genotype,
                                              cell_number_for_plot$time_genotype == "5_3_pxy_four",
                                              "12_5_pxy_4")

cell_number_for_plot %>%
  ggplot(aes(x = time_genotype,
             y = mean,
             fill = forcats::fct_rev(cell_type))) +
  geom_bar(position = "stack",
           stat = "identity") +
  scale_color_manual(values = c("#4BAB36", "#1D428F", "#C9C911", "#944491", "#33B6BC", "#631915"))
name = paste0("cell_number.pdf")
ggsave(filename = name,
       device = "pdf",
       width = 8,
       height = 8,
       units = "in")

xylem_vessels_number_wt <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "1_xylem_vessels") %>%
  filter(genotype == "1_wt") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
xylem_vessels_number_wt_k <- kruskal.test(xylem_vessels_number_wt$n ~ xylem_vessels_number_wt$time_genotype)
xylem_vessels_number_wt_k <- tidy(xylem_vessels_number_wt_k)
xylem_vessels_number_wt_k
xylem_vessels_number_wt_d <- dunnTest(x = xylem_vessels_number_wt$n,
                                          g = xylem_vessels_number_wt$time_genotype,
                                          method = "bonferroni")
xylem_vessels_number_wt_d

xylem_parenchyma_number_wt <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "2_xylem_parenchyma") %>%
  filter(genotype == "1_wt") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
xylem_parenchyma_number_wt_k <- kruskal.test(xylem_parenchyma_number_wt$n ~ xylem_parenchyma_number_wt$time_genotype)
xylem_parenchyma_number_wt_k <- tidy(xylem_parenchyma_number_wt_k)
xylem_parenchyma_number_wt_k
xylem_parenchyma_number_wt_d <- dunnTest(x = xylem_parenchyma_number_wt$n,
                                      g = xylem_parenchyma_number_wt$time_genotype,
                                      method = "bonferroni")
xylem_parenchyma_number_wt_d

cambium_number_wt <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "3_cambium") %>%
  filter(genotype == "1_wt") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cambium_number_wt_k <- kruskal.test(cambium_number_wt$n ~ cambium_number_wt$time_genotype)
cambium_number_wt_k <- tidy(cambium_number_wt_k)
cambium_number_wt_k
cambium_number_wt_d <- dunnTest(x = cambium_number_wt$n,
                                g = cambium_number_wt$time_genotype,
                                method = "bonferroni")
cambium_number_wt_d

phloem_parenchyma_number_wt <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "5_phloem_parenchyma") %>%
  filter(genotype == "1_wt") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
phloem_parenchyma_number_wt_k <- kruskal.test(phloem_parenchyma_number_wt$n ~ phloem_parenchyma_number_wt$time_genotype)
phloem_parenchyma_number_wt_k <- tidy(phloem_parenchyma_number_wt_k)
phloem_parenchyma_number_wt_k
phloem_parenchyma_number_wt_d <- dunnTest(x = phloem_parenchyma_number_wt$n,
                                          g = phloem_parenchyma_number_wt$time_genotype,
                                          method = "bonferroni")
phloem_parenchyma_number_wt_d

periderm_number_wt <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "6_periderm") %>%
  filter(genotype == "1_wt") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
periderm_number_wt_k <- kruskal.test(periderm_number_wt$n ~ periderm_number_wt$time_genotype)
periderm_number_wt_k <- tidy(periderm_number_wt_k)
periderm_number_wt_k
periderm_number_wt_d <- dunnTest(x = periderm_number_wt$n,
                                 g = periderm_number_wt$time_genotype,
                                 method = "bonferroni")
periderm_number_wt_d

xylem_vessels_number_pxy_two <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "1_xylem_vessels") %>%
  filter(genotype == "2_pxy_two") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
xylem_vessels_number_pxy_two_k <- kruskal.test(xylem_vessels_number_pxy_two$n ~ xylem_vessels_number_pxy_two$time_genotype)
xylem_vessels_number_pxy_two_k <- tidy(xylem_vessels_number_pxy_two_k)
xylem_vessels_number_pxy_two_k
xylem_vessels_number_pxy_two_d <- dunnTest(x = xylem_vessels_number_pxy_two$n,
                                      g = xylem_vessels_number_pxy_two$time_genotype,
                                      method = "bonferroni")
xylem_vessels_number_pxy_two_d

xylem_parenchyma_number_pxy_two <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "2_xylem_parenchyma") %>%
  filter(genotype == "2_pxy_two") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
xylem_parenchyma_number_pxy_two_k <- kruskal.test(xylem_parenchyma_number_pxy_two$n ~ xylem_parenchyma_number_pxy_two$time_genotype)
xylem_parenchyma_number_pxy_two_k <- tidy(xylem_parenchyma_number_pxy_two_k)
xylem_parenchyma_number_pxy_two_k

cambium_number_pxy_two <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "3_cambium") %>%
  filter(genotype == "2_pxy_two") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cambium_number_pxy_two_k <- kruskal.test(cambium_number_pxy_two$n ~ cambium_number_pxy_two$time_genotype)
cambium_number_pxy_two_k <- tidy(cambium_number_pxy_two_k)
cambium_number_pxy_two_k
cambium_number_pxy_two_d <- dunnTest(x = cambium_number_pxy_two$n,
                                g = cambium_number_pxy_two$time_genotype,
                                method = "bonferroni")
cambium_number_pxy_two_d

phloem_parenchyma_number_pxy_two <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "5_phloem_parenchyma") %>%
  filter(genotype == "2_pxy_two") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
phloem_parenchyma_number_pxy_two_k <- kruskal.test(phloem_parenchyma_number_pxy_two$n ~ phloem_parenchyma_number_pxy_two$time_genotype)
phloem_parenchyma_number_pxy_two_k <- tidy(phloem_parenchyma_number_pxy_two_k)
phloem_parenchyma_number_pxy_two_k
phloem_parenchyma_number_pxy_two_d <- dunnTest(x = phloem_parenchyma_number_pxy_two$n,
                                          g = phloem_parenchyma_number_pxy_two$time_genotype,
                                          method = "bonferroni")
phloem_parenchyma_number_pxy_two_d

periderm_number_pxy_two <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "6_periderm") %>%
  filter(genotype == "2_pxy_two") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
periderm_number_pxy_two_k <- kruskal.test(periderm_number_pxy_two$n ~ periderm_number_pxy_two$time_genotype)
periderm_number_pxy_two_k <- tidy(periderm_number_pxy_two_k)
periderm_number_pxy_two_k
periderm_number_pxy_two_d <- dunnTest(x = periderm_number_pxy_two$n,
                                 g = periderm_number_pxy_two$time_genotype,
                                 method = "bonferroni")
periderm_number_pxy_two_d

xylem_vessels_number_pxy_four <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "1_xylem_vessels") %>%
  filter(genotype == "3_pxy_four") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
xylem_vessels_number_pxy_four_k <- kruskal.test(xylem_vessels_number_pxy_four$n ~ xylem_vessels_number_pxy_four$time_genotype)
xylem_vessels_number_pxy_four_k <- tidy(xylem_vessels_number_pxy_four_k)
xylem_vessels_number_pxy_four_k

xylem_parenchyma_number_pxy_four <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "2_xylem_parenchyma") %>%
  filter(genotype == "3_pxy_four") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
xylem_parenchyma_number_pxy_four_k <- kruskal.test(xylem_parenchyma_number_pxy_four$n ~ xylem_parenchyma_number_pxy_four$time_genotype)
xylem_parenchyma_number_pxy_four_k <- tidy(xylem_parenchyma_number_pxy_four_k)
xylem_parenchyma_number_pxy_four_k
xylem_parenchyma_number_pxy_four_d <- dunnTest(x = xylem_parenchyma_number_pxy_four$n,
                                               g = xylem_parenchyma_number_pxy_four$time_genotype,
                                               method = "bonferroni")
xylem_parenchyma_number_pxy_four_d

cambium_number_pxy_four <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "3_cambium") %>%
  filter(genotype == "3_pxy_four") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cambium_number_pxy_four_k <- kruskal.test(cambium_number_pxy_four$n ~ cambium_number_pxy_four$time_genotype)
cambium_number_pxy_four_k <- tidy(cambium_number_pxy_four_k)
cambium_number_pxy_four_k
cambium_number_pxy_four_d <- dunnTest(x = cambium_number_pxy_four$n,
                                     g = cambium_number_pxy_four$time_genotype,
                                     method = "bonferroni")
cambium_number_pxy_four_d

phloem_parenchyma_number_pxy_four <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "5_phloem_parenchyma") %>%
  filter(genotype == "3_pxy_four") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
phloem_parenchyma_number_pxy_four_k <- kruskal.test(phloem_parenchyma_number_pxy_four$n ~ phloem_parenchyma_number_pxy_four$time_genotype)
phloem_parenchyma_number_pxy_four_k <- tidy(phloem_parenchyma_number_pxy_four_k)
phloem_parenchyma_number_pxy_four_k

periderm_number_pxy_four <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "6_periderm") %>%
  filter(genotype == "3_pxy_four") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
periderm_number_pxy_four_k <- kruskal.test(periderm_number_pxy_four$n ~ periderm_number_pxy_four$time_genotype)
periderm_number_pxy_four_k <- tidy(periderm_number_pxy_four_k)
periderm_number_pxy_four_k
periderm_number_pxy_four_d <- dunnTest(x = periderm_number_pxy_four$n,
                                      g = periderm_number_pxy_four$time_genotype,
                                      method = "bonferroni")
periderm_number_pxy_four_d

xylem_vessels_number_3.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "1_xylem_vessels") %>%
  filter(time_point == "3.5")
xylem_vessels_number_3.5_k <- kruskal.test(xylem_vessels_number_3.5$n ~ xylem_vessels_number_3.5$genotype)
xylem_vessels_number_3.5_k <- tidy(xylem_vessels_number_3.5_k)
xylem_vessels_number_3.5_k

xylem_vessels_number_4 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "1_xylem_vessels") %>%
  filter(time_point == "4")
xylem_vessels_number_4_k <- kruskal.test(xylem_vessels_number_4$n ~ xylem_vessels_number_4$genotype)
xylem_vessels_number_4_k <- tidy(xylem_vessels_number_4_k)
xylem_vessels_number_4_k

xylem_vessels_number_4.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "1_xylem_vessels") %>%
  filter(time_point == "4.5")
xylem_vessels_number_4.5_k <- kruskal.test(xylem_vessels_number_4.5$n ~ xylem_vessels_number_4.5$genotype)
xylem_vessels_number_4.5_k <- tidy(xylem_vessels_number_4.5_k)
xylem_vessels_number_4.5_k

xylem_vessels_number_5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "1_xylem_vessels") %>%
  filter(time_point == "5")
xylem_vessels_number_5_k <- kruskal.test(xylem_vessels_number_5$n ~ xylem_vessels_number_5$genotype)
xylem_vessels_number_5_k <- tidy(xylem_vessels_number_5_k)
xylem_vessels_number_5_k

xylem_parenchyma_number_3.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "2_xylem_parenchyma") %>%
  filter(time_point == "3.5")
xylem_parenchyma_number_3.5_k <- kruskal.test(xylem_parenchyma_number_3.5$n ~ xylem_parenchyma_number_3.5$genotype)
xylem_parenchyma_number_3.5_k <- tidy(xylem_parenchyma_number_3.5_k)
xylem_parenchyma_number_3.5_k

xylem_parenchyma_number_4 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "2_xylem_parenchyma") %>%
  filter(time_point == "4")
xylem_parenchyma_number_4_k <- kruskal.test(xylem_parenchyma_number_4$n ~ xylem_parenchyma_number_4$genotype)
xylem_parenchyma_number_4_k <- tidy(xylem_parenchyma_number_4_k)
xylem_parenchyma_number_4_k

xylem_parenchyma_number_4.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "2_xylem_parenchyma") %>%
  filter(time_point == "4.5")
xylem_parenchyma_number_4.5_k <- kruskal.test(xylem_parenchyma_number_4.5$n ~ xylem_parenchyma_number_4.5$genotype)
xylem_parenchyma_number_4.5_k <- tidy(xylem_parenchyma_number_4.5_k)
xylem_parenchyma_number_4.5_k

xylem_parenchyma_number_5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "2_xylem_parenchyma") %>%
  filter(time_point == "5")
xylem_parenchyma_number_5_k <- kruskal.test(xylem_parenchyma_number_5$n ~ xylem_parenchyma_number_5$genotype)
xylem_parenchyma_number_5_k <- tidy(xylem_parenchyma_number_5_k)
xylem_parenchyma_number_5_k

cambium_number_3.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "3_cambium") %>%
  filter(time_point == "3.5")
cambium_number_3.5_k <- kruskal.test(cambium_number_3.5$n ~ cambium_number_3.5$genotype)
cambium_number_3.5_k <- tidy(cambium_number_3.5_k)
cambium_number_3.5_k

cambium_number_4 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "3_cambium") %>%
  filter(time_point == "4")
cambium_number_4_k <- kruskal.test(cambium_number_4$n ~ cambium_number_4$genotype)
cambium_number_4_k <- tidy(cambium_number_4_k)
cambium_number_4_k

cambium_number_4.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "3_cambium") %>%
  filter(time_point == "4.5")
cambium_number_4.5_k <- kruskal.test(cambium_number_4.5$n ~ cambium_number_4.5$genotype)
cambium_number_4.5_k <- tidy(cambium_number_4.5_k)
cambium_number_4.5_k

cambium_number_5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "3_cambium") %>%
  filter(time_point == "5")
cambium_number_5_k <- kruskal.test(cambium_number_5$n ~ cambium_number_5$genotype)
cambium_number_5_k <- tidy(cambium_number_5_k)
cambium_number_5_k

phloem_parenchyma_number_3.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "5_phloem_parenchyma") %>%
  filter(time_point == "3.5")
phloem_parenchyma_number_3.5_k <- kruskal.test(phloem_parenchyma_number_3.5$n ~ phloem_parenchyma_number_3.5$genotype)
phloem_parenchyma_number_3.5_k <- tidy(phloem_parenchyma_number_3.5_k)
phloem_parenchyma_number_3.5_k

phloem_parenchyma_number_4 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "5_phloem_parenchyma") %>%
  filter(time_point == "4")
phloem_parenchyma_number_4_k <- kruskal.test(phloem_parenchyma_number_4$n ~ phloem_parenchyma_number_4$genotype)
phloem_parenchyma_number_4_k <- tidy(phloem_parenchyma_number_4_k)
phloem_parenchyma_number_4_k

phloem_parenchyma_number_4.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "5_phloem_parenchyma") %>%
  filter(time_point == "4.5")
phloem_parenchyma_number_4.5_k <- kruskal.test(phloem_parenchyma_number_4.5$n ~ phloem_parenchyma_number_4.5$genotype)
phloem_parenchyma_number_4.5_k <- tidy(phloem_parenchyma_number_4.5_k)
phloem_parenchyma_number_4.5_k

phloem_parenchyma_number_5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "5_phloem_parenchyma") %>%
  filter(time_point == "5")
phloem_parenchyma_number_5_k <- kruskal.test(phloem_parenchyma_number_5$n ~ phloem_parenchyma_number_5$genotype)
phloem_parenchyma_number_5_k <- tidy(phloem_parenchyma_number_5_k)
phloem_parenchyma_number_5_k

periderm_number_3.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "6_periderm") %>%
  filter(time_point == "3.5")
periderm_number_3.5_k <- kruskal.test(periderm_number_3.5$n ~ periderm_number_3.5$genotype)
periderm_number_3.5_k <- tidy(periderm_number_3.5_k)
periderm_number_3.5_k

periderm_number_4 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "6_periderm") %>%
  filter(time_point == "4")
periderm_number_4_k <- kruskal.test(periderm_number_4$n ~ periderm_number_4$genotype)
periderm_number_4_k <- tidy(periderm_number_4_k)
periderm_number_4_k

periderm_number_4.5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "6_periderm") %>%
  filter(time_point == "4.5")
periderm_number_4.5_k <- kruskal.test(periderm_number_4.5$n ~ periderm_number_4.5$genotype)
periderm_number_4.5_k <- tidy(periderm_number_4.5_k)
periderm_number_4.5_k

periderm_number_5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "6_periderm") %>%
  filter(time_point == "5")
periderm_number_5_k <- kruskal.test(periderm_number_5$n ~ periderm_number_5$genotype)
periderm_number_5_k <- tidy(periderm_number_5_k)
periderm_number_5_k

phloem_fibers_number_5 <- cell_number %>%
  select(time_point,
         genotype,
         cell_type,
         n) %>%
  filter(cell_type == "4_phloem_fiber") %>%
  filter(time_point == "5")
phloem_fibers_number_5_k <- kruskal.test(phloem_fibers_number_5$n ~ phloem_fibers_number_5$genotype)
phloem_fibers_number_5_k <- tidy(phloem_fibers_number_5_k)
phloem_fibers_number_5_k

cell_number_total <- tibble(time_point = NA,
                            genotype = NA,
                            plant = NA)
for (i in unique(cell_number$time_point)) {
  filtered1 <- cell_number %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$plant)) {
      filtered3 <- filtered2 %>%
        filter(plant == t)
      p <- sum(filtered3$n)
      cell_number_total <- add_row(.data = cell_number_total,
                                   time_point = i,
                                   genotype = g,
                                   plant = p)
    }
  }
}
cell_number_total <- cell_number_total[-1, ]

cell_number_wt <- cell_number_total %>%
  filter(genotype == "1_wt") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cell_number_wt_k <- kruskal.test(cell_number_wt$plant ~ cell_number_wt$time_genotype)
cell_number_wt_k <- tidy(cell_number_wt_k)
cell_number_wt_k
cell_number_wt_d <- dunnTest(x = cell_number_wt$plant,
                             g = cell_number_wt$time_genotype,
                             method = "bonferroni")
cell_number_wt_d

cell_number_pxy_two <- cell_number_total %>%
  filter(genotype == "2_pxy_two") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cell_number_pxy_two_k <- kruskal.test(cell_number_pxy_two$plant ~ cell_number_pxy_two$time_genotype)
cell_number_pxy_two_k <- tidy(cell_number_pxy_two_k)
cell_number_pxy_two_k
cell_number_pxy_two_d <- dunnTest(x = cell_number_pxy_two$plant,
                                  g = cell_number_pxy_two$time_genotype,
                                  method = "bonferroni")
cell_number_pxy_two_d

cell_number_pxy_four <- cell_number_total %>%
  filter(genotype == "3_pxy_four") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cell_number_pxy_four_k <- kruskal.test(cell_number_pxy_four$plant ~ cell_number_pxy_four$time_genotype)
cell_number_pxy_four_k <- tidy(cell_number_pxy_four_k)
cell_number_pxy_four_k
cell_number_pxy_four_d <- dunnTest(x = cell_number_pxy_four$plant,
                                   g = cell_number_pxy_four$time_genotype,
                                   method = "bonferroni")
cell_number_pxy_four_d

cell_number_3.5 <- cell_number_total %>%
  filter(time_point == "3.5") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cell_number_3.5_k <- kruskal.test(cell_number_3.5$plant ~ cell_number_3.5$time_genotype)
cell_number_3.5_k <- tidy(cell_number_3.5_k)
cell_number_3.5_k

cell_number_4 <- cell_number_total %>%
  filter(time_point == "4") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cell_number_4_k <- kruskal.test(cell_number_4$plant ~ cell_number_4$time_genotype)
cell_number_4_k <- tidy(cell_number_4_k)
cell_number_4_k

cell_number_4.5 <- cell_number_total %>%
  filter(time_point == "4.5") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cell_number_4.5_k <- kruskal.test(cell_number_4.5$plant ~ cell_number_4.5$time_genotype)
cell_number_4.5_k <- tidy(cell_number_4.5_k)
cell_number_4.5_k

cell_number_5 <- cell_number_total %>%
  filter(time_point == "5") %>%
  unite(col = time_genotype,
        time_point,
        genotype)
cell_number_5_k <- kruskal.test(cell_number_5$plant ~ cell_number_5$time_genotype)
cell_number_5_k <- tidy(cell_number_5_k)
cell_number_5_k

#Area

area_pooled <- tibble(time_point = NA,
                      genotype = NA,
                      cell_type = NA,
                      mean = NA,
                      sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$area)
      s <- sd(filtered3$area)
      area_pooled <- add_row(.data = area_pooled,
                             time_point = i,
                             genotype = g,
                             cell_type = t,
                             mean = m,
                             sd = s)
    }
  }
}
area_pooled <- area_pooled[-1, ]

area_stats <- area_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
area_stats <- na.omit(area_stats)
area_stats$time_genotype <- factor(area_stats$time_genotype, levels = c("3.5_1_wt",
                                                                        "3.5_2_pxy_two",
                                                                        "3.5_3_pxy_four",
                                                                        "4_1_wt",
                                                                        "4_2_pxy_two",
                                                                        "4_3_pxy_four",
                                                                        "4.5_1_wt",
                                                                        "4.5_2_pxy_two",
                                                                        "4.5_3_pxy_four",
                                                                        "5_1_wt",
                                                                        "5_2_pxy_two",
                                                                        "5_3_pxy_four"))

area <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         area)
area <- na.omit(area)
area$time_genotype <- factor(area$time_genotype, levels = c("3.5_1_wt",
                                                            "3.5_2_pxy_two",
                                                            "3.5_3_pxy_four",
                                                            "4_1_wt",
                                                            "4_2_pxy_two",
                                                            "4_3_pxy_four",
                                                            "4.5_1_wt",
                                                            "4.5_2_pxy_two",
                                                            "4.5_3_pxy_four",
                                                            "5_1_wt",
                                                            "5_2_pxy_two",
                                                            "5_3_pxy_four"))

for (t in unique(area_pooled$cell_type)) {
  area_stats_t <- area_stats %>%
    filter(cell_type == t)
  
  area_t <- area %>%
    filter(cell_type == t)

  area_for_plot <- tibble(time_genotype = NA,
                          area = NA)
  for (i in unique(area_t$time_genotype)) {
    filtered <- area_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$area,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$area)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$area > low & filtered$area < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             area)
    area_for_plot <- add_row(area_for_plot,
                             filtered1)
  }
  area_for_plot <- area_for_plot[-1, ]

  ggplot() +
    ggbeeswarm::geom_quasirandom(data = area_for_plot,
                                 aes(x = time_genotype,
                                     y = area,
                                     colour = time_genotype)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = area_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = area_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("area_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_area_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         area) %>%
  filter(cell_type != "4_phloem_fiber")
attributes_for_area_test <- na.omit(attributes_for_area_test)
area_test_time <- tibble(cell_type = NA,
                         genotype = NA,
                         time_point = NA,
                         Shapiro_Wilk = NA,
                         Kruskal_Wallis = NA,
                         Dunn = NA)
for (t in unique(attributes_for_area_test$cell_type)) {
  filtered <- attributes_for_area_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$area ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$area,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$area)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      area_test_time <- add_row(.data = area_test_time,
                                cell_type = t,
                                genotype = g,
                                time_point = i,
                                Shapiro_Wilk = s,
                                Kruskal_Wallis = k,
                                Dunn = d1)
    }
  }
}
area_test_time <- area_test_time[-1, ]

attributes_for_area_genotype_test <- attributes_for_area_test %>%
  filter(!(time_point == 3.5 & cell_type == "2_xylem_parenchyma")) %>%
  filter(!(time_point == 3.5 & cell_type == "5_phloem_parenchyma"))

area_test_genotype <- tibble(cell_type = NA,
                             time_point = NA,
                             genotype = NA,
                             Kruskal_Wallis = NA,
                             Dunn = NA)
for (t in unique(attributes_for_area_genotype_test$cell_type)) {
  filtered <- attributes_for_area_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$area ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$area,
                      g = filtered1$genotype,
                      method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      area_test_genotype <- add_row(.data = area_test_genotype,
                                    cell_type = t,
                                    time_point = i,
                                    genotype = g,
                                    Kruskal_Wallis = k,
                                    Dunn = d1)
    }
  }
}
area_test_genotype <- area_test_genotype[-1, ]

for_area_xylem_parenchyma_genotype_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         area) %>%
  filter(cell_type == "2_xylem_parenchyma") %>%
  filter(time_point == 3.5)
k <- kruskal.test(for_area_xylem_parenchyma_genotype_test$area ~ for_area_xylem_parenchyma_genotype_test$genotype)
k <- tidy(k)
k <- k$p.value
for(g in unique(for_area_xylem_parenchyma_genotype_test$genotype)) {
  filtered <- for_area_xylem_parenchyma_genotype_test %>%
    filter(genotype == g)
  area_test_genotype <- add_row(.data = area_test_genotype,
                                cell_type = "2_xylem_parenchyma",
                                time_point = 3.5,
                                genotype = g,
                                Kruskal_Wallis = k,
                                Dunn = NA)
}
for_area_phloem_parenchyma_genotype_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         area) %>%
  filter(cell_type == "5_phloem_parenchyma") %>%
  filter(time_point == 3.5)
k <- kruskal.test(for_area_phloem_parenchyma_genotype_test$area ~ for_area_phloem_parenchyma_genotype_test$genotype)
k <- tidy(k)
k <- k$p.value
for(g in unique(for_area_phloem_parenchyma_genotype_test$genotype)) {
  filtered <- for_area_phloem_parenchyma_genotype_test %>%
    filter(genotype == g)
  area_test_genotype <- add_row(.data = area_test_genotype,
                                cell_type = "5_phloem_parenchyma",
                                time_point = 3.5,
                                genotype = g,
                                Kruskal_Wallis = k,
                                Dunn = NA)
}
for_area_phloem_fiber_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         area) %>%
  filter(cell_type == "4_phloem_fiber") %>%
  filter(time_point == 5)
k <- kruskal.test(for_area_phloem_fiber_test$area ~ for_area_phloem_fiber_test$genotype)
k <- tidy(k)
k <- k$p.value
for(g in unique(for_area_phloem_fiber_test$genotype)) {
  filtered <- for_area_phloem_fiber_test %>%
    filter(genotype == g)
  area_test_genotype <- add_row(.data = area_test_genotype,
                                cell_type = "4_phloem_fiber",
                                time_point = 5,
                                genotype = g,
                                Kruskal_Wallis = k,
                                Dunn = NA)
}

#aspect_ratio

aspect_pooled <- tibble(time_point = NA,
                        genotype = NA,
                        cell_type = NA,
                        mean = NA,
                        sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$aspect_ratio)
      s <- sd(filtered3$aspect_ratio)
      aspect_pooled <- add_row(.data = aspect_pooled,
                               time_point = i,
                               genotype = g,
                               cell_type = t,
                               mean = m,
                               sd = s)
    }
  }
}
aspect_pooled <- aspect_pooled[-1, ]

aspect_stats <- aspect_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
aspect_stats <- na.omit(aspect_stats)
aspect_stats$time_genotype <- factor(aspect_stats$time_genotype, levels = c("3.5_1_wt",
                                                                            "3.5_2_pxy_two",
                                                                            "3.5_3_pxy_four",
                                                                            "4_1_wt",
                                                                            "4_2_pxy_two",
                                                                            "4_3_pxy_four",
                                                                            "4.5_1_wt",
                                                                            "4.5_2_pxy_two",
                                                                            "4.5_3_pxy_four",
                                                                            "5_1_wt",
                                                                            "5_2_pxy_two",
                                                                            "5_3_pxy_four"))

aspect <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         aspect_ratio)
aspect <- na.omit(aspect)
aspect$time_genotype <- factor(aspect$time_genotype, levels = c("3.5_1_wt",
                                                                "3.5_2_pxy_two",
                                                                "3.5_3_pxy_four",
                                                                "4_1_wt",
                                                                "4_2_pxy_two",
                                                                "4_3_pxy_four",
                                                                "4.5_1_wt",
                                                                "4.5_2_pxy_two",
                                                                "4.5_3_pxy_four",
                                                                "5_1_wt",
                                                                "5_2_pxy_two",
                                                                "5_3_pxy_four"))

for (t in unique(aspect_pooled$cell_type)) {
  aspect_stats_t <- aspect_stats %>%
    filter(cell_type == t)
  
  aspect_t <- aspect %>%
    filter(cell_type == t)
  
  aspect_for_plot <- tibble(time_genotype = NA,
                            aspect_ratio = NA)
  for (i in unique(aspect_t$time_genotype)) {
    filtered <- aspect_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$aspect_ratio,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$aspect_ratio)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$aspect_ratio > low & filtered$aspect_ratio < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             aspect_ratio)
    aspect_for_plot <- add_row(aspect_for_plot,
                               filtered1)
  }
  aspect_for_plot <- aspect_for_plot[-1, ]
  
  ggplot() +
    ggbeeswarm::geom_quasirandom(data = aspect_for_plot,
                                 aes(x = time_genotype,
                                     y = aspect_ratio,
                                     colour = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = aspect_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = aspect_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("aspect_ratio_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_aspect_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         aspect_ratio)
attributes_for_aspect_test <- na.omit(attributes_for_aspect_test)

attributes_for_aspect_time_test <- attributes_for_aspect_test %>%
  filter(cell_type != "4_phloem_fiber")

aspect_test_time <- tibble(cell_type = NA,
                           genotype = NA,
                           time_point = NA,
                           Shapiro_Wilk = NA,
                           Kruskal_Wallis = NA,
                           Dunn = NA)
for (t in unique(attributes_for_aspect_time_test$cell_type)) {
  filtered <- attributes_for_aspect_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$aspect_ratio ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$aspect_ratio,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$aspect_ratio)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      aspect_test_time <- add_row(.data = aspect_test_time,
                                  cell_type = t,
                                  genotype = g,
                                  time_point = i,
                                  Shapiro_Wilk = s,
                                  Kruskal_Wallis = k,
                                  Dunn = d1)
    }
  }
}
aspect_test_time <- aspect_test_time[-1, ]

attributes_for_aspect_genotype_test <- attributes_for_aspect_test %>%
  filter(!(time_point == 3.5 & cell_type == "5_phloem_parenchyma")) %>%
  filter(!(time_point == 4 & cell_type == "5_phloem_parenchyma")) %>%
  filter(!(time_point == 3.5 & cell_type == "3_cambium")) %>%
  filter(cell_type != "4_phloem_fiber")

aspect_test_genotype <- tibble(cell_type = NA,
                               time_point = NA,
                               genotype = NA,
                               Kruskal_Wallis = NA,
                               Dunn = NA)
for (t in unique(attributes_for_aspect_genotype_test$cell_type)) {
  filtered <- attributes_for_aspect_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$aspect_ratio ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$aspect_ratio,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      aspect_test_genotype <- add_row(.data = aspect_test_genotype,
                                      cell_type = t,
                                      time_point = i,
                                      genotype = g,
                                      Kruskal_Wallis = k,
                                      Dunn = d1)
    }
  }
}
aspect_test_genotype <- aspect_test_genotype[-1, ]

attributes_for_aspect_genotype_test_ns <- attributes_for_aspect_test %>%
  filter((time_point == 3.5 & cell_type == "5_phloem_parenchyma") |
           (time_point == 4 & cell_type == "5_phloem_parenchyma") |
           (time_point == 3.5 & cell_type == "3_cambium") |
           (cell_type == "4_phloem_fiber"))


for (t in unique(attributes_for_aspect_genotype_test_ns$cell_type)) {
  filtered <- attributes_for_aspect_genotype_test_ns %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$aspect_ratio ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      aspect_test_genotype <- add_row(.data = aspect_test_genotype,
                                      cell_type = t,
                                      time_point = i,
                                      genotype = g,
                                      Kruskal_Wallis = k,
                                      Dunn = NA)
    }
  }
}

#betweenness_centrality

betweenness_pooled <- tibble(time_point = NA,
                             genotype = NA,
                             cell_type = NA,
                             mean = NA,
                             sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$betweenness_centrality)
      s <- sd(filtered3$betweenness_centrality)
      betweenness_pooled <- add_row(.data = betweenness_pooled,
                                    time_point = i,
                                    genotype = g,
                                    cell_type = t,
                                    mean = m,
                                    sd = s)
    }
  }
}
betweenness_pooled <- betweenness_pooled[-1, ]

betweenness_stats <- betweenness_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
betweenness_stats <- na.omit(betweenness_stats)
betweenness_stats$time_genotype <- factor(betweenness_stats$time_genotype, levels = c("3.5_1_wt",
                                                                                      "3.5_2_pxy_two",
                                                                                      "3.5_3_pxy_four",
                                                                                      "4_1_wt",
                                                                                      "4_2_pxy_two",
                                                                                      "4_3_pxy_four",
                                                                                      "4.5_1_wt",
                                                                                      "4.5_2_pxy_two",
                                                                                      "4.5_3_pxy_four",
                                                                                      "5_1_wt",
                                                                                      "5_2_pxy_two",
                                                                                      "5_3_pxy_four"))

betweenness <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         betweenness_centrality)
betweenness <- na.omit(betweenness)
betweenness$time_genotype <- factor(betweenness$time_genotype, levels = c("3.5_1_wt",
                                                                          "3.5_2_pxy_two",
                                                                          "3.5_3_pxy_four",
                                                                          "4_1_wt",
                                                                          "4_2_pxy_two",
                                                                          "4_3_pxy_four",
                                                                          "4.5_1_wt",
                                                                          "4.5_2_pxy_two",
                                                                          "4.5_3_pxy_four",
                                                                          "5_1_wt",
                                                                          "5_2_pxy_two",
                                                                          "5_3_pxy_four"))

for (t in unique(betweenness_pooled$cell_type)) {
  betweenness_stats_t <- betweenness_stats %>%
    filter(cell_type == t)
  
  betweenness_t <- betweenness %>%
    filter(cell_type == t)
  
  betweenness_for_plot <- tibble(time_genotype = NA,
                                 betweenness_centrality = NA)
  for (i in unique(betweenness_t$time_genotype)) {
    filtered <- betweenness_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$betweenness_centrality,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$betweenness_centrality)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$betweenness_centrality > low & filtered$betweenness_centrality < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             betweenness_centrality)
    betweenness_for_plot <- add_row(betweenness_for_plot,
                                    filtered1)
  }
  betweenness_for_plot <- betweenness_for_plot[-1, ]
  
  ggplot() +
    ggbeeswarm::geom_quasirandom(data = betweenness_for_plot,
                                 aes(x = time_genotype,
                                     y = betweenness_centrality,
                                     colour = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = betweenness_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = betweenness_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("betweenness_centrality_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_betweenness_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         betweenness_centrality)
attributes_for_betweenness_test <- na.omit(attributes_for_betweenness_test)

attributes_for_betweenness_time_test <- attributes_for_betweenness_test %>%
  filter(cell_type != "4_phloem_fiber")

betweenness_test_time <- tibble(cell_type = NA,
                                genotype = NA,
                                time_point = NA,
                                Shapiro_Wilk = NA,
                                Kruskal_Wallis = NA,
                                Dunn = NA)
for (t in unique(attributes_for_betweenness_time_test$cell_type)) {
  filtered <- attributes_for_betweenness_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$betweenness_centrality ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$betweenness_centrality,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$betweenness_centrality)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      betweenness_test_time <- add_row(.data = betweenness_test_time,
                                       cell_type = t,
                                       genotype = g,
                                       time_point = i,
                                       Shapiro_Wilk = s,
                                       Kruskal_Wallis = k,
                                       Dunn = d1)
    }
  }
}
betweenness_test_time <- betweenness_test_time[-1, ]

attributes_for_betweenness_genotype_test <- attributes_for_betweenness_test %>%
  filter(!(time_point == 3.5 & cell_type == "3_cambium")) %>%
  filter(!(time_point == 4 & cell_type == "3_cambium")) %>%
  filter(!(time_point == 5 & cell_type == "1_xylem_vessels")) %>%
  filter(!(time_point == 4 & cell_type == "5_phloem_parenchyma")) %>%
  filter(!(time_point == 5 & cell_type == "5_phloem_parenchyma")) %>%
  filter(!(time_point == 3.5 & cell_type == "6_periderm")) %>%
  filter(!(time_point == 4.5 & cell_type == "6_periderm")) %>%
  filter(cell_type != "4_phloem_fiber")

betweenness_test_genotype <- tibble(cell_type = NA,
                                    time_point = NA,
                                    genotype = NA,
                                    Kruskal_Wallis = NA,
                                    Dunn = NA)
for (t in unique(attributes_for_betweenness_genotype_test$cell_type)) {
  filtered <- attributes_for_betweenness_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$betweenness_centrality ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$betweenness_centrality,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      betweenness_test_genotype <- add_row(.data = betweenness_test_genotype,
                                           cell_type = t,
                                           time_point = i,
                                           genotype = g,
                                           Kruskal_Wallis = k,
                                           Dunn = d1)
    }
  }
}
betweenness_test_genotype <- betweenness_test_genotype[-1, ]

attributes_for_betweenness_genotype_test_ns <- attributes_for_betweenness_test %>%
  filter((time_point == 3.5 & cell_type == "3_cambium") |
           (time_point == 4 & cell_type == "3_cambium") |
           (time_point == 5 & cell_type == "1_xylem_vessels") |
           (time_point == 4 & cell_type == "5_phloem_parenchyma") |
           (time_point == 5 & cell_type == "5_phloem_parenchyma") |
           (time_point == 3.5 & cell_type == "6_periderm") |
           (time_point == 4.5 & cell_type == "6_periderm") |
           (cell_type == "4_phloem_fiber"))


for (t in unique(attributes_for_betweenness_genotype_test_ns$cell_type)) {
  filtered <- attributes_for_betweenness_genotype_test_ns %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$betweenness_centrality ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      betweenness_test_genotype <- add_row(.data = betweenness_test_genotype,
                                           cell_type = t,
                                           time_point = i,
                                           genotype = g,
                                           Kruskal_Wallis = k,
                                           Dunn = NA)
    }
  }
}

# Circularity

circularity_pooled <- tibble(time_point = NA,
                             genotype = NA,
                             cell_type = NA,
                             mean = NA,
                             sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$circularity)
      s <- sd(filtered3$circularity)
      circularity_pooled <- add_row(.data = circularity_pooled,
                                    time_point = i,
                                    genotype = g,
                                    cell_type = t,
                                    mean = m,
                                    sd = s)
    }
  }
}
circularity_pooled <- circularity_pooled[-1, ]

circularity_stats <- circularity_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
circularity_stats <- na.omit(circularity_stats)
circularity_stats$time_genotype <- factor(circularity_stats$time_genotype,
                                          levels = c("3.5_1_wt",
                                                     "3.5_2_pxy_two",
                                                     "3.5_3_pxy_four",
                                                     "4_1_wt",
                                                     "4_2_pxy_two",
                                                     "4_3_pxy_four",
                                                     "4.5_1_wt",
                                                     "4.5_2_pxy_two",
                                                     "4.5_3_pxy_four",
                                                     "5_1_wt",
                                                     "5_2_pxy_two",
                                                     "5_3_pxy_four"))

circularity <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         circularity)
circularity <- na.omit(circularity)
circularity$time_genotype <- factor(circularity$time_genotype,
                                    levels = c("3.5_1_wt",
                                               "3.5_2_pxy_two",
                                               "3.5_3_pxy_four",
                                               "4_1_wt",
                                               "4_2_pxy_two",
                                               "4_3_pxy_four",
                                               "4.5_1_wt",
                                               "4.5_2_pxy_two",
                                               "4.5_3_pxy_four",
                                               "5_1_wt",
                                               "5_2_pxy_two",
                                               "5_3_pxy_four"))

for (t in unique(circularity_pooled$cell_type)) {
  circularity_stats_t <- circularity_stats %>%
    filter(cell_type == t)

  circularity_t <- circularity %>%
    filter(cell_type == t)

  circularity_for_plot <- tibble(time_genotype = NA,
                                 circularity = NA)
  for (i in unique(circularity_t$time_genotype)) {
    filtered <- circularity_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$circularity,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$circularity)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$circularity > low & filtered$circularity < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             circularity)
    circularity_for_plot <- add_row(circularity_for_plot,
                                    filtered1)
  }
  circularity_for_plot <- circularity_for_plot[-1, ]

  ggplot() +
    ggbeeswarm::geom_quasirandom(data = circularity_for_plot,
                                 aes(x = time_genotype,
                                     y = circularity,
                                     colour = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = circularity_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = circularity_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("circularity_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_circularity_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         circularity) %>%
  filter(cell_type != "4_phloem_fiber")
attributes_for_circularity_test <- na.omit(attributes_for_circularity_test)

circularity_test_time <- tibble(cell_type = NA,
                                genotype = NA,
                                time_point = NA,
                                Shapiro_Wilk = NA,
                                Kruskal_Wallis = NA,
                                Dunn = NA)
for (t in unique(attributes_for_circularity_test$cell_type)) {
  filtered <- attributes_for_circularity_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$circularity ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$circularity,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$circularity)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      circularity_test_time <- add_row(.data = circularity_test_time,
                                       cell_type = t,
                                       genotype = g,
                                       time_point = i,
                                       Shapiro_Wilk = s,
                                       Kruskal_Wallis = k,
                                       Dunn = d1)
    }
  }
}
circularity_test_time <- circularity_test_time[-1, ]

attributes_for_circularity_genotype_test <- attributes_for_circularity_test

circularity_test_genotype <- tibble(cell_type = NA,
                                    time_point = NA,
                                    genotype = NA,
                                    Kruskal_Wallis = NA,
                                    Dunn = NA)
for (t in unique(attributes_for_circularity_genotype_test$cell_type)) {
  filtered <- attributes_for_circularity_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$circularity ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$circularity,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      circularity_test_genotype <- add_row(.data = circularity_test_genotype,
                                           cell_type = t,
                                           time_point = i,
                                           genotype = g,
                                           Kruskal_Wallis = k,
                                           Dunn = d1)
    }
  }
}
circularity_test_genotype <- circularity_test_genotype[-1, ]

for_phloem_fiber_circularity_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         circularity) %>%
  filter(cell_type == "4_phloem_fiber") %>%
  filter(time_point == 5)
k <- kruskal.test(for_phloem_fiber_circularity_test$circularity ~ for_phloem_fiber_circularity_test$genotype)
k <- tidy(k)
k <- k$p.value
for(g in unique(for_phloem_fiber_circularity_test$genotype)) {
  filtered <- for_phloem_fiber_circularity_test %>%
    filter(genotype == g)
  circularity_test_genotype <- add_row(.data = circularity_test_genotype,
                                       cell_type = "4_phloem_fiber",
                                       time_point = 5,
                                       genotype = g,
                                       Kruskal_Wallis = k,
                                       Dunn = NA)
}

#Incline angle

incline_pooled <- tibble(time_point = NA,
                         genotype = NA,
                         cell_type = NA,
                         mean = NA,
                         sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$incline_angle)
      s <- sd(filtered3$incline_angle)
      incline_pooled <- add_row(.data = incline_pooled,
                                time_point = i,
                                genotype = g,
                                cell_type = t,
                                mean = m,
                                sd = s)
    }
  }
}
incline_pooled <- incline_pooled[-1, ]

incline_stats <- incline_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
incline_stats <- na.omit(incline_stats)
incline_stats$time_genotype <- factor(incline_stats$time_genotype,
                                      levels = c("3.5_1_wt",
                                                 "3.5_2_pxy_two",
                                                 "3.5_3_pxy_four",
                                                 "4_1_wt",
                                                 "4_2_pxy_two",
                                                 "4_3_pxy_four",
                                                 "4.5_1_wt",
                                                 "4.5_2_pxy_two",
                                                 "4.5_3_pxy_four",
                                                 "5_1_wt",
                                                 "5_2_pxy_two",
                                                 "5_3_pxy_four"))

incline <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         incline_angle)
incline <- na.omit(incline)
incline$time_genotype <- factor(incline$time_genotype,
                                levels = c("3.5_1_wt",
                                           "3.5_2_pxy_two",
                                           "3.5_3_pxy_four",
                                           "4_1_wt",
                                           "4_2_pxy_two",
                                           "4_3_pxy_four",
                                           "4.5_1_wt",
                                           "4.5_2_pxy_two",
                                           "4.5_3_pxy_four",
                                           "5_1_wt",
                                           "5_2_pxy_two",
                                           "5_3_pxy_four"))

for (t in unique(incline_pooled$cell_type)) {
  incline_stats_t <- incline_stats %>%
    filter(cell_type == t)
  
  incline_t <- incline %>%
    filter(cell_type == t)
  
  incline_for_plot <- tibble(time_genotype = NA,
                             incline_angle = NA)
  for (i in unique(incline_t$time_genotype)) {
    filtered <- incline_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$incline_angle,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$incline_angle)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$incline_angle > low & filtered$incline_angle < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             incline_angle)
    incline_for_plot <- add_row(incline_for_plot,
                                filtered1)
  }
  incline_for_plot <- incline_for_plot[-1, ]
  
  ggplot() +
    ggbeeswarm::geom_quasirandom(data = incline_for_plot,
                                 aes(x = time_genotype,
                                     y = incline_angle,
                                     colour = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = incline_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = incline_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("incline_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_incline_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         incline_angle)
attributes_for_incline_test <- na.omit(attributes_for_incline_test)

attributes_for_incline_time_test <- attributes_for_incline_test %>%
  filter(!(genotype == "1_wt" & cell_type == "1_xylem_vessels")) %>%
  filter(!(genotype == "3_pxy_four" & cell_type == "1_xylem_vessels")) %>%
  filter(!(genotype == "3_pxy_four" & cell_type == "3_cambium")) %>%
  filter(cell_type != "4_phloem_fiber")

incline_test_time <- tibble(cell_type = NA,
                            genotype = NA,
                            time_point = NA,
                            Shapiro_Wilk = NA,
                            Kruskal_Wallis = NA,
                            Dunn = NA)
for (t in unique(attributes_for_incline_time_test$cell_type)) {
  filtered <- attributes_for_incline_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$incline_angle ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$incline_angle,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$incline_angle)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      incline_test_time <- add_row(.data = incline_test_time,
                                   cell_type = t,
                                   genotype = g,
                                   time_point = i,
                                   Shapiro_Wilk = s,
                                   Kruskal_Wallis = k,
                                   Dunn = d1)
    }
  }
}
incline_test_time <- incline_test_time[-1, ]

attributes_for_incline_time_test_ns <- attributes_for_incline_test %>%
  filter((genotype == "1_wt" & cell_type == "1_xylem_vessels") |
           (genotype == "3_pxy_four" & cell_type == "1_xylem_vessels") |
           (genotype == "3_pxy_four" & cell_type == "3_cambium"))

for (t in unique(attributes_for_incline_time_test_ns$cell_type)) {
  filtered <- attributes_for_incline_time_test_ns %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$incline_angle ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$incline_angle)
      s <- tidy(s)
      s <- s$p.value
      incline_test_time <- add_row(.data = incline_test_time,
                                   cell_type = t,
                                   genotype = g,
                                   time_point = i,
                                   Shapiro_Wilk = s,
                                   Kruskal_Wallis = k,
                                   Dunn = NA)
    }
  }
}

attributes_for_incline_genotype_test <- attributes_for_incline_test %>%
  filter(!(time_point == 3.5 & cell_type == "1_xylem_vessels")) %>%
  filter(!(time_point == 3.5 & cell_type == "2_xylem_parenchyma")) %>%
  filter(!(time_point == 5 & cell_type == "2_xylem_parenchyma"))

incline_test_genotype <- tibble(cell_type = NA,
                                time_point = NA,
                                genotype = NA,
                                Kruskal_Wallis = NA,
                                Dunn = NA)
for (t in unique(attributes_for_incline_genotype_test$cell_type)) {
  filtered <- attributes_for_incline_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$incline_angle ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$incline_angle,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      incline_test_genotype <- add_row(.data = incline_test_genotype,
                                       cell_type = t,
                                       time_point = i,
                                       genotype = g,
                                       Kruskal_Wallis = k,
                                       Dunn = d1)
    }
  }
}
incline_test_genotype <- incline_test_genotype[-1, ]

attributes_for_incline_genotype_test_ns <- attributes_for_incline_test %>%
  filter((time_point == 3.5 & cell_type == "1_xylem_vessels") |
           (time_point == 3.5 & cell_type == "2_xylem_parenchyma") |
           (time_point == 5 & cell_type == "2_xylem_parenchyma"))

for (t in unique(attributes_for_incline_genotype_test_ns$cell_type)) {
  filtered <- attributes_for_incline_genotype_test_ns %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$incline_angle ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      incline_test_genotype <- add_row(.data = incline_test_genotype,
                                       cell_type = t,
                                       time_point = i,
                                       genotype = g,
                                       Kruskal_Wallis = k,
                                       Dunn = NA)
    }
  }
}

#Lobeyness

lobeyness_pooled <- tibble(time_point = NA,
                           genotype = NA,
                           cell_type = NA,
                           mean = NA,
                           sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$lobeyness)
      s <- sd(filtered3$lobeyness)
      lobeyness_pooled <- add_row(.data = lobeyness_pooled,
                                  time_point = i,
                                  genotype = g,
                                  cell_type = t,
                                  mean = m,
                                  sd = s)
    }
  }
}
lobeyness_pooled <- lobeyness_pooled[-1, ]

lobeyness_stats <- lobeyness_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
lobeyness_stats <- na.omit(lobeyness_stats)
lobeyness_stats$time_genotype <- factor(lobeyness_stats$time_genotype,
                                        levels = c("3.5_1_wt",
                                                   "3.5_2_pxy_two",
                                                   "3.5_3_pxy_four",
                                                   "4_1_wt",
                                                   "4_2_pxy_two",
                                                   "4_3_pxy_four",
                                                   "4.5_1_wt",
                                                   "4.5_2_pxy_two",
                                                   "4.5_3_pxy_four",
                                                   "5_1_wt",
                                                   "5_2_pxy_two",
                                                   "5_3_pxy_four"))

lobeyness <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         lobeyness)
lobeyness <- na.omit(lobeyness)
lobeyness$time_genotype <- factor(lobeyness$time_genotype,
                                  levels = c("3.5_1_wt",
                                             "3.5_2_pxy_two",
                                             "3.5_3_pxy_four",
                                             "4_1_wt",
                                             "4_2_pxy_two",
                                             "4_3_pxy_four",
                                             "4.5_1_wt",
                                             "4.5_2_pxy_two",
                                             "4.5_3_pxy_four",
                                             "5_1_wt",
                                             "5_2_pxy_two",
                                             "5_3_pxy_four"))

for (t in unique(lobeyness_pooled$cell_type)) {
  lobeyness_stats_t <- lobeyness_stats %>%
    filter(cell_type == t)
  
  lobeyness_t <- lobeyness %>%
    filter(cell_type == t)
  
  lobeyness_for_plot <- tibble(time_genotype = NA,
                               lobeyness = NA)
  for (i in unique(lobeyness_t$time_genotype)) {
    filtered <- lobeyness_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$lobeyness,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$lobeyness)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$lobeyness > low & filtered$lobeyness < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             lobeyness)
    lobeyness_for_plot <- add_row(lobeyness_for_plot,
                                  filtered1)
  }
  lobeyness_for_plot <- lobeyness_for_plot[-1, ]
  
  ggplot() +
    ggbeeswarm::geom_quasirandom(data = lobeyness_for_plot,
                                 aes(x = time_genotype,
                                     y = lobeyness,
                                     colour = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = lobeyness_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = lobeyness_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("lobeyness_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_lobeyness_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         lobeyness)
attributes_for_lobeyness_test <- na.omit(attributes_for_lobeyness_test)

attributes_for_lobeyness_time_test <- attributes_for_lobeyness_test %>%
  filter(cell_type != "4_phloem_fiber")

lobeyness_test_time <- tibble(cell_type = NA,
                              genotype = NA,
                              time_point = NA,
                              Shapiro_Wilk = NA,
                              Kruskal_Wallis = NA,
                              Dunn = NA)
for (t in unique(attributes_for_lobeyness_time_test$cell_type)) {
  filtered <- attributes_for_lobeyness_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$lobeyness ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$lobeyness,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$lobeyness)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      lobeyness_test_time <- add_row(.data = lobeyness_test_time,
                                     cell_type = t,
                                     genotype = g,
                                     time_point = i,
                                     Shapiro_Wilk = s,
                                     Kruskal_Wallis = k,
                                     Dunn = d1)
    }
  }
}
lobeyness_test_time <- lobeyness_test_time[-1, ]

attributes_for_lobeyness_genotype_test <- attributes_for_lobeyness_test %>%
  filter(!(time_point == 3.5 & cell_type == "1_xylem_vessels")) %>%
  filter(!(time_point == 5 & cell_type == "1_xylem_vessels")) %>%
  filter(!(time_point == 3.5 & cell_type == "5_phloem_parenchyma")) %>%
  filter(!(time_point == 3.5 & cell_type == "6_periderm")) %>%
  filter(cell_type != "4_phloem_fiber")

lobeyness_test_genotype <- tibble(cell_type = NA,
                                  time_point = NA,
                                  genotype = NA,
                                  Kruskal_Wallis = NA,
                                  Dunn = NA)
for (t in unique(attributes_for_lobeyness_genotype_test$cell_type)) {
  filtered <- attributes_for_lobeyness_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$lobeyness ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$lobeyness,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      lobeyness_test_genotype <- add_row(.data = lobeyness_test_genotype,
                                         cell_type = t,
                                         time_point = i,
                                         genotype = g,
                                         Kruskal_Wallis = k,
                                         Dunn = d1)
    }
  }
}
lobeyness_test_genotype <- lobeyness_test_genotype[-1, ]

attributes_for_lobeyness_genotype_test_ns <- attributes_for_lobeyness_test %>%
  filter((time_point == 3.5 & cell_type == "1_xylem_vessels") |
           (time_point == 5 & cell_type == "1_xylem_vessels") |
           (time_point == 3.5 & cell_type == "5_phloem_parenchyma") |
           (time_point == 3.5 & cell_type == "6_periderm") |
           (cell_type == "4_phloem_fiber"))

for (t in unique(attributes_for_lobeyness_genotype_test_ns$cell_type)) {
  filtered <- attributes_for_lobeyness_genotype_test_ns %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$lobeyness ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      lobeyness_test_genotype <- add_row(.data = lobeyness_test_genotype,
                                       cell_type = t,
                                       time_point = i,
                                       genotype = g,
                                       Kruskal_Wallis = k,
                                       Dunn = NA)
    }
  }
}

#Major axis

major_pooled <- tibble(time_point = NA,
                       genotype = NA,
                       cell_type = NA,
                       mean = NA,
                       sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$major_axis)
      s <- sd(filtered3$major_axis)
      major_pooled <- add_row(.data = major_pooled,
                              time_point = i,
                              genotype = g,
                              cell_type = t,
                              mean = m,
                              sd = s)
    }
  }
}
major_pooled <- major_pooled[-1, ]

major_stats <- major_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
major_stats <- na.omit(major_stats)
major_stats$time_genotype <- factor(major_stats$time_genotype,
                                    levels = c("3.5_1_wt",
                                               "3.5_2_pxy_two",
                                               "3.5_3_pxy_four",
                                               "4_1_wt",
                                               "4_2_pxy_two",
                                               "4_3_pxy_four",
                                               "4.5_1_wt",
                                               "4.5_2_pxy_two",
                                               "4.5_3_pxy_four",
                                               "5_1_wt",
                                               "5_2_pxy_two",
                                               "5_3_pxy_four"))

major <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         major_axis)
major <- na.omit(major)
major$time_genotype <- factor(major$time_genotype,
                              levels = c("3.5_1_wt",
                                         "3.5_2_pxy_two",
                                         "3.5_3_pxy_four",
                                         "4_1_wt",
                                         "4_2_pxy_two",
                                         "4_3_pxy_four",
                                         "4.5_1_wt",
                                         "4.5_2_pxy_two",
                                         "4.5_3_pxy_four",
                                         "5_1_wt",
                                         "5_2_pxy_two",
                                         "5_3_pxy_four"))

for (t in unique(major_pooled$cell_type)) {
  major_stats_t <- major_stats %>%
    filter(cell_type == t)
  
  major_t <- major %>%
    filter(cell_type == t)
  
  major_for_plot <- tibble(time_genotype = NA,
                           major_axis = NA)
  for (i in unique(major_t$time_genotype)) {
    filtered <- major_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$major_axis,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$major_axis)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$major_axis > low & filtered$major_axis < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             major_axis)
    major_for_plot <- add_row(major_for_plot,
                              filtered1)
  }
  major_for_plot <- major_for_plot[-1, ]
  
  ggplot() +
    ggbeeswarm::geom_quasirandom(data = major_for_plot,
                                 aes(x = time_genotype,
                                     y = major_axis,
                                     colour = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = major_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = major_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("major_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_major_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         major_axis)
attributes_for_major_test <- na.omit(attributes_for_major_test)

attributes_for_major_time_test <- attributes_for_major_test %>%
  filter(cell_type != "4_phloem_fiber")

major_test_time <- tibble(cell_type = NA,
                          genotype = NA,
                          time_point = NA,
                          Shapiro_Wilk = NA,
                          Kruskal_Wallis = NA,
                          Dunn = NA)
for (t in unique(attributes_for_major_time_test$cell_type)) {
  filtered <- attributes_for_major_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$major_axis ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$major_axis,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$major_axis)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      major_test_time <- add_row(.data = major_test_time,
                                 cell_type = t,
                                 genotype = g,
                                 time_point = i,
                                 Shapiro_Wilk = s,
                                 Kruskal_Wallis = k,
                                 Dunn = d1)
    }
  }
}
major_test_time <- major_test_time[-1, ]

attributes_for_major_genotype_test <- attributes_for_major_test %>%
  filter(!(time_point == 3.5 & cell_type == "5_phloem_parenchyma")) %>%
  filter(!(time_point == 4 & cell_type == "6_periderm")) %>%
  filter(cell_type != "4_phloem_fiber")

major_test_genotype <- tibble(cell_type = NA,
                              time_point = NA,
                              genotype = NA,
                              Kruskal_Wallis = NA,
                              Dunn = NA)
for (t in unique(attributes_for_major_genotype_test$cell_type)) {
  filtered <- attributes_for_major_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$major_axis ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$major_axis,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      major_test_genotype <- add_row(.data = major_test_genotype,
                                     cell_type = t,
                                     time_point = i,
                                     genotype = g,
                                     Kruskal_Wallis = k,
                                     Dunn = d1)
    }
  }
}
major_test_genotype <- major_test_genotype[-1, ]

attributes_for_major_genotype_test_ns <- attributes_for_major_test %>%
  filter((time_point == 3.5 & cell_type == "5_phloem_parenchyma") |
           (time_point == 4 & cell_type == "6_periderm") |
           (cell_type == "4_phloem_fiber"))

for (t in unique(attributes_for_major_genotype_test_ns$cell_type)) {
  filtered <- attributes_for_major_genotype_test_ns %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$major_axis ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      major_test_genotype <- add_row(.data = major_test_genotype,
                                         cell_type = t,
                                         time_point = i,
                                         genotype = g,
                                         Kruskal_Wallis = k,
                                         Dunn = NA)
    }
  }
}

#Minor axis

minor_pooled <- tibble(time_point = NA,
                       genotype = NA,
                       cell_type = NA,
                       mean = NA,
                       sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$minor_axis)
      s <- sd(filtered3$minor_axis)
      minor_pooled <- add_row(.data = minor_pooled,
                              time_point = i,
                              genotype = g,
                              cell_type = t,
                              mean = m,
                              sd = s)
    }
  }
}
minor_pooled <- minor_pooled[-1, ]

minor_stats <- minor_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
minor_stats <- na.omit(minor_stats)
minor_stats$time_genotype <- factor(minor_stats$time_genotype,
                                    levels = c("3.5_1_wt",
                                               "3.5_2_pxy_two",
                                               "3.5_3_pxy_four",
                                               "4_1_wt",
                                               "4_2_pxy_two",
                                               "4_3_pxy_four",
                                               "4.5_1_wt",
                                               "4.5_2_pxy_two",
                                               "4.5_3_pxy_four",
                                               "5_1_wt",
                                               "5_2_pxy_two",
                                               "5_3_pxy_four"))

minor <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         minor_axis)
minor <- na.omit(minor)
minor$time_genotype <- factor(minor$time_genotype,
                              levels = c("3.5_1_wt",
                                         "3.5_2_pxy_two",
                                         "3.5_3_pxy_four",
                                         "4_1_wt",
                                         "4_2_pxy_two",
                                         "4_3_pxy_four",
                                         "4.5_1_wt",
                                         "4.5_2_pxy_two",
                                         "4.5_3_pxy_four",
                                         "5_1_wt",
                                         "5_2_pxy_two",
                                         "5_3_pxy_four"))

for (t in unique(minor_pooled$cell_type)) {
  minor_stats_t <- minor_stats %>%
    filter(cell_type == t)
  
  minor_t <- minor %>%
    filter(cell_type == t)
  
  minor_for_plot <- tibble(time_genotype = NA,
                           minor_axis = NA)
  for (i in unique(minor_t$time_genotype)) {
    filtered <- minor_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$minor_axis,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$minor_axis)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$minor_axis > low & filtered$minor_axis < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             minor_axis)
    minor_for_plot <- add_row(minor_for_plot,
                              filtered1)
  }
  minor_for_plot <- minor_for_plot[-1, ]
  
  ggplot() +
    ggbeeswarm::geom_quasirandom(data = minor_for_plot,
                                 aes(x = time_genotype,
                                     y = minor_axis,
                                     colour = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = minor_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = minor_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("minor_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_minor_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         minor_axis) #pause
attributes_for_minor_test <- na.omit(attributes_for_minor_test)

attributes_for_minor_time_test <- attributes_for_minor_test %>%
  filter(cell_type != "4_phloem_fiber")

minor_test_time <- tibble(cell_type = NA,
                          genotype = NA,
                          time_point = NA,
                          Shapiro_Wilk = NA,
                          Kruskal_Wallis = NA,
                          Dunn = NA)
for (t in unique(attributes_for_minor_time_test$cell_type)) {
  filtered <- attributes_for_minor_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$minor_axis ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$minor_axis,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$minor_axis)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      minor_test_time <- add_row(.data = minor_test_time,
                                 cell_type = t,
                                 genotype = g,
                                 time_point = i,
                                 Shapiro_Wilk = s,
                                 Kruskal_Wallis = k,
                                 Dunn = d1)
    }
  }
}
minor_test_time <- minor_test_time[-1, ]

attributes_for_minor_genotype_test <- attributes_for_minor_test %>%
  filter(!(time_point == 3.5 & cell_type == "2_xylem_parenchyma")) %>%
  filter(!(time_point == 4.5 & cell_type == "2_xylem_parenchyma")) %>%
  filter(!(time_point == 3.5 & cell_type == "5_phloem_parenchyma")) %>%
  filter(cell_type != "4_phloem_fiber")

minor_test_genotype <- tibble(cell_type = NA,
                              time_point = NA,
                              genotype = NA,
                              Kruskal_Wallis = NA,
                              Dunn = NA)
for (t in unique(attributes_for_minor_genotype_test$cell_type)) {
  filtered <- attributes_for_minor_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$minor_axis ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$minor_axis,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      minor_test_genotype <- add_row(.data = minor_test_genotype,
                                     cell_type = t,
                                     time_point = i,
                                     genotype = g,
                                     Kruskal_Wallis = k,
                                     Dunn = d1)
    }
  }
}
minor_test_genotype <- minor_test_genotype[-1, ]

attributes_for_minor_genotype_test_ns <- attributes_for_minor_test %>%
  filter((time_point == 3.5 & cell_type == "2_xylem_parenchyma") |
           (time_point == 4.5 & cell_type == "2_xylem_parenchyma") |
           (time_point == 3.5 & cell_type == "5_phloem_parenchyma") |
           (cell_type == "4_phloem_fiber"))

for (t in unique(attributes_for_minor_genotype_test_ns$cell_type)) {
  filtered <- attributes_for_minor_genotype_test_ns %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$minor_axis ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      minor_test_genotype <- add_row(.data = minor_test_genotype,
                                     cell_type = t,
                                     time_point = i,
                                     genotype = g,
                                     Kruskal_Wallis = k,
                                     Dunn = NA)
    }
  }
}

#Neighbors

neighbors_pooled <- tibble(time_point = NA,
                           genotype = NA,
                           cell_type = NA,
                           mean = NA,
                           sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$neighbors)
      s <- sd(filtered3$neighbors)
      neighbors_pooled <- add_row(.data = neighbors_pooled,
                                  time_point = i,
                                  genotype = g,
                                  cell_type = t,
                                  mean = m,
                                  sd = s)
    }
  }
}
neighbors_pooled <- neighbors_pooled[-1, ]

neighbors_stats <- neighbors_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
neighbors_stats <- na.omit(neighbors_stats)
neighbors_stats$time_genotype <- factor(neighbors_stats$time_genotype,
                                        levels = c("3.5_1_wt",
                                                   "3.5_2_pxy_two",
                                                   "3.5_3_pxy_four",
                                                   "4_1_wt",
                                                   "4_2_pxy_two",
                                                   "4_3_pxy_four",
                                                   "4.5_1_wt",
                                                   "4.5_2_pxy_two",
                                                   "4.5_3_pxy_four",
                                                   "5_1_wt",
                                                   "5_2_pxy_two",
                                                   "5_3_pxy_four"))

neighbors <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         neighbors)
neighbors <- na.omit(neighbors)
neighbors$time_genotype <- factor(neighbors$time_genotype,
                                  levels = c("3.5_1_wt",
                                             "3.5_2_pxy_two",
                                             "3.5_3_pxy_four",
                                             "4_1_wt",
                                             "4_2_pxy_two",
                                             "4_3_pxy_four",
                                             "4.5_1_wt",
                                             "4.5_2_pxy_two",
                                             "4.5_3_pxy_four",
                                             "5_1_wt",
                                             "5_2_pxy_two",
                                             "5_3_pxy_four"))

for (t in unique(neighbors_pooled$cell_type)) {
  neighbors_stats_t <- neighbors_stats %>%
    filter(cell_type == t)
  
  neighbors_t <- neighbors %>%
    filter(cell_type == t)
  
  neighbors_for_plot <- tibble(time_genotype = NA,
                               neighbors = NA)
  for (i in unique(neighbors_t$time_genotype)) {
    filtered <- neighbors_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$neighbors,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$neighbors)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$neighbors > low & filtered$neighbors < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             neighbors)
    neighbors_for_plot <- add_row(neighbors_for_plot,
                              filtered1)
  }
  neighbors_for_plot <- neighbors_for_plot[-1, ]
  
  ggplot() +
    geom_jitter(data = neighbors_for_plot,
                                 aes(x = time_genotype,
                                     y = neighbors,
                                     color = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = neighbors_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = neighbors_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("neighbors_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_neighbors_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         neighbors)
attributes_for_neighbors_test <- na.omit(attributes_for_neighbors_test)

attributes_for_neighbors_time_test <- attributes_for_neighbors_test %>%
  filter(!(genotype == "1_wt" & cell_type == "3_cambium")) %>%
  filter(!(genotype == "2_pxy_two" & cell_type == "5_phloem_parenchyma")) %>%
  filter(cell_type != "4_phloem_fiber")

neighbors_test_time <- tibble(cell_type = NA,
                              genotype = NA,
                              time_point = NA,
                              Shapiro_Wilk = NA,
                              Kruskal_Wallis = NA,
                              Dunn = NA)
for (t in unique(attributes_for_neighbors_time_test$cell_type)) {
  filtered <- attributes_for_neighbors_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$neighbors ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$neighbors,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$neighbors)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      neighbors_test_time <- add_row(.data = neighbors_test_time,
                                     cell_type = t,
                                     genotype = g,
                                     time_point = i,
                                     Shapiro_Wilk = s,
                                     Kruskal_Wallis = k,
                                     Dunn = d1)
    }
  }
}
neighbors_test_time <- neighbors_test_time[-1, ]

attributes_for_neighbors_time_test_ns <- attributes_for_neighbors_test %>%
  filter((genotype == "1_wt" & cell_type == "3_cambium") |
           (genotype == "2_pxy_two" & cell_type == "5_phloem_parenchyma"))

for (t in unique(attributes_for_neighbors_time_test_ns$cell_type)) {
  filtered <- attributes_for_neighbors_time_test_ns %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$neighbors ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$neighbors)
      s <- tidy(s)
      s <- s$p.value
      neighbors_test_time <- add_row(.data = neighbors_test_time,
                                     cell_type = t,
                                     genotype = g,
                                     time_point = i,
                                     Shapiro_Wilk = s,
                                     Kruskal_Wallis = k,
                                     Dunn = NA)
    }
  }
}

attributes_for_neighbors_genotype_test <- attributes_for_neighbors_test %>%
  filter(!(time_point == 3.5 & cell_type == "3_cambium")) %>%
  filter(!(time_point == 4 & cell_type == "3_cambium")) %>%
  filter(!(time_point == 5 & cell_type == "3_cambium")) %>%
  filter(!(time_point == 3.5 & cell_type == "5_phloem_parenchyma")) %>%
  filter(!(time_point == 3.5 & cell_type == "6_periderm")) %>%
  filter(!(time_point == 4 & cell_type == "6_periderm")) %>%
  filter(!(time_point == 4.5 & cell_type == "6_periderm")) %>%
  filter(cell_type != "4_phloem_fiber")

neighbors_test_genotype <- tibble(cell_type = NA,
                                  time_point = NA,
                                  genotype = NA,
                                  Kruskal_Wallis = NA,
                                  Dunn = NA)
for (t in unique(attributes_for_neighbors_genotype_test$cell_type)) {
  filtered <- attributes_for_neighbors_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$neighbors ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$neighbors,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      neighbors_test_genotype <- add_row(.data = neighbors_test_genotype,
                                         cell_type = t,
                                         time_point = i,
                                         genotype = g,
                                         Kruskal_Wallis = k,
                                         Dunn = d1)
    }
  }
}
neighbors_test_genotype <- neighbors_test_genotype[-1, ]

attributes_for_neighbors_genotype_test_ns <- attributes_for_neighbors_test %>%
  filter((time_point == 3.5 & cell_type == "3_cambium") |
           (time_point == 4 & cell_type == "3_cambium") |
           (time_point == 5 & cell_type == "3_cambium") |
           (time_point == 3.5 & cell_type == "5_phloem_parenchyma") |
           (time_point == 3.5 & cell_type == "6_periderm") |
           (time_point == 4 & cell_type == "6_periderm") |
           (time_point == 4.5 & cell_type == "6_periderm") |
           (cell_type == "4_phloem_fiber"))

for (t in unique(attributes_for_neighbors_genotype_test_ns$cell_type)) {
  filtered <- attributes_for_neighbors_genotype_test_ns %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$neighbors ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      neighbors_test_genotype <- add_row(.data = neighbors_test_genotype,
                                         cell_type = t,
                                         time_point = i,
                                         genotype = g,
                                         Kruskal_Wallis = k,
                                         Dunn = NA)
    }
  }
}

#Perimeter

perimeter_pooled <- tibble(time_point = NA,
                           genotype = NA,
                           cell_type = NA,
                           mean = NA,
                           sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$perimeter)
      s <- sd(filtered3$perimeter)
      perimeter_pooled <- add_row(.data = perimeter_pooled,
                                  time_point = i,
                                  genotype = g,
                                  cell_type = t,
                                  mean = m,
                                  sd = s)
    }
  }
}
perimeter_pooled <- perimeter_pooled[-1, ]

perimeter_stats <- perimeter_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
perimeter_stats <- na.omit(perimeter_stats)
perimeter_stats$time_genotype <- factor(perimeter_stats$time_genotype,
                                        levels = c("3.5_1_wt",
                                                   "3.5_2_pxy_two",
                                                   "3.5_3_pxy_four",
                                                   "4_1_wt",
                                                   "4_2_pxy_two",
                                                   "4_3_pxy_four",
                                                   "4.5_1_wt",
                                                   "4.5_2_pxy_two",
                                                   "4.5_3_pxy_four",
                                                   "5_1_wt",
                                                   "5_2_pxy_two",
                                                   "5_3_pxy_four"))

perimeter <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         perimeter)
perimeter <- na.omit(perimeter)
perimeter$time_genotype <- factor(perimeter$time_genotype,
                                  levels = c("3.5_1_wt",
                                             "3.5_2_pxy_two",
                                             "3.5_3_pxy_four",
                                             "4_1_wt",
                                             "4_2_pxy_two",
                                             "4_3_pxy_four",
                                             "4.5_1_wt",
                                             "4.5_2_pxy_two",
                                             "4.5_3_pxy_four",
                                             "5_1_wt",
                                             "5_2_pxy_two",
                                             "5_3_pxy_four"))

for (t in unique(perimeter_pooled$cell_type)) {
  perimeter_stats_t <- perimeter_stats %>%
    filter(cell_type == t)

  perimeter_t <- perimeter %>%
    filter(cell_type == t)

  perimeter_for_plot <- tibble(time_genotype = NA,
                               perimeter = NA)
  for (i in unique(perimeter_t$time_genotype)) {
    filtered <- perimeter_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$perimeter,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$perimeter)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$perimeter > low & filtered$perimeter < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             perimeter)
    perimeter_for_plot <- add_row(perimeter_for_plot,
                                  filtered1)
  }
  perimeter_for_plot <- perimeter_for_plot[-1, ]

  ggplot() +
    ggbeeswarm::geom_quasirandom(data = perimeter_for_plot,
                                 aes(x = time_genotype,
                                     y = perimeter,
                                     colour = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = perimeter_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = perimeter_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("perimeter_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_perimeter_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         perimeter)
attributes_for_perimeter_test <- na.omit(attributes_for_perimeter_test)

attributes_for_perimeter_time_test <- attributes_for_perimeter_test %>%
  filter(cell_type != "4_phloem_fiber")

perimeter_test_time <- tibble(cell_type = NA,
                              genotype = NA,
                              time_point = NA,
                              Shapiro_Wilk = NA,
                              Kruskal_Wallis = NA,
                              Dunn = NA)
for (t in unique(attributes_for_perimeter_time_test$cell_type)) {
  filtered <- attributes_for_perimeter_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$perimeter ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$perimeter,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$perimeter)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      perimeter_test_time <- add_row(.data = perimeter_test_time,
                                     cell_type = t,
                                     genotype = g,
                                     time_point = i,
                                     Shapiro_Wilk = s,
                                     Kruskal_Wallis = k,
                                     Dunn = d1)
    }
  }
}
perimeter_test_time <- perimeter_test_time[-1, ]

attributes_for_perimeter_genotype_test <- attributes_for_perimeter_test %>%
  filter(!(time_point == 3.5 & cell_type == "2_xylem_parenchyma")) %>%
  filter(!(time_point == 3.5 & cell_type == "5_phloem_parenchyma")) %>%
  filter(cell_type != "4_phloem_fiber")

perimeter_test_genotype <- tibble(cell_type = NA,
                                  time_point = NA,
                                  genotype = NA,
                                  Kruskal_Wallis = NA,
                                  Dunn = NA)
for (t in unique(attributes_for_perimeter_genotype_test$cell_type)) {
  filtered <- attributes_for_perimeter_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$perimeter ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$perimeter,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      perimeter_test_genotype <- add_row(.data = perimeter_test_genotype,
                                         cell_type = t,
                                         time_point = i,
                                         genotype = g,
                                         Kruskal_Wallis = k,
                                         Dunn = d1)
    }
  }
}
perimeter_test_genotype <- perimeter_test_genotype[-1, ]

attributes_for_perimeter_genotype_test_ns <- attributes_for_perimeter_test %>%
  filter((time_point == 3.5 & cell_type == "2_xylem_parenchyma") |
           (time_point == 3.5 & cell_type == "5_phloem_parenchyma") |
           (cell_type == "4_phloem_fiber"))

for (t in unique(attributes_for_perimeter_genotype_test_ns$cell_type)) {
  filtered <- attributes_for_perimeter_genotype_test_ns %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$perimeter ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      perimeter_test_genotype <- add_row(.data = perimeter_test_genotype,
                                         cell_type = t,
                                         time_point = i,
                                         genotype = g,
                                         Kruskal_Wallis = k,
                                         Dunn = NA)
    }
  }
}

#Rectangularity

rectangularity_pooled <- tibble(time_point = NA,
                                genotype = NA,
                                cell_type = NA,
                                mean = NA,
                                sd = NA)
for (i in unique(attributes$time_point)) {
  filtered1 <- attributes %>%
    filter(time_point == i)
  for (g in unique(filtered1$genotype)) {
    filtered2 <- filtered1 %>%
      filter(genotype == g)
    for (t in unique(filtered2$cell_type)) {
      filtered3 <- filtered2 %>%
        filter(cell_type == t)
      m <- mean(filtered3$rectangularity)
      s <- sd(filtered3$rectangularity)
      rectangularity_pooled <- add_row(.data = rectangularity_pooled,
                                       time_point = i,
                                       genotype = g,
                                       cell_type = t,
                                       mean = m,
                                       sd = s)
    }
  }
}
rectangularity_pooled <- rectangularity_pooled[-1, ]

rectangularity_stats <- rectangularity_pooled %>%
  unite(col = time_genotype,
        time_point,
        genotype)
rectangularity_stats <- na.omit(rectangularity_stats)
rectangularity_stats$time_genotype <- factor(rectangularity_stats$time_genotype,
                                             levels = c("3.5_1_wt",
                                                        "3.5_2_pxy_two",
                                                        "3.5_3_pxy_four",
                                                        "4_1_wt",
                                                        "4_2_pxy_two",
                                                        "4_3_pxy_four",
                                                        "4.5_1_wt",
                                                        "4.5_2_pxy_two",
                                                        "4.5_3_pxy_four",
                                                        "5_1_wt",
                                                        "5_2_pxy_two",
                                                        "5_3_pxy_four"))

rectangularity <- attributes %>%
  unite(col = time_genotype,
        time_point,
        genotype) %>%
  select(time_genotype,
         cell_type,
         rectangularity)
rectangularity <- na.omit(rectangularity)
rectangularity$time_genotype <- factor(rectangularity$time_genotype,
                                       levels = c("3.5_1_wt",
                                                  "3.5_2_pxy_two",
                                                  "3.5_3_pxy_four",
                                                  "4_1_wt",
                                                  "4_2_pxy_two",
                                                  "4_3_pxy_four",
                                                  "4.5_1_wt",
                                                  "4.5_2_pxy_two",
                                                  "4.5_3_pxy_four",
                                                  "5_1_wt",
                                                  "5_2_pxy_two",
                                                  "5_3_pxy_four"))

for (t in unique(rectangularity_pooled$cell_type)) {
  rectangularity_stats_t <- rectangularity_stats %>%
    filter(cell_type == t)
  
  rectangularity_t <- rectangularity %>%
    filter(cell_type == t)
  
  rectangularity_for_plot <- tibble(time_genotype = NA,
                                    rectangularity = NA)
  for (i in unique(rectangularity_t$time_genotype)) {
    filtered <- rectangularity_t %>%
      filter(time_genotype == i)
    Q <- quantile(filtered$rectangularity,
                  probs = c(.25, .75),
                  na.rm = FALSE)
    iqr <- IQR(filtered$rectangularity)
    up <- Q[2] + 1.5 * iqr
    low <- Q[1] - 1.5 * iqr
    filtered1 <- subset(filtered,
                        filtered$rectangularity > low & filtered$rectangularity < up)
    filtered1 <- filtered1 %>%
      select(time_genotype,
             rectangularity)
    rectangularity_for_plot <- add_row(rectangularity_for_plot,
                                       filtered1)
  }
  rectangularity_for_plot <- rectangularity_for_plot[-1, ]
  
  ggplot() +
    ggbeeswarm::geom_quasirandom(data = rectangularity_for_plot,
                                 aes(x = time_genotype,
                                     y = rectangularity,
                                     colour = time_genotype,)) +
    scale_color_manual(values = c("#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C",
                                  "#444F6A",
                                  "#7C7A77",
                                  "#C0B16C")) +
    geom_linerange(data = rectangularity_stats_t,
                   aes(x = time_genotype,
                       ymin = mean - sd,
                       ymax = mean + sd),
                   colour = "red") +
    ggbeeswarm::geom_quasirandom(data = rectangularity_stats_t,
                                 aes(x = time_genotype,
                                     y = mean),
                                 colour = "red",
                                 shape = 3,
                                 size = 5) +
    theme_minimal()
  name = paste0("rectangularity_", t, ".pdf")
  ggsave(filename = name,
         device = "pdf",
         width = 8,
         height = 8,
         units = "in")
}

attributes_for_rectangularity_test <- attributes %>%
  select(time_point,
         genotype,
         cell_type,
         rectangularity)
attributes_for_rectangularity_test <- na.omit(attributes_for_rectangularity_test)

attributes_for_rectangularity_time_test <- attributes_for_rectangularity_test %>%
  filter(!(genotype == "2_pxy_two" & cell_type == "2_xylem_parenchyma")) %>%
  filter(!(genotype == "3_pxy_four" & cell_type == "3_cambium")) %>%
  filter(cell_type != "4_phloem_fiber")

rectangularity_test_time <- tibble(cell_type = NA,
                                   genotype = NA,
                                   time_point = NA,
                                   Shapiro_Wilk = NA,
                                   Kruskal_Wallis = NA,
                                   Dunn = NA)
for (t in unique(attributes_for_rectangularity_time_test$cell_type)) {
  filtered <- attributes_for_rectangularity_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$rectangularity ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$rectangularity,
                  g = filtered1$time_point,
                  method = "bonferroni")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$rectangularity)
      s <- tidy(s)
      s <- s$p.value
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      rectangularity_test_time <- add_row(.data = rectangularity_test_time,
                                          cell_type = t,
                                          genotype = g,
                                          time_point = i,
                                          Shapiro_Wilk = s,
                                          Kruskal_Wallis = k,
                                          Dunn = d1)
    }
  }
}
rectangularity_test_time <- rectangularity_test_time[-1, ]

attributes_for_rectangularity_time_test_ns <- attributes_for_rectangularity_test %>%
  filter((genotype == "2_pxy_two" & cell_type == "2_xylem_parenchyma") |
           (genotype == "3_pxy_four" & cell_type == "3_cambium"))

for (t in unique(attributes_for_rectangularity_time_test_ns$cell_type)) {
  filtered <- attributes_for_rectangularity_time_test_ns %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$rectangularity ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      s <- shapiro.test(filtered2$rectangularity)
      s <- tidy(s)
      s <- s$p.value
      rectangularity_test_time <- add_row(.data = rectangularity_test_time,
                                          cell_type = t,
                                          genotype = g,
                                          time_point = i,
                                          Shapiro_Wilk = s,
                                          Kruskal_Wallis = k,
                                          Dunn = NA)
    }
  }
}

attributes_for_rectangularity_genotype_test <- attributes_for_rectangularity_test %>%
  filter(!(time_point == 4.5 & cell_type == "1_xylem_vessels")) %>%
  filter(!(time_point == 4 & cell_type == "2_xylem_parenchyma")) %>%
  filter(!(time_point == 4.5 & cell_type == "3_cambium")) %>%
  filter(cell_type != "4_phloem_fiber")

rectangularity_test_genotype <- tibble(cell_type = NA,
                                       time_point = NA,
                                       genotype = NA,
                                       Kruskal_Wallis = NA,
                                       Dunn = NA)
for (t in unique(attributes_for_rectangularity_genotype_test$cell_type)) {
  filtered <- attributes_for_rectangularity_genotype_test %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$rectangularity ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    d <- dunnTest(x = filtered1$rectangularity,
                  g = filtered1$genotype,
                  method = "bonferroni")
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.05)
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      d1 <- d %>%
        filter(Group == g)
      d1 <- d1[1,3]
      rectangularity_test_genotype <- add_row(.data = rectangularity_test_genotype,
                                              cell_type = t,
                                              time_point = i,
                                              genotype = g,
                                              Kruskal_Wallis = k,
                                              Dunn = d1)
    }
  }
}
rectangularity_test_genotype <- rectangularity_test_genotype[-1, ]

attributes_for_rectangularity_genotype_test_ns <- attributes_for_rectangularity_test %>%
  filter((time_point == 4.5 & cell_type == "1_xylem_vessels") |
           (time_point == 4 & cell_type == "2_xylem_parenchyma") |
           (time_point == 4.5 & cell_type == "3_cambium") |
           (cell_type == "4_phloem_fiber"))

for (t in unique(attributes_for_rectangularity_genotype_test_ns$cell_type)) {
  filtered <- attributes_for_rectangularity_genotype_test_ns %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$rectangularity ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      rectangularity_test_genotype <- add_row(.data = rectangularity_test_genotype,
                                              cell_type = t,
                                              time_point = i,
                                              genotype = g,
                                              Kruskal_Wallis = k,
                                              Dunn = NA)
    }
  }
}

#tissue area

area_for_tissue_area <- attributes %>%
  select(time_point,
         genotype,
         plant,
         cell_type,
         area)
area_for_tissue_area <- na.omit(area_for_tissue_area)

tissue_area_for_plot <- tibble(time_point = NA,
                               genotype = NA,
                               cell_type = NA,
                               area_sum_mean = NA)
for (i in unique(area_for_tissue_area$time_point)) {
  filtered <- area_for_tissue_area %>%
    filter(time_point == i)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    for (t in unique(filtered1$cell_type)) {
      filtered2 <- filtered1 %>%
        filter(cell_type == t)
      area_plant <- tibble(plant = NA,
                           area = NA)
      for (p in unique(filtered2$plant)) {
        filtered3 <- filtered2 %>%
          filter(plant == p)
        s = sum(filtered3$area)
        area_plant <- add_row(.data = area_plant,
                              plant = p,
                              area = s)
      }
      area_plant <- area_plant[-1,]
      m = mean(area_plant$area)
      tissue_area_for_plot <- add_row(.data = tissue_area_for_plot,
                                      time_point = i,
                                      genotype = g,
                                      cell_type = t,
                                      area_sum_mean = m)
    }
  }
}
tissue_area_for_plot <- tissue_area_for_plot[-1, ]

tissue_area_for_plot <- tissue_area_for_plot %>%
  unite(col = time_genotype,
        time_point,
        genotype)
tissue_area_for_plot$time_genotype <- factor(tissue_area_for_plot$time_genotype,
                                             levels = c("3.5_1_wt",
                                                        "3.5_2_pxy_two",
                                                        "3.5_3_pxy_four",
                                                        "4_1_wt",
                                                        "4_2_pxy_two",
                                                        "4_3_pxy_four",
                                                        "4.5_1_wt",
                                                        "4.5_2_pxy_two",
                                                        "4.5_3_pxy_four",
                                                        "5_1_wt",
                                                        "5_2_pxy_two",
                                                        "5_3_pxy_four"))

tissue_area_for_plot %>%
  ggplot(aes(x = time_genotype,
             y = area_sum_mean,
             fill = forcats::fct_rev(cell_type))) +
  geom_bar(position = "stack",
           stat = "identity") +
  theme_minimal()
name = paste0("tissue_area.pdf")
ggsave(filename = name,
       device = "pdf",
       width = 8,
       height = 8,
       units = "in")

tissue_area <- tibble(time_point = NA,
                      genotype = NA,
                      cell_type = NA,
                      plant = NA,
                      area_sum = NA)
for (i in unique(area_for_tissue_area$time_point)) {
  filtered <- area_for_tissue_area %>%
    filter(time_point == i)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    for (t in unique(filtered1$cell_type)) {
      filtered2 <- filtered1 %>%
        filter(cell_type == t)
      area_plant <- tibble(plant = NA,
                           area = NA)
      for (p in unique(filtered2$plant)) {
        filtered3 <- filtered2 %>%
          filter(plant == p)
        s = sum(filtered3$area)
        tissue_area <- add_row(.data = tissue_area,
                               time_point = i,
                               genotype = g,
                               cell_type = t,
                               plant = p,
                               area_sum = s)
      }
    }
  }
}
tissue_area <- tissue_area[-1,]

tissue_area_time_test <- tissue_area %>%
  filter(cell_type != "4_phloem_fiber")

tissue_test_time <- tibble(cell_type = NA,
                           genotype = NA,
                           time_point = NA,
                           Kruskal_Wallis = NA,
                           Dunn = NA)
for(t in unique(tissue_area_time_test$cell_type)) {
  filtered <- tissue_area_time_test %>%
    filter(cell_type == t)
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    k <- kruskal.test(filtered1$area_sum ~ filtered1$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered1$time_point <- as.character(filtered1$time_point)
    d <- dunnTest(x = filtered1$area_sum,
                  g = filtered1$time_point,
                  method = "none")
    filtered1$time_point <- as.numeric(filtered1$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.5)
    for (i in unique(filtered1$time_point)) {
      filtered2 <- filtered1 %>%
        filter(time_point == i)
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      tissue_test_time <- add_row(.data = tissue_test_time,
                                  cell_type = t,
                                  genotype = g,
                                  time_point = i,
                                  Kruskal_Wallis = k,
                                  Dunn = d1)
    }
  }
}
tissue_test_time <- tissue_test_time[-1,]

tissue_test_genotype <- tibble(cell_type = NA,
                               time_point = NA,
                               genotype = NA,
                               Kruskal_Wallis = NA)
for (t in unique(tissue_area$cell_type)) {
  filtered <- tissue_area %>%
    filter(cell_type == t)
  for (i in unique(filtered$time_point)) {
    filtered1 <- filtered %>%
      filter(time_point == i)
    k <- kruskal.test(filtered1$area_sum ~ filtered1$genotype)
    k <- tidy(k)
    k <- k$p.value
    for (g in unique(filtered1$genotype)) {
      filtered2 <- filtered1 %>%
        filter(genotype == g)
      tissue_test_genotype <- add_row(.data = tissue_test_genotype,
                                      cell_type = t,
                                      time_point = i,
                                      genotype = g,
                                      Kruskal_Wallis = k)
    }
  }
}
tissue_test_genotype <- tissue_test_genotype[-1,]

tissue_test_genotype_significant <- tissue_area %>%
  filter(cell_type == "1_xylem_vessels",
         time_point == "3.5")
d <- dunnTest(x = tissue_test_genotype_significant$area_sum,
              g = tissue_test_genotype_significant$genotype,
              method = "none")
d <- d[["res"]]
d <- as.data.frame(d)
d <- cldList(P.adj ~ Comparison,
             data = d,
             threshold = 0.05)
d

hypocotyl_test_time <- tibble(genotype = NA,
                              time_point = NA,
                              Kruskal_Wallis = NA,
                              Dunn = NA)
for(g in unique(tissue_area$genotype)) {
    filtered <- tissue_area %>%
      filter(genotype == g)
    k <- kruskal.test(filtered$area_sum ~ filtered$time_point)
    k <- tidy(k)
    k <- k$p.value
    filtered$time_point <- as.character(filtered$time_point)
    d <- dunnTest(x = filtered$area_sum,
                  g = filtered$time_point,
                  method = "none")
    filtered$time_point <- as.numeric(filtered$time_point)
    d <- d[["res"]]
    d <- as.data.frame(d)
    d <- cldList(P.adj ~ Comparison,
                 data = d,
                 threshold = 0.5)
    for (i in unique(filtered$time_point)) {
      filtered1 <- filtered %>%
        filter(time_point == i)
      d1 <- d %>%
        filter(Group == i)
      d1 <- d1[1,3]
      hypocotyl_test_time <- add_row(.data = hypocotyl_test_time,
                                     genotype = g,
                                     time_point = i,
                                     Kruskal_Wallis = k,
                                     Dunn = d1)
    }
}
hypocotyl_test_time <- hypocotyl_test_time[-1,]

hypocotyl_test_genotype <- tibble(time_point = NA,
                                  genotype = NA,
                                  Kruskal_Wallis = NA)
for(i in unique(tissue_area$time_point)) {
  filtered <- tissue_area %>%
    filter(time_point == i)
  k <- kruskal.test(filtered$area_sum ~ filtered$genotype)
  k <- tidy(k)
  k <- k$p.value
  for (g in unique(filtered$genotype)) {
    filtered1 <- filtered %>%
      filter(genotype == g)
    hypocotyl_test_genotype <- add_row(.data = hypocotyl_test_genotype,
                                       time_point = i,
                                       genotype = g,
                                       Kruskal_Wallis = k)
  }
}
hypocotyl_test_genotype <- hypocotyl_test_genotype[-1,]
