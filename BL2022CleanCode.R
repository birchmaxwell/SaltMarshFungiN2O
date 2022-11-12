# This code will be for the Special Topics Report for Frontiers in Marine Science
# November 2022 Submission 

# Required Packages #####
# If packages are NOT already installed, delete the pound (#) symbol 
# before the necessary package

# install.packages("tidyverse")
library(tidyverse) # ggplot and other useful analysis functions
#install.packages("viridis")
library(viridisLite) #needed to install viridis
library(viridis) # colorblind assist 
# install.packages("patchwork")
library(patchwork) # putting plots next to each other
library(readxl) # reading excel files into R
# install.packages("ggpmisc")
library(ggpmisc) #linear regression
# other minor packages for nice tables and stitching images together
# install.packages("magrittr")
library(magrittr)
# install.packages("grid")
library(grid)
# install.packages("stringr")
library(stringr)
# install.packages('gt')
library(gt)
# install.packages('glue')
library(glue)

# For good looking figures #####
bw <- theme_bw() + theme(panel.grid.major = element_blank(), 
                         panel.grid.minor = element_blank(),
                         panel.background = element_blank())

colorZ <- c("grey0", "royalblue1", "goldenrod2")

my.formula <- y ~ x
EqStat <- ggpmisc::stat_poly_eq(formula = my.formula, aes(label = paste(..rr.label..,
                                                               ..p.value.label..,
                                                               sep = "~~~~")), 
                       parse = TRUE)

# Calling in individual sheets #####
# note that each sheet in the excel file is for a different fungal strain
# which is specified by a fairly obvious abbreviation for said strains
setwd("/SpecialTopic2022FMSCI") #change for your path to the saved excel file
virens <- readxl::read_excel("Organized.xlsx", sheet = 2) 
Rhodo <- readxl::read_excel("Organized.xlsx", sheet = 4)
Purpureo <- readxl::read_excel("Organized.xlsx", sheet = 6)
harzianum <- readxl::read_excel("Organized.xlsx", sheet = 8)

# Getting Total N2O and CO2 Values and Putting them into the Data #####
# This will account for the dissolved portion of gases
# On the excel documents N2O and CO2 values are in ppm
# Beta/Gamma coefficients is just nomenclature used by the first author for
# easy differentiation between N2O and CO2 calculations

# N2O
# Beta Coefficient (mol L-1 atm-1) 
# Previously calculated from from Weiss and Price 1980 by Xuefeng Peng
# 22 C, 35 psi
totalN2O <- function(N2OPPM){
  # dissolved fraction 
  Vmedia <- 0.020 # 20 mL
  Atm <- 1 # negligible pressure difference in the serum vial from standard pressure (atm)
  BetaN2O <- 0.02259 # note units above
  DN2Onmol <- BetaN2O * Vmedia *Atm * (N2OPPM/1000000)
  # as gas is in ppm (x parts/1000000 parts)
  # answer is expressed a "x mol * 10^9"
  
  # gaseous fraction
  Vhead <- 0.045 # 45 mL
  Temp <- 295.15 # in Kelvin
  IGC <- 0.08205734 # (L*atm)/(K*mol)
  GN2Onmol <- ((N2OPPM/1000000)*Vhead*Atm)/(Temp*IGC)
  # as gas is in ppm (x parts/1000000 parts)
  # answer is expressed a "x mol * 10^9"
  
  #total
  N2Onmol <- (GN2Onmol + DN2Onmol)*1000000000
  # multiply by 10^9 for nmol units
}

# Percent N2O converted from N-NO2- to N-N2O
pConvert <- function(x){
  nitrite <- 10 # uM
  Vmedia <- 0.020 # 20 mL
  con <- ((2*x)/(nitrite * Vmedia * 1000))*100 # times 1000 for the nm to um
                                                      # 2 units N out
}

# CO2
# Gamma Coefficient (ml ml-1 atm -1)
# Previously calculated from from Weiss 1974 by Xuefeng Peng
# 22 C, 35 psi
totalCO2 <- function(CO2PPM){
  # dissolved fraction
  Vmedia <- 20 # in mL
  Atm <- 1 # negligible pressure difference in the serum vial from standard pressure (atm)
  GammaCO2 <- 0.6307 # note units above
  DCO2mL <- (GammaCO2 * Vmedia *Atm * CO2PPM/1000000)
  # as gas is in ppm (x parts/1000000 parts),
  # the final answer will be in mL
  
  #gasesous fraction
  Vhead <- 45
  GCO2mL <- Vhead * CO2PPM/1000000
  # final answer in mL
  
  #total in mL
  CO2mL <- DCO2mL + GCO2mL
  
  #total in mol
  Temp <- 295.15 # in Kelvin
  IGC <- 0.08205734 # (L*atm)/(K*mol)
  CO2umol <- ((Atm*CO2mL/1000)/(IGC*Temp))*1000000
  # final answer in umol
}

# Purpureo 
# a little different than others because of different NO2- conditions 
purpAdjustN2O_0 <- Purpureo %>%
  dplyr::filter(Gas_ppm == "N2O", Nitrite == "0") #subsetting data
purpAdjustN2O_0$R1 <- totalN2O(purpAdjustN2O_0$R1)
purpAdjustN2O_0$R2 <- totalN2O(purpAdjustN2O_0$R2)
purpAdjustN2O_0$R3 <- totalN2O(purpAdjustN2O_0$R3)
purpAdjustN2O_10 <- Purpureo %>%
  dplyr::filter(Gas_ppm == "N2O", Nitrite == "10") #subsetting data
purpAdjustN2O_10$R1 <- totalN2O(purpAdjustN2O_10$R1)
purpAdjustN2O_10$R2 <- totalN2O(purpAdjustN2O_10$R2)
purpAdjustN2O_10$R3 <- totalN2O(purpAdjustN2O_10$R3)
purpAdjustN2O_100 <- Purpureo %>%
  dplyr::filter(Gas_ppm == "N2O", Nitrite == "100") #subsetting data
purpAdjustN2O_100$R1 <- totalN2O(purpAdjustN2O_100$R1)
purpAdjustN2O_100$R2 <- totalN2O(purpAdjustN2O_100$R2)
purpAdjustN2O_100$R3 <- totalN2O(purpAdjustN2O_100$R3)
purpAdjustCO2_10 <- Purpureo %>%
  dplyr::filter(Gas_ppm == "CO2", Nitrite == "10") #subsetting data
purpAdjustCO2_10$R1 <- totalCO2(purpAdjustCO2_10$R1)
purpAdjustCO2_10$R2 <- totalCO2(purpAdjustCO2_10$R2)
purpAdjustCO2_10$R3 <- totalCO2(purpAdjustCO2_10$R3)
purp <- full_join(full_join(purpAdjustN2O_0, purpAdjustN2O_10), 
                  full_join(purpAdjustN2O_100, purpAdjustCO2_10))

# Virens 
virensAdjustN2O_10 <- virens %>%
  dplyr::filter(Gas_ppm == "N2O") #subsetting data
virensAdjustN2O_10$R1 <- totalN2O(virensAdjustN2O_10$R1)
virensAdjustN2O_10$R2 <- totalN2O(virensAdjustN2O_10$R2)
virensAdjustN2O_10$R3 <- totalN2O(virensAdjustN2O_10$R3)
virensAdjustCO2_10 <- virens %>%
  dplyr::filter(Gas_ppm == "CO2") #subsetting data
virensAdjustCO2_10$R1 <- totalCO2(virensAdjustCO2_10$R1)
virensAdjustCO2_10$R2 <- totalCO2(virensAdjustCO2_10$R2)
virensAdjustCO2_10$R3 <- totalCO2(virensAdjustCO2_10$R3)
virR <- full_join(virensAdjustN2O_10, virensAdjustCO2_10)

# Rhodo 
rhodoAdjustN2O_10 <- Rhodo %>%
  dplyr::filter(Gas_ppm == "N2O") #subsetting data
rhodoAdjustN2O_10$R1 <- totalN2O(rhodoAdjustN2O_10$R1)
rhodoAdjustN2O_10$R2 <- totalN2O(rhodoAdjustN2O_10$R2)
rhodoAdjustN2O_10$R3 <- totalN2O(rhodoAdjustN2O_10$R3)
rhodoAdjustCO2_10 <- Rhodo %>%
  dplyr::filter(Gas_ppm == "CO2") #subsetting data
rhodoAdjustCO2_10$R1 <- totalCO2(rhodoAdjustCO2_10$R1)
rhodoAdjustCO2_10$R2 <- totalCO2(rhodoAdjustCO2_10$R2)
rhodoAdjustCO2_10$R3 <- totalCO2(rhodoAdjustCO2_10$R3)
rhoR <- full_join(rhodoAdjustN2O_10, rhodoAdjustCO2_10)

# Harz 
harzAdjustN2O_10 <- harzianum %>%
  dplyr::filter(Gas_ppm == "N2O") #subsetting data
harzAdjustN2O_10$R1 <- totalN2O(harzAdjustN2O_10$R1)
harzAdjustN2O_10$R2 <- totalN2O(harzAdjustN2O_10$R2)
harzAdjustN2O_10$R3 <- totalN2O(harzAdjustN2O_10$R3)
harzAdjustCO2_10 <- harzianum %>%
  dplyr::filter(Gas_ppm == "CO2") #subsetting data
harzAdjustCO2_10$R1 <- totalCO2(harzAdjustCO2_10$R1)
harzAdjustCO2_10$R2 <- totalCO2(harzAdjustCO2_10$R2)
harzAdjustCO2_10$R3 <- totalCO2(harzAdjustCO2_10$R3)
harz <- full_join(harzAdjustN2O_10, harzAdjustCO2_10)

# Figure 2: N2O and CO2 Headspace Gas Accumulation Curves #####
# Purpureo 
# purpureo N2O
purpureoN2O_10 <- purp %>%
  dplyr::filter(Gas_ppm == "N2O", Nitrite == "10") %>% 
  tidyr::pivot_longer(c(3:5)) %>%
  ggplot2::ggplot(aes(x = Day, y = value, group = name)) +
  geom_line(aes(color = name)) + 
  geom_point(aes(shape = name, color = name), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  labs(title = "P. lilacinum", x = NULL, y = expression(N [2]~O~(nmol))) +
  theme(legend.position = "none", plot.title = element_text(face = "italic")) +
  annotate("text", x = 2, y = (35*0.92), label = "(A)") +
  scale_y_continuous(limits = c(0, 35), breaks = seq(0, 35, by = 7))
purpureoN2O_10
# purpureo CO2
purpureoCO2_10 <- purp %>%
  dplyr::filter(Gas_ppm == "CO2", Nitrite == "10") %>% 
  tidyr::pivot_longer(c(3:5)) %>%
  ggplot2::ggplot(aes(x = Day, y = (value), group = name)) +
  geom_line(aes(color = name)) + 
  geom_point(aes(shape = name, color = name), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none") +
  labs(x = "Time (Day)", y = expression(CO [2]~("\U003BCmol"))) +
  annotate("text", x = 2, y = (215*0.92), label = "(E)") +
  scale_y_continuous(limits = c(0, 215), breaks = seq(0, 215, by = 43))
purpureoCO2_10

# Virens 
# virens N2O
virensN2O_10 <- virR %>%
  dplyr::filter(Gas_ppm == "N2O") %>% 
  tidyr::pivot_longer(c(3:5)) %>%
  ggplot2::ggplot(aes(x = Day, y = value, group = name)) +
  geom_line(aes(color = name)) + 
  geom_point(aes(shape = name, color = name), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none", plot.title = element_text(face = "italic")) +
  labs(title = "T. virens", x = NULL, y = NULL) +
  annotate("text", x = 1.89, y = (25*0.92), label = "(B)") +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5))
virensN2O_10
# virens CO2
virensCO2_10 <- virR %>%
  dplyr::filter(Gas_ppm == "CO2") %>% 
  tidyr::pivot_longer(c(3:5)) %>%
  ggplot2::ggplot(aes(x = Day, y = (value), group = name)) +
  geom_line(aes(color = name)) + 
  geom_point(aes(shape = name, color = name), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none") +
  labs(x = "Time (Day)", y = NULL) +
  annotate("text", x = 1.89, y = (65*0.92), label = "(F)") +
  scale_y_continuous(limits = c(0, 65), breaks = seq(0, 65, by = 13))
virensCO2_10

# Rhodo 
# Rhodo N2O
rhodoN2O_10 <- rhoR %>%
  dplyr::filter(Gas_ppm == "N2O") %>% 
  tidyr::pivot_longer(c(3:5),names_to = "Replicate") %>%
  ggplot2::ggplot(aes(x = Day, y = value, group = Replicate)) +
  geom_line(aes(color = Replicate)) + 
  geom_point(aes(shape = Replicate, color = Replicate), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  scale_x_continuous(breaks = seq(1, 14, 3)) +
  theme(legend.position = "none", plot.title = element_text(face = "italic")) +
  labs(title = "R. glutinis", x = NULL, y = NULL) +
  annotate("text", x = 0.75, y = (5.5*0.92), label = "(D)") +
  scale_y_continuous(limits = c(0, 5.5), breaks = seq(0, 5.5, by = 1.1)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 4))
rhodoN2O_10
# Rhodo CO2
rhodoCO2_10 <- rhoR %>%
  dplyr::filter(Gas_ppm == "CO2") %>% 
  tidyr::pivot_longer(c(3:5)) %>%
  ggplot2::ggplot(aes(x = Day, y = (value), group = name)) +
  geom_line(aes(color = name)) + 
  geom_point(aes(shape = name, color = name), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none") +
  scale_x_continuous(breaks = seq(1, 14, 3)) +
  labs(x = "Time (Day)", y = NULL) +
  annotate("text", x = 0.75, y = (150*0.92), label = "(H)") +
  scale_y_continuous(limits = c(0, 150), breaks = seq(0, 150, by = 30)) +
  scale_x_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 4))
rhodoCO2_10

# Harzianum 
# harzianum N2O
harzianumN2O_10 <- harz %>%
  dplyr::filter(Gas_ppm == "N2O") %>% 
  tidyr::pivot_longer(c(3:5), names_to = "Replicate") %>%
  ggplot2::ggplot(aes(x = Day, y = value, group = Replicate)) +
  geom_line(aes(color = Replicate)) + 
  geom_point(aes(shape = Replicate, color = Replicate), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none", plot.title = element_text(face = "italic")) +
  labs(title = "T. harzianum", x = NULL, y = NULL) +
  annotate("text", x = 0.35, y = (5.5*0.92), label = "(C)") +
  scale_y_continuous(limits = c(0, 5.5), breaks = seq(0, 5.5, by = 1.1))
harzianumN2O_10
# harzianum CO2
harzianumCO2_10 <- harz %>%
  dplyr::filter(Gas_ppm == "CO2") %>% 
  tidyr::pivot_longer(c(3:5)) %>%
  ggplot2::ggplot(aes(x = Day, y = (value), group = name)) +
  geom_line(aes(color = name)) + 
  geom_point(aes(shape = name, color = name), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none") +
  labs(x = "Time (Day)", y = NULL) +
  annotate("text", x = 0.35, y = (5.5*0.92), label = "(G)") +
  scale_y_continuous(limits = c(0, 5.5), breaks = seq(0, 5.5, by = 1.1))
harzianumCO2_10

# Complied and final Figure 2 
fig2 <- purpureoN2O_10 + virensN2O_10 + harzianumN2O_10 + rhodoN2O_10 + 
  purpureoCO2_10 + virensCO2_10 + harzianumCO2_10 + rhodoCO2_10 + 
  patchwork::plot_layout(ncol = 4) 
fig2
ggsave(file = "MainFig2.tiff", width = 8, height = 4, dpi=300, compression = 'lzw')


# Figure 3: Regression N2O, CO2 #####
# Purpureo 
purpureoReg <- purp %>%
  dplyr::filter(Nitrite == "10") %>% 
  tidyr::pivot_longer(c(3:5), names_to = "Replicate") %>%
  tidyr::pivot_wider(names_from = "Gas_ppm", values_from = "value") %>%
  ggplot2::ggplot(aes(x = N2O, y = (CO2), group = Replicate, color = Replicate)) + 
  geom_smooth(method = 'lm', se = FALSE, formula = y~x, aes(color = Replicate), 
              linetype = "dashed") +
  geom_point(aes(color = Replicate, shape = Replicate), size = 3) +
  bw + EqStat +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none") +
  labs(title = "P. lilacinum", 
       x = NULL, 
       y = expression(CO [2]~("\U003BCmol"))) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"),
        axis.text = element_text(size = 11), axis.title = element_text(size = 13)) +
  annotate("text", x = (30*0.95), y = 300, label = "(A)") +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, by = 50)) +
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, by = 6))
purpureoReg

# Virens 
virensReg <- virR %>%
  tidyr::pivot_longer(c(3:5), names_to = "Replicate") %>%
  tidyr::pivot_wider(names_from = "Gas_ppm", values_from = "value") %>%
  ggplot2::ggplot(aes(x = N2O, y = (CO2), group = Replicate, color = Replicate)) + 
  geom_smooth(method = 'lm', se = FALSE, formula = y~x, aes(color = Replicate),
              linetype = "dashed") + 
  geom_point(aes(color = Replicate, shape = Replicate), size = 3) +
  bw + EqStat +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none") +
  labs(title = "T. virens", 
       x = NULL, 
       y = NULL) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"),
        axis.text = element_text(size = 11), axis.title = element_text(size = 13)) +
  annotate("text", x = (25*0.95), y = 90, label = "(B)") +
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 15)) +
  scale_x_continuous(limits = c(0, 25), breaks = seq(0, 25, by = 5))
virensReg

# Harzianum 
harzianumReg <- harz %>%
  tidyr::pivot_longer(c(3:5), names_to = "Replicate") %>%
  tidyr::pivot_wider(names_from = "Gas_ppm", values_from = "value") %>%
  ggplot2::ggplot(aes(x = N2O, y = (CO2), group = Replicate, color = Replicate)) + 
  geom_smooth(method = 'lm', se = FALSE, formula = y~x, aes(color = Replicate),
              linetype = "dashed") + 
  geom_point(aes(color = Replicate, shape = Replicate), size = 3) +
  bw + EqStat +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none") +
  labs(title = "T. harzianum", 
       x = expression(N [2]~O~(nmol)), 
       y = expression(CO [2]~("\U003BCmol"))) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"),
        axis.text = element_text(size = 11), axis.title = element_text(size = 13)) +
  annotate("text", x = (5*0.95), y = 9, label = "(C)") +
  scale_y_continuous(limits = c(0, 9), breaks = seq(0, 9, by = 1.5)) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1))
harzianumReg

# Rhodo 
rhodoReg <- rhoR %>%
  tidyr::pivot_longer(c(3:5), names_to = "Replicate") %>%
  tidyr::pivot_wider(names_from = "Gas_ppm", values_from = "value") %>%
  ggplot2::ggplot(aes(x = N2O, y = (CO2), group = Replicate, color = Replicate)) + 
  geom_smooth(method = 'lm', se = FALSE, formula = y~x, aes(color = Replicate),
              linetype = "dashed") + 
  geom_point(aes(color = Replicate, shape = Replicate), size = 3) +
  bw + EqStat +
  scale_color_manual(values = colorZ) +
  theme(legend.position = "none") +
  labs(title = "R. glutinis", 
       x = expression(N [2]~O~(nmol)), 
       y = NULL) +
  theme(legend.position = "none", plot.title = element_text(face = "italic"),
        axis.text = element_text(size = 11), axis.title = element_text(size = 13)) +
  annotate("text", x = (5*0.95), y = 210, label = "(D)") +
  scale_y_continuous(limits = c(0, 210), breaks = seq(0, 210, by = 35)) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, by = 1))
rhodoReg

# Complied and final Figure 3 
fig3 <- purpureoReg + virensReg + harzianumReg + rhodoReg + plot_layout(ncol = 2)
fig3
ggsave(file = "MainFig3.tiff", width = 6, height = 7, dpi=300, compression = 'lzw')

# Figure 4: N2O Growth for Purpureo w/ different NO2- Concentrations #####
# O uM 
purpureoN2O_0_ <- purp %>%
  dplyr::filter(Gas_ppm == "N2O", Nitrite == "0") %>% 
  tidyr::pivot_longer(c(3:5)) %>%
  ggplot2::ggplot(aes(x = Day, y = value, group = name)) +
  geom_line(aes(color = name)) + 
  geom_point(aes(shape = name, color = name), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) + 
  labs(title = expression("0 \U003BCM NO"[2]^'-'), x = NULL, y = expression(N [2]~O~(nmol))) +
  annotate("text", x = 2, y = 3.5, label = "(A)") +
  theme(legend.position = "none",axis.text = element_text(size = 11), 
        axis.title = element_text(size = 13)) +
  scale_y_continuous(limits = c(0, 3.5))
purpureoN2O_0_

# 10 uM 
purpureoN2O_10_ <- purp %>%
  dplyr::filter(Gas_ppm == "N2O", Nitrite == "10") %>% 
  tidyr::pivot_longer(c(3:5)) %>%
  ggplot2::ggplot(aes(x = Day, y = value, group = name)) +
  geom_line(aes(color = name)) + 
  geom_point(aes(shape = name, color = name), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  labs(title = expression("10 \U003BCM NO"[2]^'-'), x = "Time (Day)", y = NULL) +
  annotate("text", x = 2, y = 35, label = "(B)") +
  theme(legend.position = "none",axis.text = element_text(size = 11), 
        axis.title = element_text(size = 13)) +
  scale_y_continuous(limits = c(0, 35))
purpureoN2O_10_

# 100 uM 
purpureoN2O_100_ <- purp %>%
  dplyr::filter(Gas_ppm == "N2O", Nitrite == "100") %>% 
  tidyr::pivot_longer(c(3:5), names_to = "Replicate") %>%
  ggplot2::ggplot(aes(x = Day, y = value, group = Replicate)) +
  geom_line(aes(color = Replicate)) + 
  geom_point(aes(shape = Replicate, color = Replicate), size = 3) + 
  bw +
  scale_color_manual(values = colorZ) +
  labs(title = expression("100 \U003BCM NO"[2]^'-'), x = NULL, y = NULL) +
  annotate("text", x = 2, y = 350, label = "(C)") +
  theme(legend.position = "none",axis.text = element_text(size = 11), 
        axis.title = element_text(size = 13)) +
  scale_y_continuous(limits = c(0, 350))
purpureoN2O_100_

# Complied and final Figure 4 
fig4 <- purpureoN2O_0_ + purpureoN2O_10_ + purpureoN2O_100_ 
fig4
ggsave(file = "MainFig4.tiff", width = 6, height = 3, dpi=300, compression = 'lzw')

# Percent Yield Table #####

pr1 <- max(purp$R1)
pr2 <- max(purp$R2)
pr3 <- max(purp$R3)

vr1 <- max(virR$R1)
vr2 <- max(virR$R2)
vr3 <- max(virR$R3)

rr1 <- max(rhoR$R1)
rr2 <- max(rhoR$R2)
rr3 <- max(rhoR$R3)

hr1 <- max(harz$R1)
hr2 <- max(harz$R2)
hr3 <- max(harz$R3)

table1 <- data.frame(strain = c("Purp", "Virens", "Rhodo", "harz"),
                     mean = c(mean(c(pConvert(pr1), pConvert(pr2), pConvert(pr3))),
                              mean(c(pConvert(vr1), pConvert(vr2), pConvert(vr3))),
                              mean(c(pConvert(rr1), pConvert(rr2), pConvert(rr3))),
                              mean(c(pConvert(hr1), pConvert(hr2), pConvert(hr3)))),
                     sd = c(sd(c(pConvert(pr1), pConvert(pr2), pConvert(pr3))),
                            sd(c(pConvert(vr1), pConvert(vr2), pConvert(vr3))),
                            sd(c(pConvert(rr1), pConvert(rr2), pConvert(rr3))),
                            sd(c(pConvert(hr1), pConvert(hr2), pConvert(hr3)))))
table1

# Average N2O Peak for Each Strain #####

meanPurp <- purp %>% 
  filter(Gas_ppm == "N2O", Nitrite == "10") 
pr1 <- max(meanPurp$R1)
pr2 <- max(meanPurp$R2)
pr3 <- max(meanPurp$R3)
mean(c(pr1, pr2, pr3))
sd(c(pr1, pr2, pr3))
purpT <- data.frame(avg = mean(c(pr1, pr2, pr3)),
                    sd = sd(c(pr1, pr2, pr3)))

meanPurpH <- purp %>% 
  filter(Gas_ppm == "N2O", Nitrite == "100") 
prH2 <- max(meanPurpH$R2)
prH3 <- max(meanPurpH$R3)
purpTH <- data.frame(avg = mean(c(prH2, prH3)),
                     sd = sd(c(prH2, prH3)))
purpTH

meanPurpL <- purp %>% 
  filter(Gas_ppm == "N2O", Nitrite == "0") 
prL1 <- max(meanPurpL$R1)
prL2 <- max(meanPurpL$R2)
prL3 <- max(meanPurpL$R3)
purpLT <- data.frame(avg = mean(c(prL1, prL2, prL3)),
                     sd = sd(c(prL1, prL2, prL3)))
purpLT
meanPurp2 <- purp %>% 
  filter(Gas_ppm == "CO2", Nitrite == "10") 
pcr1 <- max(meanPurp2$R1)
pcr2 <- max(meanPurp2$R2)
pcr3 <- max(meanPurp2$R3)
purpCT <- data.frame(avg = mean(c(pcr1, pcr2, pcr3)),
                     sd = sd(c(pcr1, pcr2, pcr3)))
purpCT

meanvirR <- virR %>% 
  filter(Gas_ppm == "N2O") %>%
  na.omit()
vr1 <- max(meanvirR$R1)
vr2 <- max(meanvirR$R2)
vr3 <- max(meanvirR$R3)
virenT <- data.frame(mean = mean(c(vr1, vr2, vr3)),
                     sd = sd(c(vr1, vr2, vr3)))
virenT
meanvirR2 <- virR %>% 
  filter(Gas_ppm == "CO2") %>%
  na.omit()
vcr1 <- max(meanvirR2$R1)
vcr2 <- max(meanvirR2$R2)
vcr3 <- max(meanvirR2$R3)
virenCT <- data.frame(avg = mean(c(vcr1, vcr2, vcr3)),
                      sd = sd(c(vcr1, vcr2, vcr3)))
virenCT

meanR <- rhoR %>% 
  filter(Gas_ppm == "N2O") %>%
  na.omit()
rr1 <- max(meanR$R1)
rr2 <- max(meanR$R2)
rr3 <- max(meanR$R3)
rhodoT <- data.frame(avg = mean(c(rr1, rr2, rr3)),
                     sd = sd(c(rr1, rr2, rr3)))
rhodoT
meanR2 <- rhoR %>% 
  filter(Gas_ppm == "CO2") %>%
  na.omit()
rcr1 <- max(meanR2$R1)
rcr2 <- max(meanR2$R2)
rcr3 <- max(meanR2$R3)
rhodoCT <- data.frame(avg = mean(c(rcr1, rcr2, rcr3)),
                      sd = sd(c(rcr1, rcr2, rcr3)))
rhodoCT

meanhaR <- harz %>% 
  filter(Gas_ppm == "N2O") %>%
  na.omit()
hr1 <- max(meanhaR$R1)
hr2 <- max(meanhaR$R2)
hr3 <- max(meanhaR$R3)
harzT <- data.frame(avg = mean(c(hr1, hr2, hr3)),
                    sd = sd(c(hr1, hr2, hr3)))
harzT
meanhaR2 <- harz %>% 
  filter(Gas_ppm == "CO2") %>%
  na.omit()
hcr1 <- max(meanhaR2$R1)
hcr2 <- max(meanhaR2$R2)
hcr3 <- max(meanhaR2$R3)
harzCT <- data.frame(avg = mean(c(hcr1, hcr2, hcr3)),
                     sd = sd(c(hcr1, hcr2, hcr3)))
harzCT

# Strain info, Table 1 and Supp Table 1
# https://gt.rstudio.com/articles/intro-creating-gt-tables.html
mean(c(34.5, 39.9, 36.3))

df <- data.frame(Strain = c("T. harizanum", "T. virens", "P. lilacinum", "R. glutinis"),
                 Media = c("Complex", "Mineral", "Complex", "Complex"),
                 Average = c(7.46, 30.56, 31.00, 33.41),
                 SD = c(1.57, 2.09, 1.31, 1.20))

df2 <- data.frame(Strain = c("T. harizanum", "T. virens", "P. lilacinum", "R. glutinis"),
                  r = c(98.93, 97.65, 100, 98.30),
                  q = c(99.88, 99.40, 99.40, 99.63))

gt(df) %>%
  tab_header(
    title = "Fungal Site Prefrence (\u2030)",
  ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    location = cells_body(columns = Strain)
  ) %>%
  cols_label(
    Average = "Mean",
    SD = "SD"
  ) %>% gtsave("tab1.docx")

gt(df2) %>%
  tab_header(
    title = "Percent idenity to known fungal seqeunces from NCBI nt database through blastn",
  ) %>%
  tab_style(
    style = cell_text(style = "italic"),
    location = cells_body(columns = Strain)
  ) %>%
  cols_label(
    r = "18S",
    q = "28S"
  ) %>% gtsave("tab2.docx")