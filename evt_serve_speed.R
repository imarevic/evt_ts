# ------------------------------------ # 
# ----- evt serve speed analysis ----- #
# ------------------------------------ # 


# ----- setup ----- #
rm(list=ls())
if (!require(tidyverse)) install.packages('tidyverse')
if (!require(dplyr)) install.packages('dplyr')
if (!require(tidyr)) install.packages('tidyr')
if (!require(ggplot2)) install.packages('ggplot2')
if (!require(cowplot)) install.packages('cowplot')
if (!require(stringr)) install.packages('stringr')
if (!require(evd)) install.packages('evd')
if (!require(evir)) install.packages('evir')
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) # asuming rstudio is used
print(getwd())
source('helpers.R')
source('plotting.R')

# ----- load and preprocess data ----- #
subdir <- "data"
df_raw = load_data(subdir)
df_base = preprocess_data(df_raw)

# ----- calc max serve speed per player ----- #
df_max_speed = df_base %>%
                  group_by(across(all_of(c("competition", "server")))) %>% summarise(serve_speed_kmh = max(serve_speed_kmh))
df_max_speed <- df_max_speed[order(df_max_speed$server),]
df_max_speed <- df_max_speed %>%
                  separate(server, c("server_first_name", "server_last_name_1",
                                     "server_last_name_2", "server_last_name_3", "server_last_name_4"), " ")
df_max_speed$server <- paste(df_max_speed$server_last_name_1, "_", df_max_speed$server_last_name_2)
df_max_speed = df_max_speed %>%
  group_by(across(all_of(c("competition", "server")))) %>% summarise(serve_speed_kmh = max(serve_speed_kmh))

# remove duplicate players and extract final max_serve_speed
df_max_speed_cleaned <- remove_duplicate_players(df_max_speed)
df_max_speed_cl = df_max_speed_cleaned %>%
  group_by(across(all_of(c("competition", "server")))) %>% summarise(serve_speed_kmh = max(serve_speed_kmh))

# add top records manually and remove measurement errors
df_max_speed_verified = add_verified_records(df_max_speed_cl)

# calc distinctly ordered serve speeds
df_men <- filter(df_max_speed_verified, competition == "men")
df_women <- filter(df_max_speed_verified, competition == "women")
df_men_ordered <- calc_ordered_serve_speeds(df_men, 0.005)
df_women_ordered <- calc_ordered_serve_speeds(df_women, 0.005)
df_women_ordered <- df_women_ordered[ which(df_women_ordered$serve_speed_final <= 210.8), ]
df_final <- rbind(df_men_ordered, df_women_ordered)

# ----- plot histogram of serve speed ----- #
hist_speed <- ggplot(df_final, aes(x=serve_speed_final, fill=competition)) + 
  geom_histogram(bins=50, color="black", position="dodge") +
  labs(x ="serve speed (km/h)", y = "frequency") +
  scale_fill_manual(values=c("#697CC3", "#DC801D")) +
  theme(legend.justification="top") +
  theme_classic()

hist_speed

df_men <- filter(df_final, competition == "men")
df_women <- filter(df_final, competition == "women")
m_arr = sort(df_men$serve_speed_final)
w_arr = sort(df_women$serve_speed_final)
# ----- fait tails inspection (qq-plot; em-plot (zipf); me-plot; ms-plot) ----- #
# qplot against exponential (xi=0)
evir::qplot(m_arr,xi=0)
evir::qplot(w_arr,xi=0)
# em plot
emplot(m_arr,'xy')
emplot(w_arr,'xy')
# me plot
meplot(m_arr)
meplot(w_arr)
# ms plot
ms_plot(m_arr)
ms_plot(w_arr)

# ----- evt modeling ----- #
# tail index estimation (pickands, moments, ml, pwm) 
source("evi.R")

# evi estimation for men
evi_pick_m <- pickand_evi(m_arr)
evi_mom_m <- moment_evi(m_arr)
evi_red_mom_m <- reduced_moment_evi(m_arr)
colnames(evi_pick_m) <- c("k", "xi"); colnames(evi_mom_m) <- c("k", "xi"); colnames(evi_red_mom_m) <- c("k", "xi")
evi_pick_m$estimator <- "pickands"; evi_mom_m$estimator <- "moment"; evi_red_mom_m$estimator <- "reduced moment"
evi_men <- rbind(evi_pick_m, evi_mom_m, evi_red_mom_m)

# calc one final xi
# pickands: 30-80; moment & red. moment: 150-300
pick_stable_k <- filter(evi_men, ((estimator == "pickands")&(k>=30)&(k<=80))); pick_mean_m <- mean(pick_stable_k$xi)
mom_stable_k <- filter(evi_men, ((estimator == "moment")&(k>=150)&(k<=300))); mom_mean_m <- mean(mom_stable_k$xi)
mom_red_stable_k <- filter(evi_men, ((estimator == "reduced moment")&(k>=150)&(k<=300))); mom_red_mean_m <- mean(mom_red_stable_k$xi)
xi_m <- mean(c(pick_mean_m, mom_mean_m, mom_red_mean_m))

evi_plt_men <- ggplot(evi_men, aes(x=k, y=xi, group=estimator, color=estimator)) +
  geom_line(aes(linetype=estimator)) + xlim(0, 400) + ylim(-2, 1) +
  geom_hline(yintercept=xi_m, linetype="dotdash", color = "#C01900") +
  theme_classic() + scale_color_manual(values=c("#697CC3", "#DC801D", "#000000")) +
  labs(title = expression(paste(xi , " estimates for men")), x="k", y = expression(xi)) +
  theme(legend.justification="top", plot.title = element_text(hjust = 0.5))
evi_plt_men

# evi estimation for women
evi_pick_w <- pickand_evi(w_arr)
evi_mom_w <- moment_evi(w_arr)
evi_red_mo_w <- reduced_moment_evi(w_arr)
colnames(evi_pick_w) <- c("k", "xi"); colnames(evi_mom_w) <- c("k", "xi"); colnames(evi_red_mo_w) <- c("k", "xi")
evi_pick_w$estimator <- "pickands"; evi_mom_w$estimator <- "moment"; evi_red_mo_w$estimator <- "reduced moment"
evi_women <- rbind(evi_pick_w, evi_mom_w, evi_red_mo_w)

# calc one final xi
# pickands: 30-80; moment & red. moment: 150-300
pick_stable_k <- filter(evi_women, ((estimator == "pickands")&(k>=30)&(k<=80))); pick_mean_w <- mean(pick_stable_k$xi)
mom_stable_k <- filter(evi_women, ((estimator == "moment")&(k>=150)&(k<=300))); mom_mean_w <- mean(mom_stable_k$xi)
mom_red_stable_k <- filter(evi_women, ((estimator == "reduced moment")&(k>=150)&(k<=300))); mom_red_mean_w <- mean(mom_red_stable_k$xi)
xi_w <- mean(c(pick_mean_w, mom_mean_w, mom_red_mean_w))

evi_plt_women <- ggplot(evi_women, aes(x=k, y=xi, group=estimator, color=estimator)) +
  geom_line(aes(linetype=estimator)) + xlim(0, 400) + ylim(-2, 1) +
  geom_hline(yintercept=xi_w, linetype="dotdash", color = "#C01900") +
  theme_classic() + scale_color_manual(values=c("#697CC3", "#DC801D", "#000000")) +
  labs(title = expression(paste(xi , " estimates for women")), x="k", y = expression(xi)) +
  theme(legend.justification="top", plot.title = element_text(hjust = 0.5))
evi_plt_women

# combined evi plot
plot_grid(evi_plt_men, evi_plt_women, nrow= 2)

# end point estimation
# TODO: implement a and b estimators


# probability of non-official serve records
# TODO: quantify probability of non-official serve speeds in relation to estimated endpoint



