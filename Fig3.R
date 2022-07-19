library(ggthemes)
library(tidyverse)
# library(ggridges)


# pbl_di_p2 <- pbl_di %>%
#   dplyr::select(treatment, theory_pre1:inquiry_post3) %>%
#   mutate(id = 1:nrow(.),
#          theory_pre1 = as.numeric(theory_pre1),
#          theory_pre2 = as.numeric(theory_pre2),
#          theory_pre3 = as.numeric(theory_pre3),
#          theory_post1 = as.numeric(theory_post1),
#          theory_post2 = as.numeric(theory_post2),
#          theory_post3 = as.numeric(theory_post3)) %>%
#   pivot_longer(theory_pre1:inquiry_post3, names_to="vars", values_to="values") %>%
#   mutate(vignette = str_sub(vars, -1,-1),
#          prepost = case_when(
#                       str_detect(vars, "pre") ~ "pretest",
#                       str_detect(vars, "post") ~ "posttest"
#                       ),
#          vars = case_when(
#                   str_detect(vars, "theory") ~ "theory",
#                   str_detect(vars, "inquiry") ~ "inquiry"
#                   )) %>%
#   pivot_wider(id_cols = c(id, prepost, vignette, treatment), names_from = "vars", values_from = "values")
#   
# 
# pbl_di_p2$treatment <- as.factor(pbl_di_p2$treatment)
# 
# p <- ggplot(pbl_di_p2, aes(x = inquiry, y = theory)) +
#          # stat_density_2d(aes(fill = stat(level)), geom = "polygon", alpha = .3, h = c(1.2,1.2), show.legend = F) +
#          # scale_fill_viridis_c() +
#          geom_point(aes(color=treatment, size = 4)) +
#          theme_few() +
#          scale_shape_manual(values = c(12,15)) +
#          scale_color_manual(values = c("#00B6D6", "#0B51E3", "#0AD182",
#                                        "#D60E00", "#D10A9E", "#FF620D")) +
#          scale_fill_manual(values = c("#00B6D6", "#0B51E3", "#0AD182",
#                                       "#D60E00", "#D10A9E", "#FF620D")) +
#          scale_y_continuous(limits = c(0,.5), breaks = c(0,.1,.2,.3,.4,.5), minor_breaks = NULL, expand = c(0, 0)) +
#          scale_x_continuous(limits = c(0,3), breaks = c(0,1,2,3), minor_breaks = NULL, expand = c(0, 0)) +
#          xlab("Realized inquiry steps in analyses") +
#          ylab("% of analyses containing theory-practice integration")
# 
# p$labels$shape <- "treatment"
# # ggsave("Fig2.tiff", width = 90, height = 70, units = "mm", dpi = 500, scale = 1.6)
# 
# p
# 
# 
# ################################################################### #
# 
# 
# p2_di_pre <- pbl_di_p2 %>%
#   dplyr::filter(prepost == "pretest" & treatment == 0)
# p2_di_post <- pbl_di_p2 %>%
#   dplyr::filter(prepost == "posttest" & treatment == 0)
# 
# p2_pb_pre <- pbl_di_p2 %>%
#   dplyr::filter(prepost == "pretest" & treatment == 1)
# p2_pb_post <- pbl_di_p2 %>%
#   dplyr::filter(prepost == "posttest" & treatment == 1)
# 
# ggplot(p2_di_pre, aes(x = inquiry, y = theory)) +
#   geom_hex(bins = 20) +
#   scale_fill_continuous(type = "viridis") +
#   scale_y_continuous(limits = c(-.05,1), minor_breaks = NULL, expand = c(0, 0)) +
#   scale_x_continuous(limits = c(0,3), breaks = c(0,1,2,3), minor_breaks = NULL, expand = c(0, 0)) +
#   xlab("Realized inquiry steps in analyses") +
#   ylab("% of analyses containing theory-practice integration") +
#   theme_bw()
# 
# ggplot(p2_di_post, aes(x = inquiry, y = theory)) +
#   geom_hex(bins = 20) +
#   scale_fill_continuous(type = "viridis") +
#   scale_y_continuous(limits = c(-.05,1), minor_breaks = NULL, expand = c(0, 0)) +
#   scale_x_continuous(limits = c(0,3), breaks = c(0,1,2,3), minor_breaks = NULL, expand = c(0, 0)) +
#   xlab("Realized inquiry steps in analyses") +
#   ylab("% of analyses containing theory-practice integration") +
#   theme_bw()
# 
# 
# ggplot(p2_pb_pre, aes(x = inquiry, y = theory)) +
#   geom_hex(bins = 20) +
#   scale_fill_continuous(type = "viridis") +
#   scale_y_continuous(limits = c(-.05,1), minor_breaks = NULL, expand = c(0, 0)) +
#   scale_x_continuous(limits = c(0,3), breaks = c(0,1,2,3), minor_breaks = NULL, expand = c(0, 0)) +
#   xlab("Realized inquiry steps in analyses") +
#   ylab("% of analyses containing theory-practice integration") +
#   theme_bw()
# 
# ggplot(p2_pb_post, aes(x = inquiry, y = theory)) +
#   geom_hex(bins = 20) +
#   scale_fill_continuous(type = "viridis") +
#   scale_y_continuous(limits = c(-.05,1), minor_breaks = NULL, expand = c(0, 0)) +
#   scale_x_continuous(limits = c(0,3), breaks = c(0,1,2,3), minor_breaks = NULL, expand = c(0, 0)) +
#   xlab("Realized inquiry steps in analyses") +
#   ylab("% of analyses containing theory-practice integration") +
#   theme_bw()



###### PER TEST ######################################################################## #

# 
# 
# pbl_di_p2 <- pbl_di %>%
#   mutate(id = 1:nrow(.),
#          theory_pre1 = as.numeric(theory_pre1),
#          theory_pre2 = as.numeric(theory_pre2),
#          theory_pre3 = as.numeric(theory_pre3),
#          theory_post1 = as.numeric(theory_post1),
#          theory_post2 = as.numeric(theory_post2),
#          theory_post3 = as.numeric(theory_post3),
#          theory_pre = rowMeans(data.frame(theory_pre1, theory_pre2, theory_pre3)),
#          theory_pos = rowMeans(data.frame(theory_post1, theory_post2, theory_post3)),
#          inquir_pre = rowMeans(data.frame(inquiry_pre1, inquiry_pre2, inquiry_pre3)),
#          inquir_pos = rowMeans(data.frame(inquiry_post1, inquiry_post2, inquiry_post3))) %>%
#   dplyr::select(id, treatment, theory_pre:inquir_pos) %>%
#   pivot_longer(theory_pre:inquir_pos, names_to="vars", values_to="values") %>%
#   mutate(prepost = case_when(
#            str_detect(vars, "pre") ~ "pretest",
#            str_detect(vars, "pos") ~ "posttest"
#          ),
#          vars = case_when(
#            str_detect(vars, "theory") ~ "theory",
#            str_detect(vars, "inquir") ~ "inquiry"
#          )) %>%
#   pivot_wider(id_cols = c(id, prepost, treatment), names_from = "vars", values_from = "values")
# 
# 
# pbl_di_p2$treatment <- factor(pbl_di_p2$treatment, 
#                               levels = c(0,1), 
#                               labels = c("DI", "PB"))
# 
# 
# pbl_di_p2$prepost <- as.character(pbl_di_p2$prepost, 
#                               levels = c("pretest","posttest"))
# 
# 
# 
# ggplot(pbl_di_p2, aes(x = inquiry, y = theory)) +
#   geom_hex(bins = 20) +
#   scale_fill_continuous(type = "viridis") +
#   scale_y_continuous(limits = c(-.05,1.05), minor_breaks = NULL, expand = c(0, 0)) +
#   scale_x_continuous(limits = c(-.1,3.1), breaks = c(0,1,2,3), minor_breaks = NULL, expand = c(0, 0)) +
#   xlab("Realized inquiry steps in analyses") +
#   ylab("% of analyses containing theory-practice integration") +
#   theme_bw() +
#   facet_wrap(treatment~prepost)
# 
#   
# 
# ggplot(pbl_di_p2, aes(x = inquiry, y = theory)) +
#   geom_hex(bins = 20) +
#   scale_fill_continuous(type = "viridis") +
#   scale_y_continuous(limits = c(-.05,1.05), minor_breaks = NULL, expand = c(0, 0)) +
#   scale_x_continuous(limits = c(-.1,3.1), breaks = c(0,1,2,3), minor_breaks = NULL, expand = c(0, 0)) +
#   xlab("Realized inquiry steps in analyses") +
#   ylab("% of analyses containing theory-practice integration") +
#   theme_bw() +
#   facet_wrap(~prepost)
# 
# 
# 
# 
# ggplot(pbl_di_p2, aes(x = theory, y = treatment)) +
#   geom_density_ridges(aes(fill = prepost, color = prepost), alpha=.5, scale = 0.9) +
#   scale_fill_discrete(type = "viridis") +
#   scale_x_continuous(limits = c(0,1), minor_breaks = NULL, expand = c(0, 0)) +
#   xlab("Realized inquiry steps in analyses") +
#   ylab("% of analyses containing theory-practice integration") +
#   theme_bw()
  

############################################################# #
####  FIG 3                                                ####
####  REVISION 1                                           ## #
############################################################# #

########### CHANGE ##########################################


pbl_di_p2 <- pbl_di %>%
  mutate(id = 1:nrow(.),
         theory_pre1 = as.numeric(theory_pre1),
         theory_pre2 = as.numeric(theory_pre2),
         theory_pre3 = as.numeric(theory_pre3),
         theory_post1 = as.numeric(theory_post1),
         theory_post2 = as.numeric(theory_post2),
         theory_post3 = as.numeric(theory_post3),
         theory_pre = rowMeans(data.frame(theory_pre1, theory_pre2, theory_pre3)),
         theory_pos = rowMeans(data.frame(theory_post1, theory_post2, theory_post3)),
         inquir_pre = rowMeans(data.frame(inquiry_pre1, inquiry_pre2, inquiry_pre3)),
         inquir_pos = rowMeans(data.frame(inquiry_post1, inquiry_post2, inquiry_post3)),
         theory = theory_pos - theory_pre,
         inquiry = inquir_pos - inquir_pre) %>%
  dplyr::select(id, treatment, theory, inquiry)


pbl_di_p2$treatment <- factor(pbl_di_p2$treatment, 
                              levels = c(0,1), 
                              labels = c("DI", "PB"))




# ggplot(pbl_di_p2, aes(x = inquiry, y = theory)) +
#   geom_hex(bins = 30) +
#   scale_fill_continuous(type = "viridis") +
#   scale_y_continuous(limits = c(-1.05,1.05), minor_breaks = NULL, expand = c(0, 0)) +
#   scale_x_continuous(limits = c(-3.1,3.1), breaks = c(-3,-2,-1,0,1,2,3), minor_breaks = NULL, expand = c(0, 0)) +
#   xlab("Realized inquiry steps in analyses") +
#   ylab("% of analyses containing theory-practice integration") +
#   theme_bw() +
#   facet_wrap(~treatment)

ggplot(pbl_di_p2, aes(x = inquiry, y = theory)) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")  +
  scale_fill_continuous(type = "viridis") +
  scale_y_continuous(limits = c(-1.1,1.1), labels = c("-100%", "-50%", "0%", "+50%", "+100%"), minor_breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous(limits = c(-3.1,3.1), breaks = c(-3,-2,-1,0,1,2,3), labels = c("-3", "-2", "-1", "0", "+1", "+2", "+3"), minor_breaks = NULL, expand = c(0, 0)) +
  xlab("change in realized inquiry steps in analyses") +
  ylab("change in % of analyses \ncontaining theory-practice integration") +
  labs(fill = "density") +
  geom_hline(yintercept=0, color = "white", alpha = .2) +
  geom_vline(xintercept=0, color = "white", alpha = .2) +
  theme_bw() +
  facet_wrap(~treatment)



ggsave("Fig3.tiff", width = 20, height = 15, dpi = 300, units = "cm", scale = .8)


############################################################# #
####  FIG 3                                                ####
####  REVISION 2                                           ## #
############################################################# #

pbl_di <- rio::import("data_public/ZA6259_v1-0-0.sav")


# construct treatment variable
# 0 = DI, 1 = PBL
pbl_di$treatment <- ifelse(c(pbl_di$seminartyp == 4 | pbl_di$seminartyp == 2), 0, 1)


# get variables on theory and selection from other data set (needs to be reintegrated into original data set)
ts <- rio::import("data_public/ts.Rdata") %>%
  dplyr::select(code, anz.komm.43.1, anz.komm.43.2, anz.komm.43.7, anz.komm.43.3, anz.komm.43.4, anz.komm.43.5, 
                theorie.r.43.1, theorie.r.43.2, theorie.r.43.7, theorie.r.43.3, theorie.r.43.4, theorie.r.43.5,
                doz_pass, doz_gef) %>%
  dplyr::mutate(code = str_trim(code))

pbl_di <- left_join(pbl_di, ts, by ="code")


# construct centered variable for attendance
pbl_di <- pbl_di %>%
  dplyr::mutate(attendance = rowSums(data.frame(anwesend_erst, anwesend_zweit)))

# anwesend_MEAN <- pbl_di %>%
#                     summarize(anwesend_M = mean(anwesend, na.rm=TRUE))

# construct variable literature
pbl_di <- pbl_di %>%
  dplyr::mutate(literature = rowSums(data.frame(T2_text_1, T2_text_2, T2_text_3)))

# construct the variable lit_pre (literature read before the treatment)
pbl_di <- pbl_di %>%
  dplyr::mutate(lit_pre = rowSums(data.frame(T1_text_1, T1_text_2, T1_text_3)))

# construct the variable prior_knowledge, 
# based on a test on declarative knowledge on classroom management
pbl_di <- pbl_di %>%
  mutate(prior_knowledge = rowMeans(data.frame(T1_wiss_crm_verh1, T1_wiss_crm_verh2, T1_wiss_crm_verh3, T1_wiss_crm_verh4, T1_wiss_crm_verh5, T1_wiss_crm_verh6, T1_wiss_crm_bez1, T1_wiss_crm_bez2, T1_wiss_crm_unt1, T1_wiss_crm_unt2, T1_wiss_crm_unt3, T1_wiss_crm_unt4, T1_wiss_crm_unt5, T1_wiss_crm_unt6), na.rm = T))


pbl_di <- pbl_di %>%
  mutate(sel_att_pre1 = case_when(
    is.na(theorie.r.43.1) & is.na(theorie.r.43.2) & is.na(theorie.r.43.7) &
      is.na(A43_1) & is.na(A43_2) & is.na(A43_7) ~ as.numeric(NA),
    TRUE ~ as.numeric(anz.komm.43.1)),
    sel_att_pre2 = case_when(
      is.na(theorie.r.43.1) & is.na(theorie.r.43.2) & is.na(theorie.r.43.7) &
        is.na(A43_1) & is.na(A43_2) & is.na(A43_7) ~ as.numeric(NA),
      TRUE ~ as.numeric(anz.komm.43.2)),
    sel_att_pre3 = case_when(
      is.na(theorie.r.43.1) & is.na(theorie.r.43.2) & is.na(theorie.r.43.7) &
        is.na(A43_1) & is.na(A43_2) & is.na(A43_7) ~ as.numeric(NA),
      TRUE ~ as.numeric(anz.komm.43.7)),
    sel_att_post1 = case_when(
      is.na(theorie.r.43.3) & is.na(theorie.r.43.4) & is.na(theorie.r.43.5) &
        is.na(A43_3) & is.na(A43_4) & is.na(A43_5) ~ as.numeric(NA),
      TRUE ~ as.numeric(anz.komm.43.3)),
    sel_att_post2 = case_when(
      is.na(theorie.r.43.3) & is.na(theorie.r.43.4) & is.na(theorie.r.43.5) &
        is.na(A43_3) & is.na(A43_4) & is.na(A43_5) ~ as.numeric(NA),
      TRUE ~ as.numeric(anz.komm.43.4)),
    sel_att_post3 = case_when(
      is.na(theorie.r.43.3) & is.na(theorie.r.43.4) & is.na(theorie.r.43.5) &
        is.na(A43_3) & is.na(A43_4) & is.na(A43_5) ~ as.numeric(NA),
      TRUE ~ as.numeric(anz.komm.43.5))
  )


# deleting some variables to avoid problems with imputation
pbl_di <- pbl_di %>%
  mutate(theory_pre1 = theorie.r.43.1,
         theory_pre2 = theorie.r.43.2,
         theory_pre3 = theorie.r.43.7,
         theory_post1 = theorie.r.43.3,
         theory_post2 = theorie.r.43.4,
         theory_post3 = theorie.r.43.5,
         inquiry_pre1 = A43_1, 
         inquiry_pre2 = A43_2, 
         inquiry_pre3 = A43_7, 
         inquiry_post1 = A43_3, 
         inquiry_post2 = A43_4, 
         inquiry_post3 = A43_5) %>% # rename some variables for easier understanding
  dplyr::select(treatment, seminar, attendance, 
                sel_att_pre1, sel_att_pre2, sel_att_pre3, sel_att_post1, sel_att_post2, sel_att_post3,
                theory_pre1, theory_pre2, theory_pre3, theory_post1, theory_post2, theory_post3, 
                inquiry_pre1, inquiry_pre2, inquiry_pre3, inquiry_post1, inquiry_post2, inquiry_post3, literature,
                T2_anstrS_1, T2_anstrS_2, T2_anstrS_3, T2_anstrT_1, T2_anstrT_2, T2_anstrT_3, T2_anstrT_4,
                doz_pass, doz_gef,
                geschl, unterrichtet, erf_vid, lit_pre, prior_knowledge) %>%
  dplyr::filter(!is.na(seminar))








pbl_di_p <- pbl_di %>%
  mutate(doz = rowMeans(data.frame(doz_pass, doz_gef), na.rm = T),
         theory_pre = rowMeans(data.frame(theory_pre1, theory_pre2, theory_pre3), na.rm = T),
         theory_post = rowMeans(data.frame(theory_post1, theory_post2, theory_post3), na.rm = T),
         theory_change = theory_post - theory_pre) %>%
  select(doz, treatment, theory_pre, theory_post, theory_change) %>%
  pivot_longer(cols=3:5, names_to = "time", values_to = "theory")

# ggplot(pbl_di_p, aes(x=doz, y=theory, color = time)) +
#   stat_summary(fun.data= mean_cl_normal) + 
#   geom_smooth(method='lm') +
#   # geom_point() +
#   scale_color_viridis_d() +
#   scale_y_continuous(limits = c(-.05,.3)) +
#   theme_minimal()


library(ggside)

# ggplot(pbl_di_p, aes(x=doz, y=theory, color = time)) +
#   geom_xsidedensity(aes(y=stat(density))) +
#   geom_ysidedensity(aes(x=stat(density))) +
#   stat_summary(fun.data= mean_cl_normal) + 
#   geom_smooth(method='lm') +
#   # scale_color_viridis_d() +
#   scale_y_continuous(limits = c(0,1)) +
#   theme_bw()



## nur POSTTEST
ggplot(pbl_di_p%>%dplyr::filter(time=="theory_post"), aes(x=doz, y=theory)) +
  # stat_summary(fun.data= mean_cl_normal) + 
  # geom_point(position = position_dodge2(width = .2), alpha = .5) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")  +
  geom_smooth(method='lm', color = "red") +
  # scale_color_viridis_d() +
  scale_fill_continuous(type = "viridis") +
  scale_y_continuous(limits = c(-.2,1.2), 
                     expand = c(0,0), 
                     breaks = c(0,.2,.4,.6,.8,1), 
                     labels = c("0", ".2", ".4", ".6", ".8", "1")) +
  scale_x_continuous(limits = c(0.5,7.5), 
                     expand = c(0,0), 
                     breaks = c(1:6)) +
  geom_xsidedensity(aes(y=stat(density)), fill = "#37678c", color = NA) +
  geom_ysidedensity(aes(x=stat(density)), fill = "#37678c", color = NA, scale = "free_y") +
  scale_ysidex_continuous() +
  scale_xsidey_continuous() +
  xlab("instructor's positive attitude") +
  ylab("ratio of analyses containing\ntheory-practice integration (per person)") +
  labs(fill = "density") +
  theme_bw()


## CHANGE
ggplot(pbl_di_p%>%dplyr::filter(time=="theory_change"), aes(x=doz, y=theory)) +
  # stat_summary(fun.data= mean_cl_normal) + 
  # geom_point(position = position_dodge2(width = .2), alpha = .5) +
  stat_density_2d(aes(fill = ..level..), geom = "polygon")  +
  geom_smooth(method='lm', color = "red") +
  # scale_color_viridis_d() +
  scale_fill_continuous(type = "viridis") +
  scale_y_continuous(limits = c(-.5,1.2), 
                     expand = c(0,0), 
                     breaks = c(-.4, -.2, 0,.2,.4,.6,.8,1),
                     labels = c("-40%", "-20%", "0%", "+20%", "+40%", "+60%", "+80%", "+100%")
                     ) +
  scale_x_continuous(limits = c(0.5,7), 
                     expand = c(0,0), 
                     breaks = c(1:6)) +
  geom_xsidedensity(aes(y=stat(density)), fill = "#37678c", color = NA) +
  geom_ysidedensity(aes(x=stat(density)), fill = "#37678c", color = NA, scale = "free_y") +
  # coord_cartesian(xlim=c(1, 6)) +
  scale_ysidex_continuous() +
  scale_xsidey_continuous() +
  xlab("instructor's positive attitude") +
  ylab("change in % of analyses \ncontaining theory-practice integration") +
  labs(fill = "density") +
  theme_bw()

ggsave("Fig3.tiff", width = 20, height = 15, dpi = 300, units = "cm", scale = .8)
