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



ggsave("Fig2.tiff", width = 20, height = 15, dpi = 300, units = "cm", scale = .8)
