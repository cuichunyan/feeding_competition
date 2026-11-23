library(readxl)
library(dplyr)
library(lubridate)
library(tidyr)
library(glmmTMB)
library(stringr)
library(purrr)
library(broom) 
library(ggplot2)
library(ordinal)
library(emmeans)
library(ggpubr)
library(showtext)
showtext_auto()
library(scales)
library(patchwork)
library(pinyin)
library(circlize)

#translate English
mypydic <- pydic(method = 'toneless', dic = 'pinyin2') 
 
id_key <- read.csv("individual_id_key.csv", stringsAsFactors = FALSE, 
                   fileEncoding = "UTF-8")

stopifnot(!anyNA(id_key),
          !anyDuplicated(id_key$id),
          !anyDuplicated(id_key$short_id))

id_labels <- setNames(id_key$short_id, id_key$id)

id_label <- function(x) {
  x <- as.character(x)
  labels <- unname(id_labels[x])
  ifelse(is.na(labels), x, labels)
}

age_colors <- c(Adult_male = "#D55E00", Adult_female = "#009E73", Immature = "#619CFF")
sex_shapes <- c(F = 21, M = 22)
 
omu_mapping <- c("红点" = "Hongdian", "米粒" = "Mili", "豁鼻子" = "Huobizi",
                 "细毛17" = "Ximao17", "春芬17" = "Chunfen17", "二八" = "Erba",
                 "七哥" = "Qige", "二己" = "Erji", "二十大" = "Ershida",
                 "帅哥" = "Shuaige", "六辛" = "Liuxin", "全雄" = "All-male")
omu_order_en <- unname(omu_mapping)

#read_excel: 雌性生育
data_female_birth <- read_excel("feeding_competition.xlsx", sheet = "雌性生育") %>%
  mutate(母亲 = py(as.character(母亲), dic = mypydic, sep = "")) %>%
  rename(mother_id = 母亲, birth_date = 出生日期) %>%
  mutate(birth_date = as.Date(birth_date)) %>%
  select(mother_id, birth_date)

#read_excel:group_component
data_group_component <- suppressWarnings(read_excel("feeding_competition.xlsx", 
                                                    sheet = "群体组成")) %>%
  mutate(个体 = py(as.character(个体), dic = mypydic, sep = "")) %>%
  mutate(单元 = recode(单元, !!!omu_mapping)) %>%
  mutate(单元 = factor(单元, levels = omu_order_en)) %>%
  mutate(日期 = as.Date(日期, format="%Y-%m-%d")) %>%
  {date_sequence <- seq(min(.$日期), as.Date("2024-10-23"), by = "day")
  bind_rows(lapply(date_sequence, function(current_date) {
    if (current_date %in% .$日期) {
      filter(., 日期 == current_date)
    } else {
      previous_data <- filter(., 日期 == max(.$日期[.$日期 < current_date]))
      mutate(previous_data, 日期 = current_date)
    }
  }))
  } %>%
  mutate(id_age = ifelse(is.na(出生日期), 15, time_length(interval(出生日期, 日期), 
                                                      unit = "year"))) %>%
  mutate(id_age = round(id_age, 1)) %>%
  mutate(age_group = case_when(性别 == "M" & id_age >= 6.5 ~ "Adult_male",
                               性别 == "F" & id_age >= 5 ~ "Adult_female",
                               性别 == "M" & id_age >= 1 & id_age < 6.5 ~ "Immature",
                               性别 == "F" & id_age >= 1 & id_age < 5 ~ "Immature",
                               id_age < 1 ~ "Infant")) %>%
  mutate(age_group = factor(age_group, levels = 
                              c("Adult_male", "Adult_female", "Immature", "Infant"))) %>%
  arrange(日期) %>%
  filter(单元 != "斜刘海") %>%
  left_join(data_female_birth, by = c("个体" = "mother_id")) %>%
  mutate(reproductive_status = case_when(age_group != "Adult_female" ~ NA_character_,
                                         is.na(birth_date) ~ "non_parous",
                                         日期 < birth_date ~ "non_parous",
                                         日期 >= birth_date ~ "parous"))

#read_excel:focal_patch
data_focal_patch <- suppressWarnings(read_excel("feeding_competition.xlsx",
                                                sheet = "焦点斑块")) %>%
  mutate(个体 = py(as.character(个体), dic = mypydic, sep = "")) %>%
  mutate(单元 = recode(单元, !!!omu_mapping)) %>%  
  mutate(单元 = factor(单元, levels = omu_order_en)) %>% 
  mutate(停止取食 = if_else(is.na(停止取食), 离开斑块, 停止取食)) %>% 
  mutate(patch_id = paste(日期, 开始时间, 食物斑块, sep = "_"),
         patch_type = if_else(grepl("投食", patch_id), "Provisioned Patch", "Natural Patch"),
         feeding_time = as.numeric(difftime(离开斑块, 进入斑块, units = "min")),
         观察时间 = as.numeric(difftime(结束时间, 开始时间, units = "min"))) %>% 
  left_join(data_group_component, by = c("日期", "个体")) %>% 
  select(date = 日期, patch_id, patch_type, obs_time = 观察时间, patch_quality = 斑块质量,
         id = 个体, sex = 性别, id_age, age_group, reproductive_status, omu = 单元.x, 
         omu_size = 单元大小, enter_patch = 进入斑块, out_patch = 离开斑块, feeding_time) %>% 
  filter(obs_time >= 10, age_group != "Infant")  %>% 
  mutate(breeding_state = if_else(month(date) %in% 2:7, "bre", "non_bre"))  

#read_excel:patch_contest
data_patch_contest <- read_excel("feeding_competition.xlsx",
                                 sheet = "冲突事件") %>%
  mutate(个体 = py(as.character(个体), dic = mypydic, sep = ""),
         对象 = py(as.character(对象), dic = mypydic, sep = "")) %>%
  filter(!is.na(食物斑块)) %>% 
  mutate(patch_id = paste(日期, 开始时间, 食物斑块, sep = "_")) %>% 
  select(日期, patch_id, 时间, 个体, 行为, 对象) %>% 
  separate_rows(个体, sep = "[、\\s]+") %>%
  separate_rows(对象, sep = "[、\\s]+") %>% 
  semi_join(data_focal_patch, by = "patch_id") %>%  
  inner_join(data_group_component, by = c("日期", "个体")) %>%
  rename(个体_单元 = 单元, 个体_性别 = 性别, 个体_age_group = age_group, 
         个体_id_age = id_age, 个体_reproductive_status = reproductive_status) %>%
  inner_join(data_group_component, by = c("日期" = "日期", "对象" = "个体")) %>%
  rename(对象_单元 = 单元, 对象_性别 = 性别, 对象_age_group = age_group, 
         对象_id_age = id_age, 对象_reproductive_status = reproductive_status) %>% 
  select(date = 日期, patch_id, 
         id1 = 个体, id1_sex = 个体_性别, id1_age = 个体_age_group, id1_omu = 个体_单元,
         Conflict_behave = 行为, id1_age_numb = 个体_id_age, 
         id1_reproductive_status = 个体_reproductive_status,
         id2 = 对象, id2_sex = 对象_性别, id2_age = 对象_age_group, id2_omu = 对象_单元, 
         id2_age_numb = 对象_id_age, id2_reproductive_status = 对象_reproductive_status) %>% 
  filter(patch_id %in% data_focal_patch$patch_id,
         id1_age != "Infant", id2_age != "Infant",
         id1_omu == id2_omu) %>%
  semi_join(data_focal_patch, by = c("patch_id" = "patch_id", "id1" = "id")) %>%
  semi_join(data_focal_patch, by = c("patch_id" = "patch_id", "id2" = "id")) 

#sample description
length(unique(data_focal_patch$id)) #共涉及81个个体的取食行为

length(unique(data_focal_patch$date)) #共观察178天

data_focal_patch %>%
  distinct(patch_id, date, patch_type, obs_time) %>%
  group_by(patch_type) %>%
  summarise(patch_n = n(),
            total_obs_hour = sum(obs_time, na.rm = TRUE) / 60,
            mean_obs_min = mean(obs_time, na.rm = TRUE),
            sd_obs_min = sd(obs_time, na.rm = TRUE),
            min_obs_min = min(obs_time, na.rm = TRUE),
            max_obs_min = max(obs_time, na.rm = TRUE),
            mean_obs_hour = mean_obs_min / 60,
            sd_obs_hour = sd_obs_min / 60, .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) #观察自然斑块81个,投食斑块162个;自然斑块的平均观察时间为27.1 ± 19.2 min(范围:10-119.8 min),投食斑块的平均观察时间为34.8 ± 14.6 min(范围:10.2-82.3 min);累积观察130.4h

data_focal_patch %>%
  distinct(patch_id, date, patch_type, obs_time) %>%
  mutate(month = format(as.Date(date), "%Y-%m")) %>%
  group_by(month, patch_type) %>%
  summarise(patch_n = n(),
            total_obs_hour = sum(obs_time, na.rm = TRUE) / 60,
            mean_obs_min = mean(obs_time, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) #各月份观察天数,不同类型斑块的观察时间

data_focal_patch %>%
  distinct(patch_id, patch_quality) %>%
  mutate(quality_kg = patch_quality * 0.5) %>%
  summarise(mean_val = mean(quality_kg, na.rm = TRUE),
            sd_val = sd(quality_kg, na.rm = TRUE),
            min_val = min(quality_kg, na.rm = TRUE),
            max_val = max(quality_kg, na.rm = TRUE)) #投食斑块的重量

data_patch_contest %>% 
  left_join(distinct(data_focal_patch, patch_id, patch_type), by = "patch_id") %>% 
  count(patch_type) #共发生了564次竞争，其中自然斑块发生了8次，投食斑块发生了556次
length(unique(data_focal_patch$omu))

data_focal_patch %>%
  group_by(age_group) %>%
  summarise(个体数量 = n_distinct(id)) #观察42个雌性,39个雄性

#H1:投食斑块是否会提高群内觅食竞争强度？
data_H1_patch_level <- data_focal_patch %>%
  group_by(patch_type, patch_id) %>%
  summarise(obs_time = first(obs_time),
            visitor_n = n_distinct(id), .groups = "drop") %>%
  left_join(data_patch_contest %>%
              count(patch_id, name = "conflict_n"), by = "patch_id") %>% 
  mutate(conflict_n = replace_na(conflict_n, 0),
         obs_hour = obs_time / 60,
         conflict_rate = conflict_n / obs_hour) %>% 
  left_join(data_focal_patch %>%
              distinct(patch_id, patch_quality), by = "patch_id") %>% 
  mutate(patch_type = factor(patch_type, levels = c("Natural Patch", "Provisioned Patch"))) 
 
wilcox.test(visitor_n ~ patch_type, data = data_H1_patch_level) 
wilcox.test(conflict_rate ~ patch_type, data = data_H1_patch_level)  
#投食斑块的访问个体数显著高于自然斑块(W=2977,p<0.001);投食斑块的冲突频率显著高于自然斑块(W = 1157.5, p < 0.001)

data_H1_patch_level %>%
  group_by(patch_type) %>%
  summarise(patch_n = n(),
            visitor_n_mean = mean(visitor_n, na.rm = TRUE),
            visitor_n_sd = sd(visitor_n, na.rm = TRUE),
            visitor_n_min = min(visitor_n, na.rm = TRUE),
            visitor_n_max = max(visitor_n, na.rm = TRUE),
            total_conflict_n = sum(conflict_n, na.rm = TRUE),
            total_obs_hour = sum(obs_hour, na.rm = TRUE),
            overall_conflict_rate_hour = total_conflict_n / total_obs_hour,
            conflict_rate_mean = mean(conflict_rate, na.rm = TRUE),
            conflict_rate_sd = sd(conflict_rate, na.rm = TRUE),
            conflict_rate_min = min(conflict_rate, na.rm = TRUE),
            conflict_rate_max = max(conflict_rate, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(where(is.numeric), ~ round(.x, 2))) #投食斑块的访问个体数为8.42±4.47(4-30),自然斑块的访问个体数为5.51±3.03(2-19);投食斑块的冲突频率为6.26±6.19(0-39.48)次/小时,自然斑块的冲突频率为0.21±0.81(0-5)次/小时。

glmmTMB(conflict_n ~ patch_quality + visitor_n + offset(log(obs_hour)),
        family = nbinom2,
        data = data_H1_patch_level %>% filter(patch_type != "Natural Patch")) %>% 
  summary() #GLM结果显示,随着投食斑块的质量增加冲突频率也随之增加(β=0.085,P=0.029),而斑块访问个体数与冲突频率间的关系并不显著(β=-0.014,P=0.356)

#H2:投食斑块是否使进入顺序更稳定并与个体属性相关
data_feeding_order_patch <- data_focal_patch %>%
  filter(!is.na(enter_patch)) %>%
  group_by(patch_type, patch_id, omu, id) %>%
  arrange(enter_patch, .by_group = TRUE) %>%
  summarise(first_enter_time = min(enter_patch, na.rm = TRUE),
            total_enter_count = n(),
            sex = first(sex),
            age_group = first(age_group),
            id_age = first(id_age),
            reproductive_status = first(reproductive_status),
            breeding_state = first(breeding_state),
            omu_size = first(omu_size),
            patch_quality = first(patch_quality),
            .groups = "drop") %>%
  group_by(patch_type, patch_id, omu) %>%
  arrange(first_enter_time, id, .by_group = TRUE) %>%
  mutate(gap_sec = as.numeric(difftime(first_enter_time,
                                       lag(first_enter_time), units = "secs")),
         feeding_order = cumsum(is.na(gap_sec) | gap_sec >= 0),
         feeding_order_max = max(feeding_order, na.rm = TRUE),
         feeding_order_percent = if_else(feeding_order_max > 1,
                                         (feeding_order - 1) / (feeding_order_max - 1), 0)) %>%
  ungroup() %>%
  mutate(enter_time = first_enter_time,
         omu = factor(omu, levels = omu_order_en),
         id_omu = interaction(omu, id, drop = TRUE),
         patch_type = factor(patch_type, levels = c("Natural Patch", "Provisioned Patch")),
         age_group = factor(age_group, levels = c("Adult_male", "Adult_female", "Immature", "Infant")),
         sex = factor(sex, levels = c("F", "M")),
         factor_feeding_order = ordered(feeding_order))  

table_kw_order <- data_feeding_order_patch %>%
  group_by(patch_type, omu) %>%
  filter(n_distinct(id) >= 2, n() >= 3) %>%
  summarise(tidy(kruskal.test(feeding_order_percent ~ as.factor(id))), .groups = "drop") %>% 
  mutate(kw_label = sprintf("'K-W:'~italic(H)==%.2f~','~df==%d~','~italic(p)%s",
                            statistic, parameter,
                            if_else(p.value < 0.001, "<0.001", 
                                    sprintf("==%.3f", p.value))))
#Kruskal-Wallis检验结果显示,个体进入斑块顺序在自然斑块中无显著差异,而在投食斑块中,除红点,六辛,全雄外,其他单元内个体的进入顺序差异显著

data_sample_n_plot <- data_feeding_order_patch %>%
  group_by(omu, patch_type) %>%
  summarise(n_patch = n_distinct(patch_id), .groups = "drop") %>%
  mutate(sample_label = paste0("n = ", n_patch), 
         individual_label = factor(paste(omu, "998 | SAMPLE_N", sep = " | ")),
         x_pos = if_else(patch_type == "Natural Patch", -1.12, 1.12),
         hjust_pos = if_else(patch_type == "Natural Patch", 0, 1))

# 每组 Dunn 检验只计算一次，供字母标记和两两比较表共用
dunn_results <- data_feeding_order_patch %>%
  semi_join(table_kw_order %>% filter(p.value < 0.05), by = c("patch_type", "omu")) %>%
  group_by(patch_type, omu) %>%
  summarise(test = list(dunn.test::dunn.test(feeding_order_percent, id, 
                                             method = "bh", kw = FALSE, list = TRUE)),
            .groups = "drop")

data_dunn_letters <- data_feeding_order_patch %>%
  semi_join(table_kw_order %>% filter(p.value < 0.05), by = c("patch_type", "omu")) %>%
  group_by(patch_type, omu) %>%
  group_modify(~ {
    dt <- dunn_results$test[[which(
      dunn_results$patch_type == .y$patch_type & dunn_results$omu == .y$omu
    )]]
    p_vec <- setNames(dt$P.adjusted, str_replace_all(dt$comparisons, " - ", "-"))
    raw_letters <- multcompView::multcompLetters(p_vec, threshold = 0.05)$Letters
    id_order <- .x %>% group_by(id) %>% 
      summarise(m = mean(feeding_order_percent, na.rm=T)) %>% arrange(m) %>% pull(id)
    old_chars <- raw_letters[as.character(id_order)] %>% str_split("") %>% 
      unlist() %>% unique()
    char_map <- setNames(letters[seq_along(old_chars)], old_chars)
    tibble(id = names(raw_letters),
           dunn_letter = map_chr(raw_letters, ~ paste(sort(char_map[str_split(.x, "")[[1]]]), 
                                                      collapse = ""))) %>%
      right_join(tibble(id = as.character(unique(.x$id))), by = "id") %>%
      mutate(dunn_letter = replace_na(dunn_letter, "a"))
  }) %>% ungroup()

data_summary <- data_feeding_order_patch %>%
  group_by(omu, id, patch_type) %>%
  summarise(mean_order = mean(feeding_order_percent, na.rm = TRUE), .groups = "drop") %>%
  left_join(data_dunn_letters, by = c("patch_type", "omu", "id"))

data_ind_attr <- data_feeding_order_patch %>%
  count(omu, id, sex, age_group, name = "attr_n") %>%
  group_by(omu, id) %>%
  arrange(desc(attr_n), .by_group = TRUE) %>%
  dplyr::slice(1) %>%
  ungroup() %>%
  select(omu, id, sex, age_group)

data_plot <- data_summary %>%
  pivot_wider(id_cols = c(omu, id), names_from = patch_type, values_from = mean_order) %>%
  left_join(data_ind_attr, by = c("omu", "id")) %>%
  mutate(x_natural = -`Natural Patch`, x_provisioned = `Provisioned Patch`,
         provisioned_sort = replace_na(`Provisioned Patch`, Inf),
         natural_sort = replace_na(`Natural Patch`, Inf)) %>%
  group_by(omu) %>%
  arrange(provisioned_sort, natural_sort, id, .by_group = TRUE) %>%
  mutate(plot_rank = row_number()) %>%
  ungroup() %>%
  mutate(individual_label_raw = paste(omu, sprintf("%03d", plot_rank), id, sep = " | "))

label_levels <- data_plot %>%
  group_by(omu) %>%
  arrange(plot_rank, .by_group = TRUE) %>%
  summarise(lvls = list(c(individual_label_raw,
                          paste(first(omu), "998 | SAMPLE_N", sep = " | "),
                          paste(first(omu), "999 | KW_LABEL", sep = " | "))), 
            .groups = "drop") %>%
  pull(lvls) %>%
  unlist() %>%
  unique()

data_plot <- data_plot %>%
  mutate(individual_label = factor(individual_label_raw, levels = label_levels))

data_segment <- data_plot %>%
  filter(!is.na(x_natural), !is.na(x_provisioned))

data_point <- data_summary %>%
  semi_join(data_plot, by = c("omu", "id")) %>%
  left_join(data_ind_attr, by = c("omu", "id")) %>%
  left_join(data_plot %>% select(omu, id, individual_label), by = c("omu", "id")) %>%
  mutate(x_order = if_else(patch_type == "Natural Patch", -mean_order, mean_order),
         x_order_plot = case_when(x_order == 0 & patch_type == "Natural Patch" ~ -0.02,
                                  x_order == 0 & patch_type == "Provisioned Patch" ~ 0.02,
                                  TRUE ~ x_order),
         x_letter = if_else(patch_type == "Natural Patch",
                            pmax(x_order_plot - 0.06, -1.08),
                            pmin(x_order_plot + 0.06, 1.08)),
         hjust_letter = if_else(patch_type == "Natural Patch", 1, 0)) 

data_kw_plot <- table_kw_order %>%
  filter(omu %in% data_plot$omu) %>%
  mutate(individual_label = factor(paste(omu, "999 | KW_LABEL", sep = " | "),
                                   levels = label_levels),
         x_pos = if_else(patch_type == "Natural Patch", -1.12, 1.12),
         hjust_pos = if_else(patch_type == "Natural Patch", 0, 1))

ggplot(data_plot, aes(y = individual_label)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey45", linewidth = 0.4) +
  geom_point(data = data_point,
             aes(x = x_order_plot, fill = age_group, shape = sex),
             size = 2.5, color = "grey20", stroke = 0.35, alpha = 0.95, 
             show.legend = c(shape = TRUE, fill = FALSE, color = FALSE)) +
  geom_segment(data = data_point,
               aes(x = 0, xend = x_order_plot,
                   yend = individual_label, color = age_group),
               linewidth = 0.8, alpha = 0.9,
               show.legend = c(color = TRUE)) +
  geom_text(data = data_point %>% drop_na(dunn_letter),
            aes(x = x_letter, label = dunn_letter, hjust = hjust_letter),
            size = 3.5, family = "sans", color = "black", show.legend = FALSE) +
  geom_text(data = data_kw_plot %>% 
              mutate(kw_label = str_replace_all(kw_label, fixed("chi^2"), "italic(H)") %>%
                       str_replace_all("χ²", "italic(H)")), 
            aes(x = x_pos, label = kw_label, hjust = hjust_pos),
            vjust = 0.5, size = 2.8, family = "sans", 
            parse = TRUE, show.legend = FALSE) + 
  geom_text(data = data_sample_n_plot,
            aes(x = x_pos, label = sample_label, hjust = hjust_pos), 
            y = -Inf, vjust = -0.5, inherit.aes = FALSE, size = 3.2, family = "sans")+
  facet_wrap(~ omu, scales = "free_y", ncol = 3) +
  scale_x_continuous(limits = c(-1.12, 1.12),
                     breaks = c(-1, -0.5, 0, 0.5, 1),
                     labels = c("Natural 1.0", "Natural 0.5", "0", "Provisioned 0.5", "Provisioned 1.0")) +
  scale_y_discrete(labels = function(x) {
    id_label(str_replace_all(x, "^[^|]+ \\| [0-9]+ \\| |.*KW_LABEL$", ""))
  }) +
  scale_shape_manual(values = sex_shapes, 
                     labels = c("F" = "Female", "M" = "Male"),
                     name = "Sex") +
  scale_fill_manual(values = age_colors, guide = "none") +
  scale_color_manual(values = age_colors, name = "Age Group", drop = FALSE) +
  guides(shape = guide_legend(override.aes = 
                                list(fill = "grey70", color = "grey20", size = 3.5, 
                                     linewidth = 0, linetype = 0), order = 1),
         color = guide_legend(override.aes = list(linewidth = 1.5, alpha = 1, shape = NA),
                              order = 2)) +
  labs(x = NULL, y = NULL) +
  coord_cartesian(clip = "off") +
  theme_bw() + 
  theme(text = element_text(family = "sans", size = 14),   
        panel.grid = element_blank(),
        legend.position = "top",
        legend.box = "horizontal",
        legend.title = element_text(size = 10),  
        legend.text = element_text(size = 10),                    
        axis.text.x = element_text(size = 8, color = "black"),   
        axis.text.y = element_text(size = 8, color = "black"),   
        strip.text = element_text(face = "bold", size = 12), 
        plot.margin = margin(5.5, 24, 5.5, 24))

ggsave("Fig2_feeding_order.pdf", width = 400, height = 300, units = "mm", device = cairo_pdf, bg = "white")#图注:图中比较了不同单元内个体在自然斑块和投食斑块中的标准化进入顺序。横轴表示进入顺序,0表示最早进入,1表示最晚进入;自然斑块在左侧,投食斑块在右侧。不同形状表示性别,不同颜色表示年龄组。图中的字母为Dunntest事后比较结果,相同字母的个体间差异不显著;不同字母的个体间进入顺序存在显著差异。 

data_dunn_pairs <- data_feeding_order_patch %>%
  semi_join(table_kw_order %>% filter(p.value < 0.05), by = c("patch_type", "omu")) %>%
  group_by(patch_type, omu) %>%
  group_modify(~ {
    dt <- dunn_results$test[[which(
      dunn_results$patch_type == .y$patch_type & dunn_results$omu == .y$omu
    )]]
    
    tibble(comparison = str_replace_all(dt$comparisons, " - ", " vs "),
           id1 = stringr::str_split_i(dt$comparisons, " - ", 1),
           id2 = stringr::str_split_i(dt$comparisons, " - ", 2),
           z_value = dt$Z, p_adjusted = dt$P.adjusted)
  }) %>% 
  ungroup()
 
data_full_combined_table <- data_dunn_pairs %>%
  left_join(data_summary, by = c("patch_type", "omu", "id1" = "id")) %>%
  rename(mean_order_id1 = mean_order, letter_id1 = dunn_letter) %>% 
  left_join(data_summary, by = c("patch_type", "omu", "id2" = "id")) %>%
  rename(mean_order_id2 = mean_order, letter_id2 = dunn_letter) %>% 
  select(patch_type, omu, comparison, 
         id1, mean_order_id1, letter_id1, 
         id2, mean_order_id2, letter_id2, 
         z_value, p_adjusted)  

data_feeding_order_patch %>%
  semi_join(table_kw_order %>% filter(p.value < 0.05), by = c("patch_type", "omu")) %>%
  select(patch_type, omu, id, feeding_order, feeding_order_percent)  
 
data_feeding_order_patch$age_group <- relevel(factor(data_feeding_order_patch$age_group), 
                                         ref = "Immature")

model_h2 <- clmm(factor_feeding_order ~ age_group + 
                   (1|id_omu), data = data_feeding_order_patch %>% 
                   filter(patch_type == "Provisioned Patch", omu != "All-male"), Hess = TRUE) 

summary(model_h2) #CLMM结果显示,在投食斑块中,相比未成年个体，成年雄性(β=-3.860,P<0.001)和成年雌性(β=-1.343,P<0.001)更早进入斑块。这种差异在不同个体间表明较为明显(SD=1.06)。
 
combinations <- list(list(label = "Adult Male",    ref_level = "Adult_male"),
                     list(label = "Adult Female",  ref_level = "Adult_female"),
                     list(label = "Immature", ref_level = "Immature"))

all_category_probs <- data.frame()

for (cond in combinations) { 
  temp_data <- data_feeding_order_patch %>% 
    filter(patch_type == "Provisioned Patch", omu != "All-male") 
  
  temp_data$age_group <- relevel(factor(temp_data$age_group), ref = cond$ref_level)
   
  temp_model <- clmm(factor_feeding_order ~ age_group + (1|id_omu), 
                     data = temp_data, Hess = TRUE) 
  
  alpha <- temp_model$alpha
  cum_prob <- plogis(as.numeric(alpha))
  prob <- c(cum_prob[1], diff(cum_prob), 1 - cum_prob[length(cum_prob)])
  
  temp_prob_df <- data.frame(
    Group = cond$label,                                
    feeding_order = 1:(length(alpha) + 1),
    prob = prob,
    prob_label = paste0(round(prob * 100, 1), "%"))
  
  all_category_probs <- rbind(all_category_probs, temp_prob_df)
}
 
all_category_probs$Group <- factor(all_category_probs$Group, 
                                   levels = sapply(combinations, function(x) x$label))

prob_max <- max(all_category_probs$prob)  

ggplot(all_category_probs, aes(x = feeding_order, y = prob, fill = Group)) +
  geom_col(width = 0.65, colour = NA, alpha = 0.75) +
  geom_text(aes(label = prob_label), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = setNames(age_colors, 
                                      c("Adult Male", "Adult Female", "Immature")),
                    guide = "none") +
  scale_x_continuous(breaks = unique(all_category_probs$feeding_order)) +
  scale_y_continuous(name = "Predicted probability",
                     labels = scales::percent_format(accuracy = 1),
                     limits = c(0, prob_max * 1.1), expand = expansion(mult = c(0, 0))) +
  facet_wrap(~ Group, ncol = 3) +
  labs(x = "Feeding order") +
  theme_bw() +
  theme(text = element_text(family = "sans", size = 14),
        panel.grid = element_blank(),
        axis.text.x = element_text(size = 14, color = "black"),
        axis.text.y = element_text(size = 14, color = "black"),
        strip.text = element_text(face = "bold", size = 16, margin = margin(t = 2, b = 2)),
        strip.background = element_rect(fill = "grey90"),
        plot.title = element_text(face = "bold", hjust = 0.5),
        plot.margin = margin(0, 24, 5.5, 24))

ggsave("Fig3_feeding_order.pdf", width = 400, height = 300, units = "mm", device = cairo_pdf, bg = "white")

model_h2_null <- update(model_h2, . ~ . - age_group)
anova(model_h2_null, model_h2)
 
emm_order <- emmeans(model_h2, ~ age_group, mode = "latent")

order_table <- as.data.frame(emm_order)
order_table[order(order_table$emmean), ]
 
order_pairs <- pairs(emm_order, adjust = "tukey")

summary(order_pairs, infer = c(TRUE, TRUE)) #成年雄性比成年雌性(estimate=-2.517,p<0.001)和未成熟个体(estimate=-3.860,p<.001)占据更早的位置,而成年雌性比未成熟个体占据更早的位置(estimate=-1.343,p<0.001)。

prob_estimates <- emmeans(model_h2, ~ age_group | factor_feeding_order, mode = "prob")

prob_pairs <- pairs(prob_estimates, adjust = "none")

prob_pair_table <- as.data.frame(prob_pairs)
 
prob_pair_table$p_holm <- p.adjust(prob_pair_table$p.value, method = "holm")

prob_pair_table

#H3:较早进入是否带来取食收益提高攻击发起？
order_time_conflict <- data_feeding_order_patch %>%
  select(patch_id, patch_type, omu, id, feeding_order, sex, age_group, reproductive_status,
         feeding_order_percent) %>%
  left_join(data_focal_patch %>%  
              group_by(patch_id, patch_type, obs_time, omu, id) %>%
              summarise(feeding_time = sum(feeding_time, na.rm = TRUE),
                        entry_count = n(), .groups = "drop"),
            by = c("patch_id", "patch_type","omu", "id")) %>%
  filter(!is.na(feeding_time), feeding_time > 0,
         !is.na(age_group),
         !is.na(sex)) %>%
  mutate(patch_type = factor(patch_type, levels = c("Natural Patch", "Provisioned Patch")),
         age_group = factor(age_group, levels = c("Adult_male", "Adult_female", "Immature")),
         sex = factor(sex, levels = c("F", "M"))) %>%
  left_join(data_patch_contest %>%
              filter(id1 != id2, id1_omu == id2_omu) %>%
              count(patch_id, omu = id1_omu, id = id1, name = "initiated_conflict_n"),
            by = c("patch_id", "omu", "id")) %>% 
  mutate(initiated_conflict_n = replace_na(initiated_conflict_n, 0),
         initiated_conflict = if_else(initiated_conflict_n > 0, 1, 0),
         id_omu = interaction(omu, id, drop = TRUE))  

order_time_conflict$age_group <- relevel(factor(order_time_conflict$age_group), 
                                         ref = "Immature")

feeding_time_model <- glmmTMB(feeding_time ~ feeding_order_percent + patch_type + age_group +  (1|patch_id) +  (1|id_omu),
                              family = Gamma(link = "log"), 
                              data = order_time_conflict)

summary(feeding_time_model)
#GLMM结果显示,取食顺序会影响个体取食时间,越早进入的个体取食时间越长(β=-0.142,P=0.001);相比于自然斑块,个体在投食斑块中的取食时间更长(β=0.482,P<0.001);相比于未成年个体,成年雄性(β=0.150,P=0.166)和成年雌性(β=0.280,P=0.001)的取食时间相对较长。在不同斑块(SD=0.49)和不同个体间(SD=0.32)差异较大。

order_time_conflict %>% 
  group_by(patch_type, age_group) %>%
  summarise(mean_feeding_time = mean(feeding_time, na.rm = TRUE),
            sd_feeding_time = sd(feeding_time, na.rm = TRUE), 
            max_feeding_time = max(feeding_time, na.rm = TRUE),
            min_feeding_time = min(feeding_time, na.rm = TRUE),
            .groups = "drop")  
  
kruskal.test(feeding_time ~ age_group, data = order_time_conflict %>% filter(patch_type == "Provisioned Patch")) #在投食斑块中,成年雄性(平均取食时间=17.7±9.0 min)和成年雌性(平均取食时间=19.1±10.3 min)的取食时间显著长于未成年个体(平均取食时间=13.5±8.2 min)(W=1056,p=0.001)。)) 

order_time_conflict %>% 
  filter(patch_type == "Provisioned Patch") %>% 
  with(pairwise.wilcox.test(feeding_time, age_group, p.adjust.method = "holm"))

feeding_conflict_model <- glmmTMB(initiated_conflict_n ~ feeding_order_percent + patch_type + age_group +  offset(log(feeding_time)) + (1|patch_id) + (1|id_omu),
                                  family = "poisson", 
                                  data = order_time_conflict) 

summary(feeding_conflict_model)
#GLMM结果显示,取食顺序同样会影响冲突频率,越早进入的个体发起冲突的频率越高(β=-0.587,P=0.004);投食斑块的冲突频率高于自然斑块(β=2.924,P<0.001);相比于未成年个体,成年雄性(β=3.513,P<0.001)和成年雌性(β=2.475,P<0.001)发起冲突频率显著较高。在不同斑块(SD=0.66)和不同单元个体间(SD=0.95)差异较大。

#H4:食物斑块内的个体冲突网络(较早进入的个体更容易攻击较晚个体)
order_lookup <- data_feeding_order_patch %>% 
  mutate(patch_id = as.character(patch_id),
         across(c(id, omu), ~ str_squish(as.character(.x)))) %>%
  distinct(patch_id, omu, id, .keep_all = TRUE) %>%
  select(patch_id, patch_type, patch_quality, id, feeding_order, omu_size)

conflict_order <- data_patch_contest %>%
  filter(id1 != id2, id1_omu == id2_omu) %>%
  mutate(conflict_id = row_number(), patch_id = as.character(patch_id), 
         across(c(id1, id2, id1_omu, id2_omu), ~ str_squish(as.character(.x)))) %>% 
  left_join(order_lookup %>%
              select(patch_id, patch_type, patch_quality, id, omu_size,
                     actor_order = feeding_order),
            by = c("patch_id", "id1" = "id")) %>% 
  left_join(order_lookup %>% select(patch_id, id, rec_order = feeding_order), 
            by = c("patch_id", "id2" = "id")) %>% 
  mutate(omu = id1_omu, order_diff = rec_order - actor_order,
         direction = case_when(order_diff > 0 ~ "early_to_late",
                               order_diff < 0 ~ "late_to_early"),
         early_actor = as.integer(order_diff > 0))  
 
conflict_order %>%
  count(direction) %>%
  mutate(prop = n / sum(n)) #在发生冲突的个体对,75.67%为较早进入个体攻击较晚进入个体,24.29%为较晚进入个体攻击较早进入个体.

binom.test(x = sum(conflict_order$direction == "early_to_late", na.rm = TRUE),
           n = sum(conflict_order$direction %in% c("early_to_late", "late_to_early")),
           p = 0.5, alternative = "two.sided")#在563次定向冲突事件中,426次(75.67%)为早到者攻击晚到者,Binomial test结果显示,早到者针对晚到者的冲突发生率显著高于50% 的随机预期(Binomial test:n=563,p<0.001,95%CI=[0.719,0.792]),说明在该物种的食物斑块竞争中存在显著的顺序效应。
 
data_node <- data_feeding_order_patch %>%
  summarise(prov = mean(feeding_order_percent[patch_type == "Provisioned Patch"],
                        na.rm = TRUE),
            nat = mean(feeding_order_percent[patch_type == "Natural Patch"],
                       na.rm = TRUE), across(c(sex, age_group), first),
            .by = c(omu, id)) %>%
  arrange(factor(omu, levels = omu_order_en),
          coalesce(prov, Inf), coalesce(nat, Inf), id) %>%
  mutate(rank = row_number(), .by = omu)

individual_attributes <- data_node %>%
  arrange(omu, rank) %>%
  select(omu, id, sex, age_group, feeding_rank = rank)
 
data_edge <- data_patch_contest %>%
  filter(!is.na(id1), !is.na(id2),
         id1 != id2, id1_omu == id2_omu) %>%
  left_join(data_focal_patch %>% distinct(patch_id, patch_type), by = "patch_id") %>%
  count(omu = factor(id1_omu, levels = omu_order_en),
        actor = id1, rec = id2, patch_type) %>%
  inner_join(data_node %>% select(omu, actor = id, r1 = rank),
             by = c("omu", "actor")) %>%
  inner_join(data_node %>% select(omu, rec = id, r2 = rank),
             by = c("omu", "rec")) %>%
  filter(r1 != r2)
 
conflict_agg <- individual_attributes %>%
  reframe(expand_grid(actor = id, recipient = id,
                      patch_type = c("Provisioned Patch", "Natural Patch")),
          .by = omu) %>%
  left_join(data_edge %>%
              select(omu, actor, recipient = rec,
                     patch_type, total_conflicts = n),
            by = c("omu", "actor", "recipient", "patch_type")) %>%
  mutate(total_conflicts = replace_na(total_conflicts, 0L)) %>%
  left_join(individual_attributes %>%
              select(omu, actor = id, actor_rank = feeding_rank),
            by = c("omu", "actor")) %>%
  left_join(individual_attributes %>%
              select(omu, recipient = id, recipient_rank = feeding_rank),
            by = c("omu", "recipient")) %>%
  arrange(omu, actor_rank, recipient_rank, patch_type)
 
conflict_summary <- data_patch_contest %>%
  filter(!is.na(id1), !is.na(id2),
         id1 != id2, id1_omu == id2_omu) %>%
  count(omu = id1_omu, age_group = as.character(id1_age),
        name = "initiated_conflicts") %>%
  complete(omu, age_group = c("Adult_male", "Adult_female", "Immature"),
           fill = list(initiated_conflicts = 0L)) %>%
  group_by(omu) %>%
  mutate(total_conflicts = sum(initiated_conflicts),
         percent = if_else(total_conflicts > 0,
                           100 * initiated_conflicts / total_conflicts, NA_real_)) %>%
  ungroup()

reverse_color <- "#8C8C8C"

label_pt <- 6
legend_pt <- 10
title_pt <- 12
base_pt <- 12

omu_names <- unique(as.character(individual_attributes$omu))

draw_age_inset <- function(unit_stats) {
  
  groups <- c("Adult_male", "Adult_female", "Immature")
  p <- unit_stats$percent[match(groups, unit_stats$age_group)]
  
  x <- c(0.93, 1.06, 1.19)
  y0 <- 0.84
  y100 <- 1.12
  ytop <- y0 + (y100 - y0) * p / 100
  
  old_xpd <- par(xpd = NA)
  on.exit(par(old_xpd))
  
  ticks <- seq(0, 100, by = 20)
  tick_y <- y0 + (y100 - y0) * ticks / 100
  
  segments(0.87, y0, 0.87, y100, col = "grey65", lwd = 0.4)
  
  segments(0.86, tick_y, 0.87, tick_y, col = "grey65", lwd = 0.4)
  
  text(0.85, tick_y, labels = ticks, adj = c(1, 0.5),
       cex = 5 / base_pt, col = "grey40")
  
  segments(0.87, y0, 1.24, y0, col = "grey65", lwd = 0.4)
  
  rect(x - 0.035, y0, x + 0.035, ytop,
       col = unname(age_colors[groups]), border = NA)
  
  text(x, ytop + 0.012, labels = sprintf("%.1f", p),
       adj = c(0.5, 0), cex = 5 / base_pt, col = "black")
  
  text(x, y0 - 0.025, labels = c("AM", "AF", "IM"),
       adj = c(0.5, 1), cex = 5.5 / base_pt)
  
  text(1.06, 1.23,
       labels = "Conflicts initiated (%)", cex = 6 / base_pt)
}
 
draw_segmented_ribbon <- function(from, p1, to, p2, col,
                                  n_dash = 8, duty = 0.65) {
  
  draw_link <- circlize::circos.link
  local_env <- new.env(parent = environment(draw_link))
  
  local_env$polygon <- function(x, ..., col, border = NA) {
    
    f <- parent.frame()
    
    if (!exists("d1x", envir = f, inherits = FALSE)) {
      stop("当前弦带几何形状不支持分段绘制。")
    }
    
    side_a <- f$d1x
    side_b <- f$d2x
    
    sample_side <- function(z, t) {
      
      distance <- c(0, cumsum(sqrt(rowSums((z[-1, , drop = FALSE] - z[-nrow(z), , 
                                                                      drop = FALSE])^2))))
      
      position <- distance / distance[length(distance)]
      
      cbind(approx(position, z[, 1], xout = t, ties = "ordered")$y,
            approx(position, z[, 2], xout = t, ties = "ordered")$y)
    }
    
    for (j in seq_len(n_dash)) {
      
      start <- (j - 1) / n_dash
      end <- if (j == n_dash) 1 else (j - 1 + duty) / n_dash
      
      t <- seq(start, end, length.out = 25)
      
      a <- sample_side(side_a, t)
      b <- sample_side(side_b, t)
      
      ribbon_piece <- rbind(a, if (j == n_dash) f$r2,
                            b[nrow(b):1, , drop = FALSE], if (j == 1) f$r1)
      
      graphics::polygon(ribbon_piece, col = col, border = NA)
    }
  }
  
  environment(draw_link) <- local_env
  
  draw_link(sector.index1 = from, point1 = p1, sector.index2 = to,
            point2 = p2, col = col, border = NA, directional = 1,
            arr.type = "big.arrow", arr.length = 0.03)
}

draw_conflict_network <- function() {
  
  n_col <- 3
  n_row <- ceiling(length(omu_names) / n_col)
  
  panels <- seq_len(n_row * n_col)
  panels[panels > length(omu_names)] <- NA_integer_
  
  layout(rbind(rep(1, n_col),
               matrix(ifelse(is.na(panels), 0, panels + 1), ncol = n_col, byrow = TRUE)),
         heights = c(12, rep(388 / n_row, n_row)))
  
  par(ps = base_pt, cex = 1, family = "sans",
      bg = "white", fg = "black", col = "black", mar = rep(0, 4))
  
  plot.new()
  
  legend_groups <- list(
    list(title = "Sex", legend = c("Female", "Male"),
         pch = c(21, 22), pt.bg = c("grey70", "grey70"),
         col = c("grey20", "grey20")),
    list(title = "Age group",
         legend = c("Adult male", "Adult female", "Immature"),
         col = unname(age_colors[c("Adult_male", "Adult_female", "Immature")]),
         lty = 1, lwd = 2),
    list(title = "Patch Type", legend = c("Natural Patch", "Provisioned Patch"),
         col = c("grey50", "grey50"), lty = c("11", "solid"), lwd = 2))
  
  legend_common <- list(y = 0.95, horiz = TRUE, bty = "n",
                        cex = legend_pt / base_pt, pt.cex = 1,
                        seg.len = 2, x.intersp = 0.6, text.col = "black")
  
  legend_widths <- vapply(legend_groups, function(g) {
    do.call(legend, c(list(x = 0, plot = FALSE), legend_common, g))$rect$w
  }, numeric(1))
  
  legend_gap <- 0.025
  legend_x <- 0.5 -
    (sum(legend_widths) +
       (length(legend_groups) - 1) * legend_gap) / 2
  
  for (j in seq_along(legend_groups)) {
    
    do.call(legend, c(list(x = legend_x), legend_common, legend_groups[[j]]))
    
    legend_x <- legend_x + legend_widths[j] + legend_gap
  }
  
  for (o in omu_names) {
    
    par(mar = c(0, 0, 0.9, 0))
    
    nodes <- individual_attributes %>%
      filter(as.character(omu) == o) %>%
      arrange(feeding_rank)
    
    edges <- conflict_agg %>%
      filter(as.character(omu) == o) %>%
      transmute(actor = as.character(actor),
                recipient = as.character(recipient),
                patch_type, total_conflicts, 
                later_to_earlier = actor_rank > recipient_rank)
    
    unit_stats <- conflict_summary %>%
      filter(as.character(omu) == o)
    
    ids <- as.character(nodes$id)
    
    sector_colors <- setNames(unname(age_colors[as.character(nodes$age_group)]),
                              ids)
    
    link_fill <- adjustcolor(ifelse(edges$later_to_earlier, reverse_color,
                                    unname(sector_colors[edges$actor])),
                             alpha.f = 0.45)
    
    sector_size <- setNames(vapply(ids, function(id) {
      max(sum(edges$total_conflicts[edges$actor == id]) +
            sum(edges$total_conflicts[edges$recipient == id]), 1)
    }, numeric(1)), ids)
    
    circos.clear()
    
    circos.par(start.degree = 90, clock.wise = TRUE,
               gap.after = rep(3, length(ids)),
               cell.padding = c(0, 0, 0, 0),
               track.margin = c(0.003, 0.003),
               canvas.xlim = c(-1, 1.25),
               canvas.ylim = c(-1, 1.25))
     
    link_pos <- chordDiagram(edges %>% select(actor, recipient, total_conflicts),
                             order = ids,
                             grid.col = sector_colors, grid.border = NA,
                             col = link_fill, transparency = 0,
                             directional = 1,
                             direction.type = "arrows",
                             link.arr.type = "big.arrow",
                             link.arr.length = 0.03,
                             link.arr.col = link_fill,
                             link.target.prop = FALSE,
                             link.border = NA,
                             link.lty = 1,
                             link.largest.ontop = TRUE,
                             link.visible = edges$patch_type == "Provisioned Patch",
                             annotationTrack = "grid", annotationTrackHeight = 0.04,
                             preAllocateTracks = list(list(track.height = 0.14),
                                                      list(track.height = 0.05)),
                             xmax = sector_size, reduce = 0)
     
    natural_rows <- which(edges$patch_type == "Natural Patch" & edges$total_conflicts > 0)
    
    for (k in natural_rows) {
      
      draw_segmented_ribbon(from = as.character(link_pos$rn[k]),
                            p1 = c(link_pos$x1[k] - abs(link_pos$value1[k]), link_pos$x1[k]),
                            to = as.character(link_pos$cn[k]),
                            p2 = c(link_pos$x2[k] - abs(link_pos$value2[k]), link_pos$x2[k]),
                            col = link_fill[k], n_dash = 8, duty = 0.65)
    }
    
    circos.trackPlotRegion(track.index = 1, bg.border = NA, panel.fun = function(x, y) {
        
        i <- match(CELL_META$sector.index, ids)
        
        circos.text(CELL_META$xcenter, 0,
                    paste0(nodes$feeding_rank[i], "_", id_label(ids[i])),
                    facing = "clockwise", niceFacing = TRUE, 
                    adj = c(0, 0.5), col = "black", cex = label_pt / base_pt)
      }
    )
    
    circos.trackPlotRegion(track.index = 2, bg.border = NA, panel.fun = function(x, y) {
        
        i <- match(CELL_META$sector.index, ids)
        
        circos.points(CELL_META$xcenter, 0.5,
                      pch = unname(sex_shapes[as.character(nodes$sex[i])]),
                      bg = unname(sector_colors[ids[i]]),
                      col = "grey20", cex = 0.95)
      }
    )
    
    title(main = sprintf("%s (n = %d)", o, sum(edges$total_conflicts)),
          col.main = "black", font.main = 2, cex.main = title_pt / base_pt, line = 0.2)
    
    draw_age_inset(unit_stats)
    circos.clear()
  }
}

circos.clear()
draw_conflict_network()

cairo_pdf("Fig5_conflict_chord.pdf", width = 300 / 25.4, height = 400 / 25.4,
          pointsize = base_pt, family = "sans", bg = "white")

draw_conflict_network()
dev.off()

#较早进入的个体在斑块内的社交网络中是否具有更高的中心性
conflict_order %>% 
  group_by(omu = as.character(omu), id = id1) %>%
  summarise(initiated_n = n(), target_n = n_distinct(id2),
            early_percent = 100 * sum(direction == "early_to_late") / 
              na_if(sum(direction %in% c("early_to_late", "late_to_early")), 0), 
            .groups = "drop") %>% 
  group_by(omu) %>%
  mutate(conflict_percent = 100 * initiated_n / sum(initiated_n)) %>%
  right_join(data_node %>% mutate(omu = as.character(omu)), by = c("omu", "id")) %>%
  replace_na(list(initiated_n = 0, target_n = 0, conflict_percent = 0)) %>%
  arrange(omu, desc(initiated_n), rank) %>%
  transmute(OMU = omu, 个体ID = coalesce(unname(id_labels[id]), id),
            取食顺序排名 = rank, 冲突发起数 = initiated_n, 
            冲突占比 = round(conflict_percent, 1),
            冲突对象数 = target_n,
            `早→晚比例` = round(early_percent, 1))  
 




