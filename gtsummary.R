# package          -----------------------------------------------------------------
library(tidyverse)
library(gtsummary)

# data             --------------------------------------------------------------------
data_mtcars <-  mtcars %>% janitor::clean_names()
data_iris <- iris %>% janitor::clean_names()


# summary          -----------------------------------------------------------------
data_mtcars %>% 
  select(mpg, wt, hp, cyl) %>% 
  gtsummary::tbl_summary(by = cyl) %>%
  add_p()

data_iris %>% 
  tbl_summary(by = species) %>% 
  add_p()


# custom summary   ----------------------------------------------------------
data_mtcars %>% 
  select(mpg, wt, hp, cyl) %>% #select
  tbl_summary( by = cyl, #group
               type = mpg ~ "continuous2", #stat type
               statistic = list( #stat
                 mpg ~ c("{mean} ({sd})", "{min}, {max}")),
               label = wt ~ "Peso em libras", #change label
               digits = starts_with("wt") ~ 1 ) #define digits

# add on functions --------------------------------------------------------
data_mtcars %>% 
  select(mpg, wt, hp, vs, cyl) %>% #select
  tbl_summary( by = vs,
               statistic = list(mpg ~ "{mean} ({sd})",
                                cyl  ~ "{p}%")) %>% 
  #add_p() %>% 
  #add_q(method = "bonferroni") %>% 
  add_difference() %>%
  add_n() %>% 
  modify_caption("**Tabela1. Comparação consumo combustivel motores em V**") %>% 
  #modify_spanning_header(c("stat_1","stat_2") ~ "**0=S e 1=V**") %>% 
  modify_header(label = "variable", 
                stat_1 = "**S Engine** ({n})",
                stat_2 = "**V Engine** ({n})") %>% 
  bold_labels() 


# using add        ---------------------------------------------------------------
data_mtcars %>% 
  select(mpg, wt, hp, vs, cyl) %>% #select
  tbl_summary( by = vs) %>% 
  add_overall() %>% 
  add_n() %>% 
  add_stat_label(label = all_categorical() ~ "No (%)") %>% 
  modify_header(label = "Variable",
                stat_1 = "**S Engine** ({n})",
                stat_2 = "**V Engine** ({n})")
               
#add stat
data_mtcars %>% 
  select(mpg, wt, hp, vs, cyl) %>% #select
  tbl_summary( by = vs) %>% 
  add_stat(lm(mpg~wt, data = mtcars))
  
            

# bold labels      -------------------------------------------------------------
data_mtcars %>% 
  select(mpg, wt, hp, vs, cyl) %>% #select
  tbl_summary( by = vs) %>% 
  add_p() %>% 
  bold_labels() %>% 
  italicize_levels() %>% 
  bold_p(t = 0.8)


# modify           ------------------------------------------------------------------
data_mtcars %>% 
  select(mpg, wt, hp, vs, cyl) %>% #select
  tbl_summary( by = vs) %>% 
  modify_header(label = "Variable", 
                stat_1 = "**S Egine**",
                stat_2 = "**V Engine**") %>% 
  modify_spanning_header(all_stat_cols() ~ "**Cars**")


# crosstab         ----------------------------------------------------------------
data_mtcars %>% 
  select(mpg, wt, hp, vs, cyl) %>% #select
  mutate(vs = ifelse(vs == 0, "S Engine", "V Engine")) %>% 
  tbl_cross(
    row = vs ,
    col = cyl,
    label = list(vs ~ "type of veicle", cyl ~ "cilynder"),
    percent = "row",
    margin = "row"
  ) %>% 
  modify_header(label = "type") %>% 
  add_p(source_note = TRUE)
  


# survey summary   ----------------------------------------------------------
survey::svydesign(
  ids = ~ 1,
  data = as.data.frame(Titanic),
  weights = ~Freq ) %>% 
  tbl_svysummary( by = Survived) %>% 
  add_p() %>% 
  modify_spanning_header(all_stat_cols()~ "**Survived**") %>% 
  bold_p(t = 0.5)
  

# regress summary  ------------------------------------------------------
m1 <- glm(vs ~ wt + mpg + cyl, 
          data = mtcars,
          family = binomial(link = "logit"))

summary(m1) #necessary exponentiate

mdl_1 <- 
  m1 %>% 
  tbl_regression(exponentiate = TRUE) %>% 
  add_global_p() %>% 
  add_glance_table()
  
#mdl suported:lm, glm, aov, clm, clmm, survival:coxph, survreg, nnet::multinom


mdl_mtcars <- glm(am ~ mpg + factor(cyl), 
                  data = mtcars, 
                  family = binomial(link = "logit"))

tbl_regression(mdl_mtcars,
               exponentiate = TRUE, 
               label = list(`factor(cyl)` = "No of cylinders",
                            mpg = "miles per gallon"))

# inline text      -------------------------------------------------------------

`r inline_text(mdl_1, variable = cyl )`



# tbl merge        ---------------------------------------------------------------
library(survival)

#univariate mdl
tbl_uv_surv <- 
  trial %>% 
  select(age, grade, death, ttdeath) %>% 
  tbl_uvregression(
    method = coxph,
    y = Surv(ttdeath, death),
    exponentiate = TRUE) %>% 
  add_global_p()

#multi var mdl
tbl_mv_surv <- 
  coxph(Surv(ttdeath, death) ~ age + grade, 
        data = trial) %>% 
  tbl_regression(exponentiate = TRUE) %>% 
  add_global_p()

#merge
tbl_surv_merge <- 
  tbl_merge(
    list(tbl_uv_surv, tbl_mv_surv),
    tab_spanner = c("**Univarible**","**Multivariable**"))

tbl_surv_merge


# tbl stack        ---------------------------------------------------------------
t3 <- 
  coxph(Surv(ttdeath, death)~ trt,
        data = trial) %>% 
  tbl_regression(
    show_single_row  = trt,
    label = trt ~"Drug B vs A",
    exponentiate = TRUE
  )

t4 <- 
  coxph(Surv(ttdeath, death)~ trt + grade + stage + marker,
        data = trial) %>% 
  tbl_regression(
    show_single_row = trt, 
    label  = trt  ~ "Drug B vs A",
    exponentiate = TRUE,
    include = "trt"
  )


tbl_surv_stack <- 
  tbl_stack(
    list(t3, t4),
    group_header = c("Unadjusted", "Ajusted")  )

# tbl split        ---------------------------------------------------------------
tt <- 
  trial %>% 
  tbl_summary(by = trt) %>% 
  add_p()

tbl_split(tt, variables = c(marker, grade))



# tbl strata       --------------------------------------------------------------
tbl_strat <- 
  trial %>% 
  mutate(grade = paste("Grade", grade)) %>% 
  tbl_strata(
    strata = grade, 
    ~tbl_summary(.x, by = trt, missing = "no") %>% 
      modify_header(all_stat_cols()~ "**{level}**")
  )

tbl_strat

#mtcars
tbl_strata_mtcars <- 
  mtcars %>% 
  select(vs, cyl, mpg, wt) %>% 
  mutate(cyl = paste(cyl, "Cylinder"),
         vs  = factor(vs, labels = c("V Engine", "S Engine"))) %>% 
  tbl_strata(
    strata = cyl, 
    ~.x %>% 
      tbl_summary(by = vs,
                  type = where(is.numeric) ~ "continuous") %>% 
      modify_header(all_stat_cols() ~ "**{level}**")
  )

tbl_strata_mtcars

# themes           ------------------------------------------------------------------
mdl_lreg_mtcars <- 
  glm(vs ~ mpg + wt + cyl,
      data = mtcars,
      family = binomial(link = "logit"))

tbl_regression(mdl_lreg_mtcars, exponentiate = TRUE) %>% 
  modify_caption("Default theme")

mdl_lreg_mtcars <- 
  glm(vs ~ mpg + wt + cyl,
      data = mtcars,
      family = binomial(link = "logit"))

#normal
tbl_regression(mdl_lreg_mtcars, exponentiate = TRUE) %>% 
  modify_caption("Default theme")

#compact
reset_gtsummary_theme()
theme_gtsummary_compact()

tbl_regression(mdl_lreg_mtcars, exponentiate = TRUE) %>% 
  modify_caption("Default theme")

#jama
reset_gtsummary_theme()
theme_gtsummary_journal(journal = "qjecon")

tbl_regression(mdl_lreg_mtcars, exponentiate = TRUE) %>% 
  modify_caption("Default theme")

# events num       --------------------------------------------------------------
glm(response ~ trt, 
    data = trial, 
    family = binomial(link= "logit")) %>% 
  tbl_regression() %>% 
  add_n() %>% 
  add_nevent()


# chg var values   ----------------------------------------------------------
trial %>% 
  select(grade) %>% 
  mutate(grade = factor(grade, labels = c("A","B","C"))) %>% 
  tbl_summary()


# save as img      -------------------------------------------------------------
library(kableExtra)

m_linear <- lm(mpg ~ cyl, data = mtcars)

tbl_regression(m_linear) %>% 
  as_kable() %>% 
  kableExtra::as_image(file = "teste.png")

# 3 way contg tbl  -------------------------------------------------------
trial %>%
  select(age, grade, stage, trt) %>%
  mutate(grade = paste("Grade", grade)) %>%
  tbl_strata(
    strata = grade,
    .tbl_fun =
      ~ .x %>%
      tbl_cross(stage, trt, margin = NULL) 
  )


# 2 mdl in 1 tbl   ----------------------------------------------------------
reset_gtsummary_theme()

c("cyl", "cyl + wt") %>% 
  map(
    ~ paste("mpg", .x, sep  = " ~ ") %>% 
      as.formula() %>% 
      lm(data = mtcars) %>% 
      tbl_regression()) %>% 
  tbl_merge(
    tab_spanner = c("**Univariate**","**Multivariate**")
  )
        
