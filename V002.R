# IMPORTACAO E BIBLIOTECAS ------------------------------------------------
#BIBLIOTECAS
library(dplyr)
library(readr)
library(kableExtra)
library(knitr)
library(stringr)
library(ggplot2)
library(tidyr)
#DATAFRAME
df1 <- read_csv("Obitos_gest_puerp_causas_externas_2011_2020.csv")
df2 <- read_csv("Obitos_nao_gest_puerp_causas_externas_2011_2020.csv")
df1$gestante <- TRUE
df2$gestante <- FALSE
df2$obito_em_idade_fertil <- NA
df2$periodo_do_obito <- NA
df <- rbind(df1,df2)
colnames(df1)
colnames(df2)
df$data_obito <- as.Date(df$data_obito, format = "%d/%m/%Y")
df$data_nasc <- as.Date(df$data_nasc, format = "%d/%m/%Y")
df$anos <- -as.numeric(format(df$data_nasc, "%Y")) + as.numeric(format(df$data_obito, "%Y"))

#CIDs
automobilisticos <- paste0("V", sprintf("%02d", seq(2, 95)))
Quedas <- paste0("W", sprintf("%02d", seq(1, 87)))
Eventos <- paste0("X", sprintf("%02d", seq(4, 37)))
Envenenamento <-  paste0("X", sprintf("%02d", seq(41, 59)))
suicidio <- paste0("X", sprintf("%02d", seq(60, 84)))
homicidio <- c(paste0("X", sprintf("%02d", seq(91, 99))),
               paste0("Y", sprintf("%02d", seq(0, 9))))

df <- df %>% mutate(
  CID_detalhado = case_when(
    str_detect(causabas_categoria,paste(automobilisticos, collapse = "|") )  ~ 'Acidentes automobilísticos',
    str_detect(causabas_categoria,paste(Quedas, collapse = "|") )  ~ 'Acidentes Quedas/afogamento/inalação/corrente elétrica',
    str_detect(causabas_categoria,paste(Eventos, collapse = "|") )  ~ 'Acidentes Eventos ambientais',
    str_detect(causabas_categoria,paste(Envenenamento, collapse = "|") )  ~ 'Acidentes Envenenamento acidental',
    str_detect(causabas_categoria,paste(suicidio, collapse = "|") )  ~ 'Suicídio',
    str_detect(causabas_categoria,paste(homicidio, collapse = "|") )  ~ 'Homicídio',
    TRUE ~ 'Outro'
  ),
  CID = substr(causabas_categoria, 1, 3)
)


# 1. Qual o panorama geral de mortalidade materna por causas externa --------
## FAIXA ETARIA E DADOS IMPLAUSIVEIS -----
### IDADE (considerando anos < 10 | anos > 55 como implausivel ----
df <- df %>% mutate(
  faixa_etaria = case_when(
  (anos < 10 ) ~ 'até 10 anos',
  (anos >= 10 & anos < 16) ~ 'De 10 a 15 anos',
  (anos >= 16 & anos < 20) ~ 'De 16 a 19 anos',
  (anos >= 20 & anos < 30) ~ 'De 20 a 29 anos',
  (anos >= 30 & anos < 40) ~ 'De 30 a 39 anos',
  (anos >= 40 & anos <= 50) ~ 'De 40 a 50 anos',
  (anos > 50) ~ 'Mais de 50 anos'
  )
)

##TABELAS DE Frequência ----
### CID----
g1 <- df  %>% mutate(
  CIDs = case_when(
    str_detect(CID_detalhado, 'Acidentes automobilísticos')  ~ 'x1',
    str_detect(CID_detalhado,'Acidentes Quedas/afogamento/inalação/corrente elétrica' )  ~ 'x4',
    str_detect(CID_detalhado,'Acidentes Eventos ambientais' )  ~ 'x3',
    str_detect(CID_detalhado, 'Acidentes Envenenamento acidental')  ~ 'x2',
    str_detect(CID_detalhado,  'Suicídio')  ~ 'x7',
    str_detect(CID_detalhado, 'Homicídio')  ~ 'x5',
    str_detect(CID_detalhado, 'Outro')  ~'x6'
  )) %>% 
  select(CIDs) %>% ggplot() +
  aes(x = CIDs) +
  geom_bar(fill = "#112446") +
  labs(
    x = "CID",
    y = "Frequência"
  ) +
  theme_minimal()
df %>% 
  select(CID_detalhado) %>% 
  table(useNA = 'always') %>% 
  questionr::freq(cum = FALSE,
                  total = TRUE,
                  na.last = FALSE,
                  valid = FALSE)
### ANO----
# g2 <- df %>% 
#   select(ano_obito) %>% ggplot() +
#   aes(x = ano_obito) +
#   geom_bar(fill = "#112446") +
#   labs(
#     x = "Ano de Obito",
#     y = "Frequência"
#   ) +
#   theme_minimal()+ scale_x_continuous(breaks = seq(2011, 2020, by = 1))+
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
df %>% 
  select(ano_obito) %>% 
  table(useNA = 'always') %>% 
  questionr::freq(cum = FALSE,
                  total = TRUE,
                  na.last = FALSE,
                  valid = FALSE)
### RACA----
g3 <- df %>% 
  select(raca_cor) %>% ggplot() +
  aes(x = raca_cor) +
  geom_bar(fill = "#112446") +
  labs(
    x = "Raça/Cor",
    y = "Frequência"
  ) +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
df %>% 
  select(raca_cor) %>% 
  table(useNA = 'always') %>% 
  questionr::freq(cum = FALSE,
                  total = TRUE,
                  na.last = FALSE,
                  valid = FALSE)
### ESCOLARIDADE----
g4 <- df %>% 
  select(escolaridade) %>% ggplot() +
  aes(x = escolaridade) +
  geom_bar(fill = "#112446") +
  labs(
    x = "Escolaridade",
    y = "Frequência"
  ) +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
df %>% 
  select(escolaridade) %>% 
  table(useNA = 'always') %>% 
  questionr::freq(cum = FALSE,
                  total = TRUE,
                  na.last = FALSE,
                  valid = FALSE)
### FAIXA ETARIA----
g5 <- df %>% 
  select(faixa_etaria) %>% ggplot() +
  aes(x = faixa_etaria) +
  geom_bar(fill = "#112446") +
  labs(
    x = "Faixa Etaria",
    y = "Frequência"
  ) +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
df %>% 
  select(faixa_etaria) %>% 
  table(useNA = 'always') %>% 
  questionr::freq(cum = FALSE,
                  total = TRUE,
                  na.last = FALSE,
                  valid = FALSE)
### ESTADO CIVIL----
g6 <- df %>% 
  select(est_civil) %>% ggplot() +
  aes(x = est_civil) +
  geom_bar(fill = "#112446") +
  labs(
    x = "Estado Civil",
    y = "Frequência"
  ) +
  theme_minimal()+ theme(axis.text.x = element_text(angle = 45, hjust = 1))
df %>% 
  select(est_civil) %>% 
  table(useNA = 'always') %>% 
  questionr::freq(cum = FALSE,
                  total = TRUE,
                  na.last = FALSE,
                  valid = FALSE)
### UF OCORRENCIA----
top_ufs <- df %>%
  count(ocor_sigla_uf) %>%
  arrange(desc(n)) %>%
  head(10)
df_top <- df %>%
  filter(ocor_sigla_uf %in% top_ufs$ocor_sigla_uf)
g7 <- df_top %>%
  ggplot() +
  aes(x = ocor_sigla_uf) +
  geom_bar(fill = "#112446") +
  labs(
    x = "UF de Ocorrência",
    y = "Frequência"
  ) +
  theme_minimal()
df %>% 
  select(ocor_sigla_uf) %>% 
  table() %>% 
  questionr::freq(cum = FALSE,
                  total = TRUE,
                  na.last = FALSE,
                  valid = FALSE)

## NUMERO DE CASOS POR ANO----

g2 <- df %>% 
  group_by(ano_obito) %>%
  summarise(count = n()) %>%
  ggplot() +
  aes(x = ano_obito, y = count) +
  geom_line(size=1)  +
  labs(x = "Ano", y = "Frequência") +
  theme(
    plot.title = element_text(size = 8L,
                              face = "bold"),
    axis.title.y = element_text(size = 7L,
                                face = "bold"),
    axis.title.x = element_text(size = 7L,face = "bold")
  ) + scale_x_continuous(breaks = seq(2011, 2020, by = 1)) +
  theme_minimal() +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### JUNTAR OS GRAFICOS
ggpubr::ggarrange(g1,g2,g3,g4,g5,g6,g7)

# 2. Existe alguma tendência nos últimos anos nas taxas de acident --------
df  %>%
  group_by(ano_obito, CID_detalhado) %>%
  summarise(count = n()) %>% 
  mutate(
  CIDs = case_when(
    str_detect(CID_detalhado, 'Acidentes automobilísticos')  ~ 'x1',
    str_detect(CID_detalhado,'Acidentes Quedas/afogamento/inalação/corrente elétrica' )  ~ 'x4',
    str_detect(CID_detalhado,'Acidentes Eventos ambientais' )  ~ 'x3',
    str_detect(CID_detalhado, 'Acidentes Envenenamento acidental')  ~ 'x2',
    str_detect(CID_detalhado,  'Suicídio')  ~ 'x7',
    str_detect(CID_detalhado, 'Homicídio')  ~ 'x5',
    str_detect(CID_detalhado, 'Outro')  ~'x6'
  ))  %>% 
  ggplot() +
  aes(x = ano_obito, y = count, colour = CIDs) +
  geom_line(size=1) +
  scale_color_manual(
    values = c(x1 = "#CE00FF",
               x2 = "#B17BA0",
               x3 = "#000000",
               x4 = "#00EADF",
               x5 = "#FA0000",
               x6 = "#78E200",
               x7 = "#F3DA00")
  ) +
  labs(x = "Ano", y = "Frequência", title = "Frequência de CID's por Ano") +
  theme(
    plot.title = element_text(size = 8L,
                              face = "bold"),
    axis.title.y = element_text(size = 7L,
                                face = "bold"),
    axis.title.x = element_text(size = 7L,face = "bold")
  ) + scale_x_continuous(breaks = seq(2011, 2020, by = 1))
dados <- df  %>%
  group_by(ano_obito, CID_detalhado) %>%
  summarise(count = n()) %>% 
  mutate(
    CIDs = case_when(
      str_detect(CID_detalhado, 'Acidentes automobilísticos')  ~ 'Acidentes',
      str_detect(CID_detalhado,'Acidentes Quedas/afogamento/inalação/corrente elétrica' )  ~ 'Acidentes',
      str_detect(CID_detalhado,'Acidentes Eventos ambientais' )  ~ 'Acidentes',
      str_detect(CID_detalhado, 'Acidentes Envenenamento acidental')  ~ 'Acidentes',
      str_detect(CID_detalhado,  'Suicídio')  ~ 'Suicídio',
      str_detect(CID_detalhado, 'Homicídio')  ~ 'Homicídio',
      str_detect(CID_detalhado, 'Outro')  ~'Outro'
    )) %>% select(-CID_detalhado)
# Calcula a soma de mortes por ano
total_por_ano <- dados %>%
  group_by(ano_obito) %>%
  summarize(total_mortes = sum(count))

# Calcula a porcentagem de mortes por CID em cada ano
porcentagem_mortes <- dados %>%
  group_by(ano_obito, CIDs) %>%
  summarize(count = sum(count)) %>%
  left_join(total_por_ano, by = "ano_obito") %>%
  mutate(porcentagem = count / total_mortes * 100) %>%
  select(-count, -total_mortes) %>%
  pivot_wider(names_from = CIDs, values_from = porcentagem) %>%
  mutate(across(everything(), ~ sprintf("%.2f%%", .)))

colnames(porcentagem_mortes)[1] <- 'Ano'

# 3. Algum dos períodos de gestação ou puerpério (até 42 dias ou t --------

## CRIACAO DO DATASET
df_suicidio <- df1 %>% mutate(
  Suicidio = case_when(
    str_detect(causabas_categoria,paste(suicidio, collapse = "|") )  ~ 'Sim',
    TRUE ~ 'Nao'
  )
)%>% select(Homicidio,periodo_do_obito)# %>% 
  # group_by(Suicidio,periodo_do_obito) %>%
  # summarise(count = n()) 
df_suicidio$periodo_do_obito[df_suicidio$periodo_do_obito  %>% is.na()] <- 'Nao Informado'
df_homicidio <-  df1 %>% mutate(
  Homicidio = case_when(
    str_detect(causabas_categoria,paste(homicidio, collapse = "|") )  ~ 'Sim',
    TRUE ~ 'Nao'
  ) 
)%>% select(Homicidio,periodo_do_obito) # %>% 
#   group_by(Homicidio,periodo_do_obito) %>%
#   summarise(count = n()) 
df_homicidio$periodo_do_obito[df_homicidio$periodo_do_obito  %>% is.na()] <- 'Nao Informado'

## TESTE QUI-QUADRADO E TESTE EXATO DE FISHER
library(gtsummary)
tbl <- df_homicidio %>%
  select(Homicidio, periodo_do_obito) %>%
  tbl_summary(by = periodo_do_obito, missing = "no") %>%
  add_p(
    test = list(
      Homicidio ~ "chisq.test",
      Homicidio ~ "fisher.test"
    ),
    test.args = list(
      Homicidio ~ list(simulate.p.value = TRUE),
      Homicidio ~ list(simulate.p.value = TRUE)
    )
  )
