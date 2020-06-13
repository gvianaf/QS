
library(tidyverse)
library(readxl)
library(janitor)
library(ggbump)

options(OutDec = ",")

# --- lê o arquivo
# dados compilados manualmente
arquivo <- "qs_results.xlsx"

dados_raw <- arquivo %>% 
  excel_sheets() %>% 
  set_names() %>% 
  map_df(read_excel, path = arquivo, .id = "SheetName") %>% 
  clean_names() %>% 
  rename(ano = sheet_name, universidade = universidades)

# seleciona e arruma
dados <- dados_raw %>% 
  select(ano, universidade, world_rank) %>% 
  # limpa variável de nome da universidade
  mutate(universidade = str_squish(str_remove_all(universidade, "Logo.+")),
         # unesp tá sem nome
         universidade = ifelse(universidade == "UNESP",
                               "Universidade Estadual Paulista UNESP",
                               universidade))

# --- verifica cada variável
dados %>% count(ano)
dados %>% count(universidade)
dados %>% count(world_rank)

# --- será que precisa mudar pra numérico o ranking?
# a ordem que está já dá o rank
dados %>% 
  filter(ano == 2021) %>%
  arrange(world_rank) %>%
  mutate(national_rank = row_number())

# --- cria variáveis de rank nacional e federal
dados_clean <- dados %>% 
  group_by(ano) %>% 
  arrange(world_rank) %>% 
  mutate(national_rank = row_number()) %>% 
  left_join(dados %>% 
              mutate(federal = ifelse(str_detect(universidade, "Federal") | universidade == "Universidade de Brasília", 1, 0)) %>% 
              filter(federal == 1) %>% 
              group_by(ano) %>% 
              mutate(federal_rank = row_number()) %>% 
              select(ano, universidade, federal_rank))

# --- insere variável de sigla
# exportei para preencher na mão
rio::export(dados_clean %>% ungroup() %>% select(universidade) %>% distinct(), "siglas.xlsx")

dados_clean <- dados_clean %>% 
  left_join(rio::import("siglas_preenchido.xlsx")) %>% 
  mutate(ano = as.double(ano)) %>%    # dá erro no geom_bump se não for numérico
  ungroup()

# salva arquivo
rio::export(dados_clean, "qs_br.xlsx")

# --- gráficos de acompanhamento https://github.com/davidsjoberg/ggbump
# arruma a base para o gráfico
# identifica as universidades que apareceram em todos os anos
n5 <- dados_clean %>% 
  count(sigla, sort = TRUE) %>% 
  filter(n == 5) %>% 
  select(sigla)

dados_clean %>% 
  filter(!is.na(federal_rank)) %>% 
  ggplot(aes(x = ano, y = federal_rank, color = sigla)) +
  geom_bump() +
  theme(legend.position = "none") +
  scale_y_continuous(trans = "reverse")

# --- gráfico profissional - FEDERAIS
# paleta de cores
cores <- c("UFRJ"    = "#A49E9E",
           "UFRGS"   = "#A49E9E",
           "UFMG"    = "#A49E9E",
           "UNIFESP" = "#A49E9E",
           "UFSC"    = "#A49E9E",
           "UFPR"    = "#A49E9E",
           "UFPE"    = "#A49E9E",
           "UFSCar"  = "#A49E9E",
           "UFF"     = "#A49E9E",
           "UFSM"    = "#A49E9E",
           "UFBA"    = "#A49E9E",
           "UFV"     = "#A49E9E",
           "UFC"     = "#A49E9E",
           "UnB"     = "#0F4C81")

qs_federal <- dados_clean %>% 
  filter(!is.na(federal_rank)) %>% 
  select(ano, federal_rank, sigla)

# usa fontes alternativas
library(showtext)
font_add("charter", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Regular.otf")
font_add("charter-bold", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/Charter Bold.otf")
font_add("fira", "C:/Users/GUILHERME/AppData/Local/Microsoft/Windows/Fonts/FiraSans-Regular.ttf")
showtext_auto()

theme_set(theme_classic(base_family = "charter"))
theme_update(legend.position = "none",
             axis.line.y = element_blank(),
             axis.line.x = element_blank())

graf <- qs_federal %>% 
  ggplot(aes(x = ano, y = federal_rank, color = sigla)) +
  geom_point(size = 4) +
  geom_bump(size = 2, smooth = 8) +
  geom_text(data = qs_federal %>% filter(ano == min(ano)), family = "fira",
            aes(x = ano - .1, label = sigla), size = 5, hjust = 1) +
  geom_text(data = qs_federal %>% filter(ano == max(ano)), family = "fira",
            aes(x = ano + .1, label = sigla), size = 5, hjust = 0) +
  geom_text(data = qs_federal %>% filter(ano == max(ano)), family = "fira",
            aes(x = ano + .8, label = glue::glue("{federal_rank}ª"), size = 5, hjust = 0)) +
  scale_y_reverse(breaks = c(seq(1, 18))) +
  scale_x_continuous(limits = c(2016.5, 2021.8),
                     breaks = c(2017, 2018, 2019, 2020, 2021)) +
  scale_color_manual(values = cores) +
  labs(title = "Evolução das Universidades Federais no Ranking Mundial QS",
       subtitle = "A UnB destaca-se entre as melhores IES Federais, voltando à 5ª posição\nDiversas IFES que antes conseguiam se classificar, não mais conseguem",
       x = "Ano de divulgação do ranking",
       y = "") +
  # cowplot::theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(plot.title = element_text(family = "charter-bold"),
        axis.title.x = element_text(hjust = .78),
        axis.text.y = element_text(size = 10))

# inserir cinco arrows
arrow <- tribble(
  ~x,    ~xend,   ~y, ~yend,
  2020.7, 2019.1, 12.5,     8,
  2020.7, 2019.1, 12.5,     9,
  2020.7, 2019.1, 12.5,    11,
  2020.7, 2020.1, 12.5,     8,
  2020.7, 2020.1, 12.5,    11
)

graf <- graf +
  annotate(geom = "curve", curvature = .2, arrow = arrow(length = unit(2, "mm")),
           x = arrow$x, xend = arrow$xend, y = arrow$y, yend = arrow$yend) +
  annotate(geom = "text", vjust = 1, family = "fira",
           label = "Não estão mais\npresentes no QS World",
           x = 2020.6, y = 12.5)

graf
ggsave("qs-federais.pdf", width = 8, height = 6)
pdftools::pdf_convert("qs-federais.pdf", format = "png", dpi = 350)

# --- gráfico profissional - NACIONAL
# paleta de cores
cores <- c("UFRJ"    = "#A49E9E",
           "UFRGS"   = "#A49E9E",
           "UFMG"    = "#A49E9E",
           "UNIFESP" = "#A49E9E",
           "UFSC"    = "#A49E9E",
           "UFPR"    = "#A49E9E",
           "UFPE"    = "#A49E9E",
           "UFSCar"  = "#A49E9E",
           "UFF"     = "#A49E9E",
           "UFSM"    = "#A49E9E",
           "UFBA"    = "#A49E9E",
           "UFV"     = "#A49E9E",
           "UFC"     = "#A49E9E",
           "USP"     = "#A49E9E",
           "Unicamp" = "#A49E9E",
           "UNESP"   = "#A49E9E",
           "PUCSP"   = "#A49E9E",
           "PUCRJ"   = "#A49E9E",
           "PUCRS"   = "#A49E9E",
           "UERJ"    = "#A49E9E",
           "UEL"     = "#A49E9E",
           "UnB"     = "#0F4C81")

qs_br <- dados_clean %>% 
  select(ano, national_rank, sigla)

graf <- qs_br %>% 
  ggplot(aes(x = ano, y = national_rank, color = sigla)) +
  geom_point(size = 4) +
  geom_bump(size = 2, smooth = 8) +
  geom_text(data = qs_br %>% filter(ano == min(ano)), family = "fira",
            aes(x = ano - .1, label = sigla), size = 5, hjust = 1) +
  geom_text(data = qs_br %>% filter(ano == max(ano)), family = "fira",
            aes(x = ano + .1, label = sigla), size = 5, hjust = 0) +
  geom_text(data = qs_br %>% filter(ano == max(ano)), family = "fira",
            aes(x = ano + .9, label = glue::glue("{national_rank}ª")), size = 5, hjust = 0) +
  scale_y_reverse(breaks = c(seq(1, 22))) +
  scale_x_continuous(limits = c(2016.4, 2022),
                     breaks = c(2017, 2018, 2019, 2020, 2021)) +
  scale_color_manual(values = cores) +
  labs(title = "Evolução das Universidades Brasileiras no Ranking Mundial QS",
       subtitle = "Após dois anos seguidos de queda, UnB avança duas posições, voltando ao 10º lugar\nDiversas IFES que antes conseguiam se classificar, não mais conseguem",
       x = "Ano de divulgação do ranking",
       y = "") +
  # cowplot::theme_minimal_grid(font_size = 14, line_size = 0) +
  theme(plot.title = element_text(family = "charter-bold"),
        axis.title.x = element_text(hjust = .74),
        axis.text.y = element_text(size = 10))

# inserir cinco arrows
arrow <- tribble(
  ~x,    ~xend,   ~y, ~yend,
  2020.7, 2020.1, 20.5,    13,
  2020.7, 2020.1, 20.5,    14,
  2020.7, 2020.1, 20.5,    11,
  2020.7, 2020.1, 20.5,    16,
  2020.7, 2020.1, 20.5,    19,
  2020.7, 2019.1, 20.5,    16,
  2020.7, 2019.1, 20.5,    17,
  2020.7, 2019.1, 20.5,    19
)

graf <- graf +
  annotate(geom = "curve", curvature = .3, arrow = arrow(length = unit(2, "mm")),
           x = arrow$x, xend = arrow$xend, y = arrow$y, yend = arrow$yend) +
  annotate(geom = "text", vjust = 1,
           label = "Não estão mais\npresentes no QS World",
           x = 2020.6, y = 20.5)

graf
ggsave("qs-br.pdf", width = 8, height = 7)
pdftools::pdf_convert("qs-br.pdf", format = "png", dpi = 350)
showtext_auto(FALSE)


