install.packages("writexl")
library(writexl)
library(tidyverse)
library(writexl)
library(tidyverse)
write_xlsx(drank_2024, path='tabel_drankgebruik_2024.xlsx')
leefbaarheid_drank_samen_per_buurt <- samengevoegd
rm(samengevoegd)
inkomen_per_buurt<-finance_gegevens_2024
rm(finance_gegevens_2024)
inkomen_2024<-inkomen_per_buurt%>%
  filter(str_detect(gwb_code,'^BU0363|^BU0599'))%>%
  select(bu_code=gwb_code,
         gem_inkomen=g_ink_pi,
         laag_inkomen=p_ink_li)

inkomen_2024<-inkomen_2024%>%
  select(-gem_inkomen)
rm(cijfers)
inkomen_2024<-inkomen_2024%>%
  semi_join(leefbaarheid_drank_samen_per_buurt, by = 'bu_code')
rm(buurten_2024)
rm(drankgebruik_wk)
inkomen_2024 <- inkomen_2024 %>%
  mutate(laag_inkomen = as.numeric(str_replace(laag_inkomen, ",", ".")))
samengevoegd_drank_leefbaarheid<-leefbaarheid_drank_samen_per_buurt
rm(leefbaarheid_drank_samen_per_buurt)
samengevoegd_drank_leefbaarheid<-samengevoegd_drank_leefbaarheid%>%
  left_join(inkomen_2024, by='bu_code')
samengevoegd_drank_leefbaarheid<-samengevoegd_drank_leefbaarheid%>%
  relocate(laag_inkomen, before = gemeente)
samengevoegd_drank_leefbaarheid <- samengevoegd_drank_leefbaarheid %>%
  rename(gemeente = before) %>%
  relocate(laag_inkomen, gemeente, .after = drankscore)
write_csv(samengevoegd_drank_leefbaarheid, "data/samengevoegd_drank_leefbaarheid.csv")
tabel_data <- samengevoegd_drank_leefbaarheid %>%
  mutate(alcohol = case_when(
    is.na(drankscore)                                ~ "Geen drankcijfer",
    drankscore > median(drankscore, na.rm = TRUE)    ~ "Veel zware drinkers",
    TRUE                                             ~ "Weinig zware drinkers"))
met_totaal <- bind_rows(tabel_data,
                        tabel_data %>% mutate(alcohol = "Totaal"))
tabel_data <- samengevoegd_drank_leefbaarheid %>%
  mutate(alcohol = case_when(
    is.na(drankscore)                                ~ "Geen drankcijfer",
    drankscore > median(drankscore, na.rm = TRUE)    ~ "Zwaar drinken",
    TRUE                                             ~ "Niet zwaar drinken"))
met_totaal <- bind_rows(tabel_data,
                        tabel_data %>% mutate(alcohol = "Totaal"))
rm(tabel1)
tabel_data <- samengevoegd_drank_leefbaarheid %>%
  filter(!is.na(drankscore)) %>%
  mutate(alcohol = if_else(drankscore > median(drankscore),
                           "Zwaar drinken", "Niet zwaar drinken"))
count(tabel_data, alcohol)
n_totaal <- nrow(tabel_data)
met_totaal <- bind_rows(tabel_data,
                        tabel_data %>% mutate(alcohol = "Totaal"))
cijfers <- met_totaal %>%
  group_by(alcohol) %>%
  summarise(
    `Laag inkomen %, gem. (SD)`     = sprintf("%.1f (%.1f)",
                                              mean(laag_inkomen, na.rm = TRUE),
                                              sd(laag_inkomen, na.rm = TRUE)),
    `Leefbaarheidsscore, gem. (SD)` = sprintf("%.2f (%.2f)", mean(lbm), sd(lbm)),
    `Sociale samenhang, gem. (SD)`  = sprintf("%.2f (%.2f)", mean(soc), sd(soc)),
    `Aantal buurten, n (%)`         = sprintf("%d (%.1f%%)", n(), n() / n_totaal * 100))
view(cijfers)
tabel1 <- cijfers %>%
  pivot_longer(-alcohol, names_to = "Kenmerk") %>%
  pivot_wider(names_from = alcohol, values_from = value) %>%
  select(Kenmerk, `Zwaar drinken`, `Niet zwaar drinken`, Totaal)

View(tabel1)
inwoners_2024 <- inkomen_per_buurt %>%
  filter(str_detect(gwb_code, "^BU0363|^BU0599")) %>%
  select(bu_code = gwb_code, inwoners = a_inw) %>%
  mutate(inwoners = as.numeric(inwoners))
personen_data <- samengevoegd_drank_leefbaarheid %>%
  filter(!is.na(drankscore)) %>%
  left_join(inwoners_2024, by = "bu_code")
personen_data <- personen_data %>%
  mutate(n_zwaar = inwoners * drankscore / 100,
         n_niet  = inwoners * (100 - drankscore) / 100)
tabel_personen <- with(personen_data, tibble(
  Kenmerk = c("Aantal mensen (geschat), n (%)",
              "Laag inkomen in de buurt, gem. %",
              "Leefbaarheidsscore van de buurt, gem.",
              "Sociale samenhang in de buurt, gem."),
  
  `Zware drinkers` = c(
    sprintf("%.0f (%.1f%%)", sum(n_zwaar), sum(n_zwaar) / sum(inwoners) * 100),
    sprintf("%.1f", weighted.mean(laag_inkomen, n_zwaar)),
    sprintf("%.2f", weighted.mean(lbm, n_zwaar)),
    sprintf("%.2f", weighted.mean(soc, n_zwaar))),
  
  `Niet-zware drinkers` = c(
    sprintf("%.0f (%.1f%%)", sum(n_niet), sum(n_niet) / sum(inwoners) * 100),
    sprintf("%.1f", weighted.mean(laag_inkomen, n_niet)),
    sprintf("%.2f", weighted.mean(lbm, n_niet)),
    sprintf("%.2f", weighted.mean(soc, n_niet))),
  
  Totaal = c(
    sprintf("%.0f (100%%)", sum(inwoners)),
    sprintf("%.1f", weighted.mean(laag_inkomen, inwoners)),
    sprintf("%.2f", weighted.mean(lbm, inwoners)),
    sprintf("%.2f", weighted.mean(soc, inwoners)))
))

View(tabel_personen)
ggplot(personen_data, aes(x = lbm, y = drankscore)) +
  geom_point() +
  labs(x = "Leefbaarheidsscore van de buurt",
       y = "Zware drinkers (%)",
       title = "Leefbaarheid en zwaar alcoholgebruik per buurt, 2024")
ggsave("grafiek_leefbaarheid_drank.png", width = 8, height = 5)
ggsave("grafiek_leefbaarheid_drank.png", width = 8, height = 5)
rm(cijfers)
rm(tabel_data)
rm(tabel1)
personen_data <- personen_data %>%
  mutate(n_zwaar = round(inwoners * drankscore / 100),
         n_niet  = inwoners - n_zwaar)
analyse_data <- samengevoegd_drank_leefbaarheid %>%
  filter(!is.na(drankscore)) %>%
  select(bu_code, drankscore, lbm, soc, laag_inkomen)
tabel_beschrijvend <- analyse_data %>%
  select(-bu_code) %>%
  pivot_longer(everything(), names_to = "Variabele", values_to = "waarde") %>%
  group_by(Variabele) %>%
  summarise(`Gem. (SD)` = sprintf("%.2f (%.2f)", mean(waarde), sd(waarde)),
            Minimum     = round(min(waarde), 2),
            Maximum     = round(max(waarde), 2))

View(tabel_beschrijvend)
tabel_beschrijvend <- tabel_beschrijvend %>%
  mutate(Rol = if_else(Variabele == "drankscore",
                       "Responsvariabele", "Verklarende variabele"),
         Variabele = recode(Variabele,
                            drankscore   = "Zwaar drinken (% volwassenen)",
                            lbm          = "Leefbaarheidsscore",
                            soc          = "Sociale samenhang",
                            laag_inkomen = "Laag inkomen (% inwoners)")) %>%
  arrange(Rol) %>%
  relocate(Rol)

View(tabel_beschrijvend)
tabel_beschrijvend <- tabel_beschrijvend %>%
  mutate(Rol = if_else(Variabele == "drankscore",
                       "Y (responsvariabele)", "X (verklarende variabele)"),
         Variabele = recode(Variabele,
                            drankscore   = "Zwaar drinken (% volwassenen)",
                            lbm          = "Leefbaarheidsscore",
                            soc          = "Sociale samenhang",
                            laag_inkomen = "Laag inkomen (% inwoners)")) %>%
  arrange(desc(Rol)) %>%
  relocate(Rol)

View(tabel_beschrijvend)
kruis_data <- analyse_data %>%
  mutate(Leefbaarheid        = c("Laag", "Midden", "Hoog")[ntile(lbm, 3)],
         `Sociale samenhang` = c("Laag", "Midden", "Hoog")[ntile(soc, 3)],
         `Laag inkomen`      = c("Laag", "Midden", "Hoog")[ntile(laag_inkomen, 3)])
rm(kruis_data)
rm(tabel_beschrijvend)
tabel_zwaar <- analyse_data %>%
  mutate(alcohol = if_else(drankscore > 9.3, "Zwaar drinken", "Niet zwaar drinken")) %>%
  group_by(alcohol) %>%
  summarise(`Leefbaarheidsscore, gem. (SD)` = sprintf("%.2f (%.2f)", mean(lbm), sd(lbm)),
            `Sociale samenhang, gem. (SD)`  = sprintf("%.2f (%.2f)", mean(soc), sd(soc)),
            `Laag inkomen %, gem. (SD)`     = sprintf("%.1f (%.1f)", mean(laag_inkomen), sd(laag_inkomen)),
            `Aantal buurten, n (%)`         = sprintf("%d (%.1f%%)", n(), n() / nrow(analyse_data) * 100)) %>%
  pivot_longer(-alcohol, names_to = "Verklarende variabele") %>%
  pivot_wider(names_from = alcohol, values_from = value) %>%
  select(`Verklarende variabele`, `Zwaar drinken`, `Niet zwaar drinken`)

View(tabel_zwaar)
library(flextable)

flextable(tabel_zwaar) %>%
  autofit() %>%
  save_as_docx(path = "tabel_zwaar_drinken.docx")
library(writexl)

write_xlsx(tabel_zwaar, "tabel_zwaar_drinken.xlsx")

analyse_data <- samengevoegd_drank_leefbaarheid %>%
  filter(!is.na(drankscore)) %>%
  select(bu_code, drankscore, lbm, soc, laag_inkomen)
ggplot(analyse_data, aes(x = lbm, y = drankscore)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(x = "Leefbaarheidsscore van de buurt",
       y = "Zware drinkers (%)",
       title = "Leefbaarheid en zwaar alcoholgebruik per buurt, 2024")
ggsave("grafiek_leefbaarheid_drank.png", width = 8, height = 5)

