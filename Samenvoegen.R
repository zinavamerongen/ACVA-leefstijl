leefbaar_2024 <- leefbaarheid %>%
  filter(jaar == 2024,
         str_detect(bu_code, "^BU0363|^BU0599")) %>%
  select(bu_code, bu_naam, lbm)
leefbaar_2024 <- leefbaarheid %>%
  filter(jaar == 2024,
         str_detect(bu_code, "^BU0363|^BU0599")) %>%
  select(bu_code, bu_naam, lbm, afw, fys, onv, soc, vrz, won)
sum(drank_2024$bu_code %in% leefbaar_2024$bu_code)
View(anti_join(drank_2024, leefbaar_2024, by = "bu_code"))
buurten_2024 <- leefbaar_2024 %>%
  inner_join(drank_2024, by = "bu_code")
samengevoegd <- leefbaar_2024 %>%
  inner_join(drank_2024, by = "bu_code")
write_csv(samengevoegd, "data/samengevoegd_2024.csv")
samengevoegd <- samengevoegd %>%
  mutate(gemeente = if_else(str_detect(bu_code, "^BU0363"),
                            "Amsterdam", "Rotterdam"))
# 1. Tabel verdubbelen: één keer per gemeente, één keer als "Totaal"
met_totaal <- bind_rows(samengevoegd,
                        samengevoegd %>% mutate(gemeente = "Totaal"))

# 2. Per groep de getallen uitrekenen
cijfers <- met_totaal %>%
  group_by(gemeente) %>%
  summarise(aantal       = n(),
            leef_gem     = mean(lbm, na.rm = TRUE),
            leef_sd      = sd(lbm, na.rm = TRUE),
            soc_gem      = mean(soc, na.rm = TRUE),
            soc_sd       = sd(soc, na.rm = TRUE),
            drank_gem    = mean(drankscore, na.rm = TRUE),
            drank_sd     = sd(drankscore, na.rm = TRUE),
            zonder_drank = sum(is.na(drankscore)))

# 3. Netjes opschrijven als "gemiddelde (SD)"
tabel1 <- cijfers %>%
  transmute(gemeente,
            `Aantal buurten`                    = as.character(aantal),
            `Leefbaarheid, gem. (SD)`           = sprintf("%.2f (%.2f)", leef_gem, leef_sd),
            `Sociale samenhang, gem. (SD)`      = sprintf("%.2f (%.2f)", soc_gem, soc_sd),
            `Zwaar drinken %, gem. (SD)`        = sprintf("%.1f (%.1f)", drank_gem, drank_sd),
            `Buurten zonder drankcijfer, n (%)` = sprintf("%d (%.1f%%)", zonder_drank,
                                                          zonder_drank / aantal * 100)) %>%
  # 4. Omdraaien: gemeenten als kolommen
  pivot_longer(-gemeente, names_to = "Kenmerk") %>%
  pivot_wider(names_from = gemeente, values_from = value)

View(tabel1)
write_csv(tabel1, "data/tabel1_beschrijvend.csv")met_totaal <- bind_rows(samengevoegd,
                        samengevoegd %>% mutate(gemeente = "Totaal"))

cijfers <- met_totaal %>%
  group_by(gemeente) %>%
  summarise(aantal       = n(),
            leef_gem     = mean(lbm, na.rm = TRUE),
            leef_sd      = sd(lbm, na.rm = TRUE),
            soc_gem      = mean(soc, na.rm = TRUE),
            soc_sd       = sd(soc, na.rm = TRUE),
            drank_gem    = mean(drankscore, na.rm = TRUE),
            drank_sd     = sd(drankscore, na.rm = TRUE),
            zonder_drank = sum(is.na(drankscore)))

tabel1 <- cijfers %>%
  transmute(gemeente,
            `Aantal buurten`                    = as.character(aantal),
            `Leefbaarheid, gem. (SD)`           = sprintf("%.2f (%.2f)", leef_gem, leef_sd),
            `Sociale samenhang, gem. (SD)`      = sprintf("%.2f (%.2f)", soc_gem, soc_sd),
            `Zwaar drinken %, gem. (SD)`        = sprintf("%.1f (%.1f)", drank_gem, drank_sd),
            `Buurten zonder drankcijfer, n (%)` = sprintf("%d (%.1f%%)", zonder_drank,
                                                          zonder_drank / aantal * 100)) %>%
  pivot_longer(-gemeente, names_to = "Kenmerk") %>%
  pivot_wider(names_from = gemeente, values_from = value)

View(tabel1)
write_csv(tabel1, "data/tabel1_beschrijvend.csv")
install.packages("flextable")
library(flextable)

flextable(tabel1) %>%
  autofit() %>%
  save_as_docx(path = "tabel1.docx")
write_csv(tabel1, "data/tabel1_beschrijvend.csv")
write_csv(tabel1, "data/tabel1_beschrijvend.csv")
