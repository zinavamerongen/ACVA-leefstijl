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