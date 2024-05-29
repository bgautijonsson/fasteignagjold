library(tidyverse)
library(readxl)
library(metill)
library(here)
library(gt)
library(gtExtras)
theme_set(theme_metill())



read_excel(
  here("data-raw", "fasteignagjold.xlsx"),
  skip = 8,
  col_names = c(
    "svfn",
    "sveitarfelag",
    "utsvar",
    "fskattur_a",
    "fskattur_b",
    "fskattur_c",
    "fraveitugjald",
    "vatnsgjald",
    "sorphreinsun_tunnugjald",
    "sorphreinsun_eydingargjald",
    "lodarleiga_ibudir",
    "lodarleiga_fyrirtaeki",
    "fjoldi_gjalda"
  ),
  col_types = rep("text", 13)
) |>
  mutate(
    sveitarfelag = str_replace_all(sveitarfelag, "[0-9]+\\)", "") |>
      str_replace_all("1 \\)", "") |>
      str_squish()
  ) |>
  drop_na(svfn) |>
  select(sveitarfelag, fskattur_a, fraveitugjald, vatnsgjald) |>
  mutate_at(
    vars(fskattur_a, fraveitugjald, vatnsgjald),
    ~ ifelse(str_detect(., "kr") | is.na(.), "0", as.character(.)) |>
      str_replace(",", "\\.") |>
      parse_number()
  ) |> 
  mutate_if(is.numeric, \(x) x / 100) |> 
  mutate(fasteignagjold = (fskattur_a + fraveitugjald + vatnsgjald)) |> 
  arrange(fasteignagjold) |> 
  gt() |> 
  cols_label(
    sveitarfelag = "",
    fskattur_a = "Fasteignaskattur",
    fraveitugjald = "Fráveitugjöld",
    vatnsgjald = "Vatnsgjöld",
    fasteignagjold = "Fasteignagjöld"
  ) |> 
  cols_align(
    align = "center", 
    columns = -1
  ) |> 
  tab_spanner(
    columns = 2:4,
    label = "Undirliðir"
  ) |> 
  fmt_percent() |> 
  gt_color_rows(
    columns = 2:5,
    palette = "Greys"
  ) |> 
  tab_header(
    "Sveitarfélögum landsins raðað eftir heildarhlutfalli fasteignamats sem skilar sér í fasteignagjöld", 
    subtitle = md(
      str_c(
        '<div style="text-align: left"> ',
        "Samhliða fasteignaskatti innheimta sveitarfélög ýmis gjöld svo sem <em>fráveitu- og vatnsgjöld</em>, ",
        "<em>lóðaleigu</em> og <em>sorpgjald</em>. Saman kallast þessi gjöld gjarnan <em>fasteignagjöld</em>. ",
        "Sum þessara gjalda eru reiknuð hlutfallslega út frá fasteignamati fasteignar eða út frá stærð hennar í fermetrum, ",
        "og þetta getur verið mismunandi eftir sveitarfélagi",
        '</div>'
      )
    )
    
  ) |> 
  tab_source_note(
    "Gögn og kóði: "
  ) |> 
  gt_theme_538() |> 
  opt_table_font(font = "Lato", weight = 400) |> 
  opt_vertical_padding(scale = 0.6) |> 
  tab_options() |> 
  gtsave(
    filename = here("Figures", "table.png"),
    expand = 10,
    zoom = 7
  )




