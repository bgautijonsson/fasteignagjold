library(tidyverse)
library(readxl)

col_names <- c(
  "svfn",
  "sveitarfelag",
  "utsvar",
  "fastskattur_a",
  "fastskattur_b",
  "fastskattur_c",
  "fraveitugjald",
  "vatnsgjald",
  "lodarleiga_ibudir",
  "lodarleiga_fyrirtaeki",
  "fjoldi_gjalda"
)

d <- read_excel(
  "data-raw/2025/fasteignagjold.xlsx", 
  skip = 8, 
  col_names = col_names, 
  col_types = rep("text", length(col_names))
) |> 
  select(-svfn, -fjoldi_gjalda, -fastskattur_b, -fastskattur_c, -lodarleiga_fyrirtaeki) |> 
  mutate(
    frav_mult = if_else(
      str_detect(fraveitugjald, "m2"),
      "m2",
      "fm"
    ),
    vatn_mult = if_else(
      str_detect(vatnsgjald, "m2"),
      "m2",
      "fm"
    ),
    lod_mult = if_else(
      str_detect(lodarleiga_ibudir, "m2"),
      "m2",
      "fm"
    )
  ) |> 
  mutate_at(
    vars(fraveitugjald, vatnsgjald, lodarleiga_ibudir),
    \(x) str_replace(x, " kr/m2", "") |> 
      str_replace("\\.", ",") |> 
      parse_number(locale = locale("is", decimal_mark = ",", grouping_mark = "."))
  ) |> 
  mutate_at(
    vars(utsvar, fastskattur_a),
    parse_number
  ) |> 
  mutate(
    fastskattur_a = fastskattur_a / 100,
    lodarleiga_ibudir = if_else(
      lod_mult == "fm",
      lodarleiga_ibudir / 100,
      lodarleiga_ibudir
    ),
    fraveitugjald = if_else(
      frav_mult == "fm",
      fraveitugjald / 100,
      fraveitugjald
    ),
    vatnsgjald = if_else(
      vatn_mult == "fm",
      vatnsgjald / 100,
      vatnsgjald
    )
  )

tribble(
  ~sveitarfelag, ~frav_intercept, ~frav_slope, ~vatn_intercept, ~vatn_slope,
  "Reykjavíkurborg", 12262, 473.29, 5084, 193.32,
  "Reykjanesbær", 
)
