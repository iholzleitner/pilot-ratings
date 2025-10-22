model_raw <- read_csv("data/manyfaces-pilot-models.csv",
                      show_col_types = FALSE)

# ethnicity ----

list(
  data.frame(
    ethnicity = c(
      "white",
      "branco",
      "branca",
      "caucasian",
      "white (italian)",
      "white british",
      "british",
      "white-french",
      "british white",
      "white european",
      "white-scottish",
      "white scottish",
      "white britisg",
      "german",
      "german,",
      "german, swedish",
      "german/argentinien",
      "dutch",
      "french",
      "scottish",
      "european",
      "eastern european",
      "croatian",
      "serbian",
      "romanian",
      "white - other",
      "meditrainian"
    ),
    ethnicity_rec = "White"
  ),
  data.frame(
    ethnicity = c(
      "black",
      "preta",
      "negra",
      "african american",
      "african (mauritian)"
    ),
    ethnicity_rec =  "Black"
  ),
  data.frame(
    ethnicity = c(
      "asian",
      "asian (indian)",
      "south asian",
      "southasian",
      "east indian",
      "chinese",
      "chinese - east asian",
      "asian/chinese",
      "malaysian chinese",
      "malaysia chinese",
      "malaysian",
      "indian",
      "pakistani",
      "hindu",
      "british indian"
    ),
    ethnicity_rec = "Asian"
  ),
  data.frame(
    ethnicity = c("indigenous"),
    ethnicity_rec = "Indigenous"
  ),
  data.frame(
    ethnicity = c(
      "middle eastern",
      "arab",
      "east african middle eastern",
      "israeli",
      "israeli.",
      "israel",
      "muslim",
      "turkish",
      "tã¼rkiye",
      "türkiye"
    ),
    ethnicity_rec = "Middle Eastern or North African (MENA)"
  ),
  data.frame(
    ethnicity = c(
      "latino",
      "latina",
      "latina branca",
      "mexicana",
      "mexicano",
      "puerto rican/hispanic",
      "hispanic",
      "south american"
    ),
    ethnicity_rec = "Latine"
  ),
  data.frame(
    ethnicity = c(
      "mixed",
      "pardo",
      "parda",
      "pardo - mixed race",
      "mixraces",
      "chinese-indian-malay mixed",
      "mixed white & asian",
      "mix white/black",
      "white / middle-eastern",
      "white / middle eastern",
      "middle eastern/white"
    ),
    ethnicity_rec = "Mixed"
  ),
  data.frame(
    ethnicity = c("ninguno", "jewish", "not hispanic/latino"),
    ethnicity_rec = "Ambiguous label"
  )
) |> do.call(dplyr::bind_rows, args = _) -> eth_recode

# check all are covered
all_eth <- model_raw |>
  mutate(ethnicity = tolower(ethnicity)) |>
  count(ethnicity) |>
  inner_join(eth_recode, by = "ethnicity") 

readr::write_csv(eth_recode, "data/eth_recode.csv")


# gender ----

list(
  data.frame(
    gender = c(
      "female",
      "mulher",
      "feminino",
      "female.",
      "f",
      "mujer",
      "femenino",
      "trans woman",
      "femal"
    ),
    gender_rec = "female"
  ),
  data.frame(
    gender = c("homem", "masculino", "mascãºlino", "m"),
    gender_rec = "male"
  ),
  data.frame(
    gender = c(
      "sexo feminino, gãªnero fluido",
      "nã£o binã¡rio",
      "genderfluid (amab)",
      "nonbinary"
    ),
    gender_rec = "non-binary"
  ),
  # don't double-categorise
  # data.frame(
  #   gender = c("sexo feminino, gãªnero fluido", "genderfluid (amab)"),
  #   gender_rec = "gender-fluid"
  # ),
  data.frame(gender = c("hetero cis", "NA"), gender_rec = NA_character_)
)  |> do.call(dplyr::bind_rows, args = _) -> gender_recode

# check all are covered
all_gen <- model_raw |>
  mutate(gender = tolower(gender)) |>
  count(gender) |>
  inner_join(gender_recode, by = "gender") 

readr::write_csv(gender_recode, "data/gender_recode.csv")

# quest-eth

list(data.frame(ethnicity = c("white", "whitr", "blanco", "weiß",
  "white/caucasian", "white caucasian", "white/caucasion",
  "white / caucasian",  "caucasian/white", "caucasian (white)",
  "whithe/caucasian", "caucasic", "caucasian", "caicasoan",
  "caucasian canadian", "caucasian/greek",  
  "white british caucasian",
  "white british", "british", "white/british", "white - british",
  "british/white", "welsh", "english", "irish", "scottish", 
  "white european", "white/european", "white, southern european",
  "white, eastern european", "european american", "eastern european",
  "white slavic", "slavic", "white/polish", "polish", "polish/white",
  "white, german", "white/greek", "greek/white", "greek", "white/italian",
  "white/swedish", "white/estonian", "white other - romanian",
  "white/spanish", "spanish", "spanish/white", "spanish-white"),
  ethnicity_rec = "White"),
data.frame(ethnicity = c("black", "black african", "black/african",
  "black/africa", "african/black", "africa/black",
  "black/ african", "african black", "black-african", 
  "african", "africa", "black south african",
  "african american", "afican american",
  "black: african american", "black american",
  "black or african american", "african american/black", 
  "black african american", "african/black american",
  "black british",
  "zulu", "kenyan", "nigeria",  
  "colored", "coloured", "coloured south african"),
  ethnicity_rec = "Black"),
data.frame(ethnicity = c("asian", "asia", "south asian", "east asian",
  "asian/indian", "indian", "bangladeshi", "pakistani",
  "sri lankan", "chinese", "japanese",
  "filipino", "philippine", "vietnamese", "sundanese",
  "indo-aryan", "indo-fijian", "british - indian",
  "british asian", "british pakistani"),  
  ethnicity_rec = "Asian"),
data.frame(ethnicity = c("native american", "native hawaiian", "samoan"),
           ethnicity_rec = "Indigenous / Pacific Islander"),
data.frame(ethnicity = c("arab", "middle eastern", "north african"),
           ethnicity_rec = "MENA"),
data.frame(ethnicity = c("latino", "latina", "latinx", "latine", "latin", 
  "hispanic", "hispanic/latino", "latinamerican", 
  "preta", "brazilian"),
  ethnicity_rec = "Latine"),
data.frame(ethnicity = c("mixed", "mixed race", "mix", "biracial",
  "mixed black caribbean and white", "cape coloured",
  "black/white", "white / puerto rican",
  "white/ latino", "white & asian", 
  "coloured / mixed race"),
  ethnicity_rec = "Mixed"),
data.frame(ethnicity = c("mauritian", "belgium, asian", "black/british",
  "eurasian", "caribbean", "carribean", 
  "british asian american","chinese/uk",
  "latin european", "white hispanic", "race",
  "american", "christian", "christianity"),
  ethnicity_rec = "Ambiguous label")
)  |> do.call(dplyr::bind_rows, args = _) -> quest_eth_recode

readr::write_csv(quest_eth_recode, "data/quest_eth_recode.csv")
