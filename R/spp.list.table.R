#' Species list to data frame with sc. names and AphiaID
#' @param x Unique species codes from CQ (the output of ggleR::spp.in.CQ(path_to_bycatch_data))
#' @return a dataset
#' @export
spp.list.table <- function(x = unique.spp){

  dt <- data.table::data.table(
    spp.code = x,
    sc.name = NA_character_,
    aphiaID = NA_character_
  )

  dt[, sc.name := dplyr::recode_factor(spp.code,
                                            Ag = "Puffinus griseus", ## aka "Ardenna grisea",
                                            Ar = "Amblyraja radiata",
                                            Alcidae = "Alcidae",
                                            Anatidae = "Anatidae",
                                            At = "Alca torda",
                                            'At?' = "Alcidae",
                                            Ba = "Balaenoptera acutorostrata",
                                            Cg = "Cepphus grylle",
                                       Cl = "Cyclopterus lumpus",
                                            Do = "Dipturus oxyrinchus",
                                            Fg = "Fulmarus glacialis",
                                            Ga = "Gavia arctica",
                                            Gar = "Gulosus aristotelis",
                                            Gad = "Gaviidae", #"Gavia adamsii",
                                            Gavia = "Gaviidae",
                                            Gaviidae = "Gaviidae",
                                            Gg = 'Galeorhinus galeus',
                                            Gi = "Gavia immer",
                                            Hg = "Halichoerus grypus",
                                            'Hg?' = "Pinnipedia",
                                            La = "Lagenorhynchus albirostris",
                                            Lar = "Larus argentatus",
                                            Larus = "Laridae",
                                            Lm = "Larus marinus",
                                            Ln = "Lamna nasus",
                                       Ma = "Mustelus asterias",
                                       Mas = "Mustelus asterias",
                                            Mb = "Morus bassanus",
                                            Melanitta = "Melanitta",
                                            Mel = "Melanitta",
                                            Mf = "Melanitta fusca",
                                            'Mf?' = "Melanitta",
                                            Mn = "Melanitta nigra",
                                            Mu = "Mustelus sp.",
                                            Mustelus = "Mustelus sp.",
                                            Bird = "Aves",
                                            Mammal = "Mammalia",
                                            Ray = "Elasmobranchii",
                                            Shark = "Elasmobranchii",
                                            Pc = "Phalacrocorax carbo",
                                            Pcr = "Podiceps cristatus",
                                            Pg = "Podiceps grisegena",
                                            Podic = "Podiceps cristatus",
                                            Pp = "Phocoena phocoena",
                                            'Pp?' = "Mammalia",
                                            Pv = "Phoca vitulina",
                                            Rb = "Raja brachyura",
                                            Rc = "Raja clavata",
                                            Rm = "Raja montagui",
                                            Sa = "Squalus acanthias",
                                            Sc = "Scyliorhinus canicula",
                                            Se = "Pinnipedia",
                                            se = "Pinnipedia",
                                            seal = "Pinnipedia",
                                            Seal = "Pinnipedia",
                                            Sm = "Somateria mollissima",
                                            Ua = "Uria aalge",
                                            .default = NA_character_)]
  dt$sc.name <- as.character(dt$sc.name)

  get_id <- function(.x){

    rec <- try(worrms::wm_records_name(.x))
    if(length(rec$AphiaID) > 0){
      acc <- subset(rec, rec$status=="accepted")
    }
    acc$AphiaID[1]
  }

  for ( i in 1:length(dt$sc.name)){
    dt$aphiaID[i] <- get_id(dt$sc.name[i])
  }

  return(dt)}
