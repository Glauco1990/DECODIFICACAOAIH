#' Decodifica o arquivo SISAAIH
#'
#' @return Três arquivos: Principal, Opm e Registro Civil
#' @export
Decodificaraih <- function() {

arquivo <- list.files("../../3 - DADOS/ARQUIVOS SISAIH/", pattern = ".txt")
filename <- stringr::str_c("../../3 - DADOS/ARQUIVOS SISAIH/", arquivo)

conn <- file(filename , open = "r") # Cria arquivo de conexão aberto
linn <- readLines(conn) # lê as linhas do arquivo de conexão
close(conn) # fecha o arquivo de conexão

HAIH <- c("NU_LOTE","QT_LOTE","APRES_LOTE","SEQ_LOTE","ORG_EMIS_AIH","CNES_HOSP","MUN_HOSP","NU_AIH",
          "IDENT_AIH","ESPEC_AIH","MOD_INTERN","SEQ_AIH5","AIH_PROX","AIH_ANT","DT_EMISSAO","DT_INTERN",
          "DT_SAIDA","PROC_SOLICITADO","ST_MUDAPROC","PROC_REALIZADO","CAR_INTERN","MOT_SAIDA",
          "IDENT_MED_SOL","DOC_MED_SOL","IDENT_MED_RESP","DOC_MED_RESP","IDENT_DIRCLINICO","DOC_DIRCLINICO",
          "IDENT_AUTORIZ","DOC_AUTORIZ","DIAG_PRIN","NM_PACIENTE","DT_NASC_PAC","SEXO_PAC","RACA_COR",
          "NM_MAE_PAC","NM_RESP_PAC","TP_DOC_PAC","ETNIA_INDIGENA","COD_SOL_LIB","NU_CNS","NAC_PAC",
          "TP_LOGRADOURO","LOGR_PAC","NU_END_PAC","COMPL_END_PAC","BAIRRO_PAC","COD_MUN_END_PAC","UF_PAC",
          "CEP_PAC","NU_PRONTUARIO","NU_ENFERMARIA","NU_LEITO","ID_PROC","IN_PROF","IDENT_PROF","CBO_PROF",
          "IN_EQUIPE","IN_SERVICO","IDENT_SERVICO","IN_EXECUTOR","IDENT_EXECUTOR","COD_PROCED","QTD_PROCED",
          "CMPT","SERVICO","CLASSIFICACAO","SAIDA_UTINEO","PESO_UTINEO","MESGEST_UTINEO","CNPJ_EMPREG",
          "CBOR","CNAER","TP_VINCPREV","QT_VIVOS","QT_MORTOS","QT_ALTA","QT_TRANSF","QT_OBITO","QT_FILHOS",
          "GRAU_INSTRU","CID_INDICACAO","TP_CONTRACEP1","TP_CONTRACEP2","ST_GESTRISCO","RESERVADO",
          "NU_PRENATAL","NU_DOC_PAC","PACIENTE_TEL_DDD","PACIENTE_TEL_NUM","JUSTIFICATIVA_CNS","DIAG_SEC_1",
          "DIAG_SEC_1_CLASS","DIAG_SEC_2","DIAG_SEC_2_CLASS","DIAG_SEC_3","DIAG_SEC_3_CLASS","DIAG_SEC_4",
          "DIAG_SEC_4_CLASS","DIAG_SEC_5","DIAG_SEC_5_CLASS","DIAG_SEC_6","DIAG_SEC_6_CLASS","DIAG_SEC_7",
          "DIAG_SEC_7_CLASS","DIAG_SEC_8","DIAG_SEC_8_CLASS","DIAG_SEC_9","DIAG_SEC_9_CLASS")

CAIH <- c("A8","A3","A6","A3","A10","A7","A6","A13","2A2","45X","A2","A3","2A13","3A8","A10","I1","A10",
          "2A2","I1","A15","I1","A15","I1","A15","I1","A15","A4","4X","4X","4X","3X","A70","A8","A1","A2",
          "2A70","I1","A4","A5","2X","A15","2A3","A50","A7","A15","A30","A6","A2","A8","A15","2A4","I0",
          "I1","A15","A6","2I1","A14","I1","A15","A10","I3","A6","2A3","632X","19X","I1","I4","I1","A14",
          "A6","A3","6I1","10X","I2","I1","A4","A2","A2","I1","A35","A12","A32","A2","A9","A50","A4","A1"
          ,"A4","A1","A4","A1","A4","A1","A4","A1","A4","A1","A4","A1","A4","A1","A4","A1","165X")

HRC <- c("NU_LOTE","QT_LOTE","APRES_LOTE","SEQ_LOTE","ORG_EMIS_AIH","CNES_HOSP","MUN_HOSP","NU_AIH",
         "IDENT_AIH","ESPEC_AIH","NUMERO_DN","NOME_RN","RS_CART","LIVRO_RN","FOLHA_RN","TERMO_RN",
         "DT_EMIS_RN","LINHA","MATRICULA")

CRC <- c("A8","A3","A6","A3","A10","A7","A6","A13","2A2","45X","A11","A70","A20","A8","A4","2A8","A3",
         "A32","1148X","383X")

HOPM <- c("NU_LOTE","QT_LOTE","APRES_LOTE","SEQ_LOTE","ORG_EMIS_AIH","CNES_HOSP","MUN_HOSP","NU_AIH",
          "IDENT_AIH","ESPEC_AIH","COD_OPM","LINHA","REG_ANVISA","SERIE","LOTE","NOTA_FISCAL",
          "CNPJ_FORN","CNPJ_FABRIC")

COPM <- c("A8","A3","A6","A3","A10","A7","A6","A13","2A2","45X","A10","A3","4A20","2A14","1089X","485X")

key <- c(0,0,0)
for (ii in 1 : length(linn))
{

  # Seleciona a linha
  ll <- linn[ii]   # recebe cada linha

  # Identifica qual é o layout da linha
  ID01 <- substring(linn[ii], 57, 58) == "01"
  ID03 <- substring(linn[ii], 57, 58) == "03"
  ID04 <- substring(linn[ii], 57, 58) == "04"
  ID05 <- substring(linn[ii], 57, 58) == "05"
  ID07 <- substring(linn[ii], 57, 58) == "07"

  # Cria um arquivo temporário para a leitura dos dados
  TmpFile01 <- tempfile()
  cat(file = TmpFile01, ll, sep = "\n")

  # AIH PRINCIPAL / AIH DE CONTINUAÇÃO / AIH DE LONGA PERMANENCIA #
  if (ID01||ID03||ID05)
  {

    # Adiciona o primeiro procedimento da linha e demais na tabela
    tAIH <- read.fortran(TmpFile01, col.names = HAIH, format = CAIH)
    if (key[1] == 0)
    {
      key[1] <- 1
      AIH <- tAIH
    }
    else
    {
      AIH <- rbind(AIH,tAIH)
    }

    # Identifica o procedimento principal
    if (ID01)
    {
      AIH$ID_PROC[dim(AIH)[1]] <- 1
      bAIH <- AIH[dim(AIH)[1],]
    }
    else
    {
      AIH$ID_PROC[dim(AIH)[1]] <- 0
      AIH[dim(AIH)[1], 11 : 53] <- bAIH[11:53]
      AIH[dim(AIH)[1], 68 : 109] <- bAIH[68:109]
    }

    # Adiciona os procedimentos secundários
    for (jj in seq(743, 1296, 79))
    {
      psec <- substring(linn[ii], jj, jj + 79 - 1)
      nelm <- stringr::str_count(psec, "0")
      if (nelm == 79)
      {
        break
      }
      else
      {
        taih <- bAIH
        TmpFile02 <- tempfile()
        cat(file = TmpFile02, psec, sep = "\n")
        PSEC <- read.fortran(TmpFile02, format = c("I1", "A15", "A6", "2I1", "A14", "I1", "A15", "A10",
                                                   "I3", "A6", "2A3"))
        unlink(TmpFile02)
        taih$IDENT_AIH <- tAIH$IDENT_AIH
        taih$ID_PROC <- 0
        taih[55 : 67] <- PSEC[1,]
        AIH <- rbind(AIH, taih)
      }

    }

  }

  # REGISTRO CIVIL
  else if (ID04)
  {

    # Adiciona o primeiro registro civil da linha e demais na tabela
    tRC <- read.fortran(TmpFile01,col.names = HRC,format = CRC)
    if (key[2] == 0)
    {
      key[2] <- 1
      RC <- tRC
    }
    else
    {
      RC <- rbind(RC,tRC)
    }
    bRC <- RC[dim(RC)[1],]

    # Adiciona os registros civis secundários
    for (jj in seq(270, 1254, 164))
    {
      psec <- substring(linn[ii], jj, jj + 164 - 1)
      nelm <- stringr::str_count(psec, "0")
      if (nelm == 164)
      {
        break
      }
      else
      {
        trc <- bRC
        TmpFile02 <- tempfile()
        cat(file = TmpFile02,psec,sep = "\n")
        PSEC <- read.fortran(TmpFile02,format = c("A11", "A70", "A20", "A8", "A4", "2A8", "A3", "A32"))
        unlink(TmpFile02)
        trc[11:19] <- PSEC[1,]
        RC <- rbind(RC,trc)
      }
    }
  }
  # OPM #
  else if (ID07)
  {

    # Adiciona o primeiro OPM da linha e demais na tabela
    tOPM <- read.fortran(TmpFile01, col.names = HOPM, format = COPM)
    if (key[3] == 0)
    {
      key[3] <- 1
      OPM <- tOPM
    }
    else
    {
      OPM <- rbind(OPM,tOPM)
    }
    bOPM <- OPM[dim(OPM)[1],]

    # Adiciona os OPM's secundários
    for (jj in seq(227, 1195, 121))
    {
      psec <- substring(linn[ii], jj, jj + 121 - 1)
      nelm <- stringr::str_count(psec, "0")
      if (nelm == 121)
      {
        break
      }
      else
      {
        topm <- bOPM
        TmpFile02 <- tempfile()
        cat(file = TmpFile02, psec, sep = "\n")
        PSEC <- read.fortran(TmpFile02,format = c("A10", "A3", "4A20", "2A14"))
        unlink(TmpFile02)
        topm[11 : 18] <- PSEC[1,]
        OPM <- rbind(OPM,topm)
      }
    }
  }
  unlink(TmpFile01)
}

# Escrevendo os arquivos
if (key[1] == 1)
{
  writexl::write_xlsx(AIH, path = stringr::str_c("../../3 - DADOS/DECODIFICAÇÔES AIH/1 - AIHP/", stringr::str_sub(arquivo, end = 23), "_AIHP.xlsx"))
}

if (key[2] == 1)
{
  writexl::write_xlsx(RC, path = stringr::str_c("../../3 - DADOS/DECODIFICAÇÔES AIH/2 - AIHOPM/", stringr::str_sub(arquivo, end = 23), "_AIHOPM.xlsx"))
}

if (key[3] == 1)
{
  writexl::write_xlsx(OPM, path = stringr::str_c("../../3 - DADOS/DECODIFICAÇÔES AIH/3 - AIHRC/", stringr::str_sub(arquivo, end = 23), "_AIHRC.xlsx"))
}
}
