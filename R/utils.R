#' @title Extract files form 7zip (7z) archive
#'
#' @name un7z
#'
#' @description Function for extract files form 7zip (7z) archive
#'
#' @param zipfile The pathname of the 7z file.
#'
#' @param dir.output The directory for extracting the files.
#'
#' @details 7-Zip is free software with open source. The most of the code is under the GNU LGPL
#' license. Some parts of the code are under the BSD 3-clause License. Also there is unRAR
#' license restriction for some parts of the code. Read 7-Zip License information.
#'
#' The main features of 7-Zip:
#'
#' High compression ratio in 7z format with LZMA and LZMA2 compression;
#' Supported formats: Packing / unpacking: 7z, XZ, BZIP2, GZIP, TAR, ZIP and WIM and Unpacking only:
#' AR, ARJ, CAB, CHM, CPIO, CramFS, DMG, EXT, FAT, GPT, HFS, IHEX, ISO, LZH, LZMA, MBR, MSI, NSIS,
#' NTFS, QCOW2, RAR, RPM, SquashFS, UDF, UEFI, VDI, VHD, VMDK, WIM, XAR and Z;
#' For ZIP and GZIP formats, 7-Zip provides a compression ratio that is 2-10 % better than the ratio provided by
#' PKZip and WinZip;
#' Strong AES-256 encryption in 7z and ZIP formats;
#' Self-extracting capability for 7z format;
#' Integration with Windows Shell;
#' Powerful File Manager;
#' Powerful command line version;
#' Plugin for FAR Manager;
#' Localizations for 87 languages. 7-Zip works in Windows 10 / 8 / 7 / Vista / XP / 2016 / 2012 / 2008 / 2003 / 2000 / NT.
#'
#' @references https://www.7-zip.org/
#'
#' @author Rumenick Pereira da Silva
#'
#' @seealso To use the function it is necessary to install the 7zip software, download in
#' https://www.7-zip.org/download.html
#'
#' @keywords 7zip
#'
#' @export
un7z <- function(zipfile, dir.output = ".") {
  if (!is.character(zipfile) || length(zipfile) != 1L) {
    stop("'un7z' must be a single character string")
  }
  if (.Platform$OS.type != "windows") {
    dir.output <- path.expand(dir.output)
    zipfile    <- path.expand(zipfile)
  } else {
    dir.output <- gsub(pattern = "/", replacement = "\\", x = path.expand(dir.output), fixed = TRUE)
    zipfile    <- gsub(pattern = "/", replacement = "\\", x = path.expand(zipfile), fixed = TRUE)
  }

  check_error <- tryCatch(system(sprintf('7z e -o%s %s', dir.output, zipfile)),
                          error = function(e) { TRUE },
                          finally = FALSE)

  if (check_error) {
    stop("Install 7zip (download in https://www.7-zip.org/download.html)")
  }
  return(check_error)
}

# un7z(zipfile = "./data/12_2019_CAGED.7z", dir.output = "./data/")


# CAGEDEST_122019_1 <- read_CAGED()
#
# clear.CAGED <- function(x) {
#   x %>%
#     select(`Admitidos/Desligados`,
#            `Competência Declarada`,
#            `CBO 2002 Ocupação`,
#            `CNAE 2.0 Classe`,
#            `Faixa Empr Início Jan`,
#            `Grau Instrução`,
#            `Qtd Hora Contrat`,
#            `Idade`,
#            `Raça Cor`,
#            `Salário Mensal`,
#            `Saldo Mov`,
#            `Sexo`,
#            `Tempo Emprego`,
#            `Tipo Mov Desagregado`,
#            `UF`,
#            `Ind Trab Parcial`,
#            `Ind Trab Intermitente`)
# }
#
# aux <- clear.CAGED(CAGEDEST_122019_1)
# names(aux) <- names(CAGEDEST_122019)
#
# identical(aux, CAGEDEST_122019)
#
# table(aux$Admitidos.Desligado == CAGEDEST_122019$Admitidos.Desligados)
#
# table(aux$Competência.Declarada == CAGEDEST_122019$Competência.Declarada)
# table(aux$CBO.2002.Ocupação == CAGEDEST_122019$CBO.2002.Ocupação)
# table(aux$CNAE.2.0.Classe == CAGEDEST_122019$CNAE.2.0.Classe)
# table(aux$Faixa.Empr.Início.Jan == CAGEDEST_122019$Faixa.Empr.Início.Jan)
# table(aux$Grau.Instrução == CAGEDEST_122019$Grau.Instrução)
# table(aux$Qtd.Hora.Contrat == CAGEDEST_122019$Qtd.Hora.Contrat)
# table(aux$Idade == CAGEDEST_122019$Idade)
# table(aux$Raça.Cor == CAGEDEST_122019$Raça.Cor)
# table(aux$Salário.Mensal == CAGEDEST_122019$Salário.Mensal)
# table(aux$Saldo.Mov == CAGEDEST_122019$Saldo.Mov, useNA = "ifany")
# table(aux$Sexo == CAGEDEST_122019$Sexo)
# table(aux$Tempo.Emprego == CAGEDEST_122019$Tempo.Emprego)
# table(aux$UF == CAGEDEST_122019$UF)
# table(aux$Ind.Trab.Parcial == CAGEDEST_122019$Ind.Trab.Parcial)
# table(aux$Ind.Trab.Intermitente == CAGEDEST_122019$Ind.Trab.Intermitente)
# identical(aux, CAGEDEST_122019, single.NA = FALSE)

# system("7z e -oC:\\Users\\rumen\\AppData\\Local\\Temp\\RtmpqOOGLl C:\\Users\\rumen\\AppData\\Local\\Temp\\RtmpqOOGLl\\CAGEDEST_122019.7z")
# td <- tempdir()
#
# file.copy(from = "./CAGEDEST_122019.7z", to = td)
