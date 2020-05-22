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
#' @param path7zip The program files directory of 7zip software.
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
un7z <- function(zipfile, dir.output = ".", path7zip = NULL) {
  
  if(system("7z", show.output.on.console = FALSE)) {
    stop("Install 7zip (download in https://www.7-zip.org/download.html)")
  }

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

  check_error <- tryCatch(system(sprintf('%s e -o%s %s', ifelse(is.null(path7zip), "7z", sQuote(path7zip)), dir.output, zipfile), show.output.on.console = FALSE),
                          error = function(e) { TRUE },
                          finally = FALSE)
  if(check_error) {
    stop("\n Erro ao descomprimir arquivo 7zip! \n")
  }
  
  invisible(NULL)
}
