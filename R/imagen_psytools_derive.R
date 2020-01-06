#!/usr/bin/env Rscript

# Copyright (c) 2017-2019 CEA
#
# This software is governed by the CeCILL license under French law and
# abiding by the rules of distribution of free software. You can use,
# modify and/ or redistribute the software under the terms of the CeCILL
# license as circulated by CEA, CNRS and INRIA at the following URL
# "http://www.cecill.info".
#
# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.
#
# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.
#
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.

library(tools)
library(Psytools)
library(stringr)  # str_locate


PSYTOOLS_BL_PSC1_DIR <- "/neurospin/imagen/BL/RAW/PSC1/psytools"
PSYTOOLS_BL_DERIVED_DIR <- "/tmp/imagen/BL/processed/psytools"
PSYTOOLS_FU1_PSC1_DIR <- "/neurospin/imagen/FU1/RAW/PSC1/psytools"
PSYTOOLS_FU1_DERIVED_DIR <- "/tmp/imagen/FU1/processed/psytools"
PSYTOOLS_FU2_PSC1_DIR <- "/neurospin/imagen/FU2/RAW/PSC1/psytools"
PSYTOOLS_FU2_DERIVED_DIR <- "/tmp/imagen/FU2/processed/psytools"
PSYTOOLS_FU3_PSC1_DIR <- "/neurospin/imagen/FU3/RAW/PSC1/psytools"
PSYTOOLS_FU3_DERIVED_DIR <- "/tmp/imagen/FU3/processed/psytools"
PSYTOOLS_SB_PSC1_DIR <- "/neurospin/imagen/STRATIFY/RAW/PSC1/psytools"
PSYTOOLS_SB_DERIVED_DIR <- "/tmp/imagen/STRATIFY/processed/psytools"

escape <- function(x) {
  if ("character" %in% class(x)) {
    # Escape double quotation marks by doubling them
    x <- gsub('"', '""', x)
    # Enclose in quotation marks strings with new lines, commas or quotation marks
    x <- gsub('^(.*[\n",\\;].*$)', '"\\1"', x)
  }
  return(x)
}


read_psytools_csv <- function(file) {
	COL_CLASSES <- c(
		User.code = "character",
		Block = "character",
		Trial = "character",
		Response.time..ms. = "numeric")
	d <- read.csv(file, colClasses = COL_CLASSES, stringsAsFactors = FALSE)

	return(d)
}


pre_process <- function(d) {
	# Add an index to preserve order
	d$rowIndex <- seq_len(nrow(d))

	# Get rid of Demo, MOCK, NPPILOT and TEST user codes (PSC1-only)
	d <- subset(d, !grepl("TEST|THOMAS_PRONK|MAREN", User.code, ignore.case = TRUE))

	return(d)
}


write_psytools_csv <- function(d, file) {
	# Roll our own quoting method
	for (column in colnames(d)) {
		d[, column] <- escape(d[, column])
	}

	# Undo R column name mangling
	columns <- sub("\\.ms\\.", "[ms]", colnames(d))  # Response time [ms]
	columns <- gsub("\\.", " ", columns)

	write.table(d, file, quote = FALSE, sep = ",", na = "",
				row.names = FALSE, col.names = columns)
}


derive <- function(d, filename) {
  requireValid <- "Valid" %in% colnames(df)
  selectFunction <- ifelse(grepl("RELIABILITY|_GEN_|INTERVIEW", filename), max, min)
  d <- selectIteration(d, selectFunction, TRUE, requireValid)
  if (grepl("^IMAGEN-IMGN_RELIABILITY", filename) || grepl("^IMAGEN-IMGN_FU_RELIABILITY", filename)) {
    d <- deriveImagenReliability(d)
    # Normalize task title name
    filename <- sub("_FU_RELIABILITY((_[^-]*)?)-", "_RELIABILITY\\1_FU-", filename)
  } else if (grepl("^IMAGEN-IMGN_GEN", filename)) {
    d <- deriveImagenGEN(d)
  } else if (grepl("^IMAGEN-IMGN_ADSR", filename)) {
    d <- deriveImagenADRS(d)
    # Typo in the ADRS task title name on the Delosis server
    filename <- sub("_ADSR_", "_ADRS_", filename)
  } else if (grepl("IMGN_TCI3", filename)) {
    d <- deriveImagenTCI3(d)
  } else if (grepl("IMGN_TCI", filename)) {
    d <- deriveImagenTCI(d, grepl("FU3", filename))
  } else if (grepl("IMGN_NEO_FFI", filename)) {
    d <- deriveImagenNEO(d, grepl("FU3", filename))
  } else if (grepl("IMGN_SURPS", filename)) {
    d <- deriveSURPS(d)
  } else if (grepl("IMGN_MAST", filename)) {
    d <- deriveImagenMAST(d)
  } else if (grepl("IMGN_CSI", filename)) {
    d <- deriveImagenCSI(d)
  } else if (grepl("IMGN_IRI", filename)) {
    d <- deriveImagenIRI(d)
  } else if (grepl("IMGN_AUDIT", filename)) {
    d <- deriveImagenAUDIT(d)
  } else if (grepl("IMGN_ESPAD", filename)) {
    d <- deriveImagenESPAD(d)
  } else if (grepl("IMGN_PDS", filename)) {
    d <- deriveImagenPDS(d)
  } else if (grepl("IMGN_CTS", filename)) {
    d <- deriveImagenCTS(d)
  } else if (grepl("IMGN_IDENT", filename)) {
    d <- deriveImagenIDENT(d)
  } else if (grepl("IMGN_KIRBY", filename)) {
    d <- deriveKIRBY(d)
  } else if (grepl("IMGN_DOT_PROBE", filename)) {
    d <- deriveImagenDOTPROBE(d)
  } else if (grepl("IMGN_LEQ", filename)) {
    d <- deriveLEQ(d)
  } else {
    d <- rotateQuestionnaire(d)
  }
  attr(d, "filename") <- filename
  return(d)
}

process <- function(psc2_dir, processed_dir, prefix = NULL) {
  filenames <- list.files(psc2_dir)

  # split between PALP and other files
  palp_filenames <- filenames[grepl("-IMGN_PALP_", filenames)]
  filenames <- filenames[!filenames %in% palp_filenames]

  # split between Core1 and other files
  core1_filenames <- filenames[grepl("Core1", filenames)]
  filenames <- filenames[!filenames %in% core1_filenames]

  # split between Core2 and other files
  core2_filenames <- filenames[grepl("Core2", filenames)]
  filenames <- filenames[!filenames %in% core2_filenames]

  # for now get rid of FU2 Parent and Stratify Screening LimeSurvey files!
  parent_filenames <- filenames[grepl("Parent", filenames)]
  filenames <- filenames[!filenames %in% parent_filenames]
  screening_filenames <- filenames[grepl("Screening", filenames, ignore.case = TRUE)]
  filenames <- filenames[!filenames %in% screening_filenames]

  # concatenate PALP files
  palp <- palp_filenames
  if (length(palp)) {
    # expect 1_1, 1_2, 1_3, etc. right after '-IMGN_PALP_'
    p <- str_locate(palp, "-IMGN_PALP_")[, 2] + 1
    # order PALP files in lexicographical order: 1_1, 1_2, 1_3, 2_1, 2_2, 2_3
    palp <- palp[order(substr(palp, p, p + 2))]
    # read the first PALP file...
    filepath <- file.path(psc2_dir, palp[1])
    d <- selectIteration(read_psytools_csv(filepath))
    # ...then concatenate the remaining PALP files
    for (filename in palp[-1]) {
      filepath <- file.path(psc2_dir, filename)
      d <- rbind(d, selectIteration(read_psytools_csv(filepath)))
    }
    d <- pre_process(d)
    d <- derivePALP(d)

    # Remove 1_1_
    filename <- paste(substr(palp[1], 1, p - 2),
                      substr(palp[1], p + 3, nchar(palp[1])), sep = "")

    filepath <- file.path(processed_dir, filename)
    write_psytools_csv(d, filepath)

    # avoid out-of-memory condition
    rm(d)
    gc()
  }

  # now iterate over non-PALP files if there are any
  for (filename in filenames) {
    filepath <- file.path(psc2_dir, filename)

    # Don't try and process a subdir
    if (dir.exists(filepath)) next
    d <- read_psytools_csv(filepath)
    d <- pre_process(d)

    # Skip files without data - they cannot be rotated!
    if (nrow(d) < 2) {
      cat(filename, ": skipping file without data.", sep = "", fill = TRUE)
      next
    }
    d <- derive(d, filename)
    filename <- attr(d, "filename")

    filepath <- file.path(processed_dir, filename)
    write_psytools_csv(d, filepath)

    # avoid out-of-memory condition
    rm(d)
    gc()
  }

  # now deal with any Core1/2 files
  for (coreList in list(core1_filenames, core2_filenames)) {
    if (length(coreList) == 0) next
    require(data.table, quietly = TRUE)
    dList <-
      convertFU3toFU2(as.data.table(rbindlist(
        lapply(
          as.list(file.path(psc2_dir, coreList)),
          FUN = read.csv,
          stringsAsFactors = FALSE
        ), fill = TRUE
      )))
    for (filename in names(dList)) {
      d <- dList[[filename]]
      d <- pre_process(d)

      # Skip files without data - they cannot be rotated!
      if (nrow(d) < 2) {
        cat(filename, ": skipping file without data.", sep = "", fill = TRUE)
        next
      }
      d <- derive(d, filename)
      filename <- paste0(prefix, attr(d, "filename"), ".csv")
      filepath <- file.path(processed_dir, filename)
      write_psytools_csv(d, filepath)

      # avoid out-of-memory condition
      rm(d)
      gc()
    }
  }
}

process(PSYTOOLS_BL_PSC1_DIR, PSYTOOLS_BL_DERIVED_DIR)
process(PSYTOOLS_FU1_PSC1_DIR, PSYTOOLS_FU1_DERIVED_DIR)
process(PSYTOOLS_FU2_PSC1_DIR, PSYTOOLS_FU2_DERIVED_DIR)
process(PSYTOOLS_FU3_PSC1_DIR, PSYTOOLS_FU3_DERIVED_DIR, prefix = "IMAGEN-")
process(PSYTOOLS_SB_PSC1_DIR, PSYTOOLS_SB_DERIVED_DIR, prefix = "STRATIFY-")
