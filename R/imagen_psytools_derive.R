#!/usr/bin/env Rscript

# Copyright (c) 2017-2018 CEA
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


PSYTOOLS_BL_PSC2_DIR <- "/neurospin/imagen/BL/RAW/PSC2/psytools"
PSYTOOLS_BL_PROCESSED_DIR <- "/neurospin/imagen/BL/processed/psytools"
PSYTOOLS_FU1_PSC2_DIR <- "/neurospin/imagen/FU1/RAW/PSC2/psytools"
PSYTOOLS_FU1_PROCESSED_DIR <- "/neurospin/imagen/FU1/processed/psytools"
PSYTOOLS_FU2_PSC2_DIR <- "/neurospin/imagen/FU2/RAW/PSC2/psytools"
PSYTOOLS_FU2_PROCESSED_DIR <- "/neurospin/imagen/FU2/processed/psytools"
PSYTOOLS_FU3_PSC2_DIR <- "/neurospin/imagen/FU3/RAW/PSC2/psytools"
PSYTOOLS_FU3_PROCESSED_DIR <- "/neurospin/imagen/FU3/processed/psytools"
PSYTOOLS_SB_PSC2_DIR <- "/neurospin/imagen/SB/RAW/PSC2/psytools"
PSYTOOLS_SB_PROCESSED_DIR <- "/neurospin/imagen/SB/processed/psytools"

escape <- function(x) {
	if (class(x) == "character") {
		# Escape double quotation marks by doubling them
		x <- gsub('"', '""', x)
		# Enclose in quotation marks strings with commas or quotation marks
		x <- gsub('^(.*[",].*$)', '"\\1"', x)
	}
	return (x)
}


read_psytools_csv <- function(file) {
	COL_CLASSES = c(
		"User.code"="character",
		"Block"="character",
		"Trial"="character",
		"Response.time..ms."="numeric")
	d <- read.csv(file, colClasses=COL_CLASSES, stringsAsFactors=FALSE)

	return (d)
}


pre_process <-function(d) {
	# Add an index to preserve order
	d$rowIndex <- seq_len(nrow(d))

	# Get rid of Demo, MOCK, NPPILOT and TEST user codes (PSC1-only)
	d <- subset(d, !grepl("TEST|THOMAS_PRONK|MAREN", User.code, ignore.case=TRUE))

	return (d)
}


write_psytools_csv <- function(d, file) {
	# Roll our own quoting method
	for (column in colnames(d)) {
		d[,column] <- escape(d[,column])
	}

	# Undo R column name mangling
	columns <- sub("\\.ms\\.", "[ms]", colnames(d))  # Response time [ms]
	columns <- gsub("\\.", " ", columns)

	write.table(d, file, quote=FALSE, sep=",", na="",
				row.names=FALSE, col.names=columns)
}


process <- function(psc2_dir, processed_dir) {
	filenames <- list.files(psc2_dir)

	# Only process CSV exports from the legacy Psytools system.
	# The new LimeSurvey system natively exports CSV in wide format.
	filenames <- filenames[grepl("^(IMAGEN|STRATIFY)-", filenames)]

	# split between PALP and other files
	palp_filenames <- split(filenames, grepl("-IMGN_PALP_", filenames))

	# concatenate PALP files
	palp <- palp_filenames$'TRUE'
	if (length(palp)) {
		# expect 1_1, 1_2, 1_3, etc. right after "-IMGN_PALP_"
		p <- str_locate(palp, "-IMGN_PALP_")[,2] + 1
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

	# now iterate over non-PALP files
	for (filename in palp_filenames$'FALSE') {
		filepath <- file.path(psc2_dir, filename)
		d <- read_psytools_csv(filepath)
		d <- pre_process(d)

		# Skip files without data - they cannot be rotated!
		if (nrow(d) < 2) {
			cat(filename, ": skipping file without data.", sep="", fill=TRUE)
			next
		}

		if (grepl("^IMAGEN-IMGN_RELIABILITY", filename) || grepl("^IMAGEN-IMGN_FU_RELIABILITY", filename)) {
			d <- selectIteration(d, max, TRUE, FALSE)
			d <- deriveImgnReliability(d)
			# Normalize task title name
			filename <- sub("_FU_RELIABILITY((_[^-]*)?)-", "_RELIABILITY\\1_FU-", filename)
		}
		else if (grepl("^IMAGEN-IMGN_GEN", filename)) {
			# Select the last complete attempt for Gen
			d <- selectIteration(d, max, TRUE, FALSE)
			d <- deriveImgnGEN(d)
		}
		else if (grepl("^IMAGEN-IMGN_ADSR", filename)) {
			d <- selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnADRS(d)
			# Typo in the ADRS task title name on the Delosis server
			filename <- sub("_ADSR_", "_ADRS_", filename)
		}
		else if (grepl("^IMAGEN-IMGN_TCI_PARENT", filename)) {
			selectIteration(d, min, TRUE, FALSE)
			d <- deriveImgnTCI(d)
		}
		else if (grepl("^IMAGEN-IMGN_TCI_CHILD", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnTCI(d)
		}
		else if (grepl("^IMAGEN-IMGN_NEO_FFI_PARENT", filename)) {
			selectIteration(d, min, TRUE, FALSE)
			d <- deriveImgnNEO(d)
		}
		else if (grepl("^IMAGEN-IMGN_NEO_FFI", filename)) { # CHILD
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnNEO(d)
		}
		else if (grepl("^IMAGEN-IMGN_SURPS_PARENT", filename)) {
			selectIteration(d, min, TRUE, FALSE)
			d <- deriveImgnSURPS(d)
		}
		else if (grepl("^IMAGEN-IMGN_SURPS", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnSURPS(d)
		}
		else if (grepl("^IMAGEN-IMGN_MAST_PARENT", filename)) {
			selectIteration(d, min, TRUE, FALSE)
			d <- deriveImgnMAST(d)
		}
		else if (grepl("^IMAGEN-IMGN_MAST_CHILD", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnMAST(d)
		}
		else if (grepl("^IMAGEN-IMGN_CSI_CHILD", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnCIS(d)
		}
		else if (grepl("^IMAGEN-IMGN_IRI_CHILD", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnIRI(d)
		}
		else if (grepl("^IMAGEN-IMGN_AUDIT_CHILD", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnAUDIT(d)
		}
		else if (grepl("^IMAGEN-IMGN_AUDIT_INTERVIEW", filename)) {
			# Select the last complete attempt for Interview
			selectIteration(d, max, TRUE, TRUE)
			d <- deriveImgnAUDIT(d)
		}
		else if (grepl("^IMAGEN-IMGN_ESPAD_CHILD", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnESPAD(d)
		}
		else if (grepl("^IMAGEN-IMGN_ESPAD_INTERVIEW", filename)) {
			# Select the last complete attempt for Interview
			selectIteration(d, max, TRUE, TRUE)
			d <- deriveImgnESPAD(d)
		}
		else if (grepl("^IMAGEN-IMGN_PDS", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnPDS(d)
		}
		else if (grepl("^IMAGEN-IMGN_CTS_PARENT", filename)) {
			selectIteration(d, min, TRUE, FALSE)
			d <- deriveImgnCTS(d)
		}
		else if (grepl("^IMAGEN-IMGN_IDENT", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnIDENT(d)
		}
		else if (grepl("^IMAGEN-IMGN_KIRBY", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveKIRBY(d)
		}
		else if (grepl("^IMAGEN-IMGN_DOT_PROBE", filename)) {
			selectIteration(d, min, TRUE, TRUE)
			d <- deriveImgnDOTPROBE(d)
		}
		else if (grepl("-BASIC_DIGEST.csv$", filename)) {
			selectIteration(d, min, TRUE, FALSE)
			d <- rotateQuestionnaire(d)
		}
		else {
			selectIteration(d, min, TRUE, TRUE)
			d <- rotateQuestionnaire(d)
		}

		filepath <- file.path(processed_dir, filename)
		write_psytools_csv(d, filepath)

		# avoid out-of-memory condition
		rm(d)
		gc()
	}
}


process(PSYTOOLS_BL_PSC2_DIR, PSYTOOLS_BL_PROCESSED_DIR)
process(PSYTOOLS_FU1_PSC2_DIR, PSYTOOLS_FU1_PROCESSED_DIR)
process(PSYTOOLS_FU2_PSC2_DIR, PSYTOOLS_FU2_PROCESSED_DIR)
process(PSYTOOLS_FU3_PSC2_DIR, PSYTOOLS_FU3_PROCESSED_DIR)
process(PSYTOOLS_SB_PSC2_DIR, PSYTOOLS_SB_PROCESSED_DIR)
