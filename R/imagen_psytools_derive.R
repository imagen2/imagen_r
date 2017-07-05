#!/usr/bin/env Rscript

# Copyright (c) 2017 CEA
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


PSYTOOLS_BL_PSC2_DIR <- "/neurospin/imagen/BL/RAW/PSC1/psytools"
PSYTOOLS_BL_PROCESSED_DIR <- "/home/dp165978/WORK_IN_PROGRESS/imagen/PSYTOOLS/R/BL"
PSYTOOLS_FU1_PSC2_DIR <- "/neurospin/imagen/FU1/RAW/PSC1/psytools"
PSYTOOLS_FU1_PROCESSED_DIR <- "/home/dp165978/WORK_IN_PROGRESS/imagen/PSYTOOLS/R/FU1"
PSYTOOLS_FU2_PSC2_DIR <- "/neurospin/imagen/FU2/RAW/PSC1/psytools"
PSYTOOLS_FU2_PROCESSED_DIR <- "/home/dp165978/WORK_IN_PROGRESS/imagen/PSYTOOLS/R/FU2"

BOGUS <- list(# BL
              "IMAGEN-IMGN_CTS_PARENT_RC5-BASIC_DIGEST",
              "IMAGEN-IMGN_KIRBY_RC5-IMAGEN_KIRBY_DIGEST",
              # FU1
              "IMAGEN-IMGN_FU_RELIABILITY_ADDITIONAL-BASIC_DIGEST",
              "IMAGEN-IMGN_FU_RELIABILITY-BASIC_DIGEST",
              "IMAGEN-IMGN_KIRBY_FU_RC5-IMAGEN_KIRBY_DIGEST",
              "IMAGEN-IMGN_TLFB_FU_RC5-BASIC_DIGEST",
              # FU2
              "IMAGEN-IMGN_GATEWAY_FU2_2-BASIC_DIGEST",
              "IMAGEN-IMGN_RELIABILITY_CORE_CHILD_FU2-BASIC_DIGEST",
              "IMAGEN-IMGN_RELIABILITY_OPT_FU2-BASIC_DIGEST",
              "IMAGEN-IMGN_RELIABILITY_PI_FU2-BASIC_DIGEST",
              "IMAGEN-IMGN_KIRBY_FU2-IMAGEN_KIRBY_DIGEST")


derivation <- function(name) {
    switch(name,
           "IMAGEN-IMGN_KIRBY_RC5-IMAGEN_KIRBY_DIGEST"=deriveKIRBY,
           "IMAGEN-IMGN_KIRBY_FU_RC5-IMAGEN_KIRBY_DIGEST"=deriveKIRBY,
           "IMAGEN-IMGN_KIRBY_FU2-IMAGEN_KIRBY_DIGEST"=deriveKIRBY,
           rotateQuestionnaire)  # default fits all other questionnaires
}


process <- function(psc2_dir, processed_dir) {
    # Iterate over exported CSV Psytools files
    for (filename in list.files(psc2_dir)) {
        # The name of the questionnaire is based on the CSV file name
        name <- file_path_sans_ext(filename)

        if (name %in% BOGUS) {
            next
        }

        # Read each exported CSV Psytools file into a data frame
        filepath <- file.path(psc2_dir, filename)
        COL_CLASSES = c(
            "User.code"="character",
            "Block"="character",
            "Trial"="character",
            "Response.time..ms."="numeric")
        df <- read.csv(filepath, colClasses=COL_CLASSES)

        # Discard uncomplete trials
        df <- subset(df, df$Completed=='t')
        # Get rid of TEST, THOMAS_PRONK and MAREN user codes (PSC1-only)
        df <- subset(df, !grepl("TEST|THOMAS_PRONK|MAREN", User.code, ignore.case=TRUE))

        # Add an index to preserve order (to simplify eyeballing)
        df$rowIndex <- seq_len(nrow(df))

        # Apply relevant derivation function to each questionnaire
        derivation_function <- derivation(name)
        df <- derivation_function(df)

        # Write data frame back to the processed CSV file
        filepath <- file.path(processed_dir, filename)
        columns <- sub("\\.ms\\.", "[ms]", colnames(df))  # Response time [ms]
        columns <- gsub("\\.", " ", columns)
        write.table(df, filepath, sep=",", na="",
                    row.names=FALSE, col.names=columns)
    }
}


process(PSYTOOLS_BL_PSC2_DIR, PSYTOOLS_BL_PROCESSED_DIR)
process(PSYTOOLS_FU1_PSC2_DIR, PSYTOOLS_FU1_PROCESSED_DIR)
process(PSYTOOLS_FU2_PSC2_DIR, PSYTOOLS_FU2_PROCESSED_DIR)
