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


PSYTOOLS_PSC2_DIR <- '/cveda/databank/RAW/PSC1/psytools'
PSYTOOLS_PROCESSED_DIR <- '/tmp/databank/processed/psytools'


# Import derivation and helper functions from the script's directory
.scriptpath <- function() {
    path <- getSrcDirectory(.scriptpath)
    if (length(path) == 0) {
        args <- commandArgs(trailingOnly=FALSE)
        path <- dirname(sub("--file=", "", args[grep("--file", args)]))
    }
    return (path)
}
path <- .scriptpath()
source(file.path(path, "psytools_task_derivations.R"))


DERIVATION = list(
    "cVEDA-cVEDA_SOCRATIS-BASIC_DIGEST"=deriveSOCRATIS,
    "cVEDA-cVEDA_SST-BASIC_DIGEST"=deriveSST,
    "cVEDA-cVEDA_KIRBY-BASIC_DIGEST"=deriveKIRBY,
    "cVEDA-cVEDA_BART-BASIC_DIGEST"=deriveBART,
    "cVEDA-cVEDA_ERT-BASIC_DIGEST"=deriveERT,
    "cVEDA-cVEDA_MID-BASIC_DIGEST"=deriveMID,
    "cVEDA-cVEDA_TMT-TMT_DIGEST"=deriveTMT,
    "cVEDA-cVEDA_WCST-BASIC_DIGEST"=deriveWCST,
    "cVEDA-cVEDA_CORSI-BASIC_DIGEST"=deriveCORSI,
    "cVEDA-cVEDA_DS-BASIC_DIGEST"=deriveDS)

# Iterate over exported CSV Psytools files
for (filename in list.files(PSYTOOLS_PSC2_DIR)) {
    # The name of the questionnaire is based on the CSV file name
    name <- file_path_sans_ext(filename)

    # Read each exported CSV Psytools file into a data frame
    filepath <- file.path(PSYTOOLS_PSC2_DIR, filename)
    COL_CLASSES = c(
        "User.code"="character",
        "Block"="character",
        "Trial"="character",
        "Response.time..ms."="numeric")
    df <- read.csv(filepath, colClasses=COL_CLASSES)

    # Remove invalid data from data frame
    # Discard uncomplete trials
    df <- subset(df, df$Completed=='t')
    # Get rid of Demo, MOCK, NPPILOT and TEST logins (PSC1-only)
    df <- subset(df, !grepl("Demo|MOCK|NPPILOT|TEST", User.code, ignore.case=TRUE))

    # Add bells & whistles to data frame before pre-processing
    # Give it a proper name
    df$TaskID <- name
    # Add an index to preserve order (to simplify eyeballing)
    df$rowIndex <- seq_len(nrow(df))

    # Apply specific derivation function to relevant questionnaires
    if (name %in% names(DERIVATION)) {
        # Add an age group, required by current derivation functions
        df$AgeGroup <- as.numeric(substr(df$User.code, 15, 15))
        # Apply derivation function
        derivation_function = DERIVATION[[name]]
        print("derivation...")
        print(name)
        df <- derivation_function(df)
        # Remove age group
        df$AgeGroup <- NULL
    } else {
        print("rotateQuestionnaire")
        print(name)
        # Rotate all data frames from long to wide format
        df <- rotateQuestionnaire(df)
    }

    # Write data frame back to processed CSV file
    filepath <- file.path(PSYTOOLS_PROCESSED_DIR, filename)
    write.csv(df, filepath, row.names=FALSE, quote=FALSE, na="")
}
