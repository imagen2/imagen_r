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


PSYTOOLS_PSC2_DIR <- '/neurospin/imagen/BL/RAW/PSC1/psytools'
PSYTOOLS_PROCESSED_DIR <- '/tmp/psytools'


derivation <- function(name) {
    switch(name,
           rotateQuestionnaire)  # default fits all other questionnaires
}


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

    # Discard uncomplete trials
    df <- subset(df, df$Completed=='t')
    # Get rid of Demo, MOCK, NPPILOT and TEST user codes (PSC1-only)
    df <- subset(df, !grepl("Demo|MOCK|NPPILOT|TEST", User.code, ignore.case=TRUE))

    # Add an index to preserve order (to simplify eyeballing)
    df$rowIndex <- seq_len(nrow(df))

    # Apply relevant derivation function to each questionnaire
    derivation_function <- derivation(name)
    df <- derivation_function(df)

    # Write data frame back to the processed CSV file
    filepath <- file.path(PSYTOOLS_PROCESSED_DIR, filename)
    columns <- sub("\\.ms\\.", "[ms]", colnames(df))  # Response time [ms]
    columns <- gsub("\\.", " ", columns)
    write.table(df, filepath, quote=FALSE, sep=",", na="",
                row.names=FALSE, col.names=columns)
}
