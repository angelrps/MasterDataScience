#!/bin/bash
DELIMITER=$1
FILE=$2
NUM_OF_COLUMNS=$(head -1 ${FILE} | tr ${DELIMITER} "\n"| wc -l)
paste <(seq 1 ${NUM_OF_COLUMNS}) <(head -1 ${FILE} | tr ${DELIMITER} "\n")


