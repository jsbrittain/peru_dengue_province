#!/usr/bin/env bash

set -eoux pipefail

SNAKEMAKE_CORES=${SNAKEMAKE_CORES:=1}
snakemake --cores ${SNAKEMAKE_CORES} --snakefile workflows/datasets/proy/workflow/Snakefile
snakemake --cores ${SNAKEMAKE_CORES} --snakefile workflows/datasets/worldclim/workflow/Snakefile
snakemake --cores ${SNAKEMAKE_CORES} --snakefile workflows/datasets/spi6/workflow/Snakefile
snakemake --cores ${SNAKEMAKE_CORES} --snakefile workflows/datasets/oni/workflow/Snakefile
snakemake --cores ${SNAKEMAKE_CORES} --snakefile workflows/datasets/icen/workflow/Snakefile
snakemake --cores ${SNAKEMAKE_CORES} --snakefile workflows/datasets/shapefiles/workflow/Snakefile
