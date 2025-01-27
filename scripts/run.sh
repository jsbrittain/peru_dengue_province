#!/usr/bin/env bash

# Halt upon error
set -eoux pipefail

# Change to app folder (this scripts parent folder)
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "${SCRIPT_DIR}/.."

# Run the program
scripts/update_datasets.sh
R -e "source('scripts/main.R')"
