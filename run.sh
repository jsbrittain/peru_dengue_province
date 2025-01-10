#!/usr/bin/env bash

# Halt upon error
set -eoux pipefail

# Change to script directory
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )
cd "${SCRIPT_DIR}"

# Run the program
R -e "source('main.R')"
