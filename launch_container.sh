#!/usr/bin/env bash

# Halt upon error
set -eoux pipefail
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Provide a name for the container here
NAME="peru-build"

# Ensure mounted directories exist before mount (consistent permissions in the container)
# mkdir -p "${SCRIPT_DIR}"/results

# Launch image
docker run \
    -v "${SCRIPT_DIR}"/results:/home/user/results \
    -v "${SCRIPT_DIR}"/data:/home/user/data \
    "${NAME}"
    # --platform linux/amd64 \
