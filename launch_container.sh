#!/usr/bin/env bash

# Halt upon error
set -eoux pipefail
SCRIPT_DIR=$( cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd )

# Provide a name for the container here
NAME="peru-build"

# Ensure mounted directories exist before mount (consistent permissions in the container)
mkdir -p "${SCRIPT_DIR}"/data

# Can restrict further with "--platform linux/amd64"
docker run \
    --platform linux/amd64 \
    -v "${SCRIPT_DIR}"/scripts:/app/scripts:ro \
    -v "${SCRIPT_DIR}"/workflows:/app/workflows:ro \
    -v "${SCRIPT_DIR}"/data:/app/data \
    "${NAME}"
