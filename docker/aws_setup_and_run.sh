#!/bin/bash
# This script will download the script from SCRIPT_LOC and the data (in zip format) from DATA_LOC
# It will then run the script and zip the output folder defined by environment variable OUTPUT_DIR
# and upload to the s3 url defined by OUTPUT_LOC
#
# Usage for
BASENAME="${0##*/}"
usage () {
  if [ "${#@}" -ne 0 ]; then
    echo "* ${*}"
    echo
  fi
  cat <<ENDUSAGE
Usage:

export SCRIPT_LOC="s3://my-bucket/script"
export DATA_LOC="s3://my-bucket/data.zip"
export OUTPUT_DIR="my-dir/output"
export OUTPUT_LOC="s3://my-bucket/output.zip"
${BASENAME} script [ <script arguments> ]
ENDUSAGE

  exit 2
}
# Standard function to print an error and exit with a failing return code
error_exit () {
  echo "${BASENAME} - ${1}" >&2
  exit 1
}
# Check what environment variables are set
if [ -z "${SCRIPT_LOC}" ]; then
  usage "SCRIPT_LOC not set, unable to determine the location of script to run from s3. Provide a s3 url i.e. s3://my-bucket/script"
fi

if [ -z "${DATA_LOC}" ]; then
  usage "DATA_LOC not defined, provide the s3 url of the zip file to download, e.g. s3://my-bucket/data.zip"
fi

if [ -z "${OUTPUT_LOC}" ]; then
  usage "OUTPUT_LOC not defined, provide the s3 url file to upload to , e.g. s3://my-bucket/output.zip"
fi

if [ -z "${OUTPUT_DIR}" ]; then
  usage "OUTPUT_DIR not defined, provide the location of the output directory, e.g. my-dir/output"
fi


scheme="$(echo "${SCRIPT_LOC}" | cut -d: -f1)"
if [ "${scheme}" != "s3" ]; then
  usage "SCRIPT_LOC given: ${SCRIPT_LOC} must be for an S3 object; expecting URL starting with s3://"
fi

scheme="$(echo "${DATA_LOC}" | cut -d: -f1)"
if [ "${scheme}" != "s3" ]; then
  usage "DATA_LOC given: ${DATA_LOC} must be for an S3 object; expecting URL starting with s3://"
fi

scheme="$(echo "${OUTPUT_LOC}" | cut -d: -f1)"
if [ "${scheme}" != "s3" ]; then
  usage "OUTPUT_LOC given: ${OUTPUT_LOC} must be for an S3 object; expecting URL starting with s3://"
fi

# Check that necessary programs are available
#which aws >/dev/null 2>&1 || error_exit "Unable to find AWS CLI executable."
#which unzip >/dev/null 2>&1 || error_exit "Unable to find unzip executable."
#
# Fetch and run a script
fetch_and_run_script () {
  # Create a temporary file and download the script to a named file "batch_run_script.sh"
  aws s3 cp "${SCRIPT_LOC}" "batch_run_script.sh" || error_exit "Failed to download S3 script from ${SCRIPT_LOC}"
  chmod u+x "batch_run_script.sh" || error_exit "Failed to chmod script."

  # Create a temporary file and download the zip file
  aws s3 cp "${DATA_LOC}" "batch_data.zip" || error_exit "Failed to download S3 data zip file from ${DATA_LOC}"

  # Create a temporary directory and unpack the zip file
  unzip -q "batch_data.zip" || error_exit "Failed to unpack zip file."

  # Run the script with the arguments
  "./batch_run_script.sh" "${@}" || error_exit "Failed to execute script."
  # zip the output 
  zip -r output.zip "${OUTPUT_DIR}" || error_exit "Failed to execute script."
  # copy it to the location specified
  exec aws s3 cp output.zip "${OUTPUT_LOC}" || error_exit "Failed to execute script."

}

# Main
fetch_and_run_script "${@}"
