#upload to s3 script to run both hydro and qual in the studies/historical folder
aws s3 cp run_dsm2.sh s3://ca.dwr.dms.dsm2/full/run_dsm2.sh
# zip without planning*.dss files 
zip -x "*lanning*.dss" -r dsm2-historical-input.zip common_input/ studies/ timeseries/ scripts/
aws s3 cp dsm2-historical-input.zip s3://ca.dwr.dms.dsm2/full/
rm dsm2-historical-input.zip
# submit aws batch job
aws batch submit-job --job-name "dsm2-full-historical" --job-queue "dsm2-large-job-queue" --job-definition "dsm2-base:7"  \
 --container-overrides 'environment=[{name=SCRIPT_LOC,value="s3://ca.dwr.dms.dsm2/full/run_dsm2.sh"},{name=DATA_LOC,value="s3://ca.dwr.dms.dsm2/full/dsm2-historical-input.zip"},{name=OUTPUT_DIR,value="studies/historical/output"},{name=OUTPUT_LOC,value="s3://ca.dwr.dms.dsm2/full/dsm2-historical-output.zip"}]'
aws s3 cp s3://ca.dwr.dms.dsm2/full/dsm2-historical-output.zip .
unzip dsm2-historical-output.zip
