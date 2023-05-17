# DSM2 Cloud Setup

DSM2 has been compiled on Linux and tested against the Windows results.
The output from hydro and qual is similar thought not exact (floating
point level differences)

AWS (Amazon Web Services) Cloud services have been used to run DSM2
using AWS Linux AMI (Amazon Machine Image). This requires a user to
start linux VM and then download and run DSM2 on that VM.

[Docker install on AWS
Linux](http://msb-confluence/display/DEV/Docker+install+on+AWS+Linux)

A serverless approach to this would be that the user submits a batch job
consisting of a specification of what container (Docker) to be used and
a zip file with the inputs. The batch job is then run on a suitable
machine and the resulting output file is zipped and uploaded to S3 (AWS
Simple Storage System)

The serverless approach allows for submission of multiple concurrent
jobs that provide the ability to do many parallel runs at the same time.
The charges are on persecond basis making efficient use of computing
resources.

The use of the cloud to run a batch DSM2 PTM is here: [How to Run a DCP
PTM Batch Job on
AWS](http://msb-confluence/display/DEV/How+to+Run+a+DCP+PTM+Batch+Job+on+AWS)
