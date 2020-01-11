#!/bin/bash
# This script is used to tag and push to the ECR registry
# Run the command below, the instance running this command needs to have a role with the policy of ECR access enabled. 
# aws ecr get-login --region us-west-2
# The command above will print out the docker login with credentails. Potential security risk if your instance is shared with others so be careful
docker tag dsm2-base 835856973547.dkr.ecr.us-west-2.amazonaws.com/dsm2-base:8.2.0
docker push 835856973547.dkr.ecr.us-west-2.amazonaws.com/dsm2-base
docker tag dsm2-aws 835856973547.dkr.ecr.us-west-2.amazonaws.com/dsm2-aws:8.2.0
docker push 835856973547.dkr.ecr.us-west-2.amazonaws.com/dsm2-aws
