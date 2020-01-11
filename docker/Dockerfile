FROM centos:7.7.1908 AS dsm2-base

RUN yum -y update \
    && yum install -y glibc.i686 libstdc++.i686 \
    && yum-config-manager --add-repo https://yum.repos.intel.com/2019/setup/intel-psxe-runtime-2019.repo \
    && rpm --import https://yum.repos.intel.com/2019/setup/RPM-GPG-KEY-intel-psxe-runtime-2019 \
    && yum install -y intel-icc-runtime-32bit intel-ifort-runtime-32bit \
    && echo 'source /opt/intel/psxe_runtime_2019.5.281/linux/bin/compilervars.sh ia32' >> /etc/bashrc \
    && yum clean all
#
#
RUN mkdir -p /usr/local/dsm2 \
    && mkdir -p /usr/local/dsm2/bin \
    && mkdir /workdir
COPY ./DSM2-8.2.0-Linux/bin /usr/local/dsm2/bin
#
WORKDIR /workdir
ENV PATH="/usr/local/dsm2/bin:${PATH}"

# AWS build over base layer to add ability to (down/up) load from S3 
FROM dsm2-base as dsm2-aws
RUN yum -y update \
    && yum install -y unzip zip \
    && yum clean all
# install AWS CLI
# only needed if running in AWS and this uses the strategy of downloading input files from s3
# and uploading the output to the same location as the input files in an output folder
RUN curl "https://s3.amazonaws.com/aws-cli/awscli-bundle.zip" -o "awscli-bundle.zip" \
    && unzip awscli-bundle.zip \
    && ./awscli-bundle/install -i /usr/local/aws -b /usr/local/bin/aws \
    && rm awscli-bundle.zip \
    && rm -rf awscli-bundle
# copy the aws setup and run script. This script uses environment variables
# SCRIPT_LOC and DATA_LOC to download the script from SCRIPT_LOC and the data from DATA_LOC
# and run the script after unzipping the data from DATA_LOC
# Both SCRIPT_LOC and DATA_LOC are s3 urls
COPY ./aws_setup_and_run.sh /usr/local/bin
ENTRYPOINT [ "aws_setup_and_run.sh" ]
CMD [ "" ]
