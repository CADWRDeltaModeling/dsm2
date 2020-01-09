FROM centos:7.7.1908 AS builder

RUN yum -y update \
    && yum install -y glibc.i686 libstdc++.i686 unzip \
    && yum-config-manager --add-repo https://yum.repos.intel.com/2019/setup/intel-psxe-runtime-2019.repo \
    && rpm --import https://yum.repos.intel.com/2019/setup/RPM-GPG-KEY-intel-psxe-runtime-2019 \
    && yum install -y intel-icc-runtime-32bit intel-ifort-runtime-32bit \
    && echo 'source /opt/intel/psxe_runtime_2019.5.281/linux/bin/compilervars.sh ia32' >> /etc/bashrc \
    && yum clean all
# install AWS CLI
# only needed if running in AWS and this uses the strategy of downloading input files from s3
# and uploading the output to the same location as the input files in an output folder
RUN curl "https://s3.amazonaws.com/aws-cli/awscli-bundle.zip" -o "awscli-bundle.zip" \
    && unzip awscli-bundle.zip \
    && ./awscli-bundle/install -i /usr/local/aws -b /usr/local/bin/aws \
    && rm awscli-bundle.zip
#
#
RUN mkdir -p /usr/local/dsm2 \
    && mkdir -p /usr/local/dsm2/bin \
    && mkdir /workdir
COPY ./DSM2-8.2.0-Linux/bin /usr/local/dsm2/bin
#
WORKDIR /workdir
ENV PATH="/usr/local/dsm2/bin:${PATH}"

