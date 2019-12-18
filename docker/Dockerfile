FROM centos:7.7.1908

RUN yum -y update
RUN yum install -y glibc.i686 libstdc++.i686
RUN yum-config-manager --add-repo https://yum.repos.intel.com/2019/setup/intel-psxe-runtime-2019.repo
RUN rpm --import https://yum.repos.intel.com/2019/setup/RPM-GPG-KEY-intel-psxe-runtime-2019
RUN yum install -y intel-icc-runtime-32bit intel-ifort-runtime-32bit
RUN echo 'source /opt/intel/psxe_runtime_2019.5.281/linux/bin/compilervars.sh ia32' >> /etc/bashrc

RUN mkdir -p /usr/local/dms2
RUN mkdir -p /usr/local/dsm2/bin
ADD ./hydro /usr/local/dsm2/bin
ADD ./qual /usr/local/dsm2/bin
RUN mkdir /workdir

