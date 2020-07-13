FROM shadowrobot/build-tools:trusty-indigo

MAINTAINER "Igor Caputo"

LABEL Description="This image allows to run Lazarus UI in Docker container" Vendor="Igor Caputo" Version="1.0"

RUN set -x && \
    export downloads_url="https://ufpr.dl.sourceforge.net/project/lazarus/Lazarus%20Linux%20amd64%20DEB/Lazarus%202.0.8" && \ 
    
    echo "Installing needed libraries" && \
    apt-get update && \
    apt-get install -y wget binutils gcc libgtk2.0-0 libgtk2.0-dev psmisc gdb && \
    
    echo "Downloading and installing Debians" && \
    cd /tmp && \
    wget "${downloads_url}/fpc-src_3.0.4-2_amd64.deb" && \
    dpkg -i fpc-src_3.0.4-2_amd64.deb && \
    wget "${downloads_url}/fpc-laz_3.0.4-1_amd64.deb" && \
    dpkg -i fpc-laz_3.0.4-1_amd64.deb && \
    wget "${downloads_url}/lazarus-project_2.0.8-0_amd64.deb" && \
    dpkg -i lazarus-project_2.0.8-0_amd64.deb && \
    
    wget "ftp://ftp.freepascal.org/pub/fpc/dist/3.0.4/x86_64-linux/x86_64-linux-prt0-pre2.26-binutils.tar.gz" && \

    tar -xf x86_64-linux-prt0-pre2.26-binutils.tar.gz && \
    cp -rf /tmp/lib/fpc/3.0.4/units/x86_64-linux/rtl/* /usr/lib/fpc/3.0.4/units/x86_64-linux/rtl/ && \

    echo "Removing cache" && \
    apt-get clean && \
    rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

ENTRYPOINT ["/usr/local/bin/entrypoint.sh"]

CMD ["/usr/bin/terminator"]
