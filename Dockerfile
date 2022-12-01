# Modified from
#   https://raw.githubusercontent.com/freechipsproject/chisel-bootcamp/master/Dockerfile

# First stage : setup the system and environment
FROM ubuntu:20.04 as base

RUN \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y \
        ca-certificates-java \
        curl \
        default-jdk-headless \
        g++ \
        gnupg \
        graphviz \
        make \
        python3-distutils \
        verilator \
        && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian all main" | tee /etc/apt/sources.list.d/sbt.list && \
    echo "deb https://repo.scala-sbt.org/scalasbt/debian /" | tee /etc/apt/sources.list.d/sbt_old.list && \
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | apt-key add && \
    apt-get update && \
    DEBIAN_FRONTEND=noninteractive apt-get install -y sbt && \
    rm -rf /var/lib/apt/lists/*

RUN curl https://bootstrap.pypa.io/get-pip.py -o get-pip.py
RUN python3 get-pip.py
RUN pip3 install notebook

RUN useradd -ms /bin/bash tutorial

ENV SCALA_VERSION=2.12.10
ENV ALMOND_VERSION=0.9.1

ENV COURSIER_CACHE=/coursier_cache

ADD . /pillars-tutorial/
WORKDIR /pillars-tutorial

ENV JUPYTER_CONFIG_DIR=/jupyter/config
ENV JUPITER_DATA_DIR=/jupyter/data

RUN mkdir -p $JUPYTER_CONFIG_DIR/custom
RUN cp tutorial/custom.js $JUPYTER_CONFIG_DIR/custom/

# Second stage - download Scala requirements and the Scala kernel
FROM base as intermediate-builder

RUN mkdir /coursier_cache

RUN \
    curl -L -o coursier https://git.io/coursier-cli && \
    chmod +x coursier && \
    ./coursier \
        bootstrap \
        -r jitpack \
        sh.almond:scala-kernel_$SCALA_VERSION:$ALMOND_VERSION \
        --sources \
        --default=true \
        -o almond && \
    ./almond --install --global && \
    \rm -rf almond couriser /root/.cache/coursier 

# Execute a notebook to ensure Chisel is downloaded into the image for offline work
RUN make build
RUN jupyter nbconvert --to notebook --output=/tmp/hello --execute tutorial/hello.ipynb

# Last stage
FROM base as final

# copy the Scala requirements and kernel into the image 
COPY --from=intermediate-builder /coursier_cache/ /coursier_cache/
COPY --from=intermediate-builder /usr/local/share/jupyter/kernels/scala/ /usr/local/share/jupyter/kernels/scala/
COPY --from=intermediate-builder /pillars-tutorial/target/ /pillars-tutorial/target/

RUN chown -R tutorial:tutorial /pillars-tutorial

USER tutorial
WORKDIR /pillars-tutorial

EXPOSE 8888
CMD jupyter notebook --no-browser --ip 0.0.0.0 --port 8888
