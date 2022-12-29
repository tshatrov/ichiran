FROM ubuntu:latest

RUN    apt-get update \
    && apt-get upgrade

RUN    DEBIAN_FRONTEND=noninteractive \
       apt-get install -y locales postgresql sbcl wget libzstd-dev bzip2 build-essential git \
    && rm -rf /var/lib/apt/lists/* \
	&& localedef -i en_US -c -f UTF-8 -A /usr/share/locale/locale.alias en_US.UTF-8 \
    && localedef -i ja_JP -c -f UTF-8 -A /usr/share/locale/locale.alias ja_JP.UTF-8
ENV LANG en_US.utf8

# envars
ENV DATABASE_NAME=jmdict
ENV ICHIRAN_DB_URL=https://github.com/tshatrov/ichiran/releases/download/ichiran-230122/ichiran-230122.pgdump
ENV SBCL_URL="http://prdownloads.sourceforge.net/sbcl/sbcl-2.2.11-source.tar.bz2?download"

# sbcl
WORKDIR /root
RUN wget ${SBCL_URL} -O sbcl.tar.bz2
RUN tar xvf sbcl.tar.bz2 --one-top-level=sbcl --strip-components 1
WORKDIR /root/sbcl
RUN sh make.sh && sh install.sh

# quicklisp
WORKDIR /root
RUN wget https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --load quicklisp.lisp --non-interactive \
         --eval "(quicklisp-quickstart:install)" \
         --eval "(ql-util:without-prompting (ql:add-to-init-file))" \
         --eval "(sb-ext:quit)"

# get ichiran postgres DB
WORKDIR /tmp
USER   postgres
RUN    wget ${ICHIRAN_DB_URL} -O ichiran.pgdump
RUN    service postgresql start \
    && psql -c "ALTER USER postgres PASSWORD 'password';"
RUN    service postgresql start \
    && createdb -E 'UTF8' -l 'ja_JP.utf8' -T template0 ${DATABASE_NAME} \
    && (pg_restore -C -d ${DATABASE_NAME} /tmp/ichiran.pgdump > /tmp/restore.log 2>&1 || true)
USER   root

# ichiran
WORKDIR /root
RUN     git clone https://gitlab.com/yamagoya/jmdictdb.git
WORKDIR /root/quicklisp/local-projects
RUN     git clone https://github.com/tshatrov/ichiran.git
WORKDIR /root/quicklisp/local-projects/ichiran
COPY    settings.lisp.docker settings.lisp
RUN        service postgresql start \
        && sbcl --non-interactive \
                --eval "(ql:quickload :ichiran)" \
                --eval "(ichiran/mnt:add-errata)" \
                --eval "(ichiran/test:run-all-tests)"

# ichiran-cli
RUN    service postgresql start \
    && sbcl --non-interactive \
            --eval "(ql:quickload :ichiran)" \
            --eval "(ichiran/cli:build)"