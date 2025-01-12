FROM ubuntu:24.04

# setup locale
RUN apt update && apt install -y locales && rm -rf /var/lib/apt/lists/* \
	&& localedef -i zh_CN -c -f UTF-8 -A /usr/share/locale/locale.alias zh_CN.UTF-8
ENV LANG=zh_CN.utf8

# install deps
RUN apt update && apt install -y \
    git build-essential autoconf texinfo pkg-config \
    libgnutls28-dev gnutls-bin libncurses-dev libsqlite3-dev libselinux-dev dbus libtree-sitter-dev zlib1g-dev libsystemd-dev emacs-nox

WORKDIR /tmp
# build emacs
RUN git clone --depth 1 https://github.com/emacs-mirror/emacs.git && \
    cd emacs && \
    sh autogen.sh && \
    sh configure --prefix=/usr --with-native-compilation=no --with-x-toolkit=no && \
    make -j8 && \
    make install

# config emacs
RUN git clone https://github.com/xhcoding/emacs.d.git ~/.emacs.d && \
    cd ~/.emacs.d/ && \
    emacs -Q --batch -l org --eval "(org-babel-tangle-file \"README.org\")" && \
    emacs --batch --load ~/.emacs.d/early-init.el --load ~/.emacs.d/init.el
    
