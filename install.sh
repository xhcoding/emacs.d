#!/bin/bash
set -e
readonly SCRIPT_LINES=51
readonly TEMP_PATH=/tmp/emacs-x-temp.tar.gz

INSTALL_DIR=/opt/emacs-x
BIN_DIR=/usr/bin

if [[ ! -z $1 ]] ; then
    INSTALL_DIR=$1
fi

if [[ ! -z $2 ]] ; then
    BIN_DIR=$2
fi

EMACS_X_PATH=$BIN_DIR/emacs-x

if [[ -d $INSTALL_DIR ]] ; then
   echo "Error: $INSTALL_DIR exists"
   exit 1
fi

mkdir -p $INSTALL_DIR

if [[ -f $TEMP_PATH ]] ; then
    rm -f $TEMP_PATH
fi

tail -n +$SCRIPT_LINES $0 > $TEMP_PATH

pushd $INSTALL_DIR

tar -zxvf $TEMP_PATH
chmod -R a+w *

cat <<EOF > $EMACS_X_PATH
#!/bin/bash
export GUIX_LOCPATH=${INSTALL_DIR}/opt/emacs/lib/locale
export PATH=${INSTALL_DIR}/opt/emacs/bin:${PATH}
source ${INSTALL_DIR}/opt/emacs/etc/profile

emacs --init-directory=${INSTALL_DIR}/opt/emacs/share/emacs.d "\$@"
EOF

chmod a+x $EMACS_X_PATH

popd
exit 0
