# This script is sourced by the user and uses
# their shell.
#
# This script tries to find its location but
# this does not work in every shell.
#
# It is known to work in bash, zsh and ksh
#
# Do not execute this script without sourcing,
# because it won't have any effect then.
# That is, always run this script with
#
#     . /path/to/emsdk_env.sh
#
# or
#
#     source /path/to/emsdk_env.sh
#
# instead of just plainly running with
#
#     ./emsdk_env.sh
#
# which won't have any effect.

CURRENT_SCRIPT=
DIR="."

# use shell specific method to get the path
# to the current file being source'd.
#
# To add a shell, add another conditional below,
# then add tests to scripts/test_source_env.sh

if [ -n "${BASH_SOURCE-}" ]; then
  CURRENT_SCRIPT="$BASH_SOURCE"
elif [ -n "${ZSH_VERSION-}" ]; then
  CURRENT_SCRIPT="${(%):-%x}"
elif [ -n "${KSH_VERSION-}" ]; then
  CURRENT_SCRIPT=${.sh.file}
fi

if [ -n "${CURRENT_SCRIPT-}" ]; then
  DIR=$(dirname "$CURRENT_SCRIPT")
fi
unset CURRENT_SCRIPT

if [ ! -f "$DIR/cabal.project" ]; then
  echo "Error: unable to determine project directory. Perhaps you are using a shell or" 1>&2
  echo "       environment that this script does not support." 1>&2
  echo 1>&2
  echo "A possible solution is to source this script while in the project directory." 1>&2
  echo 1>&2
  unset DIR
  return
fi
DIR=$(cd "$DIR"; pwd)

# set environment variables
PATH=$DIR/bin:$PATH
echo PATH=$PATH
unset DIR

