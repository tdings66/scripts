#!/usr/bin/env bash
#
# svtdbsgen.sh
#
# Script to create various source browsing databases, tag files, etc,
# from the SimpliVity code base, for use with Emacs.
#

export HOME_DIR="$HOME"
export SRC_DIR="$HOME_DIR/rfs/src"
export SEMANTICDB_DIR="$HOME_DIR/.emacs.d/semanticdb"

# Uncomment for testing...
#printf "\n$HOME_DIR"
#printf "\n$SRC_ROOT"
#printf "\n$SEMANTICDB_DIR"

export TAGS_LIST_ALL="$SRC_DIR/alltags.lst"
export TAGS_LIST_SRC="$SRC_DIR/srctags.lst"
export TAGS_LIST_SRC_BOOST="$SRC_DIR/srcboosttags.lst"
export TAGS_LIST_BOOST="$SRC_DIR/boosttags.lst"
export TAGS_LIST_STDCPP="$SRC_DIR/stdcpptags.lst"

export BROWSE_FILE_REG=$SRC_DIR/BROWSE
export BROWSE_FILE_ALL=$SRC_DIR/BROWSE_ALL
export BROWSE_FILE_SRC=$SRC_DIR/BROWSE_SRC
export BROWSE_FILE_SRC_BOOST=$SRC_DIR/BROWSE_SRC_PLUS_BOOST
export BROWSE_FILE_BOOST=$SRC_DIR/BROWSE_BOOST
export BROWSE_FILE_CPP=$SRC_DIR/BROWSE_STDCPP

export CSCOPE_OUT=$SRC_DIR/cscope.out
export CSCOPE_IN_OUT=$SRC_DIR/cscope.in.out
export CSCOPE_PO_OUT=$SRC_DIR/cscope.po.out

export ECTAGS_OUTPUT=$SRC_DIR/TAGS

export SRC_DIR=$SRC_DIR/src
export BOOST_DIR=/usr/include/boost
export CPP_DIR=/usr/include/c++

#
# First create the source lists...
#
makesourcefilelist.sh -t $TAGS_LIST_ALL -s $SRC_DIR -b $BOOST_DIR -C $CPP_DIR
makesourcefilelist.sh -t $TAGS_LIST_SRC -s $SRC_DIR
makesourcefilelist.sh -t $TAGS_LIST_SRC_BOOST -s $SRC_DIR -b $BOOST_DIR
makesourcefilelist.sh -t $TAGS_LIST_BOOST -b $BOOST_DIR
makesourcefilelist.sh -t $TAGS_LIST_STDCPP -C $CPP_DIR


#
# Next, create the various ebrowse files
#
#printf "\nCreating ebrowse file including Source, Boost, and Standard C++ libraries..."
#ebrowse --files=$TAGS_LIST_ALL -o $BROWSE_FILE_ALL
#cp $BROWSE_FILE_ALL $BROWSE_FILE_REG
#printf "\n... ebrowse file created!\n"

#printf "\nCreating ebrowse file including only Source..."
#browse --files=$TAGS_LIST_SRC -o $BROWSE_FILE_SRC
#printf "\n... ebrowse file created!\n"
#
#printf "\nCreating ebrowse file including Source and Boost..."
#ebrowse --files=$TAGS_LIST_SRC_BOOST -o $BROWSE_FILE_SRC_BOOST
#printf "\n... ebrowse file created!\n"
#
#printf "\nCreating ebrowse file including only Boost..."
#ebrowse --files=$TAGS_LIST_BOOST -o $BROWSE_FILE_BOOST
#printf "\n... ebrowse file created!\n"
#
#printf "\nCreating ebrowse file including only the Standard C++ library code..."
#ebrowse --files=$TAGS_LIST_STDCPP -o $BROWSE_FILE_CPP
#printf "\n... ebrowse file created!\n"


#
# Next, create cscope files for fast lookup
#
#printf "\nCreating cscope files including Source, Boost, and Standard C++ libraries..."
#cscope -bq -i $TAGS_LIST_ALL
#printf "\n... cscope files created!\n"


#
# Next, create the TAGS file
#
#printf "\nCreating TAGS file including Source, Boost, and Standard C++ libraries..."
##printf "Creating TAGS file including Source and Boost..."
##ctags -e -L $TAGS_LIST_SRC_BOOST
#ctags -e -L $TAGS_LIST_ALL
#printf "\n... TAGS file created!\n"


#
# Next, create the GNU Global DBs
#
printf "\nCreating GNU Global DBs including Source, Boost, and Standard C++ libraries..."
gtags -I -f $TAGS_LIST_ALL
#gtags -f $TAGS_LIST_STDCPP
#gtags -f $TAGS_LIST_BOOST
#gtags -f $TAGS_LIST_SRC
#gtags -f $TAGS_LIST_SRC_BOOST
printf "\n... GNU Global DBs created!\n"


#
# Next, create the GNU Global Hypertext
#
#printf "\nCreating GNU Global Hypertext including Source, Boost, and Standard C++ libraries..."
##htags --alphabet --auto-completion --compact --frame --form --fixed-guide --line-number --map-file --statistics
##htags --statistics --suggest
##htags --alphabet --compact --frame --form --line-number --statistics
#htags --alphabet --compact --form --line-number --statistics
#printf "\n... GNU Global Hypertext created!\n"


#
# Next, copy relevant files to SemanticDB directory
#
#printf "\nCopying eBrowse, TAGS, and cscope files to the SemanticDB directory..."
#cp $BROWSE_FILE_REG $SEMANTICDB_DIR
#cp $CSCOPE_OUT $SEMANTICDB_DIR
#cp $CSCOPE_IN_OUT $SEMANTICDB_DIR
#cp $CSCOPE_PO_OUT $SEMANTICDB_DIR
#cp $ECTAGS_OUTPUT $SEMANTICDB_DIR
#printf "\n... SemanticDB files copied!\n\n"

