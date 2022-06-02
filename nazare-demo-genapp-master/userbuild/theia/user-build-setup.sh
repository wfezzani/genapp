#!/bin/sh
################################################################
# LICENSED MATERIALS - PROPERTY OF IBM
# "RESTRICTED MATERIALS OF IBM"
# (C) COPYRIGHT IBM CORPORATION 2019. ALL RIGHTS RESERVED
# US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,
# OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE
# CONTRACT WITH IBM CORPORATION
################################################################

USER_HOST=$1          #USER='userid@host.com'
WORK_DIR=$2           #WORK_DIR='parent workspace directory'
APP_DIR=$3            #APPLICATION='application directory under parent workspace'
OUT_DIR=$4            #OUT_DIR='fully qualified directory location of output log files'
DIR_LIST=$5           #DIR_LIST='.txt file containing folders to be created during the setup'
LOCAL_LOG_DIR=$6      #LOCAL_LOG_DIR='local folder for log file'


## Create local log file and use the same name to pass to ssh commands
LOG_FILE="`date +%Y-%m-%d-%H-%M-%S`.log"
LOG_FILE=USR-BLD-SETUP-$LOG_FILE


echo '#############################' | tee -a $LOCAL_LOG_DIR/$LOG_FILE
echo '# Starting User Build Setup #' | tee -a $LOCAL_LOG_DIR/$LOG_FILE
echo '#############################' | tee -a $LOCAL_LOG_DIR/$LOG_FILE


## Check/create WORK directory and logs folder on host
echo "Verify/create necessary folders on host" | tee -a $LOCAL_LOG_DIR/$LOG_FILE
echo "ssh $USER_HOST WORK=$WORK_DIR APP=$APP_DIR OUT=$OUT_DIR LOGFILE=$LOG_FILE" | tee -a $LOCAL_LOG_DIR/$LOG_FILE
ssh $USER_HOST WORK=$WORK_DIR APP=$APP_DIR OUT=$OUT_DIR LOGFILE=$LOG_FILE '

if [[ ! -d "${OUT}" ]]; then
    echo "Setting up log folder $OUT"
    mkdir -p "${OUT}"
    chmod a+rwx "${OUT}"
fi

cd "${OUT}"
LOGFILE=$OUT/$LOGFILE
touch $LOGFILE
chmod a+rw $LOGFILE
echo "Created log file $LOGFILE" >> $LOGFILE

echo "Checking for WORK directory $WORK" >> $LOGFILE
if [[ ! -d "${WORK}" ]]; then
    echo "  Creating WORK directory" >> $LOGFILE
	mkdir -p "${WORK}"
    chmod a+rwx "${WORK}"
fi

cd "${WORK}"

echo "Checking for APP directory $APP" >> $LOGFILE
if [[ ! -d "${APP}" ]]; then
    echo "  Creating APP directory" >> $LOGFILE
    mkdir -p "${APP}"
    chmod a+rwx "${APP}"
fi

exit
'

##  Parse through the dirs.txt to get the directories to create
if [[ -s $DIR_LIST ]];
then
    for line in $(cat $DIR_LIST); do
        field1=$(echo $line | cut -f1 -d:)
        echo "Verify/create components $field1 folder on host" | tee -a $LOCAL_LOG_DIR/$LOG_FILE
        echo "ssh $USER_HOST DIR=$field1 OUT=$OUT_DIR LOGFILE=$LOG_FILE" | tee -a $LOCAL_LOG_DIR/$LOG_FILE
        ssh $USER_HOST DIR=$field1 OUT=$OUT_DIR LOGFILE=$LOG_FILE '
        LOGFILE=$OUT/$LOGFILE
        echo "Checking for component directory $DIR" >> $LOGFILE
        if [[ ! -d "${DIR}" ]]; then
             echo "  Creating component directory" >> $LOGFILE
            mkdir -p "${DIR}"
            chmod a+rwx "${DIR}"
        fi
        exit
        '
    done
else
     echo " "
     echo "#######"
     echo "## $DIR_LIST is empty "
     echo "## No component directories will be created "
     echo "#######"
     echo " ";
fi


echo "Creating .setup file" | tee -a $LOCAL_LOG_DIR/$LOG_FILE
echo "ssh $USER_HOST WORK=$WORK_DIR APP=$APP_DIR" | tee -a $LOCAL_LOG_DIR/$LOG_FILE
ssh $USER_HOST WORK=$WORK_DIR APP=$APP_DIR '
cd "${WORK}/${APP}"
echo "Creating .setup file"
echo "Setup finished" >> .setup
exit
'

#echo 'Component Directories created' | tee -a $LOCAL_LOG_DIR/$LOG_FILE
echo 'User Build Setup Finished' | tee -a $LOCAL_LOG_DIR/$LOG_FILE
