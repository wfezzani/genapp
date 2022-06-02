#!/bin/sh
################################################################
# LICENSED MATERIALS - PROPERTY OF IBM
# "RESTRICTED MATERIALS OF IBM"
# (C) COPYRIGHT IBM CORPORATION 2019. ALL RIGHTS RESERVED
# US GOVERNMENT USERS RESTRICTED RIGHTS - USE, DUPLICATION,
# OR DISCLOSURE RESTRICTED BY GSA ADP SCHEDULE
# CONTRACT WITH IBM CORPORATION
################################################################

USER_HOST=$1          #USER_HOST='userid@host'
WORK_DIR=$2           #WORK_DIR='parent directory'
APP_DIR=$3            #APPLICATION='application folder under parent'
OUT_DIR=$4            #OUT_DIR='uss directory location of output log files'
LOCAL_DIR=$5          #LOCAL_DIR='local working directory'
HIGH_LVL_QUAL=$6      #HIGH_LVL_QUAL='tso hlq where dbb will copy programs and dependencies'
FILE_LIST=$7          #FILE_LIST='file containing components for the build'
BUILD_SCRIPT=$8       #BUILD_SCRIPT='fully qualified path to build.groovy'


## Create local log file and use the same name to pass to ssh commands
LOG_FILE_NAME="`date +%Y-%m-%d-%H-%M-%S`.log"
LOG_FILE=$LOCAL_DIR/logs/$LOG_FILE_NAME
echo Log file $LOG_FILE

echo '#######################' | tee -a $LOG_FILE
echo '# Starting User Build #' | tee -a $LOG_FILE
echo '#######################' | tee -a $LOG_FILE

## Check for .setup file in APP directory on host, if not there, exit and prompt user to run User Build Setup.
echo ' ' | tee -a $LOG_FILE
echo '###########################' | tee -a $LOG_FILE
echo '# Checking for .setup file' | tee -a $LOG_FILE
echo '###########################' | tee -a $LOG_FILE
echo ' ' | tee -a $LOG_FILE
echo "  ssh -p 1022 $USER_HOST WORK=$WORK_DIR APP=$APP_DIR " 2>&1 | tee -a $LOG_FILE

# Check for the existence of the .setup file
x=$(ssh -p 1022 $USER_HOST WORK=$WORK_DIR APP=$APP_DIR  '
   if [[ -e $WORK/$APP/.setup ]];
   then
      returncode=0
   else
      returncode=99
   fi
echo $returncode
')

echo "Checking return code $x"

if [ $x -eq 99 ];
then
   echo ' ' | tee -a $LOG_FILE
   echo '#########################################################' | tee -a $LOG_FILE
   echo ' Setup has not run - Please run the User Build Setup task ' | tee -a $LOG_FILE
   echo ' Exiting User Build  ' | tee -a $LOG_FILE
   echo '#########################################################' | tee -a $LOG_FILE
   echo ' ' | tee -a $LOG_FILE
   exit 1;
fi

##  Define build list file to send to DBB
BUILD_LIST="/buildList.txt"
echo "  rm $LOCAL_DIR$BUILD_LIST"
rm $LOCAL_DIR$BUILD_LIST 2>&1 | tee -a $LOG_FILE


##  Parse through the upload-files.txt to get the upload type, the from location, and the to location
if [[ -s $FILE_LIST ]];
then
   echo "* $FILE_LIST is populated *" | tee -a $LOCAL_LOG_DIR/$LOG_FILE
   for line in $(cat $FILE_LIST); do
       field1=$(echo $line | cut -f1 -d:)
       field2=$(echo $line | cut -f2 -d:)
       field3=$(echo $line | cut -f3 -d:)
       if [ $field1 = "f" ]
       then
          zowe_option="file-to-uss"
       else
          zowe_option="dir-to-uss"
       fi

       echo 'Uploading component files' | tee -a $LOG_FILE

       echo "  zowe zos-files upload $zowe_option "$field2" "$field3"" | tee -a $LOG_FILE
       zowe zos-files upload $zowe_option "$field2" "$field3" | tee -a $LOG_FILE

       echo $field3 >> $LOCAL_DIR$BUILD_LIST | tee -a $LOG_FILE
   done
else
     echo " " | tee -a $LOG_FILE
     echo "#######" | tee -a $LOG_FILE
     echo "## $FILE_LIST is empty " | tee -a $LOG_FILE
     echo "## Exiting User Build " | tee -a $LOG_FILE
     echo "#######" | tee -a $LOG_FILE
     echo " " | tee -a $LOG_FILE
     exit 1;
fi

## Upload build list file for dbb
echo 'Upload build list to USS' 2>&1 | tee -a $LOG_FILE
echo "  zowe zos-file upload file-to-uss "$LOCAL_DIR$BUILD_LIST" "/$WORK_DIR$BUILD_LIST"" | tee -a $LOG_FILE
zowe zos-files upload file-to-uss "$LOCAL_DIR$BUILD_LIST" "/$WORK_DIR/$APP_DIR/$BUILD_LIST" | tee -a $LOG_FILE


echo 'Files copied' | tee -a $LOG_FILE

## Submit build.groovy user build
echo 'Submitting build.groovy user build' 2>&1 | tee -a $LOG_FILE
echo "  ssh -p 1022 $USER_HOST HLQ=$HIGH_LVL_QUAL WORK=$WORK_DIR APP=$APP_DIR OUT=$OUT_DIR LOGFILE=$LOG_FILE_NAME BLDLIST=$BUILD_LIST BLDSCRPT=$BUILD_SCRIPT" 2>&1 | tee -a $LOG_FILE
ssh -p 1022 $USER_HOST HLQ=$HIGH_LVL_QUAL WORK=$WORK_DIR APP=$APP_DIR OUT=$OUT_DIR LOGFILE=$LOG_FILE_NAME BLDLIST=$BUILD_LIST BLDSCRPT=$BUILD_SCRIPT '

echo "$DBB_HOME/bin/groovyz -DBB_DAEMON_PORT 8080 -DBB_DAEMON_HOST 127.0.0.1 $BLDSCRPT --userBuild -h $HLQ -w $WORK -a $APP -o $WORK/$APP/logs $WORK/$APP$BLDLIST"
$DBB_HOME/bin/groovyz -DBB_DAEMON_PORT 8080 -DBB_DAEMON_HOST 127.0.0.1 $BLDSCRPT --userBuild -h $HLQ -w $WORK -a $APP -o $WORK/$APP/logs $WORK/$APP$BLDLIST | tee -a $OUT/$LOGFILE
exit
'

echo 'User Build Finished' | tee -a $LOG_FILE
