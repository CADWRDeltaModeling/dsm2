#include <stdio.h>
#include <string.h>
#include "DWR_DMS_PTM_Globals.h"
#include "fixedData.h"
/*
 * Class:     DWR_DMS_PTM_Globals
 * Method:    getModelDate
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_DWR_DMS_PTM_Globals_getModelDate
(JNIEnv *env , jclass thisOne, jint i){
  int julianMin = (int) i;
  char date[LEN2];
  getModelDate(&julianMin, date, LEN2);
  const char* exactName = date;
  jstring nameJava = env->NewStringUTF(exactName);
  return nameJava;
}

/*
 * Class:     DWR_DMS_PTM_Globals
 * Method:    getModelTime
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_DWR_DMS_PTM_Globals_getModelTime
(JNIEnv *env, jclass thisOne, jint i){
  int julianMin = (int) i;
  char time[LEN2];
  getModelTime(&julianMin, time, LEN2);
  const char* exactName = time;
  jstring nameJava = env->NewStringUTF(exactName);
  return nameJava;
}

/*
 * Class:     DWR_DMS_PTM_Globals
 * Method:    getTimeInJulianMins
 * Signature: (Ljava/lang/String;Ljava/lang/String;)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_Globals_getTimeInJulianMins
(JNIEnv *env, jclass thisOne, jstring jstr1, jstring jstr2){
  const char * date = env->GetStringUTFChars(jstr1,JNI_FALSE);
  int dateLen = (int)env->GetStringUTFLength(jstr1);
  const char * time = env->GetStringUTFChars(jstr2,JNI_FALSE);
  int timeLen = (int)env->GetStringUTFLength(jstr2);
  char dateTime[LEN2];
  ::strcpy(dateTime,date);
  ::strcat(dateTime,time);
  jint julianMin = (jint) cdt2jmin(dateTime, dateLen+timeLen);
  return julianMin;
}
