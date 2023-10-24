#include "DWR_DMS_PTM_PTMFluxOutput.h"
#include "fixedData.h"
/*
 * Class:     DWR_DMS_PTM_PTMFluxOutput
 * Method:    initializeFluxOutput
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_DWR_DMS_PTM_PTMFluxOutput_initializeFluxOutput
(JNIEnv *env, jobject thisOne, jint i){
  int startTime = (int) i;
  initFluxOutput();
}

/*
 * Class:     DWR_DMS_PTM_PTMFluxOutput
 * Method:    writeFluxOutput
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_DWR_DMS_PTM_PTMFluxOutput_writeFluxOutput
(JNIEnv *env, jobject thisOne){
  writeFluxOutput();
}

/*
 * Class:     DWR_DMS_PTM_PTMFluxOutput
 * Method:    closeFluxOutput
 * Signature: ()V
 */
JNIEXPORT void JNICALL Java_DWR_DMS_PTM_PTMFluxOutput_closeFluxOutput
(JNIEnv *env, jobject thisOne){
  closeFluxOutput();
}

/*
 * Class:     DWR_DMS_PTM_PTMFluxOutput
 * Method:    setFlux
 * Signature: (IF)V
 */
JNIEXPORT void JNICALL Java_DWR_DMS_PTM_PTMFluxOutput_setFlux
(JNIEnv *env, jobject thisOne, jint i, jfloat f){
  int fluxId = (int) i;
  float fluxValue = (float) f;
  setFlux(&fluxId, &fluxValue);
}

/*
 * Class:     DWR_DMS_PTM_PTMFluxOutput
 * Method:    setGroup
 * Signature: (IF)V
 */
JNIEXPORT void JNICALL Java_DWR_DMS_PTM_PTMFluxOutput_setGroup
(JNIEnv *env, jobject thisOne, jint i, jfloat f){
  int groupId = (int) i;
  float groupValue = (float) f;
  setGroup(&groupId, &groupValue);
}
