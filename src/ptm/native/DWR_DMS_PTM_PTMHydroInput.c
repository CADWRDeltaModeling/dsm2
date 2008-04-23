#include <stdio.h>
#include "DWR_DMS_PTM_PTMHydroInput.h"
#include "dynamicData.h"
/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    readMultTide
 * Signature: (I)V
 */
JNIEXPORT void JNICALL Java_DWR_DMS_PTM_PTMHydroInput_readMultTide
(JNIEnv * env, jobject obj, jint i){
  int modelTime = (int) i;
  setTideFileTime(&modelTime);
  readMultTide();
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getExtFromInt
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getExtFromInt
(JNIEnv * env, jobject thisOne, jint i){
  int internal = (int) i;
  return (jint) getExtFromInt(&internal);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getUpNodeDepth
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getUpNodeDepth
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getUpNodeDepth(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getDownNodeDepth
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getDownNodeDepth
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getDownNodeDepth(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getUpNodeStage
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getUpNodeStage
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getDownNodeStage(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getDownNodeStage
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getDownNodeStage
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getDownNodeStage(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getUpNodeFlow
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getUpNodeFlow
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getUpNodeFlow(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getDownNodeFlow
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getDownNodeFlow
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getDownNodeFlow(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getUpNodeArea
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getUpNodeArea
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getUpNodeArea(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getDownNodeArea
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getDownNodeArea
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getDownNodeArea(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getReservoirVolume
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getReservoirVolume
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getReservoirVolume(&number);
}
/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getNodeNumberForConnection
 * Signature: (II)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getNodeNumberForConnection
(JNIEnv * env, jobject thisOne, jint ii, jint i){
  int reservoirNumber = (int) ii;
  int connection = (int) i;
  return (jint) getNodeNumberForConnection(&reservoirNumber, &connection);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getReservoirDepth
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getReservoirDepth
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getResevoirDepth(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getReservoirFlowForConnection
 * Signature: (II)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getReservoirFlowForConnection
(JNIEnv * env, jobject thisOne, jint ii, jint i){
  int number = (int) i;
  int reservoirNumber = (int) ii;
  int connection = (int) i;
  return (jfloat) getReservoirFlowForConnection(&reservoirNumber, &connection);
}
/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getDiversionAtNode
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getDiversionAtNode
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getDiversionAtNode(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getReservoirPumping
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getReservoirPumping
(JNIEnv * env, jobject thisOne, jint i){
  int number = (int) i;
  return (jfloat) getReservoirPumping(&number);
}
/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getBoundaryFlow
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getBoundaryFlow
(JNIEnv *env, jobject thisObject, jint i){
  int number = (int) i;
  return (jfloat) getBoundaryFlow(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getStageBoundaryFlow
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getStageBoundaryFlow
(JNIEnv *env, jobject thisObject, jint i){
  int number = (int) i;
  return (jfloat) getStageBoundaryFlow(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getConveyorFlow
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getConveyorFlow
(JNIEnv *env, jobject thisObject, jint i){
  int number = (int) i;
  return (jfloat) getConveyorFlow(&number);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getUpNodeQuality
 * Signature: (I,I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getUpNodeQuality
(JNIEnv * env, jobject thisOne, jint i, jint ii){
  int number = (int) i;
  int constituent = (int) ii;
  return (jfloat) getUpNodeQuality(&number,&constituent);
}

/*
 * Class:     DWR_DMS_PTM_PTMHydroInput
 * Method:    getDownNodeQuality
 * Signature: (I,I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMHydroInput_getDownNodeQuality
(JNIEnv * env, jobject thisOne, jint i, jint ii){
  int number = (int) i;
  int constituent = (int) ii;
  return (jfloat) getDownNodeQuality(&number,&constituent);
}
