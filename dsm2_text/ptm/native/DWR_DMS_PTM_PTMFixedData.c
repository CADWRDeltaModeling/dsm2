#include <stdio.h>
#include <string.h>
#include "DWR_DMS_PTM_PTMFixedData.h"
#include "fixedData.h"

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    initialize
 * Signature: (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_DWR_DMS_PTM_PTMFixedData_initialize
(JNIEnv *env, jobject fixedData, jstring filename){
  char localFilename[LEN3];
  const char * lFilename = env->GetStringUTFChars(filename, JNI_FALSE);
  strcpy(localFilename, lFilename);
  int filenameLength = (int)env->GetStringUTFLength(filename);
  initFixedData(localFilename, filenameLength);
  /*  env->ReleaseStringUTFChars(filename, localFilename);*/
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfWaterbodies
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfWaterbodies
(JNIEnv * env, jobject thisOne){
  jint number = (jint) getNumberOfWaterbodies();
  return number;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfChannels
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfChannels
(JNIEnv * env, jobject thisOne){
  jint nChannels = (jint) getNumberOfChannels();
  return nChannels;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfChannelGroups
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfChannelGroups
(JNIEnv * env, jobject thisOne){
  jint nGroups = (jint) getNumberOfChannelGroups();
  return nGroups;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfReservoirs
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfReservoirs
(JNIEnv * env, jobject thisOne){
  jint nReservoirs = (jint) getNumberOfReservoirs();
  return nReservoirs;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfDiversions
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfDiversions
(JNIEnv * env, jobject thisOne){
  jint diversions = getNumberOfDiversions();
  return diversions;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfPumps
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfPumps
(JNIEnv * env, jobject thisOne){
  jint Pumps = getNumberOfPumps();
  return Pumps;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfBoundaryWaterbodies
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfBoundaryWaterbodies
(JNIEnv * env, jobject thisOne){
  jint BoundaryWaterbodies = getNumberOfBoundaryWaterbodies();
  return BoundaryWaterbodies;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfConveyors
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfConveyors
(JNIEnv *env, jobject thisObject){
  jint number = getNumberOfConveyors();
  return number;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfNodes
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfNodes
(JNIEnv * env, jobject thisOne){
  jint Nodes = getNumberOfNodes();
  return Nodes;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfXSections
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfXSections
(JNIEnv * env, jobject thisOne){
  jint XSections = getNumberOfXsections();
  return XSections;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfWaterbodies
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfWaterbodies
(JNIEnv *env, jclass thisClass){
  jint nwbs = getMaximumNumberOfWaterbodies();
  return nwbs;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfChannels
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfChannels
(JNIEnv * env, jclass thisClass){
  jint channels = getMaximumNumberOfChannels();
  return channels;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfReservoirs
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfReservoirs
  (JNIEnv * env, jclass thisClass){
  jint reservoirs = getMaximumNumberOfReservoirs();
  return reservoirs;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfDiversions
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfDiversions
  (JNIEnv * env, jclass thisClass){
  jint diversions = getMaximumNumberOfDiversions();
  return diversions;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfPumps
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfPumps
  (JNIEnv * env, jclass thisClass){
  jint pumps = getMaximumNumberOfPumps();
  return pumps;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfBoundaryWaterbodies
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfBoundaryWaterbodies
  (JNIEnv * env, jclass thisClass){
  jint boundaryWaterbodies = getMaximumNumberOfBoundaryWaterbodies();
  return boundaryWaterbodies;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfStageBoundaries
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfStageBoundaries
(JNIEnv * env, jclass thisClass){
  jint number = getMaximumNumberOfStageBoundaries();
  return number;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfConveyors
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfConveyors
(JNIEnv *env, jclass thisClass){
  jint number = getMaximumNumberOfConveyors();
  return number;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfNodes
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfNodes
  (JNIEnv * env, jclass thisClass){
  jint nodes = getMaximumNumberOfNodes();
  return nodes;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfXSections
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfXSections
  (JNIEnv * env, jclass thisClass){
  jint xsections = getMaximumNumberOfXsections();
  return xsections;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getMaximumNumberOfReservoirNodes
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getMaximumNumberOfReservoirNodes
(JNIEnv *env, jclass thisClass){
  jint nresnodes = getMaximumNumberOfReservoirNodes();
  return nresnodes;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getUniqueIdForChannel
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getUniqueIdForChannel
(JNIEnv *env, jclass thisClas, jint i){
  int localId = (int) i;
  jint id = getUniqueIdForChannel(&localId);
  return id;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getUniqueIdForReservoir
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getUniqueIdForReservoir
  (JNIEnv *env, jclass thisClas, jint i){
  int lId = (int) i;
  jint id = getUniqueIdForReservoir(&lId);
  return id;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getUniqueIdForBoundary
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getUniqueIdForBoundary
  (JNIEnv *env, jclass thisClas, jint i){
  int lId = (int) i;
  jint id = getUniqueIdForBoundary(&lId);
  return id;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getUniqueIdForStageBoundary
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getUniqueIdForStageBoundary
(JNIEnv *env, jclass thisClas, jint i){
  int lId = (int) i;
  jint id = getUniqueIdForStageBoundary(&lId);
  return id;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getUniqueIdForConveyor
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getUniqueIdForConveyor
  (JNIEnv *env, jclass thisClas, jint i){
  int lId = (int) i;
  jint id = getUniqueIdForConveyor(&lId);
  return id;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getWaterbodyType
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getWaterbodyType
(JNIEnv *env , jobject thisObject, jint i){
  int nId = (int) i;
  jint type = getWaterbodyType(&nId);
  return type;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getWaterbodyGroup
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getWaterbodyGroup
(JNIEnv *env , jobject thisObject, jint i){
  int nId = (int) i;
  jint group = getWaterbodyGroup(&nId);
  return group;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getLocalIdForWaterbody
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getLocalIdForWaterbody
(JNIEnv *env, jobject thisObject, jint i){
  int nId = (int) i;
  jint lId = getLocalIdForWaterbody(&nId);
  return lId;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNodeArrayForWaterbody
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNodeArrayForWaterbody
(JNIEnv *env, jobject thisObject, jint i){
  int array[LEN1];
  int number = (int) i;
  int numberOfNodes = getNumberOfNodesForWaterbody(&number);
  jintArray nodeArrayJava = env->NewIntArray((jsize) numberOfNodes);
  getNodeArrayForWaterbody(&number, array);
  env->SetIntArrayRegion(nodeArrayJava, 
			 (jsize) 0, (jsize) numberOfNodes, 
			 (jint*) array);
  return nodeArrayJava;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getChannelLength
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getChannelLength
(JNIEnv *env, jobject thisOne, jint i){
  int channelNumber = (int)i;
  jint length = (jint) getChannelLength(&channelNumber);
  return length;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getChannelNodeArray
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getChannelNodeArray
(JNIEnv * env, jobject thisOne, jint i){
  int array[LEN1];
  int channelNumber = (int) i;
  int numberOfNodes = getChannelNumberOfNodes(&channelNumber);
  jintArray nodeArrayJava = env->NewIntArray((jsize) numberOfNodes);
  getChannelNodeArray(&channelNumber, array);
  env->SetIntArrayRegion(nodeArrayJava, 
			 (jsize) 0, (jsize) numberOfNodes, 
			 (jint*) array);
  return nodeArrayJava;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getChannelXSectionIds
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getChannelXSectionIds
(JNIEnv * env, jobject thisOne, jint i){
  int array[LEN1];
  int channelNumber = (int) i;
  int numberOfXSections = getChannelNumberOfXsections(&channelNumber);
  jintArray arrayJava = env->NewIntArray((jsize) numberOfXSections);
  getChannelXsectionIds(&channelNumber, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) numberOfXSections, 
			 (jint*) array);
  return arrayJava;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getChannelXSectionDistances
 * Signature: (I)[I
 */
JNIEXPORT jfloatArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getChannelXSectionDistances
(JNIEnv * env, jobject thisOne, jint i){
  float array[LEN1];
  int channelNumber = (int) i;
  int numberOfXSections = getChannelNumberOfXsections(&channelNumber);
  jfloatArray arrayJava = env->NewFloatArray((jsize) numberOfXSections);
  getChannelXsectionDistances(&channelNumber, array);
  env->SetFloatArrayRegion(arrayJava, 
			   (jsize) 0, (jsize) numberOfXSections, 
			   (jfloat*) array);
  return arrayJava;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getReservoirArea
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMFixedData_getReservoirArea
(JNIEnv * env, jobject thisOne, jint i){
  int reservoirNumber = (int)i;
  jfloat area = (jint) getReservoirArea(&reservoirNumber);
  return area;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getReservoirBottomElevation
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMFixedData_getReservoirBottomElevation
(JNIEnv * env, jobject thisOne, jint i){
  int reservoirNumber = (int)i;
  jfloat bottomElevation = (jint) getReservoirArea(&reservoirNumber);
  return bottomElevation;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getReservoirName
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_DWR_DMS_PTM_PTMFixedData_getReservoirName
(JNIEnv * env, jobject thisOne, jint i){
  static char name[LEN2];
  int reservoirNumber = (int) i;
  getReservoirName(&reservoirNumber, name, LEN2);
  //  name[nameLength]='\0';
  const char* exactName = name;
  jstring nameJava = env->NewStringUTF(exactName);
  return nameJava;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getReservoirNodeArray
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getReservoirNodeArray
(JNIEnv * env, jobject thisOne, jint i){
  int array[LEN1];
  int reservoirNumber = (int) i;
  int arrayLen = getReservoirNumberOfNodes(&reservoirNumber);
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getReservoirNodeArray(&reservoirNumber, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getDiversionNodeArray
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getDiversionNodeArray
(JNIEnv *env, jobject thisOne, jint i){
  int array[LEN1];
  int diversionNumber = (int) i;
  int arrayLen = getDiversionNumberOfNodes(&diversionNumber);
  if(arrayLen <= 0) return NULL;
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getDiversionNodeArray(&diversionNumber, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getPumpNodeArray
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getPumpNodeArray
(JNIEnv *env, jobject thisOne, jint i){
  int array[LEN1];
  int pumpNumber = (int) i;
  int arrayLen = getPumpNumberOfNodes(&pumpNumber);
  if(arrayLen <= 0) return NULL;
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getPumpNodeArray(&pumpNumber, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getBoundaryWaterbodyNodeArray
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getBoundaryWaterbodyNodeArray
(JNIEnv *env, jobject thisOne, jint i){
  int array[LEN1];
  int boundaryWaterbodyNumber = (int) i;
  int arrayLen = getBoundaryWaterbodyNumberOfNodes(&boundaryWaterbodyNumber);
  if(arrayLen <= 0) return NULL;
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getBoundaryWaterbodyNodeArray(&boundaryWaterbodyNumber, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getConveyorNodeArray
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getConveyorNodeArray
(JNIEnv *env, jobject thisObject, jint i){
  int array[LEN1];
  int number = (int) i;
  int arrayLen = getConveyorNumberOfNodes(&number);
  if(arrayLen <= 0) return NULL;
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getConveyorNodeArray(&number, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfWaterbodiesForNode
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfWaterbodiesForNode
(JNIEnv * env, jobject thisOne, jint i){
  int nodeNumber = (int) i;
  jint number = (jint) getMaximumNumberOfNodes();
  return number; 
}

/*
 * Class:     DWR_DMS_PTM_PTMFiWaterbodiesForNodeedData
 * Method:    getBoundaryTypeForNode
 * Signature: (I)Ljava/lang/String;
 */
JNIEXPORT jstring JNICALL Java_DWR_DMS_PTM_PTMFixedData_getBoundaryTypeForNode
(JNIEnv * env, jobject thisOne, jint i){
  static char name[LEN2];
  int nodeNumber = (int) i;
  getBoundaryTypeForNode(&nodeNumber, name, LEN2);
  //  name[nameLength]='\0';
  const char* exactName = name;
  jstring nameJava = env->NewStringUTF(exactName);
  return nameJava;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getWaterbodyIdArrayForNode
 * Signature: (I)[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getWaterbodyIdArrayForNode
(JNIEnv * env, jobject thisOne, jint i){
  int array[LEN1];
  int nodeNumber = (int) i;
  int arrayLen = getNumberOfWaterbodiesForNode(&nodeNumber);
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getWaterbodyIdArrayForNode(&nodeNumber, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getXSectionWidths
 * Signature: (I)[F
 */
JNIEXPORT jfloatArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getXSectionWidths
(JNIEnv *env,  jobject thisOne, jint number){
  float array[LEN1];
  int index = (int) number;
  //  int arrayLen = getXsectionNumberOfElevations(&index);
  int arrayLen = getXsectionNumberOfElevations();
  jfloatArray arrayJava = env->NewFloatArray((jsize) arrayLen);
  getXsectionWidths(&index, array);
  env->SetFloatArrayRegion(arrayJava, 
			   (jsize) 0, (jsize) arrayLen, 
			   (jfloat*) array);
  return arrayJava;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getXSectionElevations
 * Signature: (I)[F
 */
JNIEXPORT jfloatArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getXSectionElevations
  (JNIEnv *env,  jobject thisOne, jint number){
  float array[LEN1];
  int index = (int) number;
  //  int arrayLen = getXsectionNumberOfElevations(&index);
  int arrayLen = getXsectionNumberOfElevations();
  jfloatArray arrayJava = env->NewFloatArray((jsize) arrayLen);
  getXsectionElevations(&index, array);
  env->SetFloatArrayRegion(arrayJava, 
			   (jsize) 0, (jsize) arrayLen, 
			   (jfloat*) array);
  return arrayJava;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getXSectionAreas
 * Signature: (I)[F
 */
JNIEXPORT jfloatArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getXSectionAreas
  (JNIEnv *env,  jobject thisOne, jint number){
  float array[LEN1];
  int index = (int) number;
  //  int arrayLen = getXsectionNumberOfElevations(&index);
  int arrayLen = getXsectionNumberOfElevations();
  jfloatArray arrayJava = env->NewFloatArray((jsize) arrayLen);
  getXsectionAreas(&index, array);
  env->SetFloatArrayRegion(arrayJava, 
			   (jsize) 0, (jsize) arrayLen, 
			   (jfloat*) array);
  return arrayJava;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getXSectionMinimumElevation
 * Signature: (I)F
 */
JNIEXPORT jfloat JNICALL Java_DWR_DMS_PTM_PTMFixedData_getXSectionMinimumElevation
(JNIEnv *env,  jobject thisOne, jint number){
  int index = (int) number;
  jfloat minimumElevation = (jfloat) getXsectionMinimumElevation(&index);
  return minimumElevation;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getParticleBooleanInputs
 * Signature: ()[Z
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getParticleBooleanInputs
(JNIEnv *env,  jobject thisOne){
  int array[LEN1];
  int arrayLen = 9;
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getParticleBooleanInputs(array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getParticleFloatInputs
 * Signature: ()[F
 */
JNIEXPORT jfloatArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getParticleFloatInputs
  (JNIEnv *env,  jobject thisOne){
  float array[LEN1];
  int arrayLen = 8;
  jfloatArray arrayJava = env->NewFloatArray((jsize) arrayLen);
  getParticleFloatInputs(array);
  env->SetFloatArrayRegion(arrayJava, 
			     (jsize) 0, (jsize) arrayLen, 
			     (jfloat*) array);
  return arrayJava;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getParticleNumberOfInjections
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getParticleNumberOfInjections
(JNIEnv *env,  jobject thisOne){
  jint ninjections = (jint) getParticleNumberOfInjections();
  return ninjections;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getParticleInjectionNodes
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getParticleInjectionNodes
(JNIEnv *env,  jobject thisOne){
  int array[LEN1];
  int arrayLen = getParticleNumberOfInjections();
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getParticleInjectionNodes(array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getParticleNumberOfParticlesInjected
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getParticleNumberOfParticlesInjected
  (JNIEnv *env,  jobject thisOne){
  int array[LEN1];
  int arrayLen = getParticleNumberOfInjections();
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getParticleNumberOfParticlesInjected(array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getParticleInjectionStartJulmin
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getParticleInjectionStartJulmin
  (JNIEnv *env,  jobject thisOne){
  int array[LEN1];
  int arrayLen = getParticleNumberOfInjections();
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getParticleInjectionStartJulmin(array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getParticleInjectionLengthJulmin
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getParticleInjectionLengthJulmin
  (JNIEnv *env,  jobject thisOne){
  int array[LEN1];
  int arrayLen = getParticleNumberOfInjections();
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getParticleInjectionLengthJulmin(array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getNumberOfFluxes
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getNumberOfFluxes
(JNIEnv *env,  jobject thisOne){
  jint i = (jint) getNumberOfFluxes();
  return i;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getFluxIncoming
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getFluxIncoming
(JNIEnv *env,  jobject thisOne, jint number){
  int array[LEN1];
  int index = (int) number;
  int arrayLen = getNumberIncoming(&index);
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getFluxIncoming(&index, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getFluxOutgoing
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getFluxOutgoing
(JNIEnv *env,  jobject thisOne, jint number){
  int array[LEN1];
  int index = (int) number;
  int arrayLen = getNumberOutgoing(&index);
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getFluxOutgoing(&index, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getFluxIncomingType
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getFluxIncomingType
(JNIEnv *env,  jobject thisOne, jint number){
  int array[LEN1];
  int index = (int) number;
  int arrayLen = getNumberIncoming(&index);
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getFluxIncomingType(&index, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getFluxOutgoingType
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getFluxOutgoingType
(JNIEnv *env,  jobject thisOne, jint number){
  int array[LEN1];
  int index = (int) number;
  int arrayLen = getNumberOutgoing(&index);
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getFluxOutgoingType(&index, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getFluxIncomingAccountType
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getFluxIncomingAccountType
(JNIEnv *env,  jobject thisOne, jint number){
  int array[LEN1];
  int index = (int) number;
  int arrayLen = getNumberIncoming(&index);
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getFluxIncomingAccountType(&index, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getFluxOutgoingAccountType
 * Signature: ()[I
 */
JNIEXPORT jintArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getFluxOutgoingAccountType
(JNIEnv *env,  jobject thisOne, jint number){
  int array[LEN1];
  int index = (int) number;
  int arrayLen = getNumberOutgoing(&index);
  jintArray arrayJava = env->NewIntArray((jsize) arrayLen);
  getFluxOutgoingAccountType(&index, array);
  env->SetIntArrayRegion(arrayJava, 
			 (jsize) 0, (jsize) arrayLen, 
			 (jint*) array);
  return arrayJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getModelStartTime
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getModelStartTime
(JNIEnv *env,  jobject thisOne){
  jint i = (jint) getModelStartTime();
  return i;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getModelEndTime
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getModelEndTime
(JNIEnv *env,  jobject thisOne){
  jint i = (jint) getModelEndTime();
  return i;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getPTMTimeStep
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getPTMTimeStep
(JNIEnv *env,  jobject thisOne){
  jint i = (jint) getModelPtmTimeStep();
  return i;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getDisplayInterval
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getDisplayInterval
(JNIEnv *env, jobject thisOne){
  jint i = (jint) getDisplayInterval();
  return i;
}

/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getAnimationFileName
 * Signature: ()Ljava/lang/String{
}

 */
JNIEXPORT jstring JNICALL Java_DWR_DMS_PTM_PTMFixedData_getAnimationFileName
(JNIEnv *env,  jobject thisOne){
  static char name[LEN2];
  getAnimationFilename(name, LEN2);
  //  name[nameLength]='\0';
  const char* exactName = name;
  jstring nameJava = env->NewStringUTF(exactName);
  return nameJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getAnimationOutputInterval
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getAnimationOutputInterval
(JNIEnv *env,  jobject thisOne){
  jint i = (jint) getModelAnimationOutputInterval();
  return i;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getBehaviorFileName
 * Signature: ()Ljava/lang/String{
}

 */
JNIEXPORT jstring JNICALL Java_DWR_DMS_PTM_PTMFixedData_getBehaviorFileName
(JNIEnv *env,  jobject thisOne){
  static char name[LEN2];
  getBehaviorFilename(name, LEN2);
  //  name[nameLength]='\0';
  const char* exactName = name;
  jstring nameJava = env->NewStringUTF(exactName);
  return nameJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getTraceFileName
 * Signature: ()Ljava/lang/String{
}

 */
JNIEXPORT jstring JNICALL Java_DWR_DMS_PTM_PTMFixedData_getTraceFileName
(JNIEnv *env,  jobject thisOne){
  static char name[LEN2];
  getTraceFilename(name, LEN2);
  //  name[nameLength]='\0';
  const char* exactName = name;
  jstring nameJava = env->NewStringUTF(exactName);
  return nameJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getTraceOutputInterval
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getTraceOutputInterval
  (JNIEnv *env,  jobject thisOne){
  jint i = (jint) getModelTraceOutputInterval();
  return i;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getRestartOutputFileName
 * Signature: ()Ljava/lang/String{
}

 */
JNIEXPORT jstring JNICALL Java_DWR_DMS_PTM_PTMFixedData_getRestartOutputFileName
(JNIEnv *env,  jobject thisOne){
  static char name[LEN2];
  getRestartOutputFilename(name, LEN2);
  //  name[nameLength]='\0';
  const char* exactName = name;
  jstring nameJava = env->NewStringUTF(exactName);
  return nameJava; 
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getRestartOutputInterval
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getRestartOutputInterval
  (JNIEnv *env,  jobject thisOne){
  jint i = (jint) getRestartOutputInterval();
  return i;
}


/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getRestartInputFileName
 * Signature: ()Ljava/lang/String{
}

 */
JNIEXPORT jstring JNICALL Java_DWR_DMS_PTM_PTMFixedData_getRestartInputFileName
(JNIEnv *env,  jobject thisOne){
  static char name[LEN2];
  getRestartInputFilename(name, LEN2);
  //  name[nameLength]='\0';
  const char* exactName = name;
  jstring nameJava = env->NewStringUTF(exactName);
  return nameJava; 
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getWaterbodyAccountingType
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getWaterbodyAccountingType
(JNIEnv *env, jobject thisObject, jint id){
  int wbId = (int) id;
  jint typeId;
  typeId = (jint) getWaterbodyAccountingType(&wbId);
  return typeId;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getWaterbodyObjectType
 * Signature: (I)I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_getWaterbodyObjectType
(JNIEnv *env, jobject thisObject, jint id){
  int wbId = (int) id;
  jint typeId;
  typeId = (jint) getWaterbodyObjectType(&wbId);
  return typeId;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    doesQualBinaryExist
 * Signature: ()I
 */
JNIEXPORT jint JNICALL Java_DWR_DMS_PTM_PTMFixedData_doesQualBinaryExist
  (JNIEnv *env,  jclass thisClass){
  jint i = (jint) doesQualBinaryExist();
  return i;
}
/*
 * Class:     DWR_DMS_PTM_PTMFixedData
 * Method:    getQualConstituentNames
 * Signature: ()[Ljava/lang/String;
 */
JNIEXPORT jobjectArray JNICALL Java_DWR_DMS_PTM_PTMFixedData_getQualConstituentNames
(JNIEnv *env,  jclass thisClass){
  static char name[LEN4];
  jstring tmpstr;
  jclass clazz = env->FindClass("java/lang/String");
  int arrayLen = getNumberConstituents();
  jobjectArray arrayJava = env->NewObjectArray((jsize) arrayLen, clazz, NULL);
  for (int index = 0; index < arrayLen; index++){
    int index2 = index+1;
    getQualConstituentNames(&index2, name, LEN4);
    const char* exactName = name;
    tmpstr = env->NewStringUTF(exactName);
    env->SetObjectArrayElement(arrayJava, index, tmpstr);
    env->DeleteLocalRef(tmpstr);
  }
  return arrayJava;
}


