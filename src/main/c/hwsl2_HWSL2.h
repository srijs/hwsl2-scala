/* DO NOT EDIT THIS FILE - it is machine generated */
#include <jni.h>
/* Header for class hwsl2_HWSL2 */

#ifndef _Included_hwsl2_HWSL2
#define _Included_hwsl2_HWSL2
#ifdef __cplusplus
extern "C" {
#endif
/*
 * Class:     hwsl2_HWSL2
 * Method:    valid
 * Signature: (Ljava/nio/ByteBuffer;)Z
 */
JNIEXPORT jboolean JNICALL Java_hwsl2_HWSL2_valid
  (JNIEnv *, jclass, jobject);

/*
 * Class:     hwsl2_HWSL2
 * Method:    eq
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)Z
 */
JNIEXPORT jboolean JNICALL Java_hwsl2_HWSL2_eq
  (JNIEnv *, jclass, jobject, jobject);

/*
 * Class:     hwsl2_HWSL2
 * Method:    cmp
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)I
 */
JNIEXPORT jint JNICALL Java_hwsl2_HWSL2_cmp
  (JNIEnv *, jclass, jobject, jobject);

/*
 * Class:     hwsl2_HWSL2
 * Method:    copy
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_hwsl2_HWSL2_copy
  (JNIEnv *, jclass, jobject, jobject);

/*
 * Class:     hwsl2_HWSL2
 * Method:    unit
 * Signature: (Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_hwsl2_HWSL2_unit
  (JNIEnv *, jclass, jobject);

/*
 * Class:     hwsl2_HWSL2
 * Method:    mulBufRight
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;J)V
 */
JNIEXPORT void JNICALL Java_hwsl2_HWSL2_mulBufRight
  (JNIEnv *, jclass, jobject, jobject, jlong);

/*
 * Class:     hwsl2_HWSL2
 * Method:    mulBufLeft
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;J)V
 */
JNIEXPORT void JNICALL Java_hwsl2_HWSL2_mulBufLeft
  (JNIEnv *, jclass, jobject, jobject, jlong);

/*
 * Class:     hwsl2_HWSL2
 * Method:    mul
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_hwsl2_HWSL2_mul
  (JNIEnv *, jclass, jobject, jobject, jobject);

/*
 * Class:     hwsl2_HWSL2
 * Method:    serialize
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_hwsl2_HWSL2_serialize
  (JNIEnv *, jclass, jobject, jobject);

/*
 * Class:     hwsl2_HWSL2
 * Method:    unserialize
 * Signature: (Ljava/nio/ByteBuffer;Ljava/nio/ByteBuffer;)V
 */
JNIEXPORT void JNICALL Java_hwsl2_HWSL2_unserialize
  (JNIEnv *, jclass, jobject, jobject);

#ifdef __cplusplus
}
#endif
#endif
