#include "chiseltest_simulator_jni_JniAPI.h"
#include <jni.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <pthread.h>
#define _GNU_SOURCE
#include <dlfcn.h>

// Maps ID --> ptr to shared object
void *map[1024];
// void *add_one_ptrs[1024];
void *sim_init_ptrs[1024];
void *step_ptrs[1024];
void *finish_ptrs[1024];
void *update_ptrs[1024];
void *reset_coverage_ptrs[1024];
void *write_coverage_ptrs[1024];
void *poke_ptrs[1024];
void *peek_ptrs[1024];
void *poke_wide_ptrs[1024];
void *peek_wide_ptrs[1024];
void *set_args_ptrs[1024];

// id_counter (increment by one)
pthread_mutex_t id_lock;
int id_counter = 0; // synchronize the id_counter to be thread-safe

/* Native implementation for load_so (see jni_api in jni_api_test.scala) */
JNIEXPORT jint JNICALL Java_chiseltest_simulator_jni_JniAPI_load_1so
  (JNIEnv* env, jobject obj, jstring path) {
    // Convert jstring --> C string
    const char* path_str = (*env)->GetStringUTFChars(env, path, 0);
    // char cap[128];
    // strcpy(cap, path_str);
    // printf("%s", cap);

    // Load shared object
    void *so_handle = dlopen(path_str, RTLD_LAZY);
    (*env)->ReleaseStringUTFChars(env, path, path_str);

    // Check if load was successful
    if (so_handle == NULL) {
        fprintf (stderr, "%s\n", dlerror());
        exit(1);
    }

    pthread_mutex_lock(&id_lock);
    int so_id = id_counter;
    id_counter++;
    pthread_mutex_unlock(&id_lock);

    // // Declare desired function
    // int (*add_one)(int);

    // // Get function pointer for native add_one implementation
    // *(void **) (&add_one) = dlsym(so_handle, "add_one_c");

    char *error;
    if ((error = dlerror()) != NULL)  {
        fprintf(stderr, "%s\n", error);
        exit(EXIT_FAILURE);
    }

    // Store pointer to shared object in map
    // map[so_id] = so_handle; // pointer to SO
    // add_one_ptrs[so_id] = add_one;

    /* Accessing Harness APIs */
    // TODO: make it into some for loop for simplification

    void * (*sim_init)();
    *(void **) (&sim_init) = dlsym(so_handle, "sim_init");
    sim_init_ptrs[so_id] = sim_init;

    int64_t (*step)(int32_t);
    *(void **) (&step) = dlsym(so_handle, "step");
    step_ptrs[so_id] = step;

    void (*update)(void *);
    *(void **) (&update) = dlsym(so_handle, "update");
    update_ptrs[so_id] = update;

    void (*finish)(void *);
    *(void **) (&finish) = dlsym(so_handle, "finish");
    finish_ptrs[so_id] = finish;

    void (*reset_coverage)(void *);
    *(void **) (&reset_coverage) = dlsym(so_handle, "resetCoverage");
    reset_coverage_ptrs[so_id] = reset_coverage;

    void (*write_coverage)(void *);
    *(void **) (&write_coverage) = dlsym(so_handle, "writeCoverage");
    write_coverage_ptrs[so_id] = write_coverage;

    void (*poke)(void *, int32_t, int64_t);
    *(void **) (&poke) = dlsym(so_handle, "poke");
    poke_ptrs[so_id] = poke;

    int (*peek)(void *, int32_t);
    *(void **) (&peek) = dlsym(so_handle, "peek");
    peek_ptrs[so_id] = peek;

    void (*poke_wide)(void *, int32_t, int32_t, int64_t);
    *(void **) (&poke_wide) = dlsym(so_handle, "poke_wide");
    poke_wide_ptrs[so_id] = poke_wide;

    int64_t (*peek_wide)(void *, int32_t, int32_t);
    *(void **) (&peek_wide) = dlsym(so_handle, "peek_wide");
    peek_wide_ptrs[so_id] = peek_wide;

    void (*set_args)(void *, int32_t, char**);
    *(void **) (&set_args) = dlsym(so_handle, "set_args");
    set_args_ptrs[so_id] = set_args;

    // Return shared object id
    return so_id;
  }

JNIEXPORT jlong JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1sim_1init
  (JNIEnv *env, jobject obj, jint so_id) {
    void * (*sim_init)();
    *(void **) (&sim_init) = sim_init_ptrs[so_id];

    // Invoke API on given shared object
    return (long int) sim_init();
  }

/*
 * Class:      jni_1api_JniAPI
 * Method:     call_1step
 * Signature:  (I)I
 */
JNIEXPORT jlong JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1step
  (JNIEnv *env, jobject obj, jint so_id, jlong s, jint cycles) {
    int64_t (*step)(void *, int32_t);
    *(void **) (&step) = step_ptrs[so_id];

    return step((void *) s, cycles);
  }

/*
 * Class:      jni_1api_JniAPI
 * Method:     call_1update
 * Signature:  ()V
 */
JNIEXPORT void JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1update
  (JNIEnv *env, jobject obj, jint so_id, jlong s) {
    void (*update)(void *);
    *(void **) (&update) = update_ptrs[so_id];

    update((void *) s);
  }

/*
 * Class:      jni_1api_JniAPI
 * Method:     call_1finish
 * Signature:  ()V
 */
JNIEXPORT void JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1finish
  (JNIEnv *env, jobject obj, jint so_id, jlong s) {
    void (*finish)(void *);
    *(void **) (&finish) = finish_ptrs[so_id];

    finish((void *) s);
  }

/*
 * Class:      jni_1api_JniAPI
 * Method:     call_1resetCoverage
 * Signature:  ()V
 */
JNIEXPORT void JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1resetCoverage
  (JNIEnv *env, jobject obj, jint so_id, jlong s) {
    void (*reset_coverage)(void *);
    *(void **) (&reset_coverage) = reset_coverage_ptrs[so_id];

    reset_coverage((void *) s);
  }

/*
 * Class:      jni_1api_JniAPI
 * Method:     call_1writeCoverage
 * Signature:  (Ljava/lang/String;)V
 */
JNIEXPORT void JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1writeCoverage
  (JNIEnv *env, jobject obj, jint so_id, jlong s, jstring filename) {
    const char* filename_str = (*env)->GetStringUTFChars(env, filename, 0);
    // char cap[128];
    // strcpy(cap, filename_str);
    void (*write_coverage)(void *, const char*);
    *(void **) (&write_coverage) = write_coverage_ptrs[so_id];

    write_coverage((void *) s, filename_str);
    (*env)->ReleaseStringUTFChars(env, filename, filename_str);
  }

/*
 * Class:      jni_1api_JniAPI
 * Method:     call_1poke
 * Signature:  (II)I
 */
JNIEXPORT void JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1poke
  (JNIEnv *env, jobject obj, jint so_id, jlong s, jint id, jlong value) {
    void (*poke)(void *, int32_t, int64_t);
    *(void **) (&poke) = poke_ptrs[so_id];

    poke((void *) s, id, value);
  }

/*
 * Class:      jni_1api_JniAPI
 * Method:     call_1peek
 * Signature:  (III)I
 */
JNIEXPORT jlong JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1peek
  (JNIEnv *env, jobject obj, jint so_id, jlong s, jint id) {
    int64_t (*peek)(void *, int32_t);
    *(void **) (&peek) = peek_ptrs[so_id];

    return peek((void *) s, id);
  }

/*
 * Class:      jni_1api_JniAPI
 * Method:     call_1poke_1wide
 * Signature:  (III)V
 */
JNIEXPORT void JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1poke_1wide
  (JNIEnv *env, jobject obj, jint so_id, jlong s, jint id, jint offset, jlong value) {
    void (*poke_wide)(void*, int32_t, int32_t, int64_t);
    *(void **) (&poke_wide) = poke_wide_ptrs[so_id];

    poke_wide((void *) s, id, offset, value);
  }

/*
 * Class:      jni_1api_JniAPI
 * Method:     call_1peek_1wide
 * Signature:  (II)I
 */
JNIEXPORT jlong JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1peek_1wide
  (JNIEnv *env, jobject obj, jint so_id, jlong s, jint id, jint offset) {
    int64_t (*peek_wide)(void*, int32_t, int32_t);
    *(void **) (&peek_wide) = peek_wide_ptrs[so_id];

    return peek_wide((void *) s, id, offset);
  }

JNIEXPORT void JNICALL Java_chiseltest_simulator_jni_JniAPI_call_1set_1args
  (JNIEnv *env, jobject obj, jint so_id, jlong s, jint argc, jobjectArray argv) {
    // TODO - need to heap allocate, create a large enough 2D buffer to copy the args, otherwise try and maintain copies of obj_str and argv_str which are stored on the stack
    char buf[argc][1024];
    for (int i = 0; i < argc; i++) {
      void *obj_str = (*env)->GetObjectArrayElement(env, argv, i);
      const char* argv_str = (*env)->GetStringUTFChars(env, obj_str, 0);
      strcpy(buf[i], argv_str);
      (*env)->ReleaseStringUTFChars(env, obj_str, argv_str);
    }
    void (*set_args)(void *, int32_t, char **);
    *(void **) (&set_args) = set_args_ptrs[so_id];

    set_args((void *) s, argc, (char **) buf);
  }

