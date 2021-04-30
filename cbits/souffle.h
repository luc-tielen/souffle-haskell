#ifndef SOUFFLE_H
#define SOUFFLE_H
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C"
{
#endif

    // Opaque struct representing a Souffle program
    typedef struct souffle_interface souffle_t;
    // Opaque struct representing a Souffle relation
    typedef struct relation relation_t;
    // Opaque struct representing a byte array filled with data.
    typedef struct byte_buf byte_buf_t;

    /*
     * Initializes a Souffle program. The name of the program should be the
     * same as the filename (minus the .dl extension).
     *
     * The pointer that is returned can be NULL in case something went wrong.
     * If a valid pointer is returned, it needs to be freed by "souffle_free"
     * after it is no longer needed.
     */
    souffle_t *souffle_init(const char *progName);

    /*
     * Frees the memory in use by "program".
     * You need to check if the pointer is non-NULL before passing it to this
     * function. Not doing so results in undefined behavior.
     */
    void souffle_free(souffle_t *program);

    /*
     * Sets the number of cores this program should use.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     */
    void souffle_set_num_threads(souffle_t *program, size_t num_cores);

    /*
     * Gets the number of cores this program should use.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     * Returns the number of cores the program will use.
     */
    size_t souffle_get_num_threads(souffle_t *program);

    /*
     * Runs the Souffle program.
     * You need to check if the pointer is non-NULL before passing it to this
     * function. Not doing so results in undefined behavior.
     */
    void souffle_run(souffle_t *program);

    /*
     * Load all facts from files in a certain directory.
     * You need to check if both pointers are non-NULL before passing it to this
     * function. Not doing so results in undefined behavior.
     */
    void souffle_load_all(souffle_t *program, const char *input_directory);

    /*
     * Write out all facts of the program to CSV files
     * (as defined in the Souffle program).
     *
     * You need to check if the pointer is non-NULL before passing it to this
     * function. Not doing so results in undefined behavior.
     */
    void souffle_print_all(souffle_t *program, const char *output_directory);

    /*
     * Lookup a relation in the Souffle program.
     * You need to check if both passed pointers are non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * The returned pointer can be NULL if the relation is not found.
     * The pointer does not need to be freed, it is managed by the Souffle program.
     */
    relation_t *souffle_relation(souffle_t *program, const char *relation_name);

    /*
     * Checks if a relation contains a certain tuple.
     * You need to check if the passed pointers are non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * Returns true if the tuple was found in the relation; otherwise false.
     */
    bool souffle_contains_tuple(relation_t *relation, byte_buf_t *buf);

    /**
     * Pushes many Datalog facts from Haskell to Datalog.
     * You need to check if the passed pointers are non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     * Passing in a different count of objects to what is actually inside the
     * byte buffer will crash.
     */
    void souffle_tuple_push_many(relation_t *relation, byte_buf_t *buf, size_t size);

    /**
     * Pops many Datalog facts from Datalog to Haskell.
     * You need to check if the passed pointers are non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * Returns the byte buffer that contains the serialized Datalog facts.
     * This byte buffer is automatically managed by the C++ side and does not
     * need to be cleaned up.
     */
    byte_buf_t *souffle_tuple_pop_many(souffle_t *program, relation_t *relation);
#ifdef __cplusplus
}
#endif

#endif
