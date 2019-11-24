#ifndef SOUFFLE_H
#define SOUFFLE_H
#include <stdint.h>
#include <stdlib.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

    // Opaque struct representing a Souffle program
    typedef struct souffle_interface souffle_t;
    // Opaque struct representing a Souffle relation
    typedef struct relation relation_t;
    // Opaque struct representing an iterator to a Souffle relation
    typedef struct relation_iterator relation_iterator_t;
    // Opaque struct representing a Souffle tuple (fact).
    typedef struct tuple tuple_t;

    /*
     * Initializes a Souffle program. The name of the program should be the
     * same as the filename (minus the .dl extension).
     *
     * The pointer that is returned can be NULL in case something went wrong.
     * If a valid pointer is returned, it needs to be freed by "souffle_free"
     * after it is no longer needed.
     */
    souffle_t* souffle_init(const char* progName);

    /*
     * Frees the memory in use by "program".
     * You need to check if the pointer is non-NULL before passing it to this
     * function. Not doing so results in undefined behavior.
     */
    void souffle_free(souffle_t* program);

    /*
     * Sets the number of cores this program should use.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     */
    void souffle_set_num_threads(souffle_t* program, size_t num_cores);

    /*
     * Gets the number of cores this program should use.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     * Returns the number of cores the program will use.
     */
    size_t souffle_get_num_threads(souffle_t* program);

    /*
     * Runs the Souffle program.
     * You need to check if the pointer is non-NULL before passing it to this
     * function. Not doing so results in undefined behavior.
     */
    void souffle_run(souffle_t* program);

    /*
     * Load all facts from files in a certain directory.
     * You need to check if both pointers are non-NULL before passing it to this
     * function. Not doing so results in undefined behavior.
     */
    void souffle_load_all(souffle_t* program, const char* directory);

    /*
     * Write out all facts of the program to CSV files
     * (as defined in the Souffle program).
     *
     * You need to check if the pointer is non-NULL before passing it to this
     * function. Not doing so results in undefined behavior.
     */
    void souffle_print_all(souffle_t* program);

    /*
     * Lookup a relation in the Souffle program.
     * You need to check if both passed pointers are non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * The returned pointer can be NULL if the relation is not found.
     * The pointer does not need to be freed, it is managed by the Souffle program.
     */
    relation_t* souffle_relation(souffle_t* program, const char* relation_name);

    /*
     * Create an iterator for iterating over the facts of a relation.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * The returned pointer needs to be freed up with
     * "souffle_relation_iterator_free" after it is no longer needed.
     */
    relation_iterator_t* souffle_relation_iterator(relation_t* relation);

    /*
     * Frees a relation_iterator pointer.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     */
    void souffle_relation_iterator_free(relation_iterator_t* iterator);

    /*
     * Checks if the relation iterator contains more results.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * Returns true if iterator contains more results; otherwise false.
     */
    bool souffle_relation_iterator_has_next(const relation_iterator_t* iterator);

    /*
     * Advances the relation iterator by 1 position.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     * Always check if there is a next record with "souffle_relation_iterator_has_next"
     * before using this function to prevent crashes.
     *
     * Returns a pointer to the next record. This pointer is not allowed to be freed.
     */
    tuple_t* souffle_relation_iterator_next(relation_iterator_t* iterator);

    /*
     * Allocates memory for a tuple to be added to a relation.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * Returns a pointer to a new tuple. Use "souffle_tuple_free" when tuple
     * is no longer required.
     */
    tuple_t* souffle_tuple_alloc(relation_t* relation);

    /*
     * Frees memory of a tuple that was previously allocated.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     */
    void souffle_tuple_free(tuple_t* tuple);

    /*
     * Adds a tuple to a relation.
     * You need to check if both passed pointers are non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     */
    void souffle_tuple_add(relation_t* relation, tuple_t* tuple);

    /*
     * Pushes an integer value into a tuple.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * Pushing an integer value onto a tuple that expects another type results
     * in a crash.
     */
    void souffle_tuple_push_int(tuple_t* tuple, int32_t value);

    /*
     * Pushes a string value into a tuple.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * Pushing a string value onto a tuple that expects another type results
     * in a crash.
     */
    void souffle_tuple_push_string(tuple_t* tuple, const char* value);

    /*
     * Extracts an integer value from a tuple.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * Extracting an integer value from a tuple that expects another type results
     * in a crash.
     * The popped integer will be stored in the result pointer.
     */
    void souffle_tuple_pop_int(tuple_t* tuple, int32_t* result);

    /*
     * Extracts a string value from a tuple.
     * You need to check if the passed pointer is non-NULL before passing it
     * to this function. Not doing so results in undefined behavior.
     *
     * Extracting a string value from a tuple that expects another type results
     * in a crash.
     * The popped string will be stored in the result pointer.
     */
    void souffle_tuple_pop_string(tuple_t* tuple, char** result);

#ifdef __cplusplus
}
#endif

#endif
