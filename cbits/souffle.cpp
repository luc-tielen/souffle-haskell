#include "souffle/SouffleInterface.h"
#include "souffle.h"

template <typename T>
void tuple_push_value(tuple_t *tuple, const T &value)
{
    auto t = reinterpret_cast<souffle::tuple *>(tuple);
    assert(t);
    *t << value;
}

template <typename T>
void tuple_pop_value(tuple_t *tuple, T *result)
{
    auto t = reinterpret_cast<souffle::tuple *>(tuple);
    assert(t);
    assert(result);
    *t >> *result;
}

extern "C"
{
    souffle_t *souffle_init(const char *progName)
    {
        auto prog = souffle::ProgramFactory::newInstance(progName);
        return reinterpret_cast<souffle_t *>(prog);
    }

    void souffle_free(souffle_t *program)
    {
        auto prog = reinterpret_cast<souffle::SouffleProgram *>(program);
        assert(prog);
        delete prog;
    }

    void souffle_set_num_threads(souffle_t *program, size_t num_cores)
    {
        auto prog = reinterpret_cast<souffle::SouffleProgram *>(program);
        assert(prog);
        prog->setNumThreads(num_cores);
    }

    size_t souffle_get_num_threads(souffle_t *program)
    {
        auto prog = reinterpret_cast<souffle::SouffleProgram *>(program);
        assert(prog);
        return prog->getNumThreads();
    }

    void souffle_run(souffle_t *program)
    {
        auto prog = reinterpret_cast<souffle::SouffleProgram *>(program);
        assert(prog);
        prog->run();
    }

    void souffle_load_all(souffle_t *program, const char *input_directory)
    {
        auto prog = reinterpret_cast<souffle::SouffleProgram *>(program);
        assert(prog);
        assert(input_directory);
        prog->loadAll(input_directory);
    }

    void souffle_print_all(souffle_t *program, const char *output_directory)
    {
        auto prog = reinterpret_cast<souffle::SouffleProgram *>(program);
        assert(prog);
        assert(output_directory);
        prog->printAll(output_directory);
    }

    relation_t *souffle_relation(souffle_t *program, const char *relation_name)
    {
        auto prog = reinterpret_cast<souffle::SouffleProgram *>(program);
        assert(prog);
        assert(relation_name);
        auto relation = prog->getRelation(relation_name);
        assert(relation);
        return reinterpret_cast<relation_t *>(relation);
    }

    size_t souffle_relation_tuple_count(relation_t *relation)
    {
        auto rel = reinterpret_cast<souffle::Relation *>(relation);
        assert(rel);
        return rel->size();
    }

    struct relation_iterator
    {
        using iterator_t = souffle::Relation::iterator;
        iterator_t iterator;

        relation_iterator(const iterator_t &it)
            : iterator(it) {}
    };

    relation_iterator_t *souffle_relation_iterator(relation_t *relation)
    {
        auto rel = reinterpret_cast<souffle::Relation *>(relation);
        assert(rel);
        relation_iterator_t *it = new relation_iterator_t(rel->begin());
        return it;
    }

    void souffle_relation_iterator_free(relation_iterator_t *iterator)
    {
        assert(iterator);
        delete iterator;
    }

    tuple_t *souffle_relation_iterator_next(relation_iterator_t *iterator)
    {
        assert(iterator);
        auto tuple = reinterpret_cast<tuple_t *>(&*iterator->iterator);
        ++iterator->iterator;
        return tuple;
    }

    bool souffle_contains_tuple(relation_t *relation, tuple_t *tuple)
    {
        auto rel = reinterpret_cast<souffle::Relation *>(relation);
        auto t = reinterpret_cast<souffle::tuple *>(tuple);
        assert(rel);
        assert(t);
        return rel->contains(*t);
    }

    tuple_t *souffle_tuple_alloc(relation_t *relation)
    {
        auto rel = reinterpret_cast<souffle::Relation *>(relation);
        assert(rel);
        auto tuple = new souffle::tuple(rel);
        return reinterpret_cast<tuple_t *>(tuple);
    }

    void souffle_tuple_free(tuple_t *tuple)
    {
        auto t = reinterpret_cast<souffle::tuple *>(tuple);
        assert(t);
        delete t;
    }

    void souffle_tuple_push_int32(tuple_t *tuple, int32_t value)
    {
        tuple_push_value(tuple, value);
    }

    void souffle_tuple_push_uint32(tuple_t *tuple, uint32_t value)
    {
        tuple_push_value(tuple, value);
    }

    void souffle_tuple_push_string(tuple_t *tuple, const char *value)
    {
        tuple_push_value(tuple, value);
    }

    void souffle_tuple_add(relation_t *relation, tuple_t *tuple)
    {
        auto rel = reinterpret_cast<souffle::Relation *>(relation);
        auto t = reinterpret_cast<souffle::tuple *>(tuple);
        assert(rel);
        assert(t);
        rel->insert(*t);
    }

    void souffle_tuple_pop_int32(tuple_t *tuple, int32_t *result)
    {
        tuple_pop_value(tuple, result);
    }

    void souffle_tuple_pop_uint32(tuple_t *tuple, uint32_t *result)
    {
        tuple_pop_value(tuple, result);
    }

    void souffle_tuple_pop_string(tuple_t *tuple, char **result)
    {
        std::string value;
        tuple_pop_value(tuple, &value);
        *result = strdup(value.c_str());
    }
}
