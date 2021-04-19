#include "souffle/SouffleInterface.h"
#include "souffle.h"
#include <algorithm>
#include <string>
#include <unordered_map>
#include <vector>

#ifndef ESTIMATED_AVERAGE_STRING_SIZE
#define ESTIMATED_AVERAGE_STRING_SIZE 32
#endif
#ifndef GROW_FACTOR
#define GROW_FACTOR 2
#endif

namespace helpers
{

inline auto parse_signature(const souffle::Relation& relation)
{
    const auto arity = relation.getArity();

    std::vector<char> types;
    types.reserve(arity);

    for (size_t i = 0; i < arity; ++i)
    {
        types.push_back(*relation.getAttrType(i));
    }

    return types;
}

inline bool relation_contains_strings(const souffle::Relation& relation)
{
    const auto types = parse_signature(relation);
    return std::any_of(types.begin(), types.end(), [](auto x) { return x == 's'; });
}

using offset_t = uint32_t;

using number_t = int32_t;
using unsigned_t = uint32_t;
using float_t = float;

template <typename T>
inline void serialize_value(souffle::tuple& tuple, char* buf, offset_t& offset)
{
    auto ptr = reinterpret_cast<T*>(buf);
    tuple >> *ptr;
    offset += sizeof(T);
}

template <typename T>
inline void deserialize_value(souffle::tuple& tuple, char* buf, offset_t& offset)
{
    auto ptr = reinterpret_cast<T*>(buf);
    tuple << *ptr;
    offset += sizeof(T);
}

inline void serialize_symbol(souffle::tuple& tuple, char* buf, offset_t& offset)
{
    std::string str;
    tuple >> str;
    const uint32_t num_bytes = str.length();

    auto ptr = reinterpret_cast<uint32_t*>(buf);
    *ptr = num_bytes;

    // TODO: check if we can directly write into byte buf?
    auto string_ptr = reinterpret_cast<char*>(buf) + sizeof(uint32_t);
    std::copy(str.begin(), str.end(), string_ptr);
    offset += sizeof(uint32_t) + num_bytes;
}

inline void deserialize_symbol(souffle::tuple& tuple, char* buf, offset_t& offset)
{
    auto ptr = reinterpret_cast<uint32_t*>(buf);
    const auto num_bytes = *ptr;
    if (num_bytes == 0) {
        tuple << "";
        offset += sizeof(uint32_t);
        return;
    }

    auto string_ptr = reinterpret_cast<const char*>(buf) + sizeof(uint32_t);
    std::string str(string_ptr, num_bytes);
    tuple << str;
    offset += sizeof(uint32_t) + num_bytes;
}

using deserializer_t = void(*)(souffle::tuple&, char*, offset_t&);
using deserializer_map = std::unordered_map<char, deserializer_t>;

static const deserializer_map deserializers_map = {
    {'s', deserialize_symbol},
    {'i', deserialize_value<number_t>},
    {'u', deserialize_value<unsigned_t>},
    {'f', deserialize_value<float_t>}
};

using serializer_t = void(*)(souffle::tuple&, char*, offset_t&);
using serializer_map = std::unordered_map<char, serializer_t>;

static const serializer_map serializers_map = {
    {'s', serialize_symbol},
    {'i', serialize_value<number_t>},
    {'u', serialize_value<unsigned_t>},
    {'f', serialize_value<float_t>}
};

inline auto types_to_deserializer(const std::vector<char>& types)
{
    std::vector<deserializer_t> deserializers;
    deserializers.reserve(types.size());

    for (const auto& type: types)
    {
        const auto match = deserializers_map.find(type);
        assert(match != deserializers_map.end() &&
                ("Found unknown Souffle primitive type: " + match->first));
        deserializers.push_back(match->second);
    }

    return [deserializers = std::move(deserializers)](souffle::tuple& tuple, char* buf, offset_t& offset)
    {
        for (const auto& deserializer : deserializers)
        {
            deserializer(tuple, buf + offset, offset);
        }
    };
}

inline auto types_to_serializer(const std::vector<char>& types)
{
    std::vector<serializer_t> serializers;
    serializers.reserve(types.size());

    for (const auto& type: types)
    {
        const auto match = serializers_map.find(type);
        assert(match != serializers_map.end() &&
                ("Found unknown Souffle primitive type: " + match->first));
        serializers.push_back(match->second);
    }

    return [serializers = std::move(serializers)](souffle::tuple& tuple, char* buf, offset_t& offset)
    {
        for (const auto& serializer : serializers)
        {
            serializer(tuple, buf + offset, offset);
        }
    };
}

inline auto guess_tuple_size(const std::vector<char>& types)
{
    size_t size = 0;

    for (const auto& type : types)
    {
        size += type == 's'
             ? ESTIMATED_AVERAGE_STRING_SIZE
             : sizeof(number_t);
    }

    return size;
}

struct Serializer
{
public:
    inline Serializer(const souffle::Relation& relation)
        : m_relation(relation)
        , m_types(parse_signature(relation))
    {
        auto tuple_size = guess_tuple_size(m_types);

        m_fact_count = relation.size();
        m_num_bytes = sizeof(uint32_t) + m_fact_count * tuple_size;
        m_buf = new char[m_num_bytes];
        m_offset = 0;
    }

    inline const Serializer& serialize()
    {
        using serializer_t = void(*)(Serializer*, souffle::tuple&);
        using serializer_map_t = std::unordered_map<char, serializer_t>;

        serializer_t do_serialize_symbol = [](auto s, auto& t) {
            s->serialize_symbol(t);
        };
        serializer_t do_serialize_number = [](auto s, auto& t) {
            s->serialize_number(t);
        };
        serializer_t do_serialize_unsigned = [](auto s, auto& t) {
            s->serialize_unsigned(t);
        };
        serializer_t do_serialize_float = [](auto s, auto& t) {
            s->serialize_float(t);
        };

        static const serializer_map_t serializers_map = {
            {'s', do_serialize_symbol},
            {'i', do_serialize_number},
            {'u', do_serialize_unsigned},
            {'f', do_serialize_float},
        };

        std::vector<serializer_t> serializers;
        serializers.reserve(m_types.size());

        for (const auto& type: m_types)
        {
            const auto match = serializers_map.find(type);
            assert(match != serializers_map.end() &&
                    ("Found unknown Souffle primitive type: " + match->first));
            serializers.push_back(match->second);
        }

        const auto serialize = [this, serializers = std::move(serializers)](auto& tuple)
        {
            for (const auto& serializer : serializers)
            {
                serializer(this, tuple);
            }
        };

        auto buf = reinterpret_cast<uint32_t*>(m_buf);
        *buf = m_fact_count;
        m_offset += sizeof(uint32_t);

        for (auto& tuple: m_relation)
        {
            serialize(tuple);
        }

        return *this;
    }

    inline void serialize_number(souffle::tuple& tuple)
    {
        constexpr auto byte_count = sizeof(number_t);
        if (!has_remaining_bytes(byte_count)) {
            resize_buf(byte_count);
        }

        serialize_value<number_t>(tuple, m_buf + m_offset, m_offset);
    }

    inline void serialize_unsigned(souffle::tuple& tuple)
    {
        constexpr auto byte_count = sizeof(unsigned_t);
        if (!has_remaining_bytes(byte_count)) {
            resize_buf(byte_count);
        }

        serialize_value<unsigned_t>(tuple, m_buf + m_offset, m_offset);
    }

    inline void serialize_float(souffle::tuple& tuple)
    {
        constexpr auto byte_count = sizeof(float_t);
        if (!has_remaining_bytes(byte_count)) {
            resize_buf(byte_count);
        }

        serialize_value<float_t>(tuple, m_buf + m_offset, m_offset);
    }

    inline void serialize_symbol(souffle::tuple& tuple)
    {
        std::string str;
        tuple >> str;
        const uint32_t num_bytes = str.length();

        auto total_byte_count = sizeof(uint32_t) + num_bytes;
        if (!has_remaining_bytes(total_byte_count)) {
            resize_buf(total_byte_count);
        }

        auto buf = m_buf + m_offset;
        auto ptr = reinterpret_cast<uint32_t*>(buf);
        *ptr = num_bytes;

        // TODO: check if we can directly write into byte buf?
        auto string_ptr = reinterpret_cast<char*>(buf) + sizeof(uint32_t);
        std::copy(str.begin(), str.end(), string_ptr);
        m_offset += sizeof(uint32_t) + num_bytes;
    }

    inline bool has_remaining_bytes(size_t count) const
    {
        return m_num_bytes >= m_offset + count;
    }

    inline void resize_buf(size_t byte_count)
    {
        size_t grow_factor = GROW_FACTOR;
        while (m_offset + byte_count > m_num_bytes * grow_factor) {
            grow_factor *= 2;
        }
        const auto new_num_bytes = m_num_bytes * grow_factor;
        auto buf = new char[new_num_bytes];

        memcpy(buf, m_buf, m_offset);
        delete [] m_buf;

        m_buf = buf;
        m_num_bytes = new_num_bytes;
    }

    inline byte_buf_t *to_buf() const
    {
        return reinterpret_cast<byte_buf_t*>(m_buf);
    }

private:
    const souffle::Relation& m_relation;
    std::vector<char> m_types;
    size_t m_fact_count;
    // TODO sample string sizes for more accurate resizing of buf?
    char *m_buf;
    size_t m_num_bytes;
    offset_t m_offset;
};

inline byte_buf_t *serialize_slow(const souffle::Relation& relation)
{
    Serializer s(relation);
    return s.serialize().to_buf();
}

inline byte_buf_t *serialize_fast(const souffle::Relation& relation)
{
    const auto types = parse_signature(relation);
    const auto serialize = types_to_serializer(types);

    const auto fact_count = relation.size();
    const auto tuple_size = guess_tuple_size(types);
    const auto num_bytes = sizeof(uint32_t) + fact_count * tuple_size;
    auto buf = new char[num_bytes];
    const auto start_ptr = buf;

    offset_t offset = 0;

    auto ptr = reinterpret_cast<uint32_t*>(buf);
    *ptr = fact_count;
    offset += 4;

    for (auto& tuple: relation)
    {
        serialize(tuple, buf, offset);
    }

    return reinterpret_cast<byte_buf_t*>(start_ptr);
}

}  // namespace helpers

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

    bool souffle_contains_tuple(relation_t *rel, byte_buf_t *buf)
    {
        auto relation = reinterpret_cast<souffle::Relation *>(rel);
        auto data = reinterpret_cast<char*>(buf);
        assert(relation && "Relation is NULL in souffle_contains_tuple");
        assert(data && "byte buf is NULL in souffle_contains_tuple");

        auto& r = *relation;
        const auto types = helpers::parse_signature(r);
        const auto deserialize_tuple = helpers::types_to_deserializer(types);

        souffle::tuple tuple(relation);
        helpers::offset_t offset = 0;
        deserialize_tuple(tuple, data, offset);
        return r.contains(tuple);
    }

    void souffle_tuple_push_many(relation_t *rel, byte_buf_t *buf, size_t size)
    {
        auto relation = reinterpret_cast<souffle::Relation*>(rel);
        auto data = reinterpret_cast<char*>(buf);
        assert(data && "byte buf is NULL in souffle_tuple_push_many");
        assert(relation && "Relation is NULL in souffle_tuple_push_many");

        auto& r = *relation;
        const auto types = helpers::parse_signature(r);
        const auto deserialize_tuple = helpers::types_to_deserializer(types);

        helpers::offset_t offset = 0;
        for (size_t i = 0; i < size; ++i)
        {
            souffle::tuple tuple(relation);
            deserialize_tuple(tuple, data, offset);
            r.insert(tuple);
        }
    }

    byte_buf_t *souffle_tuple_pop_many(relation_t *rel)
    {
        auto relation = reinterpret_cast<souffle::Relation*>(rel);
        assert(relation && "Relation is NULL in souffle_tuple_pop_many");
        auto& r = *relation;
        return helpers::relation_contains_strings(r)
            ? helpers::serialize_slow(r)
            : helpers::serialize_fast(r);
    }

    void souffle_byte_buf_free(byte_buf_t *buf)
    {
        auto data = reinterpret_cast<char*>(buf);
        assert(data && "byte buf should not be NULL");
        delete[] data;
    }
}
