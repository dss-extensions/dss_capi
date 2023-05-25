/*!

dss_common.hpp: a C++ layer for DSS-Extensions/C-API, common structs
Copyright (c) 2020-2022 Paulo Meira

Version: 0.5.0 (2022-07)

**POTENTIAL BREAKING CHANGES UNTIL VERSION 1.0**

*/

#pragma once
#ifndef DSS_CPP_COMMON
#define DSS_CPP_COMMON

#include <cstdint>
#include <string>
#include <complex>
#include <vector>
#include "dss_capi_ctx.h"
#include <Eigen/Core>
#include "fmt/core.h"

namespace dss {

using std::int32_t;
using std::string;

typedef std::complex<double> complex;
using Eigen::VectorXd;
typedef Eigen::Matrix<int32_t, Eigen::Dynamic, 1> VectorXi;
typedef std::vector<string> strings;
typedef std::vector<int32_t> bools; // std::vector<bool> is a bitset, might as well reuse int32

namespace obj
{
    class DSSObj;
    class DSSBatch;
}

struct APIUtil
{
    void *ctx;
    int32_t *error_ptr;
    bool owns_ctx;
    char*** data_PPAnsiChar;
    double** data_PDouble;
    int32_t** data_PInteger;
    int8_t** data_PByte;
    int32_t* count_PPAnsiChar;
    int32_t* count_PDouble;
    int32_t* count_PInteger;
    int32_t* count_PByte;

    void check_for_error()
    {
        if (!*error_ptr) return;

        int32_t error = *error_ptr;
        char* error_msg = ctx_Error_Get_Description(ctx);
        *error_ptr = 0;
        throw std::runtime_error(error_msg);
    }

    struct ErrorChecker
    {
        APIUtil *api_util;
        ErrorChecker(APIUtil *util): api_util(util)
        {

        }

        ~ErrorChecker()
        {
            api_util->check_for_error();
        }
    };

    APIUtil(bool create_ctx=false)
    {
        if (create_ctx)
        {
            ctx = ctx_New();
            owns_ctx = true;
        }
        else
        {
            owns_ctx = false;
            ctx = ctx_Get_Prime();
        }
        ctx_DSS_Start(ctx, 0);
        error_ptr = ctx_Error_Get_NumberPtr(ctx);
        ctx_DSS_GetGRPointers(
            ctx,
            &data_PPAnsiChar,
            &data_PDouble,
            &data_PInteger,
            &data_PByte,
            &count_PPAnsiChar,
            &count_PDouble,
            &count_PInteger,
            &count_PByte
        );
    }

    APIUtil(void *context, bool is_owner=false)
    {
        ctx = context;
        owns_ctx = is_owner;
        error_ptr = ctx_Error_Get_NumberPtr(ctx);
        ctx_DSS_GetGRPointers(
            ctx,
            &data_PPAnsiChar,
            &data_PDouble,
            &data_PInteger,
            &data_PByte,
            &count_PPAnsiChar,
            &count_PDouble,
            &count_PInteger,
            &count_PByte
        );        
    }

    ~APIUtil()
    {
        if (owns_ctx)
        {
            ctx_Dispose(ctx);
        }
    }

    /*
    Create a new element of a target class T, optionally activating and 
    flagging its edition.
    */
    template <typename T, typename enabled_ = std::enable_if_t<std::is_convertible<T*, dss::obj::DSSObj*>::value>>
    T create(const char *name, bool activate=true, bool begin_edit=true)
    {
        void *ptr = Obj_New(ctx, T::dss_cls_idx, name, activate, begin_edit);
        check_for_error();
        return T(this, ptr);
    }

    /*
    Create a new element of a target class T, optionally activating and 
    flagging its edition.
    */
    template <typename T, typename enabled_ = std::enable_if_t<std::is_convertible<T*, dss::obj::DSSObj*>::value>>
    T create(const string &name, bool activate=true, bool begin_edit=true)
    {
        return create<T>(name.c_str(), activate, begin_edit);
    }

    /*
    Create a new batch of elements of a target batch class T, optionally
    flagging their edition. Names are given by the prefix concatenated with
    a sequential number starting at `start_at`.
    */
    template <typename T, typename enabled_ = std::enable_if_t<std::is_convertible<T*, dss::obj::DSSBatch*>::value>>
    T create(const char *prefix, int32_t count, bool begin_edit=true, int32_t start_at=1)
    {
        T batch(this);
        std::vector<string> names;
        std::vector<const char*> names_ptrs;
        names.reserve(count);
        names_ptrs.reserve(count);

        std::string sprefix = prefix;
        for (int32_t i = 0; i < count; ++i)
        {
            names.emplace_back(fmt::format("{}{}", prefix, start_at + i));
            names_ptrs.push_back(names.back().c_str());
        }
        Batch_CreateFromNew(ctx, &batch.pointer, batch.count, T::BatchElementClass::dss_cls_idx, &names_ptrs[0], count, begin_edit);
        check_for_error();
        return batch;
    }

    /*
    Create a new batch of elements of a target batch class T, optionally
    flagging their edition. Names are given by the prefix concatenated with
    a sequential number starting at `start_at`.
    */
    template <typename T, typename enabled_ = 
        std::enable_if_t<
            std::is_convertible<T*, dss::obj::DSSBatch*>::value
        >
    >
    T create(const string &prefix, int32_t count, bool begin_edit=true, int32_t start_at=1)
    {
        return create<T>(prefix.c_str(), count, begin_edit, start_at);
    }

    template <typename VectorT=Eigen::Matrix<double, Eigen::Dynamic, 1>, typename std::enable_if<std::is_same<typename VectorT::value_type, double>::value>::type* = nullptr>
    VectorT get_float64_gr_array()
    {
        APIUtil::ErrorChecker error_checker(this);
        VectorT res;
        res.resize(size_t(*count_PDouble));
        memcpy(&res[0], *data_PDouble, sizeof(typename VectorT::value_type) * (*count_PDouble));
        return res;
    }

    template <typename VectorT=Eigen::Matrix<int32_t, Eigen::Dynamic, 1>, typename std::enable_if<std::is_same<typename VectorT::value_type, int32_t>::value>::type* = nullptr>
    VectorT get_int32_gr_array()
    {
        APIUtil::ErrorChecker error_checker(this);
        VectorT res;
        res.resize(size_t(*count_PInteger));
        memcpy(&res[0], *data_PInteger, sizeof(typename VectorT::value_type) * (*count_PInteger));
        return res;
    }

    template <typename VectorT=Eigen::Matrix<int8_t, Eigen::Dynamic, 1>, typename std::enable_if<std::is_same<typename VectorT::value_type, int8_t>::value>::type* = nullptr>
    VectorT get_int8_gr_array()
    {
        APIUtil::ErrorChecker error_checker(this);
        VectorT res;
        res.resize(size_t(*count_PByte));
        memcpy(&res[0], *data_PByte, sizeof(typename VectorT::value_type) * (*count_PByte));
        return res;
    }

    template<typename FunctionT, typename... Args>
    strings get_string_array(FunctionT func, Args... args)
    {
        char** ResultPtr = nullptr;
        int32_t ResultCount[2] = {0, 0};
        func(ctx, &ResultPtr, ResultCount, args...);
        strings res;
        res.reserve(ResultCount[0]);
        for (int32_t i = 0; i < ResultCount[0]; ++i)
        {
            res.emplace_back(ResultPtr[i]);
        }
        return res;
    }

    template<typename FunctionT, typename... Args>
    void set_string_array(FunctionT func, const strings &value, Args... args)
    {
        std::vector<const char*> ptrs(value.size(), nullptr);
        for (size_t i = 0; i < value.size(); ++i)
        {
            ptrs[i] = value[i].c_str();
        }

        func(ctx, &ptrs[0], value.size(), args...);
    }
};

///
/// Wraps common DSSContext state data. Base of all wrapper classes.
///
class ContextState
{
public:
    ///
    /// Pointer to the DSSContext for easy access
    ///
    void *ctx;

    ///
    /// API utility functions
    ///
    APIUtil *api_util;

    ContextState(APIUtil *util)
    {
        api_util = util;
        ctx = util->ctx;
    }
};

}
#endif // #ifndef DSS_CPP_COMMON