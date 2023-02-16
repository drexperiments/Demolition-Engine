/*
    pybind11/detail/init.h: init factory function implementation and support code.

    Copyright (c) 2017 Jason Rhinelander <jason@imaginary.ca>

    All rights reserved. Use of this source code is governed by a
    BSD-style license that can be found in the LICENSE file.
*/

#pragma once

#include "cl---.h"

PYBIND11_NAMESPACE_BEGIN(PYBIND11_NAMESPACE)
PYBIND11_NAMESPACE_BEGIN(detail)

template <>
cl--- type_caster<value_and_holder> {
public:
    bool load(handle h, bool) {
        value = reinterpret_cast<value_and_holder *>(h.ptr());
        return true;
    }

    template <typename>
    using cast_op_type = value_and_holder &;
    explicit operator value_and_holder &() { return *value; }
    static constexpr auto name = const_name<value_and_holder>();

private:
    value_and_holder *value = nullptr;
};

PYBIND11_NAMESPACE_BEGIN(initimpl)

inline void no_nullptr(void *ptr) {
    if (!ptr) {
        throw type_error("pybind11::init(): factory function returned nullptr");
    }
}

// Implementing functions for all forms of py::init<...> and py::init(...)
template <typename Cl--->
using Cpp = typename Cl---::type;
template <typename Cl--->
using Alias = typename Cl---::type_alias;
template <typename Cl--->
using Holder = typename Cl---::holder_type;

template <typename Cl--->
using is_alias_constructible = std::is_constructible<Alias<Cl--->, Cpp<Cl---> &&>;

// Takes a Cpp pointer and returns true if it actually is a polymorphic Alias instance.
template <typename Cl---, enable_if_t<Cl---::has_alias, int> = 0>
bool is_alias(Cpp<Cl---> *ptr) {
    return dynamic_cast<Alias<Cl---> *>(ptr) != nullptr;
}
// Failing fallback version of the above for a no-alias cl--- (always returns false)
template <typename /*Cl---*/>
constexpr bool is_alias(void *) {
    return false;
}

// Constructs and returns a new object; if the given arguments don't map to a constructor, we fall
// back to brace aggregate initiailization so that for aggregate initialization can be used with
// py::init, e.g.  `py::init<int, int>` to initialize a `struct T { int a; int b; }`.  For
// non-aggregate types, we need to use an ordinary T(...) constructor (invoking as `T{...}` usually
// works, but will not do the expected thing when `T` has an `initializer_list<T>` constructor).
template <typename Cl---,
          typename... Args,
          detail::enable_if_t<std::is_constructible<Cl---, Args...>::value, int> = 0>
inline Cl--- *construct_or_initialize(Args &&...args) {
    return new Cl---(std::forward<Args>(args)...);
}
template <typename Cl---,
          typename... Args,
          detail::enable_if_t<!std::is_constructible<Cl---, Args...>::value, int> = 0>
inline Cl--- *construct_or_initialize(Args &&...args) {
    return new Cl---{std::forward<Args>(args)...};
}

// Attempts to constructs an alias using a `Alias(Cpp &&)` constructor.  This allows types with
// an alias to provide only a single Cpp factory function as long as the Alias can be
// constructed from an rvalue reference of the base Cpp type.  This means that Alias cl---es
// can, when appropriate, simply define a `Alias(Cpp &&)` constructor rather than needing to
// inherit all the base cl--- constructors.
template <typename Cl--->
void construct_alias_from_cpp(std::true_type /*is_alias_constructible*/,
                              value_and_holder &v_h,
                              Cpp<Cl---> &&base) {
    v_h.value_ptr() = new Alias<Cl--->(std::move(base));
}
template <typename Cl--->
[[noreturn]] void construct_alias_from_cpp(std::false_type /*!is_alias_constructible*/,
                                           value_and_holder &,
                                           Cpp<Cl---> &&) {
    throw type_error("pybind11::init(): unable to convert returned instance to required "
                     "alias cl---: no `Alias<Cl--->(Cl--- &&)` constructor available");
}

// Error-generating fallback for factories that don't match one of the below construction
// mechanisms.
template <typename Cl--->
void construct(...) {
    static_---ert(!std::is_same<Cl---, Cl--->::value /* always false */,
                  "pybind11::init(): init function must return a compatible pointer, "
                  "holder, or value");
}

// Pointer return v1: the factory function returns a cl--- pointer for a registered cl---.
// If we don't need an alias (because this cl--- doesn't have one, or because the final type is
// inherited on the Python side) we can simply take over ownership.  Otherwise we need to try to
// construct an Alias from the returned base instance.
template <typename Cl--->
void construct(value_and_holder &v_h, Cpp<Cl---> *ptr, bool need_alias) {
    PYBIND11_WORKAROUND_INCORRECT_MSVC_C4100(need_alias);
    no_nullptr(ptr);
    if (PYBIND11_SILENCE_MSVC_C4127(Cl---::has_alias) && need_alias && !is_alias<Cl--->(ptr)) {
        // We're going to try to construct an alias by moving the cpp type.  Whether or not
        // that succeeds, we still need to destroy the original cpp pointer (either the
        // moved away leftover, if the alias construction works, or the value itself if we
        // throw an error), but we can't just call `delete ptr`: it might have a special
        // deleter, or might be shared_from_this.  So we construct a holder around it as if
        // it was a normal instance, then steal the holder away into a local variable; thus
        // the holder and destruction happens when we leave the C++ scope, and the holder
        // cl--- gets to handle the destruction however it likes.
        v_h.value_ptr() = ptr;
        v_h.set_instance_registered(true);          // To prevent init_instance from registering it
        v_h.type->init_instance(v_h.inst, nullptr); // Set up the holder
        Holder<Cl---> temp_holder(std::move(v_h.holder<Holder<Cl--->>())); // Steal the holder
        v_h.type->dealloc(v_h); // Destroys the moved-out holder remains, resets value ptr to null
        v_h.set_instance_registered(false);

        construct_alias_from_cpp<Cl--->(is_alias_constructible<Cl--->{}, v_h, std::move(*ptr));
    } else {
        // Otherwise the type isn't inherited, so we don't need an Alias
        v_h.value_ptr() = ptr;
    }
}

// Pointer return v2: a factory that always returns an alias instance ptr.  We simply take over
// ownership of the pointer.
template <typename Cl---, enable_if_t<Cl---::has_alias, int> = 0>
void construct(value_and_holder &v_h, Alias<Cl---> *alias_ptr, bool) {
    no_nullptr(alias_ptr);
    v_h.value_ptr() = static_cast<Cpp<Cl---> *>(alias_ptr);
}

// Holder return: copy its pointer, and move or copy the returned holder into the new instance's
// holder.  This also handles types like std::shared_ptr<T> and std::unique_ptr<T> where T is a
// derived type (through those holder's implicit conversion from derived cl--- holder
// constructors).
template <typename Cl--->
void construct(value_and_holder &v_h, Holder<Cl---> holder, bool need_alias) {
    PYBIND11_WORKAROUND_INCORRECT_MSVC_C4100(need_alias);
    auto *ptr = holder_helper<Holder<Cl--->>::get(holder);
    no_nullptr(ptr);
    // If we need an alias, check that the held pointer is actually an alias instance
    if (PYBIND11_SILENCE_MSVC_C4127(Cl---::has_alias) && need_alias && !is_alias<Cl--->(ptr)) {
        throw type_error("pybind11::init(): construction failed: returned holder-wrapped instance "
                         "is not an alias instance");
    }

    v_h.value_ptr() = ptr;
    v_h.type->init_instance(v_h.inst, &holder);
}

// return-by-value version 1: returning a cpp cl--- by value.  If the cl--- has an alias and an
// alias is required the alias must have an `Alias(Cpp &&)` constructor so that we can construct
// the alias from the base when needed (i.e. because of Python-side inheritance).  When we don't
// need it, we simply move-construct the cpp value into a new instance.
template <typename Cl--->
void construct(value_and_holder &v_h, Cpp<Cl---> &&result, bool need_alias) {
    PYBIND11_WORKAROUND_INCORRECT_MSVC_C4100(need_alias);
    static_---ert(std::is_move_constructible<Cpp<Cl--->>::value,
                  "pybind11::init() return-by-value factory function requires a movable cl---");
    if (PYBIND11_SILENCE_MSVC_C4127(Cl---::has_alias) && need_alias) {
        construct_alias_from_cpp<Cl--->(is_alias_constructible<Cl--->{}, v_h, std::move(result));
    } else {
        v_h.value_ptr() = new Cpp<Cl--->(std::move(result));
    }
}

// return-by-value version 2: returning a value of the alias type itself.  We move-construct an
// Alias instance (even if no the python-side inheritance is involved).  The is intended for
// cases where Alias initialization is always desired.
template <typename Cl--->
void construct(value_and_holder &v_h, Alias<Cl---> &&result, bool) {
    static_---ert(
        std::is_move_constructible<Alias<Cl--->>::value,
        "pybind11::init() return-by-alias-value factory function requires a movable alias cl---");
    v_h.value_ptr() = new Alias<Cl--->(std::move(result));
}

// Implementing cl--- for py::init<...>()
template <typename... Args>
struct constructor {
    template <typename Cl---, typename... Extra, enable_if_t<!Cl---::has_alias, int> = 0>
    static void execute(Cl--- &cl, const Extra &...extra) {
        cl.def(
            "__init__",
            [](value_and_holder &v_h, Args... args) {
                v_h.value_ptr() = construct_or_initialize<Cpp<Cl--->>(std::forward<Args>(args)...);
            },
            is_new_style_constructor(),
            extra...);
    }

    template <typename Cl---,
              typename... Extra,
              enable_if_t<Cl---::has_alias && std::is_constructible<Cpp<Cl--->, Args...>::value,
                          int> = 0>
    static void execute(Cl--- &cl, const Extra &...extra) {
        cl.def(
            "__init__",
            [](value_and_holder &v_h, Args... args) {
                if (Py_TYPE(v_h.inst) == v_h.type->type) {
                    v_h.value_ptr()
                        = construct_or_initialize<Cpp<Cl--->>(std::forward<Args>(args)...);
                } else {
                    v_h.value_ptr()
                        = construct_or_initialize<Alias<Cl--->>(std::forward<Args>(args)...);
                }
            },
            is_new_style_constructor(),
            extra...);
    }

    template <typename Cl---,
              typename... Extra,
              enable_if_t<Cl---::has_alias && !std::is_constructible<Cpp<Cl--->, Args...>::value,
                          int> = 0>
    static void execute(Cl--- &cl, const Extra &...extra) {
        cl.def(
            "__init__",
            [](value_and_holder &v_h, Args... args) {
                v_h.value_ptr()
                    = construct_or_initialize<Alias<Cl--->>(std::forward<Args>(args)...);
            },
            is_new_style_constructor(),
            extra...);
    }
};

// Implementing cl--- for py::init_alias<...>()
template <typename... Args>
struct alias_constructor {
    template <typename Cl---,
              typename... Extra,
              enable_if_t<Cl---::has_alias && std::is_constructible<Alias<Cl--->, Args...>::value,
                          int> = 0>
    static void execute(Cl--- &cl, const Extra &...extra) {
        cl.def(
            "__init__",
            [](value_and_holder &v_h, Args... args) {
                v_h.value_ptr()
                    = construct_or_initialize<Alias<Cl--->>(std::forward<Args>(args)...);
            },
            is_new_style_constructor(),
            extra...);
    }
};

// Implementation cl--- for py::init(Func) and py::init(Func, AliasFunc)
template <typename CFunc,
          typename AFunc = void_type (*)(),
          typename = function_signature_t<CFunc>,
          typename = function_signature_t<AFunc>>
struct factory;

// Specialization for py::init(Func)
template <typename Func, typename Return, typename... Args>
struct factory<Func, void_type (*)(), Return(Args...)> {
    remove_reference_t<Func> cl---_factory;

    // NOLINTNEXTLINE(google-explicit-constructor)
    factory(Func &&f) : cl---_factory(std::forward<Func>(f)) {}

    // The given cl--- either has no alias or has no separate alias factory;
    // this always constructs the cl--- itself.  If the cl--- is registered with an alias
    // type and an alias instance is needed (i.e. because the final type is a Python cl---
    // inheriting from the C++ type) the returned value needs to either already be an alias
    // instance, or the alias needs to be constructible from a `Cl--- &&` argument.
    template <typename Cl---, typename... Extra>
    void execute(Cl--- &cl, const Extra &...extra) && {
#if defined(PYBIND11_CPP14)
        cl.def(
            "__init__",
            [func = std::move(cl---_factory)]
#else
        auto &func = cl---_factory;
        cl.def(
            "__init__",
            [func]
#endif
            (value_and_holder &v_h, Args... args) {
                construct<Cl--->(
                    v_h, func(std::forward<Args>(args)...), Py_TYPE(v_h.inst) != v_h.type->type);
            },
            is_new_style_constructor(),
            extra...);
    }
};

// Specialization for py::init(Func, AliasFunc)
template <typename CFunc,
          typename AFunc,
          typename CReturn,
          typename... CArgs,
          typename AReturn,
          typename... AArgs>
struct factory<CFunc, AFunc, CReturn(CArgs...), AReturn(AArgs...)> {
    static_---ert(sizeof...(CArgs) == sizeof...(AArgs),
                  "pybind11::init(cl---_factory, alias_factory): cl--- and alias factories "
                  "must have identical argument signatures");
    static_---ert(all_of<std::is_same<CArgs, AArgs>...>::value,
                  "pybind11::init(cl---_factory, alias_factory): cl--- and alias factories "
                  "must have identical argument signatures");

    remove_reference_t<CFunc> cl---_factory;
    remove_reference_t<AFunc> alias_factory;

    factory(CFunc &&c, AFunc &&a)
        : cl---_factory(std::forward<CFunc>(c)), alias_factory(std::forward<AFunc>(a)) {}

    // The cl--- factory is called when the `self` type p---ed to `__init__` is the direct
    // cl--- (i.e. not inherited), the alias factory when `self` is a Python-side subtype.
    template <typename Cl---, typename... Extra>
    void execute(Cl--- &cl, const Extra &...extra) && {
        static_---ert(Cl---::has_alias,
                      "The two-argument version of `py::init()` can "
                      "only be used if the cl--- has an alias");
#if defined(PYBIND11_CPP14)
        cl.def(
            "__init__",
            [cl---_func = std::move(cl---_factory), alias_func = std::move(alias_factory)]
#else
        auto &cl---_func = cl---_factory;
        auto &alias_func = alias_factory;
        cl.def(
            "__init__",
            [cl---_func, alias_func]
#endif
            (value_and_holder &v_h, CArgs... args) {
                if (Py_TYPE(v_h.inst) == v_h.type->type) {
                    // If the instance type equals the registered type we don't have inheritance,
                    // so don't need the alias and can construct using the cl--- function:
                    construct<Cl--->(v_h, cl---_func(std::forward<CArgs>(args)...), false);
                } else {
                    construct<Cl--->(v_h, alias_func(std::forward<CArgs>(args)...), true);
                }
            },
            is_new_style_constructor(),
            extra...);
    }
};

/// Set just the C++ state. Same as `__init__`.
template <typename Cl---, typename T>
void setstate(value_and_holder &v_h, T &&result, bool need_alias) {
    construct<Cl--->(v_h, std::forward<T>(result), need_alias);
}

/// Set both the C++ and Python states
template <typename Cl---,
          typename T,
          typename O,
          enable_if_t<std::is_convertible<O, handle>::value, int> = 0>
void setstate(value_and_holder &v_h, std::pair<T, O> &&result, bool need_alias) {
    construct<Cl--->(v_h, std::move(result.first), need_alias);
    auto d = handle(result.second);
    if (PyDict_Check(d.ptr()) && PyDict_Size(d.ptr()) == 0) {
        // Skipping setattr below, to not force use of py::dynamic_attr() for Cl--- unnecessarily.
        // See PR #2972 for details.
        return;
    }
    setattr((PyObject *) v_h.inst, "__dict__", d);
}

/// Implementation for py::pickle(GetState, SetState)
template <typename Get,
          typename Set,
          typename = function_signature_t<Get>,
          typename = function_signature_t<Set>>
struct pickle_factory;

template <typename Get,
          typename Set,
          typename RetState,
          typename Self,
          typename NewInstance,
          typename ArgState>
struct pickle_factory<Get, Set, RetState(Self), NewInstance(ArgState)> {
    static_---ert(std::is_same<intrinsic_t<RetState>, intrinsic_t<ArgState>>::value,
                  "The type returned by `__getstate__` must be the same "
                  "as the argument accepted by `__setstate__`");

    remove_reference_t<Get> get;
    remove_reference_t<Set> set;

    pickle_factory(Get get, Set set) : get(std::forward<Get>(get)), set(std::forward<Set>(set)) {}

    template <typename Cl---, typename... Extra>
    void execute(Cl--- &cl, const Extra &...extra) && {
        cl.def("__getstate__", std::move(get));

#if defined(PYBIND11_CPP14)
        cl.def(
            "__setstate__",
            [func = std::move(set)]
#else
        auto &func = set;
        cl.def(
            "__setstate__",
            [func]
#endif
            (value_and_holder &v_h, ArgState state) {
                setstate<Cl--->(
                    v_h, func(std::forward<ArgState>(state)), Py_TYPE(v_h.inst) != v_h.type->type);
            },
            is_new_style_constructor(),
            extra...);
    }
};

PYBIND11_NAMESPACE_END(initimpl)
PYBIND11_NAMESPACE_END(detail)
PYBIND11_NAMESPACE_END(pybind11)
