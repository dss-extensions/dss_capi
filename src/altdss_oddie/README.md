# AltDSS Oddie

*OpenDSSDirect.DLL Interface Extender for EPRI's official OpenDSS binaries*

**Oddie** wraps and extends the **official OpenDSSDirect.DLL** (a.k.a. DCSL) with the same API that our other 
projects use, as defined in AltDSS/DSS C-API.

Not to be confused with [OpenDSSDirect.py](https://github.com/dss-extensions/OpenDSSDirect.py/) or [OpenDSSDirect.jl](https://github.com/dss-extensions/OpenDSSDirect.jl/), which since 2018/2019 use a community alternative implementation of OpenDSS, a.k.a, AltDSS/DSS C-API from DSS-Extensions.

As with recent projects, the AltDSS prefix is being used to indicate clearly this is not endorsed or directly supported by EPRI. The intention is, of course, to track the binaries provided by EPRI and provide updates to the wrapper as time permits.

We invite users to examine the source code and reach their own conclusions about code quality. Some simple benchmarks should be available after updates to the version 10 of OpenDSS are included.

## Why?

The essential goal is to allow using the official OpenDSS binaries from DSS-Extensions, in the capacity 
that its API allows and what's available in the official engine.

Since COM is a Windows-only technology, not supported on Linux or macOS, a potential Linux version of OpenDSSDirect.DLL could
be wrapped with Oddie to provide access to the classic API part of most projects from DSS-Extensions. For some programming 
languages, like Python, this would allow instant compatibility with legacy COM-based code without requiring many changes, just
like DSS-Python. For other projects, like OpenDSSDirect.jl, 

Collectively, the projects on DSS-Extensions are mature and stable. Reworking all of them individually to allow a completely 
different low-level API is not productive and does not provide much benefits compare to the solution adopted via AltDSS Oddie.

Even before DSS-Python and OpenDSSDirect.py, the OpenDSS community already had an extensive collection of code that uses the COM DLL implementation.
Since most of the projects on DSS-Extensions try to closely follow the layout of the COM classes, users should feel at home and can reuse most of the
documentation provided at https://opendss.epri.com/opendss_documentation.html

## Is this supported by EPRI?

This project itself is not supported by EPRI, but it uses EPRI's binaries instead of our own engine. 

## What about API Extensions provided by DSS C-API?

Oddie is a thin wrapper. It only provides a couple of extra functions, but many of the features from AltDSS/DSS C-API 
requires engine changes or access to internal structures. As such, only a small selection of the API extensions/additions
made on DSS C-API was possible.

## History

Oddie is based on previous internal work done at Unicamp/Brazil to wrap the original OpenDSSDirect.DLL, started in 2016. 
Due to the issues found at the time, the AltDSS/DSS C-API library implementation was born, published in 2018.

Around September 2023, it became possible to use OpenDSSDirect.DLL without using COM structures, as it was moved from COM-style variants to plain pointers. Since then, Oddie has been implemented and tested, and the code was made public in June 2024.

## Limitations

Oddie operates as a user of the OpenDSSDirect.DLL/DCSL for OpenDSS. It cannot handle some memory operations nor modify
how most operations from OpenDSS work.

Bugs or general issues from the OpenDSSDirect.DLL API are not handled in this project.

No API extensions or extra features from AltDSS/DSS C-API are available through Oddie. Only a couple of quality of life
functions were added for easier integration with DSS-Extensions, plus the Error handling mentioned next. All the `Alt_*`,
`Obj_*` and `Batch_*` family of functions were omitted.

When a function that doesn't have an equivalent implementation is called, an error is signaled through the 
(wrapped) Error interface. The functions with missing implementations were kept as placeholder to simplify
building software that alternates between the two implementations. At C level, they can be omitted by
defining `ALTDSS_ODDIE_OMMIT_NOT_IMPL`. Sometimes, there are equivalent functions in the OpenDSSDirect.DLL/DCSL
implementation but they are missing implementation in Oddie. In these latter cases, please feel free to open an issue
in this repository.

## Extra features

Oddie tries to handle the OpenDSS Error interface to provide the high level integration for DSS-Extensions. For example,
DSS-Python/OpenDSSDirect.py (and other many projects) automatically map error numbers/descriptions from the Error interface
to native Python exceptions.

Some other quality-of-life features of the DSS-Extensions still apply. For example, Python iterators are available 
(no need to use First/Next manually), as well as the callable context (`DSSobj("redirect some_file.dss")` instead 
of `DSSObj.Text.Command = "redirect some_file.dss"`).

Since Oddie follows the idea of contexts from AltDSS/DSS C-API, it can safely load multiple different DLLs at the same time. 
For example, if you noticed some behavior changes from one version of OpenDSS to another, you could load both versions and 
interactively compare the results in Python. Beware of issues like the handling of the current working directory done by
the OpenDSS engine, and loading the same DLL multiple times **will not** result in multiple distinct instances; instead, 
the same engine will be wrapped multiple times, so avoid that!

## Status

Since the update from the old 2016 codebase required a lot of changes, we consider this project in the beta phase.

Still, like AltDSS/DSS C-API, Oddie has been successfully cross-validated with the official OpenDSS COM implementation.

Performance has not been greatly explored, but a benchmark should be available after the recent changes from OpenDSS v10 are integrated.
Without full benchmarks, it can be said that some array functions are now quite faster than the COM equivalent, but we cannot tell if that's a general observation yet.

Feedback is always welcome!

## How does one use this?

Like AltDSS/DSS C-API, this is not typically targeted directly for the end user. 
Initially, the projects DSS-Python and OpenDSSDirect.py will provide access and minimal examples. Later on, it should be available as an option on [OpenDSSDirect.jl](https://github.com/dss-extensions/OpenDSSDirect.jl/), [DSS MATLAB](https://github.com/dss-extensions/dss_matlab), [DSS Sharp](https://github.com/dss-extensions/dss_sharp), [AltDSS-Go](https://github.com/dss-extensions/AltDSS-Go), and [AltDSS-Rust](https://github.com/dss-extensions/AltDSS-Rust).

