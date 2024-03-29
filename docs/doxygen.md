<!-- This file is currently included in the Doxygen-generated files -->

`dss.hpp` is a new C++ header-only library to access DSS C-API, including new work-in-progress extensions. It is currently hosted together with the
DSS C-API C headers (as a reminder, DSS C-API is written in Pascal -- some autogenerated docs at https://dss-extensions.org/dss-extensions/pascal/).
The `dss_capi.h` header is also processed in this documentation site. It targets C++17.

`dss.hpp` is part of a larger effort to address the limitations imposed by using the Pascal language in a modern environment. While the wonderful Free Pascal compiler
has been able to support the DSS-Extensions project for several years, better integration with C++ is required to allow us to use other existing and nascent libraries
in C++, and hopefully other languages.

While the more classic API based on the original organization of the official OpenDSS COM implementation is available in the `dss::classic` namespace,
we are growing a new API in the `dss::obj` namespace (which might be renamed in a future release). It currently exposes the manipulation of all DSS data classes.

Some examples and more documentation will be added here, in the GitHub repository, and to https://github.com/dss-extensions/dss-extensions as the implementation progresses.

Currently, Eigen is required to use `dss.hpp`, but we hope to lift the requirement in the future.

**dss.hpp is a work-in-progress project**