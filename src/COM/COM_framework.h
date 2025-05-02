#pragma once

#ifndef STRICT
#define STRICT
#endif

#include "COM_targetver.h"
#define _ATL_APARTMENT_THREADED
#define _ATL_NO_AUTOMATIC_NAMESPACE
#define _ATL_CSTRING_EXPLICIT_CONSTRUCTORS	// some CString constructors will be explicit
#include <comsvcs.h>
#define ATL_NO_ASSERT_ON_DESTROY_NONEXISTENT_WINDOW
#include "COM_resource.h"
#include <atlbase.h>
#include <atlcom.h>
#include <atlctl.h>
#include <atlsafe.h>
#include <comutil.h>
#include <strsafe.h>
#include "COM_AltDSS.h"
