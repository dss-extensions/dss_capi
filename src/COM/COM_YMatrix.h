
// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only
// 
// COM_YMatrix.h : Declaration of CYMatrix

#pragma once
#include "COM_Resource.h"       // main symbols
#include "COM_DSSExtensions_i.h"
#if defined(_WIN32_WCE) && !defined(_CE_DCOM) && !defined(_CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA)
#error "Single-threaded COM objects are not properly supported on Windows CE platform, such as the Windows Mobile platforms that do not include full DCOM support. Define _CE_ALLOW_SINGLE_THREADED_OBJECTS_IN_MTA to force ATL to support creating single-thread COM object's and allow use of it's single-threaded COM object implementations. The threading model in your rgs file was set to 'Free' as that is the only threading model supported in non DCOM Windows CE platforms."
#endif

using namespace ATL;

// CYMatrix
class ATL_NO_VTABLE CYMatrix :
    public CComObjectRootEx<CComSingleThreadModel>,
    public CComCoClass<CYMatrix, &CLSID_YMatrix>,
    public ISupportErrorInfo,
    public IDispatchImpl<IYMatrix, &IID_IYMatrix, &LIBID_DSSExtensions, /*wMajor =*/ 1, /*wMinor =*/ 0>,
    public CAltDSS_COM<CYMatrix, &IID_IYMatrix>

{
public:
    CYMatrix()
    {
    }

DECLARE_REGISTRY_RESOURCEID(IDR_YMATRIX)

BEGIN_COM_MAP(CYMatrix)
    COM_INTERFACE_ENTRY(IYMatrix)
    COM_INTERFACE_ENTRY(IDispatch)
    COM_INTERFACE_ENTRY(ISupportErrorInfo)
END_COM_MAP()

    STDMETHOD(InterfaceSupportsErrorInfo)(REFIID riid);

    DECLARE_PROTECT_FINAL_CONSTRUCT()

    HRESULT FinalConstruct()
    {
        return AltDSS_COM_CheckError();
    }

    void FinalRelease()
    {
        return;
    }

public:
    STDMETHOD(ZeroInjCurr)();
    STDMETHOD(GetSourceInjCurrents)();
    STDMETHOD(GetPCInjCurr)();
    STDMETHOD_(HRESULT, BuildYMatrixD)(long BuildOps, VARIANT_BOOL AllocateVI);
    STDMETHOD_(HRESULT, AddInAuxCurrents)(long SType);
    STDMETHOD(SolveSystem)();
    STDMETHOD(SetGeneratordQdV)();
    STDMETHOD(CheckConvergence)(VARIANT_BOOL* Value);
    STDMETHOD(get_SystemYChanged)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_SystemYChanged)(VARIANT_BOOL Value);
    STDMETHOD(get_UseAuxCurrents)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_UseAuxCurrents)(VARIANT_BOOL Value);
    STDMETHOD(get_LoadsNeedUpdating)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_LoadsNeedUpdating)(VARIANT_BOOL Value);
    STDMETHOD(get_SolutionInitialized)(VARIANT_BOOL* Value);
    STDMETHOD_(HRESULT, put_SolutionInitialized)(VARIANT_BOOL Value);
    STDMETHOD(get_SolverOptions)(long* Value);
    STDMETHOD_(HRESULT, put_SolverOptions)(long Value);
    STDMETHOD(get_Iteration)(long* Value);
    STDMETHOD_(HRESULT, put_Iteration)(long Value);
    STDMETHOD_(HRESULT, GetCompressedYMatrix)(VARIANT* ColPtr, VARIANT* RowIdxPtr, VARIANT* cVals);
};

OBJECT_ENTRY_AUTO(__uuidof(YMatrix), CYMatrix)
