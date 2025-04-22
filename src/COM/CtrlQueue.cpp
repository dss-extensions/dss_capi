// CtrlQueue.cpp : Implementation of CCtrlQueue

#include "pch.h"
#include "CtrlQueue.h"


STDMETHODIMP CCtrlQueue::InterfaceSupportsErrorInfo(REFIID riid)
{
    static const IID* const arr[] = 
    {
        &IID_ICtrlQueue
    };

    for (int i = 0; i < sizeof(arr) / sizeof(arr[0]); i++)
    {
        if (InlineIsEqualGUID(*arr[i], riid))
        {
            return S_OK;
        }
    }
    return S_FALSE;
}

STDMETHODIMP CCtrlQueue::ClearQueue()
{
    dss_capi.CtrlQueue_ClearQueue(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::Delete(long ActionHandle)
{
    dss_capi.CtrlQueue_Delete(dss_capi_ctx, static_cast<int32_t>(ActionHandle));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::get_NumActions(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CtrlQueue_Get_NumActions(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::put_Action(long rhs)
{
    dss_capi.CtrlQueue_Set_Action(dss_capi_ctx, static_cast<int32_t>(rhs));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::get_ActionCode(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CtrlQueue_Get_ActionCode(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::get_DeviceHandle(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CtrlQueue_Get_DeviceHandle(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::Push(long Hour, double Seconds, long ActionCode, long DeviceHandle, long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CtrlQueue_Push(dss_capi_ctx, static_cast<int32_t>(Hour), Seconds, static_cast<int32_t>(ActionCode), static_cast<int32_t>(DeviceHandle));
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::Show()
{
    dss_capi.CtrlQueue_Show(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::ClearActions()
{
    dss_capi.CtrlQueue_ClearActions(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::get_PopAction(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CtrlQueue_Get_PopAction(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::get_QueueSize(long* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    *Value = dss_capi.CtrlQueue_Get_QueueSize(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::DoAllQueue()
{
    dss_capi.CtrlQueue_DoAllQueue(dss_capi_ctx);
    return AltDSS_COM_CheckError();
}

STDMETHODIMP CCtrlQueue::get_Queue(VARIANT* Value)
{
    if (Value == nullptr)
    {
        return E_POINTER;
    }
    return AltDSS_COM_GetStrs(dss_capi.CtrlQueue_Get_Queue, Value);
}

