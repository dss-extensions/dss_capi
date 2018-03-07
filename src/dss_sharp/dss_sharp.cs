// dss_sharp: A compatibility layer for DSS_CAPI that mimics the official OpenDSS COM interface.
// Copyright (c) 2016-2018 Paulo Meira
// version 0.9.4
//
// See LICENSE for more information.

using System;
using System.Runtime.InteropServices;

namespace dss_sharp
{
    public static class OpenDSS 
    {
        public static class Lib 
        { 
            /// <summary>
            /// Custom marshaler to avoid the deallocation of Pascal strings
            /// 
            /// The default Marshaler tries to deallocate the native memory, which
            /// is too restrictive, since Pascal uses a different memory allocation 
            /// model.
            /// 
            /// Note: we probably should use custom marshalers for every other 
            ///       pointer in future.
            /// 
            /// Credit: https://stackoverflow.com/a/38677638
            /// </summary>
            private class ConstCharPtrMarshaler : ICustomMarshaler
            {
                public object MarshalNativeToManaged(IntPtr pNativeData)
                {
                    return Marshal.PtrToStringAnsi(pNativeData);
                }

                public IntPtr MarshalManagedToNative(object ManagedObj)
                {
                    return IntPtr.Zero;
                }

                public void CleanUpNativeData(IntPtr pNativeData)
                {
                }

                public void CleanUpManagedData(object ManagedObj)
                {
                }

                public int GetNativeDataSize()
                {
                    return IntPtr.Size;
                }

                static readonly ConstCharPtrMarshaler instance = new ConstCharPtrMarshaler();

                public static ICustomMarshaler GetInstance(string cookie)
                {
                    return instance;
                }
            }
        
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_ResetStringBuffer();

            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_Dispose_PByte(ref IntPtr p);

            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_Dispose_PDouble(ref IntPtr p);

            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_Dispose_PInteger(ref IntPtr p);

            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_Dispose_PPAnsiChar(ref IntPtr p, int cnt);
        
            public static string[] get_string_array(ref IntPtr resultPtr, int resultCount)
            {
                string[] result = new string[resultCount];
                for (int i = 0; i < resultCount; ++i)
                {
                    IntPtr resultPtrInternal = Marshal.ReadIntPtr(resultPtr, IntPtr.Size * i);
                    result[i] = Marshal.PtrToStringAnsi(resultPtrInternal);
                }
                OpenDSS.Lib.DSS_Dispose_PPAnsiChar(ref resultPtr, resultCount);
                return result;
            }
        
            public static double[] get_float64_array(ref IntPtr resultPtr, int resultCount)
            {
                double[] result = new double[resultCount];
                Marshal.Copy(resultPtr, result, 0, resultCount);
                OpenDSS.Lib.DSS_Dispose_PDouble(ref resultPtr);
                return result;
            }
            
            public static int[] get_int32_array(ref IntPtr resultPtr, int resultCount)
            {
                int[] result = new int[resultCount];
                Marshal.Copy(resultPtr, result, 0, resultCount);
                OpenDSS.Lib.DSS_Dispose_PInteger(ref resultPtr);
                return result;
            }
        
            public static byte[] get_int8_array(ref IntPtr resultPtr, int resultCount)
            {
                byte[] result = new byte[resultCount];
                Marshal.Copy(resultPtr, result, 0, resultCount);
                OpenDSS.Lib.DSS_Dispose_PByte(ref resultPtr);
                return result;
            }
        
        
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void ActiveClass_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int ActiveClass_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int ActiveClass_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string ActiveClass_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void ActiveClass_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int ActiveClass_Get_NumElements();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string ActiveClass_Get_ActiveClassName();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int ActiveClass_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Bus_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Bus_Get_NumNodes();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_SeqVoltages(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_Voltages(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_Nodes(ref IntPtr /* int* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_Isc(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_Voc(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_kVBase();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_puVoltages(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_Zsc0(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_Zsc1(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_ZscMatrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Bus_ZscRefresh();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_YscMatrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Bus_Get_Coorddefined();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_x();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Set_x(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_y();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Set_y(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_Distance();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Bus_GetUniqueNodeNumber(int StartNumber);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_CplxSeqVoltages(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_Int_Duration();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_Lambda();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_Cust_Duration();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_Cust_Interrupts();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Bus_Get_N_Customers();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_N_interrupts();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_puVLL(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_VLL(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_puVmagAngle(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Bus_Get_VMagAngle(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Bus_Get_TotalMiles();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Bus_Get_SectionID();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Capacitors_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Capacitors_Get_IsDelta();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Capacitors_Get_kV();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Capacitors_Get_kvar();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Capacitors_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Capacitors_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Capacitors_Get_NumSteps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Set_IsDelta([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Set_kV(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Set_kvar(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Set_NumSteps(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Capacitors_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Capacitors_AddStep();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Capacitors_SubtractStep();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Capacitors_Get_AvailableSteps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Get_States(ref IntPtr /* int* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Set_States([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] int[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Open();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Capacitors_Close();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string CapControls_Get_Capacitor();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CapControls_Get_CTratio();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CapControls_Get_DeadTime();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CapControls_Get_Delay();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CapControls_Get_DelayOff();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CapControls_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CapControls_Get_Mode();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string CapControls_Get_MonitoredObj();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CapControls_Get_MonitoredTerm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string CapControls_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CapControls_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CapControls_Get_OFFSetting();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CapControls_Get_ONSetting();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CapControls_Get_PTratio();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort CapControls_Get_UseVoltOverride();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CapControls_Get_Vmax();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CapControls_Get_Vmin();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_Capacitor([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_CTratio(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_DeadTime(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_Delay(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_DelayOff(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_Mode(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_MonitoredObj([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_MonitoredTerm(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_OFFSetting(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_ONSetting(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_PTratio(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_UseVoltOverride([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_Vmax(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Set_Vmin(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CapControls_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CapControls_Reset();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Circuit_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_Get_NumBuses();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_Get_NumCktElements();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_Get_NumNodes();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_LineLosses(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_Losses(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllBusVmag(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllBusVolts(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllElementNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_SubstationLosses(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_TotalPower(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Disable([param: MarshalAs(UnmanagedType.LPStr)] string Name);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Enable([param: MarshalAs(UnmanagedType.LPStr)] string Name);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_FirstPCElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_FirstPDElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_NextPCElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_NextPDElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllBusNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllElementLosses(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Sample();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_SaveSample();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_SetActiveElement([param: MarshalAs(UnmanagedType.LPStr)] string FullName);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Circuit_Capacity(double Start, double Increment);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllBusVmagPu(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_SetActiveBus([param: MarshalAs(UnmanagedType.LPStr)] string BusName);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_SetActiveBusi(int BusIndex);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllNodeNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_SystemY(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllBusDistances(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllNodeDistances(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllNodeDistancesByPhase(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, int Phase);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllNodeVmagByPhase(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, int Phase);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllNodeVmagPUByPhase(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, int Phase);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_AllNodeNamesByPhase(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount, int Phase);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_SetActiveClass([param: MarshalAs(UnmanagedType.LPStr)] string ClassName);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_FirstElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_NextElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_UpdateStorage();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Circuit_Get_ParentPDElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_EndOfTimeStepUpdate();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_YNodeOrder(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_YCurrents(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Circuit_Get_YNodeVarray(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_BusNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string CktElement_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CktElement_Get_NumConductors();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CktElement_Get_NumPhases();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CktElement_Get_NumTerminals();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Set_BusNames([In] string[] /* sbyte** */ ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_Currents(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_Voltages(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CktElement_Get_EmergAmps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort CktElement_Get_Enabled();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_Losses(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CktElement_Get_NormalAmps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_PhaseLosses(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_Powers(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_SeqCurrents(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_SeqPowers(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_SeqVoltages(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Close(int Term, int Phs);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Open(int Term, int Phs);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Set_EmergAmps(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Set_Enabled([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Set_NormalAmps(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort CktElement_IsOpen(int Term, int Phs);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_AllPropertyNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CktElement_Get_NumProperties();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_Residuals(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_Yprim(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string CktElement_Get_DisplayName();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string CktElement_Get_GUID();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CktElement_Get_Handle();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Set_DisplayName([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string CktElement_Get_Controller(int idx);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string CktElement_Get_EnergyMeter();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort CktElement_Get_HasVoltControl();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort CktElement_Get_HasSwitchControl();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_CplxSeqVoltages(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_CplxSeqCurrents(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_AllVariableNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_AllVariableValues(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CktElement_Get_Variable([param: MarshalAs(UnmanagedType.LPStr)] string MyVarName, int Code);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CktElement_Get_Variablei(int Idx, int Code);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_NodeOrder(ref IntPtr /* int* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort CktElement_Get_HasOCPDevice();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CktElement_Get_NumControls();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CktElement_Get_OCPDevIndex();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CktElement_Get_OCPDevType();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_CurrentsMagAng(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CktElement_Get_VoltagesMagAng(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CmathLib_Get_cmplx(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, double RealPart, double ImagPart);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CmathLib_Get_cabs(double realpart, double imagpart);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double CmathLib_Get_cdang(double RealPart, double ImagPart);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CmathLib_Get_ctopolardeg(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, double RealPart, double ImagPart);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CmathLib_Get_pdegtocomplex(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, double magnitude, double angle);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CmathLib_Get_cmul(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, double a1, double b1, double a2, double b2);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CmathLib_Get_cdiv(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, double a1, double b1, double a2, double b2);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CtrlQueue_ClearQueue();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CtrlQueue_Delete(int ActionHandle);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CtrlQueue_Get_ActionCode();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CtrlQueue_Get_DeviceHandle();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CtrlQueue_Get_NumActions();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CtrlQueue_Show();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CtrlQueue_ClearActions();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CtrlQueue_Get_PopAction();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CtrlQueue_Set_Action(int Param1);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int CtrlQueue_Get_QueueSize();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CtrlQueue_DoAllQueue();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void CtrlQueue_Get_Queue(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int DSS_Get_NumCircuits();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_ClearAll();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSS_Get_Version();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort DSS_Start(int code);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_Get_Classes(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_Get_UserClasses(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int DSS_Get_NumClasses();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int DSS_Get_NumUserClasses();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSS_Get_DataPath();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_Set_DataPath([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSS_Reset();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSS_Get_DefaultEditor();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int DSS_SetActiveClass([param: MarshalAs(UnmanagedType.LPStr)] string ClassName);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSSElement_Get_AllPropertyNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSSElement_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int DSSElement_Get_NumProperties();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSSimComs_BusVoltagepu(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, ulong Index);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSSimComs_BusVoltage(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, ulong Index);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSSProgress_Close();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSSProgress_Set_Caption([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSSProgress_Set_PctProgress(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSSProgress_Show();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSSProperty_Get_Description();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSSProperty_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSSProperty_Get_Val();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void DSSProperty_Set_Val([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
            public static extern void DSSProperty_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            [DllImport("dss_capi", CallingConvention = CallingConvention.Cdecl, CharSet = CharSet.Ansi)]
            public static extern void DSSProperty_Set_Index(int Value);
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSS_Executive_Get_Command(int i);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int DSS_Executive_Get_NumCommands();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int DSS_Executive_Get_NumOptions();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSS_Executive_Get_Option(int i);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSS_Executive_Get_CommandHelp(int i);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSS_Executive_Get_OptionHelp(int i);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string DSS_Executive_Get_OptionValue(int i);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Error_Get_Description();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Error_Get_Number();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Fuses_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Fuses_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Fuses_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Fuses_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Fuses_Get_MonitoredObj();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Fuses_Get_MonitoredTerm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Fuses_Get_SwitchedObj();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Set_MonitoredObj([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Set_MonitoredTerm(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Set_SwitchedObj([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Fuses_Get_SwitchedTerm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Set_SwitchedTerm(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Fuses_Get_TCCcurve();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Set_TCCcurve([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Fuses_Get_RatedCurrent();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Set_RatedCurrent(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Fuses_Get_Delay();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Open();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Close();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Set_Delay(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Fuses_Get_idx();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Fuses_Set_idx(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Fuses_Get_NumPhases();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Generators_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Generators_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Generators_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Get_RegisterNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Get_RegisterValues(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Generators_Get_ForcedON();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_ForcedON([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Generators_Get_kV();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Generators_Get_kvar();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Generators_Get_kW();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Generators_Get_PF();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Generators_Get_Phases();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_kV(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_kvar(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_kW(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_PF(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_Phases(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Generators_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Generators_Get_idx();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_idx(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Generators_Get_Model();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_Model(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Generators_Get_kVArated();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_kVArated(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Generators_Get_Vmaxpu();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Generators_Get_Vminpu();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_Vmaxpu(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Generators_Set_Vminpu(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void ISources_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int ISources_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int ISources_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int ISources_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string ISources_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void ISources_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double ISources_Get_Amps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void ISources_Set_Amps(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double ISources_Get_AngleDeg();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double ISources_Get_Frequency();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void ISources_Set_AngleDeg(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void ISources_Set_Frequency(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int LineCodes_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int LineCodes_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int LineCodes_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string LineCodes_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort LineCodes_Get_IsZ1Z0();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int LineCodes_Get_Units();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_Units(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int LineCodes_Get_Phases();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_Phases(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LineCodes_Get_R1();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_R1(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LineCodes_Get_X1();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_X1(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LineCodes_Get_R0();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LineCodes_Get_X0();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_R0(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_X0(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LineCodes_Get_C0();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LineCodes_Get_C1();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_C0(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_C1(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Get_Cmatrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Get_Rmatrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Get_Xmatrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_Cmatrix([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_Rmatrix([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_Xmatrix([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LineCodes_Get_NormAmps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_NormAmps(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LineCodes_Get_EmergAmps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Set_EmergAmps(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LineCodes_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Lines_Get_Bus1();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Lines_Get_Bus2();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Lines_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_Length();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Lines_Get_LineCode();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Lines_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Lines_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Lines_Get_Phases();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_R1();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_X1();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Lines_New([param: MarshalAs(UnmanagedType.LPStr)] string Name);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Bus1([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Bus2([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Length(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_LineCode([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Phases(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_R1(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_X1(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_C0();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_C1();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Get_Cmatrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_R0();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Get_Rmatrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_X0();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Get_Xmatrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_C0(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_C1(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Cmatrix([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_R0(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Rmatrix([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_X0(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Xmatrix([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_EmergAmps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_NormAmps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_EmergAmps(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_NormAmps(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Lines_Get_Geometry();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Geometry([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_Rg();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_Rho();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Lines_Get_Xg();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Rg(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Rho(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Xg(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Get_Yprim(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Yprim([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Lines_Get_NumCust();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Lines_Get_TotalCust();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Lines_Get_Parent();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Lines_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Lines_Get_Spacing();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Spacing([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Lines_Get_Units();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Lines_Set_Units(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Loads_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Loads_Get_idx();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Loads_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Loads_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_idx(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_kV();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_kvar();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_kW();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_PF();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_kV(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_kvar(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_kW(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_PF(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Loads_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_AllocationFactor();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_Cfactor();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Loads_Get_Class_();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Loads_Get_CVRcurve();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_CVRvars();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_CVRwatts();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Loads_Get_daily();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Loads_Get_duty();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Loads_Get_Growth();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Loads_Get_IsDelta();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_kva();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_kwh();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_kwhdays();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Loads_Get_Model();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Loads_Get_NumCust();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_PctMean();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_PctStdDev();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_Rneut();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Loads_Get_Spectrum();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Loads_Get_Status();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_Vmaxpu();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_Vminemerg();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_Vminnorm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_Vminpu();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_xfkVA();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_Xneut();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Loads_Get_Yearly();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_AllocationFactor(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Cfactor(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Class_(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_CVRcurve([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_CVRvars(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_CVRwatts(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_daily([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_duty([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Growth([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_IsDelta([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_kva(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_kwh(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_kwhdays(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Model(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_NumCust(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_PctMean(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_PctStdDev(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Rneut(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Spectrum([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Status(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Vmaxpu(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Vminemerg(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Vminnorm(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Vminpu(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_xfkVA(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Xneut(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_Yearly([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Get_ZIPV(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_ZIPV([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_pctSeriesRL();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Loads_Set_pctSeriesRL(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Loads_Get_RelWeight();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string LoadShapes_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int LoadShapes_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int LoadShapes_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int LoadShapes_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int LoadShapes_Get_Npts();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Get_Pmult(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Get_Qmult(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_Npts(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_Pmult([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_Qmult([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Normalize();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Get_TimeArray(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_TimeArray([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LoadShapes_Get_HrInterval();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LoadShapes_Get_MinInterval();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LoadShapes_Get_sInterval();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_HrInterval(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_MinInterval(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_Sinterval(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LoadShapes_Get_PBase();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double LoadShapes_Get_Qbase();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_PBase(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_Qbase(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort LoadShapes_Get_UseActual();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void LoadShapes_Set_UseActual([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Meters_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Get_RegisterNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Get_RegisterValues(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Reset();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_ResetAll();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Sample();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Save();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Get_Totals(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Get_Peakcurrent(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Set_Peakcurrent([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Get_CalcCurrent(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Set_CalcCurrent([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Get_AllocFactors(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Set_AllocFactors([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Meters_Get_MeteredElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_MeteredTerminal();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Set_MeteredElement([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Set_MeteredTerminal(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Meters_Get_DIFilesAreOpen();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_CloseAllDIFiles();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_OpenAllDIFiles();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_SampleAll();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_SaveAll();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Get_AllEndElements(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_CountEndElements();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Get_AllBranchesInZone(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_CountBranches();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Meters_Get_SAIFI();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_SequenceIndex();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_Set_SequenceIndex(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Meters_Get_SAIFIKW();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_DoReliabilityCalc([MarshalAs(UnmanagedType.U2)] bool AssumeRestoration);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_SeqListSize();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_TotalCustomers();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Meters_Get_SAIDI();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Meters_Get_CustInterrupts();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_NumSections();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Meters_SetActiveSection(int SectIdx);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Meters_Get_AvgRepairTime();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Meters_Get_FaultRateXRepairHrs();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_NumSectionBranches();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_NumSectionCustomers();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_OCPDeviceType();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Meters_Get_SumBranchFltRates();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_SectSeqIdx();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Meters_Get_SectTotalCust();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Monitors_Get_FileName();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Monitors_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Monitors_Get_Mode();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Monitors_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Monitors_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Reset();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_ResetAll();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Sample();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Save();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Set_Mode(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Show();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Get_ByteStream(ref IntPtr /* byte* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Monitors_Get_SampleCount();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_SampleAll();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_SaveAll();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Monitors_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Process();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_ProcessAll();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Get_Channel(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, int Index);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Get_dblFreq(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Get_dblHour(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Monitors_Get_FileVersion();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Get_Header(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Monitors_Get_NumChannels();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Monitors_Get_RecordSize();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Monitors_Get_Element();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Set_Element([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Monitors_Get_Terminal();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Monitors_Set_Terminal(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Parser_Get_CmdString();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_Set_CmdString([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Parser_Get_NextParam();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Parser_Get_AutoIncrement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_Set_AutoIncrement([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Parser_Get_DblValue();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Parser_Get_IntValue();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Parser_Get_StrValue();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Parser_Get_WhiteSpace();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_Set_WhiteSpace([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Parser_Get_BeginQuote();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Parser_Get_EndQuote();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_Set_BeginQuote([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_Set_EndQuote([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Parser_Get_Delimiters();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_Set_Delimiters([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_ResetDelimiters();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_Get_Vector(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, int ExpectedSize);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_Get_Matrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, int ExpectedOrder);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Parser_Get_SymMatrix(ref IntPtr /* double* */ ResultPtr, ref int ResultCount, int ExpectedOrder);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PDElements_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PDElements_Get_FaultRate();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PDElements_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort PDElements_Get_IsShunt();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PDElements_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PDElements_Get_pctPermanent();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PDElements_Set_FaultRate(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PDElements_Set_pctPermanent(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string PDElements_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PDElements_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PDElements_Get_AccumulatedL();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PDElements_Get_Lambda();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PDElements_Get_Numcustomers();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PDElements_Get_ParentPDElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PDElements_Get_RepairTime();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PDElements_Get_Totalcustomers();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PDElements_Get_FromTerminal();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PDElements_Get_TotalMiles();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PDElements_Get_SectionID();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PDElements_Set_RepairTime(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PVSystems_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PVSystems_Get_RegisterNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PVSystems_Get_RegisterValues(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PVSystems_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PVSystems_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PVSystems_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int PVSystems_Get_idx();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PVSystems_Set_idx(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string PVSystems_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PVSystems_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PVSystems_Get_Irradiance();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void PVSystems_Set_Irradiance(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PVSystems_Get_kvar();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PVSystems_Get_kVArated();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PVSystems_Get_kW();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double PVSystems_Get_PF();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Reclosers_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Reclosers_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Reclosers_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Reclosers_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Reclosers_Get_MonitoredTerm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_MonitoredTerm(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Reclosers_Get_SwitchedObj();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_SwitchedObj([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Reclosers_Get_MonitoredObj();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Reclosers_Get_SwitchedTerm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_MonitoredObj([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_SwitchedTerm(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Reclosers_Get_NumFast();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Get_RecloseIntervals(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Reclosers_Get_Shots();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_NumFast(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_Shots(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Reclosers_Get_PhaseTrip();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_PhaseTrip(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Reclosers_Get_GroundInst();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Reclosers_Get_GroundTrip();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Reclosers_Get_PhaseInst();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_GroundInst(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_GroundTrip(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_PhaseInst(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Close();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Open();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Reclosers_Get_idx();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Reclosers_Set_idx(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_CTPrimary();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_Delay();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int RegControls_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_ForwardBand();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_ForwardR();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_ForwardVreg();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_ForwardX();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort RegControls_Get_IsInverseTime();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort RegControls_Get_IsReversible();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int RegControls_Get_MaxTapChange();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string RegControls_Get_MonitoredBus();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string RegControls_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int RegControls_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_PTratio();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_ReverseBand();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_ReverseR();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_ReverseVreg();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_ReverseX();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_TapDelay();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int RegControls_Get_TapWinding();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string RegControls_Get_Transformer();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double RegControls_Get_VoltageLimit();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int RegControls_Get_Winding();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int RegControls_Get_TapNumber();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_CTPrimary(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_Delay(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_ForwardBand(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_ForwardR(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_ForwardVreg(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_ForwardX(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_IsInverseTime([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_IsReversible([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_MaxTapChange(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_MonitoredBus([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_PTratio(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_ReverseBand(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_ReverseR(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_ReverseVreg(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_ReverseX(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_TapDelay(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_TapWinding(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_Transformer([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_VoltageLimit(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_Winding(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Set_TapNumber(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int RegControls_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void RegControls_Reset();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Relays_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Relays_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Relays_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Relays_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Relays_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Relays_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Relays_Get_MonitoredObj();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Relays_Set_MonitoredObj([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Relays_Get_MonitoredTerm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Relays_Get_SwitchedObj();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Relays_Set_MonitoredTerm(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Relays_Set_SwitchedObj([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Relays_Get_SwitchedTerm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Relays_Set_SwitchedTerm(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Relays_Get_idx();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Relays_Set_idx(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Sensors_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Get_Currents(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Sensors_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Sensors_Get_IsDelta();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Get_kVARS(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Get_kVS(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Get_kWS(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Sensors_Get_MeteredElement();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Sensors_Get_MeteredTerminal();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Sensors_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Sensors_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Sensors_Get_PctError();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Sensors_Get_ReverseDelta();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Sensors_Get_Weight();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Reset();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_ResetAll();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_Currents([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_IsDelta([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_kVARS([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_kVS([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_kWS([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_MeteredElement([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_MeteredTerminal(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_PctError(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_ReverseDelta([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_Weight(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Sensors_Get_kVbase();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Sensors_Set_kVbase(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Settings_Get_AllowDuplicates();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Settings_Get_AutoBusList();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Settings_Get_CktModel();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Settings_Get_EmergVmaxpu();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Settings_Get_EmergVminpu();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Settings_Get_NormVmaxpu();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Settings_Get_NormVminpu();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Settings_Get_ZoneLock();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_AllocationFactors(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_AllowDuplicates([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_AutoBusList([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_CktModel(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_EmergVmaxpu(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_EmergVminpu(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_NormVmaxpu(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_NormVminpu(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_ZoneLock([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Get_LossRegs(ref IntPtr /* int* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Settings_Get_LossWeight();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Settings_Get_Trapezoidal();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Get_UEregs(ref IntPtr /* int* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Settings_Get_UEweight();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_LossRegs([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] int[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_LossWeight(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_Trapezoidal([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_UEregs([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] int[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_UEweight(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Settings_Get_ControlTrace();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Get_VoltageBases(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_ControlTrace([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_VoltageBases([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Settings_Get_PriceCurve();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Settings_Get_PriceSignal();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_PriceCurve([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Settings_Set_PriceSignal(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_Frequency();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_Hour();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_Iterations();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_LoadMult();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_MaxIterations();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_Mode();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_Number();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_Random();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_Seconds();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_StepSize();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_Tolerance();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_Year();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Frequency(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Hour(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_LoadMult(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_MaxIterations(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Mode(int Mode);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Number(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Random(int Random);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Seconds(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_StepSize(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Tolerance(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Year(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Solve();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Solution_Get_ModeID();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_LoadModel();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_LoadModel(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Solution_Get_LDCurve();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_LDCurve([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_pctGrowth();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_pctGrowth(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_AddType();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_AddType(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_GenkW();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_GenkW(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_GenPF();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_GenPF(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_Capkvar();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Capkvar(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_Algorithm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Algorithm(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_ControlMode();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_ControlMode(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_GenMult();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_GenMult(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Solution_Get_DefaultDaily();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Solution_Get_DefaultYearly();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_DefaultDaily([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_DefaultYearly([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Get_EventLog(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_dblHour();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_dblHour(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_StepsizeHr(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_StepsizeMin(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_ControlIterations();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_MaxControlIterations();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Sample_DoControlActions();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_ControlIterations(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_MaxControlIterations(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_CheckFaultStatus();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_SolveDirect();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_SolveNoControl();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_SolvePflow();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_SolvePlusControl();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_SolveSnap();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_CheckControls();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_InitSnap();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Solution_Get_SystemYChanged();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_BuildYMatrix(int BuildOption, int AllocateVI);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_DoControlActions();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_SampleControlDevices();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Solution_Get_Converged();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Converged([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_Totaliterations();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_MostIterationsDone();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Solution_Get_ControlActionsDone();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_ControlActionsDone([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Cleanup();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_FinishTimeStep();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_Process_Time();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_Total_Time();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_Total_Time(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_Time_of_Step();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Solution_Get_IntervalHrs();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_IntervalHrs(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Solution_Get_MinIterations();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Solution_Set_MinIterations(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int SwtControls_Get_Action();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double SwtControls_Get_Delay();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int SwtControls_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort SwtControls_Get_IsLocked();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string SwtControls_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int SwtControls_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string SwtControls_Get_SwitchedObj();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int SwtControls_Get_SwitchedTerm();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Set_Action(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Set_Delay(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Set_IsLocked([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Set_SwitchedObj([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Set_SwitchedTerm(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int SwtControls_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int SwtControls_Get_NormalState();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Set_NormalState(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int SwtControls_Get_State();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Set_State(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void SwtControls_Reset();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Text_Get_Command();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Text_Set_Command([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Text_Get_Result();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_NumLoops();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_ActiveBranch();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Topology_Get_AllIsolatedBranches(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Topology_Get_AllLoopedPairs(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_BackwardBranch();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Topology_Get_BranchName();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_ForwardBranch();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_LoopedBranch();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_NumIsolatedBranches();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_ParallelBranch();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Topology_Set_BranchName([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Topology_Get_AllIsolatedLoads(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_FirstLoad();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_NextLoad();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_NumIsolatedLoads();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Topology_Get_ActiveLevel();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Topology_Get_BusName();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Topology_Set_BusName([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Transformers_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern ushort Transformers_Get_IsDelta();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_kV();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_kVA();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_MaxTap();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_MinTap();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Transformers_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Transformers_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Transformers_Get_NumTaps();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Transformers_Get_NumWindings();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_R();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_Rneut();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_Tap();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Transformers_Get_Wdg();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Transformers_Get_XfmrCode();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_Xhl();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_Xht();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_Xlt();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Transformers_Get_Xneut();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_IsDelta([MarshalAs(UnmanagedType.U2)] bool Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_kV(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_kVA(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_MaxTap(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_MinTap(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_NumTaps(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_NumWindings(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_R(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_Rneut(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_Tap(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_Wdg(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_XfmrCode([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_Xhl(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_Xht(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_Xlt(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Transformers_Set_Xneut(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Transformers_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Vsources_Get_AllNames(ref IntPtr /* sbyte** */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Vsources_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Vsources_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Vsources_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string Vsources_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Vsources_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Vsources_Get_BasekV();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Vsources_Get_pu();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Vsources_Set_BasekV(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Vsources_Set_pu(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Vsources_Get_AngleDeg();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double Vsources_Get_Frequency();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int Vsources_Get_Phases();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Vsources_Set_AngleDeg(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Vsources_Set_Frequency(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void Vsources_Set_Phases(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int XYCurves_Get_Count();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int XYCurves_Get_First();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            [return: MarshalAs(UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof(ConstCharPtrMarshaler))]
            public static extern string XYCurves_Get_Name();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int XYCurves_Get_Next();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Set_Name([param: MarshalAs(UnmanagedType.LPStr)] string Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern int XYCurves_Get_Npts();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Get_Xarray(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Set_Npts(int Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Set_Xarray([MarshalAs(UnmanagedType.LPArray, SizeParamIndex = 1)] double[] ValuePtr, int ValueCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double XYCurves_Get_x();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double XYCurves_Get_y();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Get_Yarray(ref IntPtr /* double* */ ResultPtr, ref int ResultCount);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Set_x(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Set_y(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double XYCurves_Get_Xscale();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double XYCurves_Get_Xshift();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double XYCurves_Get_Yscale();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern double XYCurves_Get_Yshift();
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Set_Xscale(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Set_Xshift(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Set_Yscale(double Value);
            
            [DllImport("dss_capi", CallingConvention=CallingConvention.Cdecl, CharSet=CharSet.Ansi)]
            public static extern void XYCurves_Set_Yshift(double Value);
            
        }

        public class DssException: Exception
        {
            public int ErrorNumber;
            public string ErrorMessage;

            public DssException()
            {
            }

            public DssException(int number, string message)
            {
                ErrorNumber = number;
                ErrorMessage = message;
            }
        }
        
        public static void CheckForError()
        {
            var error_num = OpenDSS.Lib.Error_Get_Number();
            if (error_num != 0)
            {
                throw new DssException(error_num, OpenDSS.Lib.Error_Get_Description());
            }
        }
    }

    public class DSSEvents
    {
        // Not implemented
    }
    
    public class ActiveClass
    {

        /// <summary>
        /// (read-only) Returns name of active class.
        /// </summary>
        public /*static*/ string ActiveClassName
        {
            get {
                return OpenDSS.Lib.ActiveClass_Get_ActiveClassName();
            }
        }

        /// <summary>
        /// (read-only) Array of strings consisting of all element names in the active class.
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.ActiveClass_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of elements in Active Class. Same as NumElements Property.
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.ActiveClass_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Sets first element in the active class to be the active DSS object. If object is a CktElement, ActiveCktELment also points to this element. Returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.ActiveClass_Get_First();
            }
        }

        /// <summary>
        /// (read-only) Name of the Active Element of the Active Class
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.ActiveClass_Get_Name();
            }
            set {
                OpenDSS.Lib.ActiveClass_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets next element in active class to be the active DSS object. If object is a CktElement, ActiveCktElement also points to this element.  Returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.ActiveClass_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Number of elements in this class. Same as Count property.
        /// </summary>
        public /*static*/ int NumElements
        {
            get {
                return OpenDSS.Lib.ActiveClass_Get_NumElements();
            }
        }
    } // ActiveClass 


    public class Bus
    {

        public /*static*/ int GetUniqueNodeNumber(int StartNumber)
        {
            return OpenDSS.Lib.Bus_GetUniqueNodeNumber(StartNumber);
        }

        public /*static*/ bool ZscRefresh()
        {
            return OpenDSS.Lib.Bus_ZscRefresh() != 0;
        }

        /// <summary>
        /// (read-only) False=0 else True. Indicates whether a coordinate has been defined for this bus
        /// </summary>
        public /*static*/ bool Coorddefined
        {
            get {
                return OpenDSS.Lib.Bus_Get_Coorddefined() != 0;
            }
        }

        /// <summary>
        /// (read-only) Complex Double array of Sequence Voltages (0, 1, 2) at this Bus.
        /// </summary>
        public /*static*/ double[] CplxSeqVoltages
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_CplxSeqVoltages(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Accumulated customer outage durations
        /// </summary>
        public /*static*/ double Cust_Duration
        {
            get {
                return OpenDSS.Lib.Bus_Get_Cust_Duration();
            }
        }

        /// <summary>
        /// (read-only) Annual number of customer-interruptions from this bus
        /// </summary>
        public /*static*/ double Cust_Interrupts
        {
            get {
                return OpenDSS.Lib.Bus_Get_Cust_Interrupts();
            }
        }

        /// <summary>
        /// (read-only) Distance from energymeter (if non-zero)
        /// </summary>
        public /*static*/ double Distance
        {
            get {
                return OpenDSS.Lib.Bus_Get_Distance();
            }
        }

        /// <summary>
        /// (read-only) Average interruption duration, hr.
        /// </summary>
        public /*static*/ double Int_Duration
        {
            get {
                return OpenDSS.Lib.Bus_Get_Int_Duration();
            }
        }

        /// <summary>
        /// (read-only) Short circuit currents at bus; Complex Array.
        /// </summary>
        public /*static*/ double[] Isc
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_Isc(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Accumulated failure rate downstream from this bus; faults per year
        /// </summary>
        public /*static*/ double Lambda
        {
            get {
                return OpenDSS.Lib.Bus_Get_Lambda();
            }
        }

        /// <summary>
        /// (read-only) Total numbers of customers served downline from this bus
        /// </summary>
        public /*static*/ int N_Customers
        {
            get {
                return OpenDSS.Lib.Bus_Get_N_Customers();
            }
        }

        /// <summary>
        /// (read-only) Number of interruptions this bus per year
        /// </summary>
        public /*static*/ double N_interrupts
        {
            get {
                return OpenDSS.Lib.Bus_Get_N_interrupts();
            }
        }

        /// <summary>
        /// (read-only) Name of Bus
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Bus_Get_Name();
            }
        }

        /// <summary>
        /// (read-only) Integer Array of Node Numbers defined at the bus in same order as the voltages.
        /// </summary>
        public /*static*/ int[] Nodes
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_Nodes(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_int32_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of Nodes this bus.
        /// </summary>
        public /*static*/ int NumNodes
        {
            get {
                return OpenDSS.Lib.Bus_Get_NumNodes();
            }
        }

        /// <summary>
        /// (read-only) Integer ID of the feeder section in which this bus is located.
        /// </summary>
        public /*static*/ int SectionID
        {
            get {
                return OpenDSS.Lib.Bus_Get_SectionID();
            }
        }

        /// <summary>
        /// (read-only) Double Array of sequence voltages at this bus.
        /// </summary>
        public /*static*/ double[] SeqVoltages
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_SeqVoltages(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Total length of line downline from this bus, in miles. For recloser siting algorithm.
        /// </summary>
        public /*static*/ double TotalMiles
        {
            get {
                return OpenDSS.Lib.Bus_Get_TotalMiles();
            }
        }

        /// <summary>
        /// (read-only) For 2- and 3-phase buses, returns array of complex numbers represetin L-L voltages in volts. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only first 3.
        /// </summary>
        public /*static*/ double[] VLL
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_VLL(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Variant Array of doubles containing voltages in Magnitude (VLN), angle (deg) 
        /// </summary>
        public /*static*/ double[] VMagAngle
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_VMagAngle(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Open circuit voltage; Complex array.
        /// </summary>
        public /*static*/ double[] Voc
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_Voc(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex array of voltages at this bus.
        /// </summary>
        public /*static*/ double[] Voltages
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_Voltages(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex array of Ysc matrix at bus. Column by column.
        /// </summary>
        public /*static*/ double[] YscMatrix
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_YscMatrix(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex Zero-Sequence short circuit impedance at bus.
        /// </summary>
        public /*static*/ double[] Zsc0
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_Zsc0(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex Positive-Sequence short circuit impedance at bus..
        /// </summary>
        public /*static*/ double[] Zsc1
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_Zsc1(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex array of Zsc matrix at bus. Column by column.
        /// </summary>
        public /*static*/ double[] ZscMatrix
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_ZscMatrix(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Base voltage at bus in kV
        /// </summary>
        public /*static*/ double kVBase
        {
            get {
                return OpenDSS.Lib.Bus_Get_kVBase();
            }
        }

        /// <summary>
        /// (read-only) Returns Complex array of pu L-L voltages for 2- and 3-phase buses. Returns -1.0 for 1-phase bus. If more than 3 phases, returns only 3 phases.
        /// </summary>
        public /*static*/ double[] puVLL
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_puVLL(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of doubles containig voltage magnitude, angle pairs in per unit
        /// </summary>
        public /*static*/ double[] puVmagAngle
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_puVmagAngle(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex Array of pu voltages at the bus.
        /// </summary>
        public /*static*/ double[] puVoltages
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Bus_Get_puVoltages(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// X Coordinate for bus (double)
        /// </summary>
        public /*static*/ double x
        {
            get {
                return OpenDSS.Lib.Bus_Get_x();
            }
            set {
                OpenDSS.Lib.Bus_Set_x(value);
            }
        }

        /// <summary>
        /// Y coordinate for bus(double)
        /// </summary>
        public /*static*/ double y
        {
            get {
                return OpenDSS.Lib.Bus_Get_y();
            }
            set {
                OpenDSS.Lib.Bus_Set_y(value);
            }
        }
        public dss_sharp.Bus this[int key]
        {
            get {
                OpenDSS.Lib.Circuit_SetActiveBusi(key);
                return this;
            }
        }

        public dss_sharp.Bus this[string key]
        {
            get
            {
                OpenDSS.Lib.Circuit_SetActiveBus(key);
                return this;
            }
        }
    } // Bus 


    public class Capacitors
    {

        public /*static*/ bool AddStep()
        {
            return OpenDSS.Lib.Capacitors_AddStep() != 0;
        }

        public /*static*/ void Close()
        {
            OpenDSS.Lib.Capacitors_Close();
        }

        public /*static*/ void Open()
        {
            OpenDSS.Lib.Capacitors_Open();
        }

        public /*static*/ bool SubtractStep()
        {
            return OpenDSS.Lib.Capacitors_SubtractStep() != 0;
        }

        /// <summary>
        /// (read-only) Array of strings with all Capacitor names in the circuit.
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Capacitors_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of Steps available in cap bank to be switched ON.
        /// </summary>
        public /*static*/ int AvailableSteps
        {
            get {
                return OpenDSS.Lib.Capacitors_Get_AvailableSteps();
            }
        }

        /// <summary>
        /// (read-only) Number of Capacitor objects in active circuit.
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Capacitors_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Sets the first Capacitor active. Returns 0 if no more.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Capacitors_Get_First();
            }
        }

        /// <summary>
        /// Delta connection or wye?
        /// </summary>
        public /*static*/ bool IsDelta
        {
            get {
                return OpenDSS.Lib.Capacitors_Get_IsDelta() != 0;
            }
            set {
                OpenDSS.Lib.Capacitors_Set_IsDelta(value);
            }
        }

        /// <summary>
        /// Sets the active Capacitor by Name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Capacitors_Get_Name();
            }
            set {
                OpenDSS.Lib.Capacitors_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the next Capacitor active. Returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Capacitors_Get_Next();
            }
        }

        /// <summary>
        /// Number of steps (default 1) for distributing and switching the total bank kVAR.
        /// </summary>
        public /*static*/ int NumSteps
        {
            get {
                return OpenDSS.Lib.Capacitors_Get_NumSteps();
            }
            set {
                OpenDSS.Lib.Capacitors_Set_NumSteps(value);
            }
        }

        /// <summary>
        /// (read) A array of  integer [0..numsteps-1] indicating state of each step. If value is -1 an error has occurred.
        /// (write) Array of integer [0 ..numSteps-1] indicating the state of each step
        /// </summary>
        public /*static*/ int[] States
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Capacitors_Get_States(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_int32_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Capacitors_Set_States(value, value.Length);
            }
        }

        /// <summary>
        /// Bank kV rating. Use LL for 2 or 3 phases, or actual can rating for 1 phase.
        /// </summary>
        public /*static*/ double kV
        {
            get {
                return OpenDSS.Lib.Capacitors_Get_kV();
            }
            set {
                OpenDSS.Lib.Capacitors_Set_kV(value);
            }
        }

        /// <summary>
        /// Total bank KVAR, distributed equally among phases and steps.
        /// </summary>
        public /*static*/ double kvar
        {
            get {
                return OpenDSS.Lib.Capacitors_Get_kvar();
            }
            set {
                OpenDSS.Lib.Capacitors_Set_kvar(value);
            }
        }
    } // Capacitors 


    public class CapControls
    {

        public /*static*/ void Reset()
        {
            OpenDSS.Lib.CapControls_Reset();
        }

        /// <summary>
        /// (read-only) Array of strings with all CapControl names.
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CapControls_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// Transducer ratio from pirmary current to control current.
        /// </summary>
        public /*static*/ double CTratio
        {
            get {
                return OpenDSS.Lib.CapControls_Get_CTratio();
            }
            set {
                OpenDSS.Lib.CapControls_Set_CTratio(value);
            }
        }

        /// <summary>
        /// Name of the Capacitor that is controlled.
        /// </summary>
        public /*static*/ string Capacitor
        {
            get {
                return OpenDSS.Lib.CapControls_Get_Capacitor();
            }
            set {
                OpenDSS.Lib.CapControls_Set_Capacitor(value);
            }
        }

        /// <summary>
        /// (read-only) Number of CapControls in Active Circuit
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.CapControls_Get_Count();
            }
        }

        public /*static*/ double DeadTime
        {
            get {
                return OpenDSS.Lib.CapControls_Get_DeadTime();
            }
            set {
                OpenDSS.Lib.CapControls_Set_DeadTime(value);
            }
        }

        /// <summary>
        /// Time delay [s] to switch on after arming.  Control may reset before actually switching.
        /// </summary>
        public /*static*/ double Delay
        {
            get {
                return OpenDSS.Lib.CapControls_Get_Delay();
            }
            set {
                OpenDSS.Lib.CapControls_Set_Delay(value);
            }
        }

        /// <summary>
        /// Time delay [s] before swithcing off a step. Control may reset before actually switching.
        /// </summary>
        public /*static*/ double DelayOff
        {
            get {
                return OpenDSS.Lib.CapControls_Get_DelayOff();
            }
            set {
                OpenDSS.Lib.CapControls_Set_DelayOff(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the first CapControl as active. Return 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.CapControls_Get_First();
            }
        }

        /// <summary>
        /// Type of automatic controller.
        /// </summary>
        public /*static*/ int Mode
        {
            get {
                return OpenDSS.Lib.CapControls_Get_Mode();
            }
            set {
                OpenDSS.Lib.CapControls_Set_Mode(value);
            }
        }

        /// <summary>
        /// Full name of the element that PT and CT are connected to.
        /// </summary>
        public /*static*/ string MonitoredObj
        {
            get {
                return OpenDSS.Lib.CapControls_Get_MonitoredObj();
            }
            set {
                OpenDSS.Lib.CapControls_Set_MonitoredObj(value);
            }
        }

        /// <summary>
        /// Terminal number on the element that PT and CT are connected to.
        /// </summary>
        public /*static*/ int MonitoredTerm
        {
            get {
                return OpenDSS.Lib.CapControls_Get_MonitoredTerm();
            }
            set {
                OpenDSS.Lib.CapControls_Set_MonitoredTerm(value);
            }
        }

        /// <summary>
        /// Sets a CapControl active by name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.CapControls_Get_Name();
            }
            set {
                OpenDSS.Lib.CapControls_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Gets the next CapControl in the circut. Returns 0 if none.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.CapControls_Get_Next();
            }
        }

        /// <summary>
        /// Threshold to switch off a step. See Mode for units.
        /// </summary>
        public /*static*/ double OFFSetting
        {
            get {
                return OpenDSS.Lib.CapControls_Get_OFFSetting();
            }
            set {
                OpenDSS.Lib.CapControls_Set_OFFSetting(value);
            }
        }

        /// <summary>
        /// Threshold to arm or switch on a step.  See Mode for units.
        /// </summary>
        public /*static*/ double ONSetting
        {
            get {
                return OpenDSS.Lib.CapControls_Get_ONSetting();
            }
            set {
                OpenDSS.Lib.CapControls_Set_ONSetting(value);
            }
        }

        /// <summary>
        /// Transducer ratio from primary feeder to control voltage.
        /// </summary>
        public /*static*/ double PTratio
        {
            get {
                return OpenDSS.Lib.CapControls_Get_PTratio();
            }
            set {
                OpenDSS.Lib.CapControls_Set_PTratio(value);
            }
        }

        /// <summary>
        /// Enables Vmin and Vmax to override the control Mode
        /// </summary>
        public /*static*/ bool UseVoltOverride
        {
            get {
                return OpenDSS.Lib.CapControls_Get_UseVoltOverride() != 0;
            }
            set {
                OpenDSS.Lib.CapControls_Set_UseVoltOverride(value);
            }
        }

        /// <summary>
        /// With VoltOverride, swtich off whenever PT voltage exceeds this level.
        /// </summary>
        public /*static*/ double Vmax
        {
            get {
                return OpenDSS.Lib.CapControls_Get_Vmax();
            }
            set {
                OpenDSS.Lib.CapControls_Set_Vmax(value);
            }
        }

        /// <summary>
        /// With VoltOverride, switch ON whenever PT voltage drops below this level.
        /// </summary>
        public /*static*/ double Vmin
        {
            get {
                return OpenDSS.Lib.CapControls_Get_Vmin();
            }
            set {
                OpenDSS.Lib.CapControls_Set_Vmin(value);
            }
        }
    } // CapControls 


    public class CmathLib
    {

        /// <summary>
        /// (read-only) Return abs value of complex number given in real and imag doubles
        /// </summary>
        public /*static*/ double cabs(double realpart, double imagpart)
        {
            return OpenDSS.Lib.CmathLib_Get_cabs(realpart, imagpart);
        }

        /// <summary>
        /// (read-only) Returns the angle, in degrees, of a complex number specified as two doubles: Realpart and imagpart.
        /// </summary>
        public /*static*/ double cdang(double RealPart, double ImagPart)
        {
            return OpenDSS.Lib.CmathLib_Get_cdang(RealPart, ImagPart);
        }

        /// <summary>
        /// (read-only) Divide two complex number: (a1, b1)/(a2, b2). Returns array of two doubles representing complex result.
        /// </summary>
        public /*static*/ double[] cdiv(double a1, double b1, double a2, double b2)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.CmathLib_Get_cdiv(ref resultPtr, ref resultCount, a1, b1, a2, b2);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        /// <summary>
        /// (read-only) Convert real and imaginary doubles to Array of doubles
        /// </summary>
        public /*static*/ double[] cmplx(double RealPart, double ImagPart)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.CmathLib_Get_cmplx(ref resultPtr, ref resultCount, RealPart, ImagPart);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        /// <summary>
        /// (read-only) Multiply two complex numbers: (a1, b1) * (a2, b2). Returns result as a array of two doubles.
        /// </summary>
        public /*static*/ double[] cmul(double a1, double b1, double a2, double b2)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.CmathLib_Get_cmul(ref resultPtr, ref resultCount, a1, b1, a2, b2);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        /// <summary>
        /// (read-only) Convert complex number to magnitude and angle, degrees. Returns array of two doubles.
        /// </summary>
        public /*static*/ double[] ctopolardeg(double RealPart, double ImagPart)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.CmathLib_Get_ctopolardeg(ref resultPtr, ref resultCount, RealPart, ImagPart);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        /// <summary>
        /// (read-only) Convert magnitude, angle in degrees to a complex number. Returns Array of two doubles.
        /// </summary>
        public /*static*/ double[] pdegtocomplex(double magnitude, double angle)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.CmathLib_Get_pdegtocomplex(ref resultPtr, ref resultCount, magnitude, angle);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }
    } // CmathLib 


    public class CtrlQueue
    {

        public /*static*/ void ClearActions()
        {
            OpenDSS.Lib.CtrlQueue_ClearActions();
        }

        public /*static*/ void ClearQueue()
        {
            OpenDSS.Lib.CtrlQueue_ClearQueue();
        }

        public /*static*/ void Delete(int ActionHandle)
        {
            OpenDSS.Lib.CtrlQueue_Delete(ActionHandle);
        }

        public /*static*/ void DoAllQueue()
        {
            OpenDSS.Lib.CtrlQueue_DoAllQueue();
        }

        public /*static*/ void Show()
        {
            OpenDSS.Lib.CtrlQueue_Show();
        }

        /// <summary>
        /// (read-only) Code for the active action. Long integer code to tell the control device what to do
        /// </summary>
        public /*static*/ int ActionCode
        {
            get {
                return OpenDSS.Lib.CtrlQueue_Get_ActionCode();
            }
        }

        /// <summary>
        /// (read-only) Handle (User defined) to device that must act on the pending action.
        /// </summary>
        public /*static*/ int DeviceHandle
        {
            get {
                return OpenDSS.Lib.CtrlQueue_Get_DeviceHandle();
            }
        }

        /// <summary>
        /// (read-only) Number of Actions on the current actionlist (that have been popped off the control queue by CheckControlActions)
        /// </summary>
        public /*static*/ int NumActions
        {
            get {
                return OpenDSS.Lib.CtrlQueue_Get_NumActions();
            }
        }

        /// <summary>
        /// (read-only) Pops next action off the action list and makes it the active action. Returns zero if none.
        /// </summary>
        public /*static*/ int PopAction
        {
            get {
                return OpenDSS.Lib.CtrlQueue_Get_PopAction();
            }
        }

        /// <summary>
        /// (read-only) Array of strings containing the entire queue in CSV format
        /// </summary>
        public /*static*/ string[] Queue
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CtrlQueue_Get_Queue(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of items on the OpenDSS control Queue
        /// </summary>
        public /*static*/ int QueueSize
        {
            get {
                return OpenDSS.Lib.CtrlQueue_Get_QueueSize();
            }
        }
        /// <summary>
        /// (write-only) Set the active action by index
        /// </summary>

        public /*static*/ int Action
        {
            set {
                OpenDSS.Lib.CtrlQueue_Set_Action(value);

            }
        }
    } // CtrlQueue 


    public class DSSimComs
    {

        public /*static*/ double[] BusVoltage(ulong Index)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.DSSimComs_BusVoltage(ref resultPtr, ref resultCount, Index);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        public /*static*/ double[] BusVoltagepu(ulong Index)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.DSSimComs_BusVoltagepu(ref resultPtr, ref resultCount, Index);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }
    } // DSSimComs 


    public class DSSProgress
    {

        public /*static*/ void Close()
        {
            OpenDSS.Lib.DSSProgress_Close();
        }

        public /*static*/ void Show()
        {
            OpenDSS.Lib.DSSProgress_Show();
        }
        /// <summary>
        /// (write-only) Caption to appear on the bottom of the DSS Progress form.
        /// </summary>

        public /*static*/ string Caption
        {
            set {
                OpenDSS.Lib.DSSProgress_Set_Caption(value);

            }
        }
        /// <summary>
        /// (write-only) Percent progress to indicate [0..100]
        /// </summary>

        public /*static*/ int PctProgress
        {
            set {
                OpenDSS.Lib.DSSProgress_Set_PctProgress(value);

            }
        }
    } // DSSProgress 


    public class DSSProperty
    {

        /// <summary>
        /// (read-only) Description of the property.
        /// </summary>
        public /*static*/ string Description
        {
            get {
                return OpenDSS.Lib.DSSProperty_Get_Description();
            }
        }

        /// <summary>
        /// (read-only) Name of Property
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.DSSProperty_Get_Name();
            }
        }

        public /*static*/ string Val
        {
            get {
                return OpenDSS.Lib.DSSProperty_Get_Val();
            }
            set {
                OpenDSS.Lib.DSSProperty_Set_Val(value);
            }
        }

        public dss_sharp.DSSProperty this[int key]
        {
            get {
                OpenDSS.Lib.DSSProperty_Set_Index(key);
                return this;
            }
        }
        
        public dss_sharp.DSSProperty this[string key]
        {
            get {
                OpenDSS.Lib.DSSProperty_Set_Name(key);
                return this;
            }
        }

    } // DSSProperty 


    public class DSS_Executive
    {

        /// <summary>
        /// (read-only) Get i-th command
        /// </summary>
        public /*static*/ string Command(int i)
        {
            return OpenDSS.Lib.DSS_Executive_Get_Command(i);
        }

        /// <summary>
        /// (read-only) Get help string for i-th command
        /// </summary>
        public /*static*/ string CommandHelp(int i)
        {
            return OpenDSS.Lib.DSS_Executive_Get_CommandHelp(i);
        }

        /// <summary>
        /// (read-only) Get i-th option
        /// </summary>
        public /*static*/ string Option(int i)
        {
            return OpenDSS.Lib.DSS_Executive_Get_Option(i);
        }

        /// <summary>
        /// (read-only) Get help string for i-th option
        /// </summary>
        public /*static*/ string OptionHelp(int i)
        {
            return OpenDSS.Lib.DSS_Executive_Get_OptionHelp(i);
        }

        /// <summary>
        /// (read-only) Get present value of i-th option
        /// </summary>
        public /*static*/ string OptionValue(int i)
        {
            return OpenDSS.Lib.DSS_Executive_Get_OptionValue(i);
        }

        /// <summary>
        /// (read-only) Number of DSS Executive Commands
        /// </summary>
        public /*static*/ int NumCommands
        {
            get {
                return OpenDSS.Lib.DSS_Executive_Get_NumCommands();
            }
        }

        /// <summary>
        /// (read-only) Number of DSS Executive Options
        /// </summary>
        public /*static*/ int NumOptions
        {
            get {
                return OpenDSS.Lib.DSS_Executive_Get_NumOptions();
            }
        }
    } // DSS_Executive 


    public class Error
    {

        /// <summary>
        /// (read-only) Description of error for last operation
        /// </summary>
        public /*static*/ string Description
        {
            get {
                return OpenDSS.Lib.Error_Get_Description();
            }
        }

        /// <summary>
        /// (read-only) Error Number
        /// </summary>
        public /*static*/ int Number
        {
            get {
                return OpenDSS.Lib.Error_Get_Number();
            }
        }
    } // Error 


    public class Fuses
    {

        public /*static*/ void Close()
        {
            OpenDSS.Lib.Fuses_Close();
        }

        public /*static*/ void Open()
        {
            OpenDSS.Lib.Fuses_Open();
        }

        /// <summary>
        /// (read-only) Array of strings containing names of all Fuses in the circuit
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Fuses_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of Fuse elements in the circuit
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Fuses_Get_Count();
            }
        }

        /// <summary>
        /// (read) A fixed delay time in seconds added to the fuse blowing time determined by the TCC curve. Default is 0.
        /// (write) Fixed delay time in seconds added to the fuse blowing time to represent fuse clear or other delay.
        /// </summary>
        public /*static*/ double Delay
        {
            get {
                return OpenDSS.Lib.Fuses_Get_Delay();
            }
            set {
                OpenDSS.Lib.Fuses_Set_Delay(value);
            }
        }

        /// <summary>
        /// (read-only) Set the first Fuse to be the active fuse. Returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Fuses_Get_First();
            }
        }

        /// <summary>
        /// Full name of the circuit element to which the fuse is connected.
        /// </summary>
        public /*static*/ string MonitoredObj
        {
            get {
                return OpenDSS.Lib.Fuses_Get_MonitoredObj();
            }
            set {
                OpenDSS.Lib.Fuses_Set_MonitoredObj(value);
            }
        }

        /// <summary>
        /// (read) Terminal number to which the fuse is connected.
        /// (write) Number of the terminal to which the fuse is connected
        /// </summary>
        public /*static*/ int MonitoredTerm
        {
            get {
                return OpenDSS.Lib.Fuses_Get_MonitoredTerm();
            }
            set {
                OpenDSS.Lib.Fuses_Set_MonitoredTerm(value);
            }
        }

        /// <summary>
        /// (read) Get the name of the active Fuse element
        /// (write) Set the active Fuse element by name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Fuses_Get_Name();
            }
            set {
                OpenDSS.Lib.Fuses_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Advance the active Fuse element pointer to the next fuse. Returns 0 if no more fuses.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Fuses_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Number of phases, this fuse. 
        /// </summary>
        public /*static*/ int NumPhases
        {
            get {
                return OpenDSS.Lib.Fuses_Get_NumPhases();
            }
        }

        /// <summary>
        /// (read) Multiplier or actual amps for the TCCcurve object. Defaults to 1.0.  Multipliy current values of TCC curve by this to get actual amps.
        /// (write) Multiplier or actual fuse amps for the TCC curve. Defaults to 1.0. Has to correspond to the Current axis of TCCcurve object.
        /// </summary>
        public /*static*/ double RatedCurrent
        {
            get {
                return OpenDSS.Lib.Fuses_Get_RatedCurrent();
            }
            set {
                OpenDSS.Lib.Fuses_Set_RatedCurrent(value);
            }
        }

        /// <summary>
        /// (read) Full name of the circuit element switch that the fuse controls. Defaults to the MonitoredObj.
        /// (write) Full name of the circuit element switch that the fuse controls. Defaults to MonitoredObj.
        /// </summary>
        public /*static*/ string SwitchedObj
        {
            get {
                return OpenDSS.Lib.Fuses_Get_SwitchedObj();
            }
            set {
                OpenDSS.Lib.Fuses_Set_SwitchedObj(value);
            }
        }

        /// <summary>
        /// (read) Number of the terminal containing the switch controlled by the fuse.
        /// (write) Number of the terminal of the controlled element containing the switch controlled by the fuse.
        /// </summary>
        public /*static*/ int SwitchedTerm
        {
            get {
                return OpenDSS.Lib.Fuses_Get_SwitchedTerm();
            }
            set {
                OpenDSS.Lib.Fuses_Set_SwitchedTerm(value);
            }
        }

        /// <summary>
        /// Name of the TCCcurve object that determines fuse blowing.
        /// </summary>
        public /*static*/ string TCCcurve
        {
            get {
                return OpenDSS.Lib.Fuses_Get_TCCcurve();
            }
            set {
                OpenDSS.Lib.Fuses_Set_TCCcurve(value);
            }
        }

        /// <summary>
        /// (read) Get/set active fuse by index into the list of fuses. 1 based: 1..count
        /// (write) Set Fuse active by index into the list of fuses. 1..count
        /// </summary>
        public /*static*/ int idx
        {
            get {
                return OpenDSS.Lib.Fuses_Get_idx();
            }
            set {
                OpenDSS.Lib.Fuses_Set_idx(value);
            }
        }
    } // Fuses 


    public class Generators
    {

        /// <summary>
        /// (read-only) Array of names of all Generator objects.
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Generators_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of Generator Objects in Active Circuit
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Generators_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Sets first Generator to be active.  Returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Generators_Get_First();
            }
        }

        /// <summary>
        /// Indicates whether the generator is forced ON regardles of other dispatch criteria.
        /// </summary>
        public /*static*/ bool ForcedON
        {
            get {
                return OpenDSS.Lib.Generators_Get_ForcedON() != 0;
            }
            set {
                OpenDSS.Lib.Generators_Set_ForcedON(value);
            }
        }

        /// <summary>
        /// Generator Model
        /// </summary>
        public /*static*/ int Model
        {
            get {
                return OpenDSS.Lib.Generators_Get_Model();
            }
            set {
                OpenDSS.Lib.Generators_Set_Model(value);
            }
        }

        /// <summary>
        /// Sets a generator active by name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Generators_Get_Name();
            }
            set {
                OpenDSS.Lib.Generators_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets next Generator to be active.  Returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Generators_Get_Next();
            }
        }

        /// <summary>
        /// Power factor (pos. = producing vars). Updates kvar based on present kW value.
        /// </summary>
        public /*static*/ double PF
        {
            get {
                return OpenDSS.Lib.Generators_Get_PF();
            }
            set {
                OpenDSS.Lib.Generators_Set_PF(value);
            }
        }

        /// <summary>
        /// Number of phases
        /// </summary>
        public /*static*/ int Phases
        {
            get {
                return OpenDSS.Lib.Generators_Get_Phases();
            }
            set {
                OpenDSS.Lib.Generators_Set_Phases(value);
            }
        }

        /// <summary>
        /// (read-only) Array of Names of all generator energy meter registers
        /// </summary>
        public /*static*/ string[] RegisterNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Generators_Get_RegisterNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of valus in generator energy meter registers.
        /// </summary>
        public /*static*/ double[] RegisterValues
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Generators_Get_RegisterValues(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read) vmaxpu for Generator model
        /// (write) Vmaxpu for generator model
        /// </summary>
        public /*static*/ double Vmaxpu
        {
            get {
                return OpenDSS.Lib.Generators_Get_Vmaxpu();
            }
            set {
                OpenDSS.Lib.Generators_Set_Vmaxpu(value);
            }
        }

        /// <summary>
        /// Vminpu for Generator model
        /// </summary>
        public /*static*/ double Vminpu
        {
            get {
                return OpenDSS.Lib.Generators_Get_Vminpu();
            }
            set {
                OpenDSS.Lib.Generators_Set_Vminpu(value);
            }
        }

        /// <summary>
        /// (read) Get/Set active Generator by index into generators list.  1..Count
        /// (write) Get/Set active Generator by index into generators list. 1..Count
        /// </summary>
        public /*static*/ int idx
        {
            get {
                return OpenDSS.Lib.Generators_Get_idx();
            }
            set {
                OpenDSS.Lib.Generators_Set_idx(value);
            }
        }

        /// <summary>
        /// Voltage base for the active generator, kV
        /// </summary>
        public /*static*/ double kV
        {
            get {
                return OpenDSS.Lib.Generators_Get_kV();
            }
            set {
                OpenDSS.Lib.Generators_Set_kV(value);
            }
        }

        /// <summary>
        /// (read) kVA rating of the generator
        /// (write) KVA Rating of the generator
        /// </summary>
        public /*static*/ double kVArated
        {
            get {
                return OpenDSS.Lib.Generators_Get_kVArated();
            }
            set {
                OpenDSS.Lib.Generators_Set_kVArated(value);
            }
        }

        /// <summary>
        /// (read) kW output for the active generator. kvar is updated for current power factor.
        /// (write) kW output for the active generator. kvar is updated for current power factor
        /// </summary>
        public /*static*/ double kW
        {
            get {
                return OpenDSS.Lib.Generators_Get_kW();
            }
            set {
                OpenDSS.Lib.Generators_Set_kW(value);
            }
        }

        /// <summary>
        /// (read) kvar output for the active generator. Updates power factor based on present kW value.
        /// (write) kvar output for the active generator. Updates power factor based on present kW.
        /// </summary>
        public /*static*/ double kvar
        {
            get {
                return OpenDSS.Lib.Generators_Get_kvar();
            }
            set {
                OpenDSS.Lib.Generators_Set_kvar(value);
            }
        }
    } // Generators 


    public class ISources
    {

        /// <summary>
        /// (read-only) Array of strings containing names of all ISOURCE elements.
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.ISources_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read) Get the magnitude of the ISOURCE in amps
        /// (write) Set the magnitude of the ISOURCE, amps
        /// </summary>
        public /*static*/ double Amps
        {
            get {
                return OpenDSS.Lib.ISources_Get_Amps();
            }
            set {
                OpenDSS.Lib.ISources_Set_Amps(value);
            }
        }

        /// <summary>
        /// Phase angle for ISOURCE, degrees
        /// </summary>
        public /*static*/ double AngleDeg
        {
            get {
                return OpenDSS.Lib.ISources_Get_AngleDeg();
            }
            set {
                OpenDSS.Lib.ISources_Set_AngleDeg(value);
            }
        }

        /// <summary>
        /// (read-only) Count: Number of ISOURCE elements.
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.ISources_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Set the First ISOURCE to be active; returns Zero if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.ISources_Get_First();
            }
        }

        /// <summary>
        /// (read) The present frequency of the ISOURCE, Hz
        /// (write) Set the present frequency for the ISOURCE
        /// </summary>
        public /*static*/ double Frequency
        {
            get {
                return OpenDSS.Lib.ISources_Get_Frequency();
            }
            set {
                OpenDSS.Lib.ISources_Set_Frequency(value);
            }
        }

        /// <summary>
        /// (read) Get name of active ISOURCE
        /// (write) Set Active ISOURCE by name
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.ISources_Get_Name();
            }
            set {
                OpenDSS.Lib.ISources_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the next ISOURCE element to be the active one. Returns Zero if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.ISources_Get_Next();
            }
        }
    } // ISources 


    public class LineCodes
    {

        /// <summary>
        /// (read-only) Array of strings with names of all devices
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.LineCodes_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Zero-sequence capacitance, nF per unit length
        /// </summary>
        public /*static*/ double C0
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_C0();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_C0(value);
            }
        }

        /// <summary>
        /// (read-only) Positive-sequence capacitance, nF per unit length
        /// </summary>
        public /*static*/ double C1
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_C1();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_C1(value);
            }
        }

        /// <summary>
        /// (read-only) Capacitance matrix, nF per unit length
        /// </summary>
        public /*static*/ double[] Cmatrix
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.LineCodes_Get_Cmatrix(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.LineCodes_Set_Cmatrix(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Number of LineCodes
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Emergency ampere rating
        /// </summary>
        public /*static*/ double EmergAmps
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_EmergAmps();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_EmergAmps(value);
            }
        }

        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_First();
            }
        }

        /// <summary>
        /// (read-only) Flag denoting whether impedance data were entered in symmetrical components
        /// </summary>
        public /*static*/ bool IsZ1Z0
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_IsZ1Z0() != 0;
            }
        }

        /// <summary>
        /// (read-only) Name of active LineCode
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_Name();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_Name(value);
            }
        }

        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Normal Ampere rating
        /// </summary>
        public /*static*/ double NormAmps
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_NormAmps();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_NormAmps(value);
            }
        }

        /// <summary>
        /// Number of Phases
        /// </summary>
        public /*static*/ int Phases
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_Phases();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_Phases(value);
            }
        }

        /// <summary>
        /// (read-only) Zero-Sequence Resistance, ohms per unit length
        /// </summary>
        public /*static*/ double R0
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_R0();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_R0(value);
            }
        }

        /// <summary>
        /// (read-only) Positive-sequence resistance ohms per unit length
        /// </summary>
        public /*static*/ double R1
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_R1();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_R1(value);
            }
        }

        /// <summary>
        /// (read-only) Resistance matrix, ohms per unit length
        /// </summary>
        public /*static*/ double[] Rmatrix
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.LineCodes_Get_Rmatrix(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.LineCodes_Set_Rmatrix(value, value.Length);
            }
        }

        public /*static*/ int Units
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_Units();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_Units(value);
            }
        }

        /// <summary>
        /// (read-only) Zero Sequence Reactance, Ohms per unit length
        /// </summary>
        public /*static*/ double X0
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_X0();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_X0(value);
            }
        }

        /// <summary>
        /// (read-only) Posiive-sequence reactance, ohms per unit length
        /// </summary>
        public /*static*/ double X1
        {
            get {
                return OpenDSS.Lib.LineCodes_Get_X1();
            }
            set {
                OpenDSS.Lib.LineCodes_Set_X1(value);
            }
        }

        /// <summary>
        /// (read-only) Reactance matrix, ohms per unit length
        /// </summary>
        public /*static*/ double[] Xmatrix
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.LineCodes_Get_Xmatrix(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.LineCodes_Set_Xmatrix(value, value.Length);
            }
        }
    } // LineCodes 


    public class Lines
    {

        public /*static*/ int New(string Name)
        {
            return OpenDSS.Lib.Lines_New(Name);
        }

        /// <summary>
        /// (read-only) Names of all Line Objects
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Lines_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// Name of bus for terminal 1.
        /// </summary>
        public /*static*/ string Bus1
        {
            get {
                return OpenDSS.Lib.Lines_Get_Bus1();
            }
            set {
                OpenDSS.Lib.Lines_Set_Bus1(value);
            }
        }

        /// <summary>
        /// Name of bus for terminal 2.
        /// </summary>
        public /*static*/ string Bus2
        {
            get {
                return OpenDSS.Lib.Lines_Get_Bus2();
            }
            set {
                OpenDSS.Lib.Lines_Set_Bus2(value);
            }
        }

        /// <summary>
        /// Zero Sequence capacitance, nanofarads per unit length.
        /// </summary>
        public /*static*/ double C0
        {
            get {
                return OpenDSS.Lib.Lines_Get_C0();
            }
            set {
                OpenDSS.Lib.Lines_Set_C0(value);
            }
        }

        /// <summary>
        /// Positive Sequence capacitance, nanofarads per unit length.
        /// </summary>
        public /*static*/ double C1
        {
            get {
                return OpenDSS.Lib.Lines_Get_C1();
            }
            set {
                OpenDSS.Lib.Lines_Set_C1(value);
            }
        }

        public /*static*/ double[] Cmatrix
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Lines_Get_Cmatrix(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Lines_Set_Cmatrix(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Number of Line objects in Active Circuit.
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Lines_Get_Count();
            }
        }

        /// <summary>
        /// Emergency (maximum) ampere rating of Line.
        /// </summary>
        public /*static*/ double EmergAmps
        {
            get {
                return OpenDSS.Lib.Lines_Get_EmergAmps();
            }
            set {
                OpenDSS.Lib.Lines_Set_EmergAmps(value);
            }
        }

        /// <summary>
        /// (read-only) Invoking this property sets the first element active.  Returns 0 if no lines.  Otherwise, index of the line element.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Lines_Get_First();
            }
        }

        /// <summary>
        /// Line geometry code
        /// </summary>
        public /*static*/ string Geometry
        {
            get {
                return OpenDSS.Lib.Lines_Get_Geometry();
            }
            set {
                OpenDSS.Lib.Lines_Set_Geometry(value);
            }
        }

        /// <summary>
        /// Length of line section in units compatible with the LineCode definition.
        /// </summary>
        public /*static*/ double Length
        {
            get {
                return OpenDSS.Lib.Lines_Get_Length();
            }
            set {
                OpenDSS.Lib.Lines_Set_Length(value);
            }
        }

        /// <summary>
        /// Name of LineCode object that defines the impedances.
        /// </summary>
        public /*static*/ string LineCode
        {
            get {
                return OpenDSS.Lib.Lines_Get_LineCode();
            }
            set {
                OpenDSS.Lib.Lines_Set_LineCode(value);
            }
        }

        /// <summary>
        /// Specify the name of the Line element to set it active.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Lines_Get_Name();
            }
            set {
                OpenDSS.Lib.Lines_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Invoking this property advances to the next Line element active.  Returns 0 if no more lines.  Otherwise, index of the line element.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Lines_Get_Next();
            }
        }

        /// <summary>
        /// Normal ampere rating of Line.
        /// </summary>
        public /*static*/ double NormAmps
        {
            get {
                return OpenDSS.Lib.Lines_Get_NormAmps();
            }
            set {
                OpenDSS.Lib.Lines_Set_NormAmps(value);
            }
        }

        /// <summary>
        /// (read-only) Number of customers on this line section.
        /// </summary>
        public /*static*/ int NumCust
        {
            get {
                return OpenDSS.Lib.Lines_Get_NumCust();
            }
        }

        /// <summary>
        /// (read-only) Sets Parent of the active Line to be the active line. Returns 0 if no parent or action fails.
        /// </summary>
        public /*static*/ int Parent
        {
            get {
                return OpenDSS.Lib.Lines_Get_Parent();
            }
        }

        /// <summary>
        /// Number of Phases, this Line element.
        /// </summary>
        public /*static*/ int Phases
        {
            get {
                return OpenDSS.Lib.Lines_Get_Phases();
            }
            set {
                OpenDSS.Lib.Lines_Set_Phases(value);
            }
        }

        /// <summary>
        /// Zero Sequence resistance, ohms per unit length.
        /// </summary>
        public /*static*/ double R0
        {
            get {
                return OpenDSS.Lib.Lines_Get_R0();
            }
            set {
                OpenDSS.Lib.Lines_Set_R0(value);
            }
        }

        /// <summary>
        /// Positive Sequence resistance, ohms per unit length.
        /// </summary>
        public /*static*/ double R1
        {
            get {
                return OpenDSS.Lib.Lines_Get_R1();
            }
            set {
                OpenDSS.Lib.Lines_Set_R1(value);
            }
        }

        /// <summary>
        /// Earth return resistance value used to compute line impedances at power frequency
        /// </summary>
        public /*static*/ double Rg
        {
            get {
                return OpenDSS.Lib.Lines_Get_Rg();
            }
            set {
                OpenDSS.Lib.Lines_Set_Rg(value);
            }
        }

        /// <summary>
        /// Earth Resistivity, m-ohms
        /// </summary>
        public /*static*/ double Rho
        {
            get {
                return OpenDSS.Lib.Lines_Get_Rho();
            }
            set {
                OpenDSS.Lib.Lines_Set_Rho(value);
            }
        }

        /// <summary>
        /// Resistance matrix (full), ohms per unit length. Array of doubles.
        /// </summary>
        public /*static*/ double[] Rmatrix
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Lines_Get_Rmatrix(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Lines_Set_Rmatrix(value, value.Length);
            }
        }

        /// <summary>
        /// Line spacing code
        /// </summary>
        public /*static*/ string Spacing
        {
            get {
                return OpenDSS.Lib.Lines_Get_Spacing();
            }
            set {
                OpenDSS.Lib.Lines_Set_Spacing(value);
            }
        }

        /// <summary>
        /// (read-only) Total Number of customers served from this line section.
        /// </summary>
        public /*static*/ int TotalCust
        {
            get {
                return OpenDSS.Lib.Lines_Get_TotalCust();
            }
        }

        public /*static*/ int Units
        {
            get {
                return OpenDSS.Lib.Lines_Get_Units();
            }
            set {
                OpenDSS.Lib.Lines_Set_Units(value);
            }
        }

        /// <summary>
        /// Zero Sequence reactance ohms per unit length.
        /// </summary>
        public /*static*/ double X0
        {
            get {
                return OpenDSS.Lib.Lines_Get_X0();
            }
            set {
                OpenDSS.Lib.Lines_Set_X0(value);
            }
        }

        /// <summary>
        /// Positive Sequence reactance, ohms per unit length.
        /// </summary>
        public /*static*/ double X1
        {
            get {
                return OpenDSS.Lib.Lines_Get_X1();
            }
            set {
                OpenDSS.Lib.Lines_Set_X1(value);
            }
        }

        /// <summary>
        /// Earth return reactance value used to compute line impedances at power frequency
        /// </summary>
        public /*static*/ double Xg
        {
            get {
                return OpenDSS.Lib.Lines_Get_Xg();
            }
            set {
                OpenDSS.Lib.Lines_Set_Xg(value);
            }
        }

        public /*static*/ double[] Xmatrix
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Lines_Get_Xmatrix(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Lines_Set_Xmatrix(value, value.Length);
            }
        }

        /// <summary>
        /// Yprimitive: Does Nothing at present on Put; Dangerous
        /// </summary>
        public /*static*/ double[] Yprim
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Lines_Get_Yprim(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Lines_Set_Yprim(value, value.Length);
            }
        }
    } // Lines 


    public class Loads
    {

        /// <summary>
        /// (read-only) Array of strings containing all Load names
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Loads_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Factor for allocating loads by connected xfkva
        /// </summary>
        public /*static*/ double AllocationFactor
        {
            get {
                return OpenDSS.Lib.Loads_Get_AllocationFactor();
            }
            set {
                OpenDSS.Lib.Loads_Set_AllocationFactor(value);
            }
        }

        /// <summary>
        /// (read-only) Name of a loadshape with both Mult and Qmult, for CVR factors as a function of time.
        /// </summary>
        public /*static*/ string CVRcurve
        {
            get {
                return OpenDSS.Lib.Loads_Get_CVRcurve();
            }
            set {
                OpenDSS.Lib.Loads_Set_CVRcurve(value);
            }
        }

        /// <summary>
        /// (read-only) Percent reduction in Q for percent reduction in V. Must be used with dssLoadModelCVR.
        /// </summary>
        public /*static*/ double CVRvars
        {
            get {
                return OpenDSS.Lib.Loads_Get_CVRvars();
            }
            set {
                OpenDSS.Lib.Loads_Set_CVRvars(value);
            }
        }

        /// <summary>
        /// (read-only) Percent reduction in P for percent reduction in V. Must be used with dssLoadModelCVR.
        /// </summary>
        public /*static*/ double CVRwatts
        {
            get {
                return OpenDSS.Lib.Loads_Get_CVRwatts();
            }
            set {
                OpenDSS.Lib.Loads_Set_CVRwatts(value);
            }
        }

        /// <summary>
        /// (read-only) Factor relates average to peak kw.  Used for allocation with kwh and kwhdays/
        /// </summary>
        public /*static*/ double Cfactor
        {
            get {
                return OpenDSS.Lib.Loads_Get_Cfactor();
            }
            set {
                OpenDSS.Lib.Loads_Set_Cfactor(value);
            }
        }

        public /*static*/ int Class
        {
            get {
                return OpenDSS.Lib.Loads_Get_Class_();
            }
            set {
                OpenDSS.Lib.Loads_Set_Class_(value);
            }
        }

        /// <summary>
        /// (read-only) Number of Load objects in active circuit.
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Loads_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Set first Load element to be active; returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Loads_Get_First();
            }
        }

        /// <summary>
        /// (read-only) Name of the growthshape curve for yearly load growth factors.
        /// </summary>
        public /*static*/ string Growth
        {
            get {
                return OpenDSS.Lib.Loads_Get_Growth();
            }
            set {
                OpenDSS.Lib.Loads_Set_Growth(value);
            }
        }

        /// <summary>
        /// (read-only) Delta loads are connected line-to-line.
        /// </summary>
        public /*static*/ bool IsDelta
        {
            get {
                return OpenDSS.Lib.Loads_Get_IsDelta() != 0;
            }
            set {
                OpenDSS.Lib.Loads_Set_IsDelta(value);
            }
        }

        /// <summary>
        /// (read-only) The Load Model defines variation of P and Q with voltage.
        /// </summary>
        public /*static*/ int Model
        {
            get {
                return OpenDSS.Lib.Loads_Get_Model();
            }
            set {
                OpenDSS.Lib.Loads_Set_Model(value);
            }
        }

        /// <summary>
        /// Set active load by name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Loads_Get_Name();
            }
            set {
                OpenDSS.Lib.Loads_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets next Load element to be active; returns 0 of none else index of active load.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Loads_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Number of customers in this load, defaults to one.
        /// </summary>
        public /*static*/ int NumCust
        {
            get {
                return OpenDSS.Lib.Loads_Get_NumCust();
            }
            set {
                OpenDSS.Lib.Loads_Set_NumCust(value);
            }
        }

        /// <summary>
        /// (read) Set Power Factor for Active Load. Specify leading PF as negative. Updates kvar based on kW value
        /// (write) Set Power Factor for Active Load. Specify leading PF as negative. Updates kvar based on present value of kW.
        /// </summary>
        public /*static*/ double PF
        {
            get {
                return OpenDSS.Lib.Loads_Get_PF();
            }
            set {
                OpenDSS.Lib.Loads_Set_PF(value);
            }
        }

        /// <summary>
        /// (read-only) Average percent of nominal load in Monte Carlo studies; only if no loadshape defined for this load.
        /// </summary>
        public /*static*/ double PctMean
        {
            get {
                return OpenDSS.Lib.Loads_Get_PctMean();
            }
            set {
                OpenDSS.Lib.Loads_Set_PctMean(value);
            }
        }

        /// <summary>
        /// (read-only) Percent standard deviation for Monte Carlo load studies; if there is no loadshape assigned to this load.
        /// </summary>
        public /*static*/ double PctStdDev
        {
            get {
                return OpenDSS.Lib.Loads_Get_PctStdDev();
            }
            set {
                OpenDSS.Lib.Loads_Set_PctStdDev(value);
            }
        }

        /// <summary>
        /// Relative Weighting factor for the active LOAD
        /// </summary>
        public /*static*/ double RelWeight
        {
            get {
                return OpenDSS.Lib.Loads_Get_RelWeight();
            }
        }

        /// <summary>
        /// (read-only) Neutral resistance for wye-connected loads.
        /// </summary>
        public /*static*/ double Rneut
        {
            get {
                return OpenDSS.Lib.Loads_Get_Rneut();
            }
            set {
                OpenDSS.Lib.Loads_Set_Rneut(value);
            }
        }

        /// <summary>
        /// (read-only) Name of harmonic current spectrrum shape.
        /// </summary>
        public /*static*/ string Spectrum
        {
            get {
                return OpenDSS.Lib.Loads_Get_Spectrum();
            }
            set {
                OpenDSS.Lib.Loads_Set_Spectrum(value);
            }
        }

        /// <summary>
        /// (read-only) Response to load multipliers: Fixed (growth only), Exempt (no LD curve), Variable (all).
        /// </summary>
        public /*static*/ int Status
        {
            get {
                return OpenDSS.Lib.Loads_Get_Status();
            }
            set {
                OpenDSS.Lib.Loads_Set_Status(value);
            }
        }

        /// <summary>
        /// (read-only) Maximum per-unit voltage to use the load model. Above this, constant Z applies.
        /// </summary>
        public /*static*/ double Vmaxpu
        {
            get {
                return OpenDSS.Lib.Loads_Get_Vmaxpu();
            }
            set {
                OpenDSS.Lib.Loads_Set_Vmaxpu(value);
            }
        }

        /// <summary>
        /// (read-only) Minimum voltage for unserved energy (UE) evaluation.
        /// </summary>
        public /*static*/ double Vminemerg
        {
            get {
                return OpenDSS.Lib.Loads_Get_Vminemerg();
            }
            set {
                OpenDSS.Lib.Loads_Set_Vminemerg(value);
            }
        }

        /// <summary>
        /// (read-only) Minimum voltage for energy exceeding normal (EEN) evaluations.
        /// </summary>
        public /*static*/ double Vminnorm
        {
            get {
                return OpenDSS.Lib.Loads_Get_Vminnorm();
            }
            set {
                OpenDSS.Lib.Loads_Set_Vminnorm(value);
            }
        }

        /// <summary>
        /// (read-only) Minimum voltage to apply the load model. Below this, constant Z is used.
        /// </summary>
        public /*static*/ double Vminpu
        {
            get {
                return OpenDSS.Lib.Loads_Get_Vminpu();
            }
            set {
                OpenDSS.Lib.Loads_Set_Vminpu(value);
            }
        }

        /// <summary>
        /// (read-only) Neutral reactance for wye-connected loads.
        /// </summary>
        public /*static*/ double Xneut
        {
            get {
                return OpenDSS.Lib.Loads_Get_Xneut();
            }
            set {
                OpenDSS.Lib.Loads_Set_Xneut(value);
            }
        }

        /// <summary>
        /// (read-only) Name of yearly duration loadshape
        /// </summary>
        public /*static*/ string Yearly
        {
            get {
                return OpenDSS.Lib.Loads_Get_Yearly();
            }
            set {
                OpenDSS.Lib.Loads_Set_Yearly(value);
            }
        }

        /// <summary>
        /// (read-only) Array of 7  doubles with values for ZIPV property of the LOAD object
        /// </summary>
        public /*static*/ double[] ZIPV
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Loads_Get_ZIPV(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Loads_Set_ZIPV(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Name of the loadshape for a daily load profile.
        /// </summary>
        public /*static*/ string daily
        {
            get {
                return OpenDSS.Lib.Loads_Get_daily();
            }
            set {
                OpenDSS.Lib.Loads_Set_daily(value);
            }
        }

        /// <summary>
        /// (read-only) Name of the loadshape for a duty cycle simulation.
        /// </summary>
        public /*static*/ string duty
        {
            get {
                return OpenDSS.Lib.Loads_Get_duty();
            }
            set {
                OpenDSS.Lib.Loads_Set_duty(value);
            }
        }

        public /*static*/ int idx
        {
            get {
                return OpenDSS.Lib.Loads_Get_idx();
            }
            set {
                OpenDSS.Lib.Loads_Set_idx(value);
            }
        }

        /// <summary>
        /// Set kV rating for active Load. For 2 or more phases set Line-Line kV. Else actual kV across terminals.
        /// </summary>
        public /*static*/ double kV
        {
            get {
                return OpenDSS.Lib.Loads_Get_kV();
            }
            set {
                OpenDSS.Lib.Loads_Set_kV(value);
            }
        }

        /// <summary>
        /// Set kW for active Load. Updates kvar based on present PF.
        /// </summary>
        public /*static*/ double kW
        {
            get {
                return OpenDSS.Lib.Loads_Get_kW();
            }
            set {
                OpenDSS.Lib.Loads_Set_kW(value);
            }
        }

        /// <summary>
        /// (read-only) Base load kva. Also defined kw and kvar or pf input, or load allocation by kwh or xfkva.
        /// </summary>
        public /*static*/ double kva
        {
            get {
                return OpenDSS.Lib.Loads_Get_kva();
            }
            set {
                OpenDSS.Lib.Loads_Set_kva(value);
            }
        }

        /// <summary>
        /// (read) Set kvar for active Load. Updates PF based in present kW.
        /// (write) Set kvar for active Load. Updates PF based on present kW.
        /// </summary>
        public /*static*/ double kvar
        {
            get {
                return OpenDSS.Lib.Loads_Get_kvar();
            }
            set {
                OpenDSS.Lib.Loads_Set_kvar(value);
            }
        }

        /// <summary>
        /// (read-only) kwh billed for this period. Can be used with Cfactor for load allocation.
        /// </summary>
        public /*static*/ double kwh
        {
            get {
                return OpenDSS.Lib.Loads_Get_kwh();
            }
            set {
                OpenDSS.Lib.Loads_Set_kwh(value);
            }
        }

        /// <summary>
        /// (read-only) Length of kwh billing period for average demand calculation. Default 30.
        /// </summary>
        public /*static*/ double kwhdays
        {
            get {
                return OpenDSS.Lib.Loads_Get_kwhdays();
            }
            set {
                OpenDSS.Lib.Loads_Set_kwhdays(value);
            }
        }

        /// <summary>
        /// (write-only) Percent of Load that is modeled as series R-L for harmonics studies
        /// </summary>
        public /*static*/ double pctSeriesRL
        {
            get {
                return OpenDSS.Lib.Loads_Get_pctSeriesRL();
            }
            set {
                OpenDSS.Lib.Loads_Set_pctSeriesRL(value);
            }
        }

        /// <summary>
        /// (read-only) Rated service transformer kVA for load allocation, using AllocationFactor. Affects kW, kvar, and pf.
        /// </summary>
        public /*static*/ double xfkVA
        {
            get {
                return OpenDSS.Lib.Loads_Get_xfkVA();
            }
            set {
                OpenDSS.Lib.Loads_Set_xfkVA(value);
            }
        }
    } // Loads 


    public class LoadShapes
    {

        public /*static*/ void Normalize()
        {
            OpenDSS.Lib.LoadShapes_Normalize();
        }

        /// <summary>
        /// (read-only) Array of strings containing names of all Loadshape objects currently defined.
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.LoadShapes_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of Loadshape objects currently defined in Loadshape collection
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Set the first loadshape active and return integer index of the loadshape. Returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_First();
            }
        }

        /// <summary>
        /// (read) Fixed interval time value, hours
        /// (write) Fixed interval time value, hours.
        /// </summary>
        public /*static*/ double HrInterval
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_HrInterval();
            }
            set {
                OpenDSS.Lib.LoadShapes_Set_HrInterval(value);
            }
        }

        /// <summary>
        /// Fixed Interval time value, in minutes
        /// </summary>
        public /*static*/ double MinInterval
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_MinInterval();
            }
            set {
                OpenDSS.Lib.LoadShapes_Set_MinInterval(value);
            }
        }

        /// <summary>
        /// (read) Get the Name of the active Loadshape
        /// (write) Set the active Loadshape by name
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_Name();
            }
            set {
                OpenDSS.Lib.LoadShapes_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Advance active Loadshape to the next on in the collection. Returns 0 if no more loadshapes.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_Next();
            }
        }

        /// <summary>
        /// (read) Get Number of points in active Loadshape.
        /// (write) Set number of points to allocate for active Loadshape.
        /// </summary>
        public /*static*/ int Npts
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_Npts();
            }
            set {
                OpenDSS.Lib.LoadShapes_Set_Npts(value);
            }
        }

        public /*static*/ double PBase
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_PBase();
            }
            set {
                OpenDSS.Lib.LoadShapes_Set_PBase(value);
            }
        }

        /// <summary>
        /// (read) Array of Doubles for the P multiplier in the Loadshape.
        /// (write) Array of doubles containing the P array for the Loadshape.
        /// </summary>
        public /*static*/ double[] Pmult
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.LoadShapes_Get_Pmult(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.LoadShapes_Set_Pmult(value, value.Length);
            }
        }

        /// <summary>
        /// Base for normalizing Q curve. If left at zero, the peak value is used.
        /// </summary>
        public /*static*/ double Qbase
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_Qbase();
            }
            set {
                OpenDSS.Lib.LoadShapes_Set_Qbase(value);
            }
        }

        /// <summary>
        /// Array of doubles containing the Q multipliers.
        /// </summary>
        public /*static*/ double[] Qmult
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.LoadShapes_Get_Qmult(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.LoadShapes_Set_Qmult(value, value.Length);
            }
        }

        /// <summary>
        /// Time array in hours correscponding to P and Q multipliers when the Interval=0.
        /// </summary>
        public /*static*/ double[] TimeArray
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.LoadShapes_Get_TimeArray(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.LoadShapes_Set_TimeArray(value, value.Length);
            }
        }

        /// <summary>
        /// T/F flag to let Loads know to use the actual value in the curve rather than use the value as a multiplier.
        /// </summary>
        public /*static*/ bool UseActual
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_UseActual() != 0;
            }
            set {
                OpenDSS.Lib.LoadShapes_Set_UseActual(value);
            }
        }

        public /*static*/ double sInterval
        {
            get {
                return OpenDSS.Lib.LoadShapes_Get_sInterval();
            }
            set {
                OpenDSS.Lib.LoadShapes_Set_Sinterval(value);
            }
        }
    } // LoadShapes 


    public class Meters
    {

        public /*static*/ void CloseAllDIFiles()
        {
            OpenDSS.Lib.Meters_CloseAllDIFiles();
        }

        public /*static*/ void DoReliabilityCalc(bool AssumeRestoration)
        {
            OpenDSS.Lib.Meters_DoReliabilityCalc(AssumeRestoration);
        }

        public /*static*/ void OpenAllDIFiles()
        {
            OpenDSS.Lib.Meters_OpenAllDIFiles();
        }

        public /*static*/ void Reset()
        {
            OpenDSS.Lib.Meters_Reset();
        }

        public /*static*/ void ResetAll()
        {
            OpenDSS.Lib.Meters_ResetAll();
        }

        public /*static*/ void Sample()
        {
            OpenDSS.Lib.Meters_Sample();
        }

        public /*static*/ void SampleAll()
        {
            OpenDSS.Lib.Meters_SampleAll();
        }

        public /*static*/ void Save()
        {
            OpenDSS.Lib.Meters_Save();
        }

        public /*static*/ void SaveAll()
        {
            OpenDSS.Lib.Meters_SaveAll();
        }

        public /*static*/ void SetActiveSection(int SectIdx)
        {
            OpenDSS.Lib.Meters_SetActiveSection(SectIdx);
        }

        /// <summary>
        /// (read-only) Wide string list of all branches in zone of the active energymeter object.
        /// </summary>
        public /*static*/ string[] AllBranchesInZone
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Meters_Get_AllBranchesInZone(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of names of all zone end elements.
        /// </summary>
        public /*static*/ string[] AllEndElements
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Meters_Get_AllEndElements(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of all energy Meter names
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Meters_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// Array of doubles: set the phase allocation factors for the active meter.
        /// </summary>
        public /*static*/ double[] AllocFactors
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Meters_Get_AllocFactors(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Meters_Set_AllocFactors(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Average Repair time in this section of the meter zone
        /// </summary>
        public /*static*/ double AvgRepairTime
        {
            get {
                return OpenDSS.Lib.Meters_Get_AvgRepairTime();
            }
        }

        /// <summary>
        /// Set the magnitude of the real part of the Calculated Current (normally determined by solution) for the Meter to force some behavior on Load Allocation
        /// </summary>
        public /*static*/ double[] CalcCurrent
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Meters_Get_CalcCurrent(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Meters_Set_CalcCurrent(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Number of Energy Meters in the Active Circuit
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Meters_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Number of branches in Active energymeter zone. (Same as sequencelist size)
        /// </summary>
        public /*static*/ int CountBranches
        {
            get {
                return OpenDSS.Lib.Meters_Get_CountBranches();
            }
        }

        /// <summary>
        /// (read-only) Number of zone end elements in the active meter zone.
        /// </summary>
        public /*static*/ int CountEndElements
        {
            get {
                return OpenDSS.Lib.Meters_Get_CountEndElements();
            }
        }

        /// <summary>
        /// (read-only) Total customer interruptions for this Meter zone based on reliability calcs.
        /// </summary>
        public /*static*/ double CustInterrupts
        {
            get {
                return OpenDSS.Lib.Meters_Get_CustInterrupts();
            }
        }

        /// <summary>
        /// (read-only) Global Flag in the DSS to indicate if Demand Interval (DI) files have been properly opened.
        /// </summary>
        public /*static*/ bool DIFilesAreOpen
        {
            get {
                return OpenDSS.Lib.Meters_Get_DIFilesAreOpen() != 0;
            }
        }

        /// <summary>
        /// (read-only) Sum of Fault Rate time Repair Hrs in this section of the meter zone
        /// </summary>
        public /*static*/ double FaultRateXRepairHrs
        {
            get {
                return OpenDSS.Lib.Meters_Get_FaultRateXRepairHrs();
            }
        }

        /// <summary>
        /// (read-only) Set the first energy Meter active. Returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Meters_Get_First();
            }
        }

        /// <summary>
        /// Set Name of metered element
        /// </summary>
        public /*static*/ string MeteredElement
        {
            get {
                return OpenDSS.Lib.Meters_Get_MeteredElement();
            }
            set {
                OpenDSS.Lib.Meters_Set_MeteredElement(value);
            }
        }

        /// <summary>
        /// set Number of Metered Terminal
        /// </summary>
        public /*static*/ int MeteredTerminal
        {
            get {
                return OpenDSS.Lib.Meters_Get_MeteredTerminal();
            }
            set {
                OpenDSS.Lib.Meters_Set_MeteredTerminal(value);
            }
        }

        /// <summary>
        /// (read) Get/Set the active meter  name.
        /// (write) Set a meter to be active by name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Meters_Get_Name();
            }
            set {
                OpenDSS.Lib.Meters_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the next energy Meter active.  Returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Meters_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Number of branches (lines) in this section
        /// </summary>
        public /*static*/ int NumSectionBranches
        {
            get {
                return OpenDSS.Lib.Meters_Get_NumSectionBranches();
            }
        }

        /// <summary>
        /// (read-only) Number of Customers in the active section.
        /// </summary>
        public /*static*/ int NumSectionCustomers
        {
            get {
                return OpenDSS.Lib.Meters_Get_NumSectionCustomers();
            }
        }

        /// <summary>
        /// (read-only) Number of feeder sections in this meter's zone
        /// </summary>
        public /*static*/ int NumSections
        {
            get {
                return OpenDSS.Lib.Meters_Get_NumSections();
            }
        }

        /// <summary>
        /// (read-only) Type of OCP device. 1=Fuse; 2=Recloser; 3=Relay
        /// </summary>
        public /*static*/ int OCPDeviceType
        {
            get {
                return OpenDSS.Lib.Meters_Get_OCPDeviceType();
            }
        }

        /// <summary>
        /// Array of doubles to set values of Peak Current property
        /// </summary>
        public /*static*/ double[] Peakcurrent
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Meters_Get_Peakcurrent(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Meters_Set_Peakcurrent(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Array of strings containing the names of the registers.
        /// </summary>
        public /*static*/ string[] RegisterNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Meters_Get_RegisterNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of all the values contained in the Meter registers for the active Meter.
        /// </summary>
        public /*static*/ double[] RegisterValues
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Meters_Get_RegisterValues(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) SAIDI for this meter's zone. Execute DoReliabilityCalc first.
        /// </summary>
        public /*static*/ double SAIDI
        {
            get {
                return OpenDSS.Lib.Meters_Get_SAIDI();
            }
        }

        /// <summary>
        /// (read-only) Returns SAIFI for this meter's Zone. Execute Reliability Calc method first.
        /// </summary>
        public /*static*/ double SAIFI
        {
            get {
                return OpenDSS.Lib.Meters_Get_SAIFI();
            }
        }

        /// <summary>
        /// (read-only) SAIFI based on kW rather than number of customers. Get after reliability calcs.
        /// </summary>
        public /*static*/ double SAIFIKW
        {
            get {
                return OpenDSS.Lib.Meters_Get_SAIFIKW();
            }
        }

        /// <summary>
        /// (read-only) SequenceIndex of the branch at the head of this section
        /// </summary>
        public /*static*/ int SectSeqIdx
        {
            get {
                return OpenDSS.Lib.Meters_Get_SectSeqIdx();
            }
        }

        /// <summary>
        /// (read-only) Total Customers downline from this section
        /// </summary>
        public /*static*/ int SectTotalCust
        {
            get {
                return OpenDSS.Lib.Meters_Get_SectTotalCust();
            }
        }

        /// <summary>
        /// (read-only) Size of Sequence List
        /// </summary>
        public /*static*/ int SeqListSize
        {
            get {
                return OpenDSS.Lib.Meters_Get_SeqListSize();
            }
        }

        /// <summary>
        /// Get/set Index into Meter's SequenceList that contains branch pointers in lexical order. Earlier index guaranteed to be upline from later index. Sets PDelement active.
        /// </summary>
        public /*static*/ int SequenceIndex
        {
            get {
                return OpenDSS.Lib.Meters_Get_SequenceIndex();
            }
            set {
                OpenDSS.Lib.Meters_Set_SequenceIndex(value);
            }
        }

        /// <summary>
        /// (read-only) Sum of the branch fault rates in this section of the meter's zone
        /// </summary>
        public /*static*/ double SumBranchFltRates
        {
            get {
                return OpenDSS.Lib.Meters_Get_SumBranchFltRates();
            }
        }

        /// <summary>
        /// (read-only) Total Number of customers in this zone (downline from the EnergyMeter)
        /// </summary>
        public /*static*/ int TotalCustomers
        {
            get {
                return OpenDSS.Lib.Meters_Get_TotalCustomers();
            }
        }

        /// <summary>
        /// (read-only) Totals of all registers of all meters
        /// </summary>
        public /*static*/ double[] Totals
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Meters_Get_Totals(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }
    } // Meters 


    public class Monitors
    {

        /// <summary>
        /// (read-only) Array of doubles for the specified channel  (usage: MyArray = DSSMonitor.Channel(i)) A Save or SaveAll  should be executed first. Done automatically by most standard solution modes.
        /// </summary>
        public /*static*/ double[] Channel(int Index)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.Monitors_Get_Channel(ref resultPtr, ref resultCount, Index);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        public /*static*/ void Process()
        {
            OpenDSS.Lib.Monitors_Process();
        }

        public /*static*/ void ProcessAll()
        {
            OpenDSS.Lib.Monitors_ProcessAll();
        }

        public /*static*/ void Reset()
        {
            OpenDSS.Lib.Monitors_Reset();
        }

        public /*static*/ void ResetAll()
        {
            OpenDSS.Lib.Monitors_ResetAll();
        }

        public /*static*/ void Sample()
        {
            OpenDSS.Lib.Monitors_Sample();
        }

        public /*static*/ void SampleAll()
        {
            OpenDSS.Lib.Monitors_SampleAll();
        }

        public /*static*/ void Save()
        {
            OpenDSS.Lib.Monitors_Save();
        }

        public /*static*/ void SaveAll()
        {
            OpenDSS.Lib.Monitors_SaveAll();
        }

        public /*static*/ void Show()
        {
            OpenDSS.Lib.Monitors_Show();
        }

        /// <summary>
        /// (read-only) Array of all Monitor Names
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Monitors_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Byte Array containing monitor stream values. Make sure a "save" is done first (standard solution modes do this automatically)
        /// </summary>
        public /*static*/ byte[] ByteStream
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Monitors_Get_ByteStream(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_int8_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of Monitors
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Monitors_Get_Count();
            }
        }

        /// <summary>
        /// Full object name of element being monitored.
        /// </summary>
        public /*static*/ string Element
        {
            get {
                return OpenDSS.Lib.Monitors_Get_Element();
            }
            set {
                OpenDSS.Lib.Monitors_Set_Element(value);
            }
        }

        /// <summary>
        /// (read-only) Name of CSV file associated with active Monitor.
        /// </summary>
        public /*static*/ string FileName
        {
            get {
                return OpenDSS.Lib.Monitors_Get_FileName();
            }
        }

        /// <summary>
        /// (read-only) Monitor File Version (integer)
        /// </summary>
        public /*static*/ int FileVersion
        {
            get {
                return OpenDSS.Lib.Monitors_Get_FileVersion();
            }
        }

        /// <summary>
        /// (read-only) Sets the first Monitor active.  Returns 0 if no monitors.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Monitors_Get_First();
            }
        }

        /// <summary>
        /// (read-only) Header string;  Array of strings containing Channel names
        /// </summary>
        public /*static*/ string[] Header
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Monitors_Get_Header(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// Set Monitor mode (bitmask integer - see DSS Help)
        /// </summary>
        public /*static*/ int Mode
        {
            get {
                return OpenDSS.Lib.Monitors_Get_Mode();
            }
            set {
                OpenDSS.Lib.Monitors_Set_Mode(value);
            }
        }

        /// <summary>
        /// Sets the active Monitor object by name
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Monitors_Get_Name();
            }
            set {
                OpenDSS.Lib.Monitors_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets next monitor active.  Returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Monitors_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Number of Channels in the active Monitor
        /// </summary>
        public /*static*/ int NumChannels
        {
            get {
                return OpenDSS.Lib.Monitors_Get_NumChannels();
            }
        }

        /// <summary>
        /// (read-only) Size of each record in ByteStream (Integer). Same as NumChannels.
        /// </summary>
        public /*static*/ int RecordSize
        {
            get {
                return OpenDSS.Lib.Monitors_Get_RecordSize();
            }
        }

        /// <summary>
        /// (read-only) Number of Samples in Monitor at Present
        /// </summary>
        public /*static*/ int SampleCount
        {
            get {
                return OpenDSS.Lib.Monitors_Get_SampleCount();
            }
        }

        /// <summary>
        /// (read) Terminal number of element being monitored
        /// (write) Terminal number of element being monitored.
        /// </summary>
        public /*static*/ int Terminal
        {
            get {
                return OpenDSS.Lib.Monitors_Get_Terminal();
            }
            set {
                OpenDSS.Lib.Monitors_Set_Terminal(value);
            }
        }

        /// <summary>
        /// (read-only) Array of doubles containing frequency values for harmonics mode solutions; Empty for time mode solutions (use dblHour)
        /// </summary>
        public /*static*/ double[] dblFreq
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Monitors_Get_dblFreq(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of doubles containgin time value in hours for time-sampled monitor values; Empty if frequency-sampled values for harmonics solution  (see dblFreq)
        /// </summary>
        public /*static*/ double[] dblHour
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Monitors_Get_dblHour(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }
    } // Monitors 


    public class Parser
    {

        /// <summary>
        /// (read-only) Use this property to parse a Matrix token in OpenDSS format.  Returns square matrix of order specified. Order same as default Fortran order: column by column.
        /// </summary>
        public /*static*/ double[] Matrix(int ExpectedOrder)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.Parser_Get_Matrix(ref resultPtr, ref resultCount, ExpectedOrder);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        /// <summary>
        /// (read-only) Use this property to parse a matrix token specified in lower triangle form. Symmetry is forced.
        /// </summary>
        public /*static*/ double[] SymMatrix(int ExpectedOrder)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.Parser_Get_SymMatrix(ref resultPtr, ref resultCount, ExpectedOrder);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        /// <summary>
        /// (read-only) Returns token as array of doubles. For parsing quoted array syntax.
        /// </summary>
        public /*static*/ double[] Vector(int ExpectedSize)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.Parser_Get_Vector(ref resultPtr, ref resultCount, ExpectedSize);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        public /*static*/ void ResetDelimiters()
        {
            OpenDSS.Lib.Parser_ResetDelimiters();
        }

        /// <summary>
        /// Default is FALSE. If TRUE parser automatically advances to next token after DblValue, IntValue, or StrValue. Simpler when you don't need to check for parameter names.
        /// </summary>
        public /*static*/ bool AutoIncrement
        {
            get {
                return OpenDSS.Lib.Parser_Get_AutoIncrement() != 0;
            }
            set {
                OpenDSS.Lib.Parser_Set_AutoIncrement(value);
            }
        }

        /// <summary>
        /// (read) Get String containing the the characters for Quoting in OpenDSS scripts. Matching pairs defined in EndQuote. Default is "'([{.
        /// (write) Set String containing the the characters for Quoting in OpenDSS scripts. Matching pairs defined in EndQuote. Default is "'([{.
        /// </summary>
        public /*static*/ string BeginQuote
        {
            get {
                return OpenDSS.Lib.Parser_Get_BeginQuote();
            }
            set {
                OpenDSS.Lib.Parser_Set_BeginQuote(value);
            }
        }

        /// <summary>
        /// String to be parsed. Loading this string resets the Parser to the beginning of the line. Then parse off the tokens in sequence.
        /// </summary>
        public /*static*/ string CmdString
        {
            get {
                return OpenDSS.Lib.Parser_Get_CmdString();
            }
            set {
                OpenDSS.Lib.Parser_Set_CmdString(value);
            }
        }

        /// <summary>
        /// (read-only) Return next parameter as a double.
        /// </summary>
        public /*static*/ double DblValue
        {
            get {
                return OpenDSS.Lib.Parser_Get_DblValue();
            }
        }

        /// <summary>
        /// String defining hard delimiters used to separate token on the command string. Default is , and =. The = separates token name from token value. These override whitesspace to separate tokens.
        /// </summary>
        public /*static*/ string Delimiters
        {
            get {
                return OpenDSS.Lib.Parser_Get_Delimiters();
            }
            set {
                OpenDSS.Lib.Parser_Set_Delimiters(value);
            }
        }

        /// <summary>
        /// String containing characters, in order, that match the beginning quote characters in BeginQuote. Default is "')]}
        /// </summary>
        public /*static*/ string EndQuote
        {
            get {
                return OpenDSS.Lib.Parser_Get_EndQuote();
            }
            set {
                OpenDSS.Lib.Parser_Set_EndQuote(value);
            }
        }

        /// <summary>
        /// (read-only) Return next parameter as a long integer.
        /// </summary>
        public /*static*/ int IntValue
        {
            get {
                return OpenDSS.Lib.Parser_Get_IntValue();
            }
        }

        /// <summary>
        /// (read-only) Get next token and return tag name (before = sign) if any. See AutoIncrement.
        /// </summary>
        public /*static*/ string NextParam
        {
            get {
                return OpenDSS.Lib.Parser_Get_NextParam();
            }
        }

        /// <summary>
        /// (read-only) Return next parameter as a string
        /// </summary>
        public /*static*/ string StrValue
        {
            get {
                return OpenDSS.Lib.Parser_Get_StrValue();
            }
        }

        /// <summary>
        /// (read) Get the characters used for White space in the command string.  Default is blank and Tab.
        /// (write) Set the characters used for White space in the command string.  Default is blank and Tab.
        /// </summary>
        public /*static*/ string WhiteSpace
        {
            get {
                return OpenDSS.Lib.Parser_Get_WhiteSpace();
            }
            set {
                OpenDSS.Lib.Parser_Set_WhiteSpace(value);
            }
        }
    } // Parser 


    public class PDElements
    {

        /// <summary>
        /// (read-only) accummulated failure rate for this branch on downline
        /// </summary>
        public /*static*/ double AccumulatedL
        {
            get {
                return OpenDSS.Lib.PDElements_Get_AccumulatedL();
            }
        }

        /// <summary>
        /// (read-only) Number of PD elements (including disabled elements)
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.PDElements_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Get/Set Number of failures per year. For LINE elements: Number of failures per unit length per year. 
        /// </summary>
        public /*static*/ double FaultRate
        {
            get {
                return OpenDSS.Lib.PDElements_Get_FaultRate();
            }
            set {
                OpenDSS.Lib.PDElements_Set_FaultRate(value);
            }
        }

        /// <summary>
        /// (read-only) Set the first enabled PD element to be the active element.  Returns 0 if none found.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.PDElements_Get_First();
            }
        }

        /// <summary>
        /// (read-only) Number of the terminal of active PD element that is on the "from" side. This is set after the meter zone is determined.
        /// </summary>
        public /*static*/ int FromTerminal
        {
            get {
                return OpenDSS.Lib.PDElements_Get_FromTerminal();
            }
        }

        /// <summary>
        /// (read-only) Variant boolean indicating of PD element should be treated as a shunt element rather than a series element. Applies to Capacitor and Reactor elements in particular.
        /// </summary>
        public /*static*/ bool IsShunt
        {
            get {
                return OpenDSS.Lib.PDElements_Get_IsShunt() != 0;
            }
        }

        /// <summary>
        /// (read-only) Failure rate for this branch. Faults per year including length of line.
        /// </summary>
        public /*static*/ double Lambda
        {
            get {
                return OpenDSS.Lib.PDElements_Get_Lambda();
            }
        }

        /// <summary>
        /// (read-only) Get/Set name of active PD Element. Returns null string if active element is not PDElement type.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.PDElements_Get_Name();
            }
            set {
                OpenDSS.Lib.PDElements_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Advance to the next PD element in the circuit. Enabled elements only. Returns 0 when no more elements.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.PDElements_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Number of customers, this branch
        /// </summary>
        public /*static*/ int Numcustomers
        {
            get {
                return OpenDSS.Lib.PDElements_Get_Numcustomers();
            }
        }

        /// <summary>
        /// (read-only) Sets the parent PD element to be the active circuit element.  Returns 0 if no more elements upline.
        /// </summary>
        public /*static*/ int ParentPDElement
        {
            get {
                return OpenDSS.Lib.PDElements_Get_ParentPDElement();
            }
        }

        /// <summary>
        /// Average repair time for this element in hours
        /// </summary>
        public /*static*/ double RepairTime
        {
            get {
                return OpenDSS.Lib.PDElements_Get_RepairTime();
            }
            set {
                OpenDSS.Lib.PDElements_Set_RepairTime(value);
            }
        }

        /// <summary>
        /// (read-only) Integer ID of the feeder section that this PDElement branch is part of
        /// </summary>
        public /*static*/ int SectionID
        {
            get {
                return OpenDSS.Lib.PDElements_Get_SectionID();
            }
        }

        /// <summary>
        /// (read-only) Total miles of line from this element to the end of the zone. For recloser siting algorithm.
        /// </summary>
        public /*static*/ double TotalMiles
        {
            get {
                return OpenDSS.Lib.PDElements_Get_TotalMiles();
            }
        }

        /// <summary>
        /// (read-only) Total number of customers from this branch to the end of the zone
        /// </summary>
        public /*static*/ int Totalcustomers
        {
            get {
                return OpenDSS.Lib.PDElements_Get_Totalcustomers();
            }
        }

        /// <summary>
        /// (read-only) Get/Set percent of faults that are permanent (require repair). Otherwise, fault is assumed to be transient/temporary.
        /// </summary>
        public /*static*/ double pctPermanent
        {
            get {
                return OpenDSS.Lib.PDElements_Get_pctPermanent();
            }
            set {
                OpenDSS.Lib.PDElements_Set_pctPermanent(value);
            }
        }
    } // PDElements 


    public class PVSystems
    {

        /// <summary>
        /// (read-only) Vairant array of strings with all PVSystem names
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.PVSystems_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of PVSystems
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Set first PVSystem active; returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_First();
            }
        }

        /// <summary>
        /// (read) Get the present value of the Irradiance property in W/sq-m
        /// (write) Set the present Irradiance value in W/sq-m
        /// </summary>
        public /*static*/ double Irradiance
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_Irradiance();
            }
            set {
                OpenDSS.Lib.PVSystems_Set_Irradiance(value);
            }
        }

        /// <summary>
        /// (read) Get the name of the active PVSystem
        /// (write) Set the name of the active PVSystem
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_Name();
            }
            set {
                OpenDSS.Lib.PVSystems_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets next PVSystem active; returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_Next();
            }
        }

        /// <summary>
        /// (read) Get Power factor 
        /// (write) Set PF 
        /// </summary>
        public /*static*/ double PF
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_PF();
            }
        }

        /// <summary>
        /// (read-only) Variant Array of PVSYSTEM energy meter register names
        /// </summary>
        public /*static*/ string[] RegisterNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.PVSystems_Get_RegisterNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of doubles containing values in PVSystem registers.
        /// </summary>
        public /*static*/ double[] RegisterValues
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.PVSystems_Get_RegisterValues(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read) Get/set active PVSystem by index;  1..Count
        /// (write) Get/Set Active PVSystem by index:  1.. Count
        /// </summary>
        public /*static*/ int idx
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_idx();
            }
            set {
                OpenDSS.Lib.PVSystems_Set_idx(value);
            }
        }

        /// <summary>
        /// (read) Get Rated kVA of the PVSystem
        /// (write) Set kva rated
        /// </summary>
        public /*static*/ double kVArated
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_kVArated();
            }
        }

        /// <summary>
        /// (read-only) get kW output
        /// </summary>
        public /*static*/ double kW
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_kW();
            }
        }

        /// <summary>
        /// (read) Get kvar value
        /// (write) Set kvar output value
        /// </summary>
        public /*static*/ double kvar
        {
            get {
                return OpenDSS.Lib.PVSystems_Get_kvar();
            }
        }
    } // PVSystems 


    public class Reclosers
    {

        public /*static*/ void Close()
        {
            OpenDSS.Lib.Reclosers_Close();
        }

        public /*static*/ void Open()
        {
            OpenDSS.Lib.Reclosers_Open();
        }

        /// <summary>
        /// (read-only) Array of strings with names of all Reclosers in Active Circuit
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Reclosers_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of Reclosers in active circuit.
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Set First Recloser to be Active Ckt Element. Returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_First();
            }
        }

        /// <summary>
        /// (read) Ground (3I0) instantaneous trip setting - curve multipler or actual amps.
        /// (write) Ground (3I0) trip instantaneous multiplier or actual amps
        /// </summary>
        public /*static*/ double GroundInst
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_GroundInst();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_GroundInst(value);
            }
        }

        /// <summary>
        /// (read-only) Ground (3I0) trip multiplier or actual amps
        /// </summary>
        public /*static*/ double GroundTrip
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_GroundTrip();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_GroundTrip(value);
            }
        }

        /// <summary>
        /// (read) Full name of object this Recloser is monitoring.
        /// (write) Set monitored object by full name.
        /// </summary>
        public /*static*/ string MonitoredObj
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_MonitoredObj();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_MonitoredObj(value);
            }
        }

        /// <summary>
        /// (read-only) Terminal number of Monitored object for the Recloser 
        /// </summary>
        public /*static*/ int MonitoredTerm
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_MonitoredTerm();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_MonitoredTerm(value);
            }
        }

        /// <summary>
        /// (read-only) Get Name of active Recloser or set the active Recloser by name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_Name();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Iterate to the next recloser in the circuit. Returns zero if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Number of fast shots
        /// </summary>
        public /*static*/ int NumFast
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_NumFast();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_NumFast(value);
            }
        }

        /// <summary>
        /// (read-only) Phase instantaneous curve multipler or actual amps
        /// </summary>
        public /*static*/ double PhaseInst
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_PhaseInst();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_PhaseInst(value);
            }
        }

        /// <summary>
        /// (read) Phase trip curve multiplier or actual amps
        /// (write) Phase Trip multiplier or actual amps
        /// </summary>
        public /*static*/ double PhaseTrip
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_PhaseTrip();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_PhaseTrip(value);
            }
        }

        /// <summary>
        /// (read-only) Variant Array of Doubles: reclose intervals, s, between shots.
        /// </summary>
        public /*static*/ double[] RecloseIntervals
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Reclosers_Get_RecloseIntervals(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of shots to lockout (fast + delayed)
        /// </summary>
        public /*static*/ int Shots
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_Shots();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_Shots(value);
            }
        }

        /// <summary>
        /// (read-only) Full name of the circuit element that is being switched by the Recloser.
        /// </summary>
        public /*static*/ string SwitchedObj
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_SwitchedObj();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_SwitchedObj(value);
            }
        }

        /// <summary>
        /// (read-only) Terminal number of the controlled device being switched by the Recloser
        /// </summary>
        public /*static*/ int SwitchedTerm
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_SwitchedTerm();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_SwitchedTerm(value);
            }
        }

        /// <summary>
        /// (read) Get/Set the active Recloser by index into the recloser list.  1..Count
        /// (write) Get/Set the Active Recloser by index into the recloser list. 1..Count
        /// </summary>
        public /*static*/ int idx
        {
            get {
                return OpenDSS.Lib.Reclosers_Get_idx();
            }
            set {
                OpenDSS.Lib.Reclosers_Set_idx(value);
            }
        }
    } // Reclosers 


    public class RegControls
    {

        public /*static*/ void Reset()
        {
            OpenDSS.Lib.RegControls_Reset();
        }

        /// <summary>
        /// (read-only) Array of strings containing all RegControl names
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.RegControls_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// CT primary ampere rating (secondary is 0.2 amperes)
        /// </summary>
        public /*static*/ double CTPrimary
        {
            get {
                return OpenDSS.Lib.RegControls_Get_CTPrimary();
            }
            set {
                OpenDSS.Lib.RegControls_Set_CTPrimary(value);
            }
        }

        /// <summary>
        /// (read-only) Number of RegControl objects in Active Circuit
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.RegControls_Get_Count();
            }
        }

        /// <summary>
        /// Time delay [s] after arming before the first tap change. Control may reset before actually changing taps.
        /// </summary>
        public /*static*/ double Delay
        {
            get {
                return OpenDSS.Lib.RegControls_Get_Delay();
            }
            set {
                OpenDSS.Lib.RegControls_Set_Delay(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the first RegControl active. Returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.RegControls_Get_First();
            }
        }

        /// <summary>
        /// Regulation bandwidth in forward direciton, centered on Vreg
        /// </summary>
        public /*static*/ double ForwardBand
        {
            get {
                return OpenDSS.Lib.RegControls_Get_ForwardBand();
            }
            set {
                OpenDSS.Lib.RegControls_Set_ForwardBand(value);
            }
        }

        /// <summary>
        /// LDC R setting in Volts
        /// </summary>
        public /*static*/ double ForwardR
        {
            get {
                return OpenDSS.Lib.RegControls_Get_ForwardR();
            }
            set {
                OpenDSS.Lib.RegControls_Set_ForwardR(value);
            }
        }

        /// <summary>
        /// Target voltage in the forward direction, on PT secondary base.
        /// </summary>
        public /*static*/ double ForwardVreg
        {
            get {
                return OpenDSS.Lib.RegControls_Get_ForwardVreg();
            }
            set {
                OpenDSS.Lib.RegControls_Set_ForwardVreg(value);
            }
        }

        /// <summary>
        /// LDC X setting in Volts
        /// </summary>
        public /*static*/ double ForwardX
        {
            get {
                return OpenDSS.Lib.RegControls_Get_ForwardX();
            }
            set {
                OpenDSS.Lib.RegControls_Set_ForwardX(value);
            }
        }

        /// <summary>
        /// Time delay is inversely adjsuted, proportinal to the amount of voltage outside the regulating band.
        /// </summary>
        public /*static*/ bool IsInverseTime
        {
            get {
                return OpenDSS.Lib.RegControls_Get_IsInverseTime() != 0;
            }
            set {
                OpenDSS.Lib.RegControls_Set_IsInverseTime(value);
            }
        }

        /// <summary>
        /// Regulator can use different settings in the reverse direction.  Usually not applicable to substation transformers.
        /// </summary>
        public /*static*/ bool IsReversible
        {
            get {
                return OpenDSS.Lib.RegControls_Get_IsReversible() != 0;
            }
            set {
                OpenDSS.Lib.RegControls_Set_IsReversible(value);
            }
        }

        /// <summary>
        /// Maximum tap change per iteration in STATIC solution mode. 1 is more realistic, 16 is the default for a faster soluiton.
        /// </summary>
        public /*static*/ int MaxTapChange
        {
            get {
                return OpenDSS.Lib.RegControls_Get_MaxTapChange();
            }
            set {
                OpenDSS.Lib.RegControls_Set_MaxTapChange(value);
            }
        }

        /// <summary>
        /// Name of a remote regulated bus, in lieu of LDC settings
        /// </summary>
        public /*static*/ string MonitoredBus
        {
            get {
                return OpenDSS.Lib.RegControls_Get_MonitoredBus();
            }
            set {
                OpenDSS.Lib.RegControls_Set_MonitoredBus(value);
            }
        }

        /// <summary>
        /// (read) Get/set Active RegControl  name
        /// (write) Sets a RegControl active by name
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.RegControls_Get_Name();
            }
            set {
                OpenDSS.Lib.RegControls_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the next RegControl active. Returns 0 if none.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.RegControls_Get_Next();
            }
        }

        /// <summary>
        /// PT ratio for voltage control settings
        /// </summary>
        public /*static*/ double PTratio
        {
            get {
                return OpenDSS.Lib.RegControls_Get_PTratio();
            }
            set {
                OpenDSS.Lib.RegControls_Set_PTratio(value);
            }
        }

        /// <summary>
        /// Bandwidth in reverse direction, centered on reverse Vreg.
        /// </summary>
        public /*static*/ double ReverseBand
        {
            get {
                return OpenDSS.Lib.RegControls_Get_ReverseBand();
            }
            set {
                OpenDSS.Lib.RegControls_Set_ReverseBand(value);
            }
        }

        /// <summary>
        /// Reverse LDC R setting in Volts.
        /// </summary>
        public /*static*/ double ReverseR
        {
            get {
                return OpenDSS.Lib.RegControls_Get_ReverseR();
            }
            set {
                OpenDSS.Lib.RegControls_Set_ReverseR(value);
            }
        }

        /// <summary>
        /// Target voltage in the revese direction, on PT secondary base.
        /// </summary>
        public /*static*/ double ReverseVreg
        {
            get {
                return OpenDSS.Lib.RegControls_Get_ReverseVreg();
            }
            set {
                OpenDSS.Lib.RegControls_Set_ReverseVreg(value);
            }
        }

        /// <summary>
        /// Reverse LDC X setting in volts.
        /// </summary>
        public /*static*/ double ReverseX
        {
            get {
                return OpenDSS.Lib.RegControls_Get_ReverseX();
            }
            set {
                OpenDSS.Lib.RegControls_Set_ReverseX(value);
            }
        }

        /// <summary>
        /// Time delay [s] for subsequent tap changes in a set. Control may reset before actually changing taps.
        /// </summary>
        public /*static*/ double TapDelay
        {
            get {
                return OpenDSS.Lib.RegControls_Get_TapDelay();
            }
            set {
                OpenDSS.Lib.RegControls_Set_TapDelay(value);
            }
        }

        /// <summary>
        /// (write-only) Integer number of the tap that the controlled transformer winding is currentliy on.
        /// </summary>
        public /*static*/ int TapNumber
        {
            get {
                return OpenDSS.Lib.RegControls_Get_TapNumber();
            }
            set {
                OpenDSS.Lib.RegControls_Set_TapNumber(value);
            }
        }

        /// <summary>
        /// Tapped winding number
        /// </summary>
        public /*static*/ int TapWinding
        {
            get {
                return OpenDSS.Lib.RegControls_Get_TapWinding();
            }
            set {
                OpenDSS.Lib.RegControls_Set_TapWinding(value);
            }
        }

        /// <summary>
        /// Name of the transformer this regulator controls
        /// </summary>
        public /*static*/ string Transformer
        {
            get {
                return OpenDSS.Lib.RegControls_Get_Transformer();
            }
            set {
                OpenDSS.Lib.RegControls_Set_Transformer(value);
            }
        }

        /// <summary>
        /// First house voltage limit on PT secondary base.  Setting to 0 disables this function.
        /// </summary>
        public /*static*/ double VoltageLimit
        {
            get {
                return OpenDSS.Lib.RegControls_Get_VoltageLimit();
            }
            set {
                OpenDSS.Lib.RegControls_Set_VoltageLimit(value);
            }
        }

        /// <summary>
        /// Winding number for PT and CT connections
        /// </summary>
        public /*static*/ int Winding
        {
            get {
                return OpenDSS.Lib.RegControls_Get_Winding();
            }
            set {
                OpenDSS.Lib.RegControls_Set_Winding(value);
            }
        }
    } // RegControls 


    public class Relays
    {

        /// <summary>
        /// (read-only) Array of strings containing names of all Relay elements
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Relays_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of Relays in circuit
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Relays_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Set First Relay active. If none, returns 0.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Relays_Get_First();
            }
        }

        /// <summary>
        /// (read-only) Full name of object this Relay is monitoring.
        /// </summary>
        public /*static*/ string MonitoredObj
        {
            get {
                return OpenDSS.Lib.Relays_Get_MonitoredObj();
            }
            set {
                OpenDSS.Lib.Relays_Set_MonitoredObj(value);
            }
        }

        /// <summary>
        /// (read-only) Number of terminal of monitored element that this Relay is monitoring.
        /// </summary>
        public /*static*/ int MonitoredTerm
        {
            get {
                return OpenDSS.Lib.Relays_Get_MonitoredTerm();
            }
            set {
                OpenDSS.Lib.Relays_Set_MonitoredTerm(value);
            }
        }

        /// <summary>
        /// (read) Get name of active relay.
        /// (write) Set Relay active by name
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Relays_Get_Name();
            }
            set {
                OpenDSS.Lib.Relays_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Advance to next Relay object. Returns 0 when no more relays.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Relays_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Full name of element that will be switched when relay trips.
        /// </summary>
        public /*static*/ string SwitchedObj
        {
            get {
                return OpenDSS.Lib.Relays_Get_SwitchedObj();
            }
            set {
                OpenDSS.Lib.Relays_Set_SwitchedObj(value);
            }
        }

        /// <summary>
        /// (write-only) Terminal number of the switched object that will be opened when the relay trips.
        /// </summary>
        public /*static*/ int SwitchedTerm
        {
            get {
                return OpenDSS.Lib.Relays_Get_SwitchedTerm();
            }
            set {
                OpenDSS.Lib.Relays_Set_SwitchedTerm(value);
            }
        }

        /// <summary>
        /// (read) Get/Set active Relay by index into the Relay list. 1..Count
        /// (write) Get/Set Relay active by index into relay list. 1..Count
        /// </summary>
        public /*static*/ int idx
        {
            get {
                return OpenDSS.Lib.Relays_Get_idx();
            }
            set {
                OpenDSS.Lib.Relays_Set_idx(value);
            }
        }
    } // Relays 


    public class Sensors
    {

        public /*static*/ void Reset()
        {
            OpenDSS.Lib.Sensors_Reset();
        }

        public /*static*/ void ResetAll()
        {
            OpenDSS.Lib.Sensors_ResetAll();
        }

        /// <summary>
        /// (read-only) Array of Sensor names.
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Sensors_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Number of Sensors in Active Circuit.
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Sensors_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Array of doubles for the line current measurements; don't use with kWS and kVARS.
        /// </summary>
        public /*static*/ double[] Currents
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Sensors_Get_Currents(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Sensors_Set_Currents(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Sets the first sensor active. Returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Sensors_Get_First();
            }
        }

        /// <summary>
        /// (read-only) True if measured voltages are line-line. Currents are always line currents.
        /// </summary>
        public /*static*/ bool IsDelta
        {
            get {
                return OpenDSS.Lib.Sensors_Get_IsDelta() != 0;
            }
            set {
                OpenDSS.Lib.Sensors_Set_IsDelta(value);
            }
        }

        /// <summary>
        /// (read-only) Full Name of the measured element
        /// </summary>
        public /*static*/ string MeteredElement
        {
            get {
                return OpenDSS.Lib.Sensors_Get_MeteredElement();
            }
            set {
                OpenDSS.Lib.Sensors_Set_MeteredElement(value);
            }
        }

        /// <summary>
        /// (read-only) Number of the measured terminal in the measured element.
        /// </summary>
        public /*static*/ int MeteredTerminal
        {
            get {
                return OpenDSS.Lib.Sensors_Get_MeteredTerminal();
            }
            set {
                OpenDSS.Lib.Sensors_Set_MeteredTerminal(value);
            }
        }

        /// <summary>
        /// (read) Name of the active sensor.
        /// (write) Set the active Sensor by name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Sensors_Get_Name();
            }
            set {
                OpenDSS.Lib.Sensors_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the next Sensor active. Returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Sensors_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Assumed percent error in the Sensor measurement. Default is 1.
        /// </summary>
        public /*static*/ double PctError
        {
            get {
                return OpenDSS.Lib.Sensors_Get_PctError();
            }
            set {
                OpenDSS.Lib.Sensors_Set_PctError(value);
            }
        }

        /// <summary>
        /// (read-only) True if voltage measurements are 1-3, 3-2, 2-1.
        /// </summary>
        public /*static*/ bool ReverseDelta
        {
            get {
                return OpenDSS.Lib.Sensors_Get_ReverseDelta() != 0;
            }
            set {
                OpenDSS.Lib.Sensors_Set_ReverseDelta(value);
            }
        }

        /// <summary>
        /// (read-only) Weighting factor for this Sensor measurement with respect to other Sensors. Default is 1.
        /// </summary>
        public /*static*/ double Weight
        {
            get {
                return OpenDSS.Lib.Sensors_Get_Weight();
            }
            set {
                OpenDSS.Lib.Sensors_Set_Weight(value);
            }
        }

        /// <summary>
        /// (read-only) Array of doubles for Q measurements. Overwrites Currents with a new estimate using kWS.
        /// </summary>
        public /*static*/ double[] kVARS
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Sensors_Get_kVARS(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Sensors_Set_kVARS(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Array of doubles for the LL or LN (depending on Delta connection) voltage measurements.
        /// </summary>
        public /*static*/ double[] kVS
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Sensors_Get_kVS(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Sensors_Set_kVS(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Voltage base for the sensor measurements. LL for 2 and 3-phase sensors, LN for 1-phase sensors.
        /// </summary>
        public /*static*/ double kVbase
        {
            get {
                return OpenDSS.Lib.Sensors_Get_kVbase();
            }
            set {
                OpenDSS.Lib.Sensors_Set_kVbase(value);
            }
        }

        /// <summary>
        /// (read-only) Array of doubles for P measurements. Overwrites Currents with a new estimate using kVARS.
        /// </summary>
        public /*static*/ double[] kWS
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Sensors_Get_kWS(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Sensors_Set_kWS(value, value.Length);
            }
        }
    } // Sensors 


    public class Settings
    {

        /// <summary>
        /// {True | False*} Designates whether to allow duplicate names of objects
        /// </summary>
        public /*static*/ bool AllowDuplicates
        {
            get {
                return OpenDSS.Lib.Settings_Get_AllowDuplicates() != 0;
            }
            set {
                OpenDSS.Lib.Settings_Set_AllowDuplicates(value);
            }
        }

        /// <summary>
        /// List of Buses or (File=xxxx) syntax for the AutoAdd solution mode.
        /// </summary>
        public /*static*/ string AutoBusList
        {
            get {
                return OpenDSS.Lib.Settings_Get_AutoBusList();
            }
            set {
                OpenDSS.Lib.Settings_Set_AutoBusList(value);
            }
        }

        /// <summary>
        /// {dssMultiphase * | dssPositiveSeq} IIndicate if the circuit model is positive sequence.
        /// </summary>
        public /*static*/ int CktModel
        {
            get {
                return OpenDSS.Lib.Settings_Get_CktModel();
            }
            set {
                OpenDSS.Lib.Settings_Set_CktModel(value);
            }
        }

        /// <summary>
        /// {True | False*} Denotes whether to trace the control actions to a file.
        /// </summary>
        public /*static*/ bool ControlTrace
        {
            get {
                return OpenDSS.Lib.Settings_Get_ControlTrace() != 0;
            }
            set {
                OpenDSS.Lib.Settings_Set_ControlTrace(value);
            }
        }

        /// <summary>
        /// Per Unit maximum voltage for Emergency conditions.
        /// </summary>
        public /*static*/ double EmergVmaxpu
        {
            get {
                return OpenDSS.Lib.Settings_Get_EmergVmaxpu();
            }
            set {
                OpenDSS.Lib.Settings_Set_EmergVmaxpu(value);
            }
        }

        /// <summary>
        /// Per Unit minimum voltage for Emergency conditions.
        /// </summary>
        public /*static*/ double EmergVminpu
        {
            get {
                return OpenDSS.Lib.Settings_Get_EmergVminpu();
            }
            set {
                OpenDSS.Lib.Settings_Set_EmergVminpu(value);
            }
        }

        /// <summary>
        /// Integer array defining which energy meter registers to use for computing losses
        /// </summary>
        public /*static*/ int[] LossRegs
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Settings_Get_LossRegs(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_int32_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Settings_Set_LossRegs(value, value.Length);
            }
        }

        /// <summary>
        /// Weighting factor applied to Loss register values.
        /// </summary>
        public /*static*/ double LossWeight
        {
            get {
                return OpenDSS.Lib.Settings_Get_LossWeight();
            }
            set {
                OpenDSS.Lib.Settings_Set_LossWeight(value);
            }
        }

        /// <summary>
        /// Per Unit maximum voltage for Normal conditions.
        /// </summary>
        public /*static*/ double NormVmaxpu
        {
            get {
                return OpenDSS.Lib.Settings_Get_NormVmaxpu();
            }
            set {
                OpenDSS.Lib.Settings_Set_NormVmaxpu(value);
            }
        }

        /// <summary>
        /// Per Unit minimum voltage for Normal conditions.
        /// </summary>
        public /*static*/ double NormVminpu
        {
            get {
                return OpenDSS.Lib.Settings_Get_NormVminpu();
            }
            set {
                OpenDSS.Lib.Settings_Set_NormVminpu(value);
            }
        }

        /// <summary>
        /// Name of LoadShape object that serves as the source of price signal data for yearly simulations, etc.
        /// </summary>
        public /*static*/ string PriceCurve
        {
            get {
                return OpenDSS.Lib.Settings_Get_PriceCurve();
            }
            set {
                OpenDSS.Lib.Settings_Set_PriceCurve(value);
            }
        }

        /// <summary>
        /// Price Signal for the Circuit
        /// </summary>
        public /*static*/ double PriceSignal
        {
            get {
                return OpenDSS.Lib.Settings_Get_PriceSignal();
            }
            set {
                OpenDSS.Lib.Settings_Set_PriceSignal(value);
            }
        }

        /// <summary>
        /// {True | False *} Gets value of trapezoidal integration flag in energy meters.
        /// </summary>
        public /*static*/ bool Trapezoidal
        {
            get {
                return OpenDSS.Lib.Settings_Get_Trapezoidal() != 0;
            }
            set {
                OpenDSS.Lib.Settings_Set_Trapezoidal(value);
            }
        }

        /// <summary>
        /// Array of Integers defining energy meter registers to use for computing UE
        /// </summary>
        public /*static*/ int[] UEregs
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Settings_Get_UEregs(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_int32_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Settings_Set_UEregs(value, value.Length);
            }
        }

        /// <summary>
        /// Weighting factor applied to UE register values.
        /// </summary>
        public /*static*/ double UEweight
        {
            get {
                return OpenDSS.Lib.Settings_Get_UEweight();
            }
            set {
                OpenDSS.Lib.Settings_Set_UEweight(value);
            }
        }

        /// <summary>
        /// Array of doubles defining the legal voltage bases in kV L-L
        /// </summary>
        public /*static*/ double[] VoltageBases
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Settings_Get_VoltageBases(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.Settings_Set_VoltageBases(value, value.Length);
            }
        }

        /// <summary>
        /// {True | False*}  Locks Zones on energy meters to prevent rebuilding if a circuit change occurs.
        /// </summary>
        public /*static*/ bool ZoneLock
        {
            get {
                return OpenDSS.Lib.Settings_Get_ZoneLock() != 0;
            }
            set {
                OpenDSS.Lib.Settings_Set_ZoneLock(value);
            }
        }
        /// <summary>
        /// (write-only) Sets all load allocation factors for all loads defined by XFKVA property to this value.
        /// </summary>

        public /*static*/ double AllocationFactors
        {
            set {
                OpenDSS.Lib.Settings_Set_AllocationFactors(value);

            }
        }
    } // Settings 


    public class Solution
    {

        public /*static*/ void BuildYMatrix(int BuildOption, int AllocateVI)
        {
            OpenDSS.Lib.Solution_BuildYMatrix(BuildOption, AllocateVI);
        }

        public /*static*/ void CheckControls()
        {
            OpenDSS.Lib.Solution_CheckControls();
        }

        public /*static*/ void CheckFaultStatus()
        {
            OpenDSS.Lib.Solution_CheckFaultStatus();
        }

        public /*static*/ void Cleanup()
        {
            OpenDSS.Lib.Solution_Cleanup();
        }

        public /*static*/ void DoControlActions()
        {
            OpenDSS.Lib.Solution_DoControlActions();
        }

        public /*static*/ void FinishTimeStep()
        {
            OpenDSS.Lib.Solution_FinishTimeStep();
        }

        public /*static*/ void InitSnap()
        {
            OpenDSS.Lib.Solution_InitSnap();
        }

        public /*static*/ void SampleControlDevices()
        {
            OpenDSS.Lib.Solution_SampleControlDevices();
        }

        public /*static*/ void Sample_DoControlActions()
        {
            OpenDSS.Lib.Solution_Sample_DoControlActions();
        }

        public /*static*/ void Solve()
        {
            OpenDSS.Lib.Solution_Solve();
        }

        public /*static*/ void SolveDirect()
        {
            OpenDSS.Lib.Solution_SolveDirect();
        }

        public /*static*/ void SolveNoControl()
        {
            OpenDSS.Lib.Solution_SolveNoControl();
        }

        public /*static*/ void SolvePflow()
        {
            OpenDSS.Lib.Solution_SolvePflow();
        }

        public /*static*/ void SolvePlusControl()
        {
            OpenDSS.Lib.Solution_SolvePlusControl();
        }

        public /*static*/ void SolveSnap()
        {
            OpenDSS.Lib.Solution_SolveSnap();
        }

        /// <summary>
        /// Type of device to add in AutoAdd Mode: {dssGen (Default) | dssCap}
        /// </summary>
        public /*static*/ int AddType
        {
            get {
                return OpenDSS.Lib.Solution_Get_AddType();
            }
            set {
                OpenDSS.Lib.Solution_Set_AddType(value);
            }
        }

        /// <summary>
        /// Base Solution algorithm: {dssNormalSolve | dssNewtonSolve}
        /// </summary>
        public /*static*/ int Algorithm
        {
            get {
                return OpenDSS.Lib.Solution_Get_Algorithm();
            }
            set {
                OpenDSS.Lib.Solution_Set_Algorithm(value);
            }
        }

        /// <summary>
        /// Capacitor kvar for adding capacitors in AutoAdd mode
        /// </summary>
        public /*static*/ double Capkvar
        {
            get {
                return OpenDSS.Lib.Solution_Get_Capkvar();
            }
            set {
                OpenDSS.Lib.Solution_Set_Capkvar(value);
            }
        }

        /// <summary>
        /// (read-only) Flag indicating the control actions are done.
        /// </summary>
        public /*static*/ bool ControlActionsDone
        {
            get {
                return OpenDSS.Lib.Solution_Get_ControlActionsDone() != 0;
            }
            set {
                OpenDSS.Lib.Solution_Set_ControlActionsDone(value);
            }
        }

        /// <summary>
        /// Value of the control iteration counter
        /// </summary>
        public /*static*/ int ControlIterations
        {
            get {
                return OpenDSS.Lib.Solution_Get_ControlIterations();
            }
            set {
                OpenDSS.Lib.Solution_Set_ControlIterations(value);
            }
        }

        /// <summary>
        /// {dssStatic* | dssEvent | dssTime}  Modes for control devices
        /// </summary>
        public /*static*/ int ControlMode
        {
            get {
                return OpenDSS.Lib.Solution_Get_ControlMode();
            }
            set {
                OpenDSS.Lib.Solution_Set_ControlMode(value);
            }
        }

        /// <summary>
        /// Flag to indicate whether the circuit solution converged
        /// </summary>
        public /*static*/ bool Converged
        {
            get {
                return OpenDSS.Lib.Solution_Get_Converged() != 0;
            }
            set {
                OpenDSS.Lib.Solution_Set_Converged(value);
            }
        }

        /// <summary>
        /// Default daily load shape (defaults to "Default")
        /// </summary>
        public /*static*/ string DefaultDaily
        {
            get {
                return OpenDSS.Lib.Solution_Get_DefaultDaily();
            }
            set {
                OpenDSS.Lib.Solution_Set_DefaultDaily(value);
            }
        }

        /// <summary>
        /// Default Yearly load shape (defaults to "Default")
        /// </summary>
        public /*static*/ string DefaultYearly
        {
            get {
                return OpenDSS.Lib.Solution_Get_DefaultYearly();
            }
            set {
                OpenDSS.Lib.Solution_Set_DefaultYearly(value);
            }
        }

        /// <summary>
        /// (read-only) Array of strings containing the Event Log
        /// </summary>
        public /*static*/ string[] EventLog
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Solution_Get_EventLog(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// Set the Frequency for next solution
        /// </summary>
        public /*static*/ double Frequency
        {
            get {
                return OpenDSS.Lib.Solution_Get_Frequency();
            }
            set {
                OpenDSS.Lib.Solution_Set_Frequency(value);
            }
        }

        /// <summary>
        /// Default Multiplier applied to generators (like LoadMult)
        /// </summary>
        public /*static*/ double GenMult
        {
            get {
                return OpenDSS.Lib.Solution_Get_GenMult();
            }
            set {
                OpenDSS.Lib.Solution_Set_GenMult(value);
            }
        }

        /// <summary>
        /// PF for generators in AutoAdd mode
        /// </summary>
        public /*static*/ double GenPF
        {
            get {
                return OpenDSS.Lib.Solution_Get_GenPF();
            }
            set {
                OpenDSS.Lib.Solution_Set_GenPF(value);
            }
        }

        /// <summary>
        /// Generator kW for AutoAdd mode
        /// </summary>
        public /*static*/ double GenkW
        {
            get {
                return OpenDSS.Lib.Solution_Get_GenkW();
            }
            set {
                OpenDSS.Lib.Solution_Set_GenkW(value);
            }
        }

        /// <summary>
        /// Set Hour for time series solutions.
        /// </summary>
        public /*static*/ int Hour
        {
            get {
                return OpenDSS.Lib.Solution_Get_Hour();
            }
            set {
                OpenDSS.Lib.Solution_Set_Hour(value);
            }
        }

        /// <summary>
        /// (read) Get/Set the Solution.IntervalHrs variable used for devices that integrate
        /// (write) Get/Set the Solution.IntervalHrs variable for custom solution algorithms
        /// </summary>
        public /*static*/ double IntervalHrs
        {
            get {
                return OpenDSS.Lib.Solution_Get_IntervalHrs();
            }
            set {
                OpenDSS.Lib.Solution_Set_IntervalHrs(value);
            }
        }

        /// <summary>
        /// (read-only) Number of iterations taken for last solution. (Same as TotalIterations)
        /// </summary>
        public /*static*/ int Iterations
        {
            get {
                return OpenDSS.Lib.Solution_Get_Iterations();
            }
        }

        /// <summary>
        /// Load-Duration Curve name for LD modes
        /// </summary>
        public /*static*/ string LDCurve
        {
            get {
                return OpenDSS.Lib.Solution_Get_LDCurve();
            }
            set {
                OpenDSS.Lib.Solution_Set_LDCurve(value);
            }
        }

        /// <summary>
        /// Load Model: {dssPowerFlow (default) | dssAdmittance}
        /// </summary>
        public /*static*/ int LoadModel
        {
            get {
                return OpenDSS.Lib.Solution_Get_LoadModel();
            }
            set {
                OpenDSS.Lib.Solution_Set_LoadModel(value);
            }
        }

        /// <summary>
        /// Default load multiplier applied to all non-fixed loads
        /// </summary>
        public /*static*/ double LoadMult
        {
            get {
                return OpenDSS.Lib.Solution_Get_LoadMult();
            }
            set {
                OpenDSS.Lib.Solution_Set_LoadMult(value);
            }
        }

        /// <summary>
        /// Maximum allowable control iterations
        /// </summary>
        public /*static*/ int MaxControlIterations
        {
            get {
                return OpenDSS.Lib.Solution_Get_MaxControlIterations();
            }
            set {
                OpenDSS.Lib.Solution_Set_MaxControlIterations(value);
            }
        }

        /// <summary>
        /// Max allowable iterations.
        /// </summary>
        public /*static*/ int MaxIterations
        {
            get {
                return OpenDSS.Lib.Solution_Get_MaxIterations();
            }
            set {
                OpenDSS.Lib.Solution_Set_MaxIterations(value);
            }
        }

        /// <summary>
        /// (read) Minimum number of iterations required for a power flow solution.
        /// (write) Mininum number of iterations required for a power flow solution.
        /// </summary>
        public /*static*/ int MinIterations
        {
            get {
                return OpenDSS.Lib.Solution_Get_MinIterations();
            }
            set {
                OpenDSS.Lib.Solution_Set_MinIterations(value);
            }
        }

        /// <summary>
        /// Set present solution mode (by a text code - see DSS Help)
        /// </summary>
        public /*static*/ int Mode
        {
            get {
                return OpenDSS.Lib.Solution_Get_Mode();
            }
            set {
                OpenDSS.Lib.Solution_Set_Mode(Mode);
            }
        }

        /// <summary>
        /// (read-only) ID (text) of the present solution mode
        /// </summary>
        public /*static*/ string ModeID
        {
            get {
                return OpenDSS.Lib.Solution_Get_ModeID();
            }
        }

        /// <summary>
        /// (read-only) Max number of iterations required to converge at any control iteration of the most recent solution.
        /// </summary>
        public /*static*/ int MostIterationsDone
        {
            get {
                return OpenDSS.Lib.Solution_Get_MostIterationsDone();
            }
        }

        /// <summary>
        /// Number of solutions to perform for Monte Carlo and time series simulations
        /// </summary>
        public /*static*/ int Number
        {
            get {
                return OpenDSS.Lib.Solution_Get_Number();
            }
            set {
                OpenDSS.Lib.Solution_Set_Number(value);
            }
        }

        /// <summary>
        /// (read-only) Gets the time required to perform the latest solution (Read only)
        /// </summary>
        public /*static*/ double Process_Time
        {
            get {
                return OpenDSS.Lib.Solution_Get_Process_Time();
            }
        }

        /// <summary>
        /// Randomization mode for random variables "Gaussian" or "Uniform"
        /// </summary>
        public /*static*/ int Random
        {
            get {
                return OpenDSS.Lib.Solution_Get_Random();
            }
            set {
                OpenDSS.Lib.Solution_Set_Random(Random);
            }
        }

        /// <summary>
        /// Seconds from top of the hour.
        /// </summary>
        public /*static*/ double Seconds
        {
            get {
                return OpenDSS.Lib.Solution_Get_Seconds();
            }
            set {
                OpenDSS.Lib.Solution_Set_Seconds(value);
            }
        }

        /// <summary>
        /// Time step size in sec
        /// </summary>
        public /*static*/ double StepSize
        {
            get {
                return OpenDSS.Lib.Solution_Get_StepSize();
            }
            set {
                OpenDSS.Lib.Solution_Set_StepSize(value);
            }
        }

        /// <summary>
        /// (read-only) Flag that indicates if elements of the System Y have been changed by recent activity.
        /// </summary>
        public /*static*/ bool SystemYChanged
        {
            get {
                return OpenDSS.Lib.Solution_Get_SystemYChanged() != 0;
            }
        }

        /// <summary>
        /// (read-only) Get the solution process time + sample time for time step
        /// </summary>
        public /*static*/ double Time_of_Step
        {
            get {
                return OpenDSS.Lib.Solution_Get_Time_of_Step();
            }
        }

        /// <summary>
        /// Solution convergence tolerance.
        /// </summary>
        public /*static*/ double Tolerance
        {
            get {
                return OpenDSS.Lib.Solution_Get_Tolerance();
            }
            set {
                OpenDSS.Lib.Solution_Set_Tolerance(value);
            }
        }

        /// <summary>
        /// (read) Gets the accumulated time of the simulation
        /// (write) Sets the Accumulated time of the simulation
        /// </summary>
        public /*static*/ double Total_Time
        {
            get {
                return OpenDSS.Lib.Solution_Get_Total_Time();
            }
            set {
                OpenDSS.Lib.Solution_Set_Total_Time(value);
            }
        }

        /// <summary>
        /// (read-only) Total iterations including control iterations for most recent solution.
        /// </summary>
        public /*static*/ int Totaliterations
        {
            get {
                return OpenDSS.Lib.Solution_Get_Totaliterations();
            }
        }

        /// <summary>
        /// Set year for planning studies
        /// </summary>
        public /*static*/ int Year
        {
            get {
                return OpenDSS.Lib.Solution_Get_Year();
            }
            set {
                OpenDSS.Lib.Solution_Set_Year(value);
            }
        }

        /// <summary>
        /// Hour as a double, including fractional part
        /// </summary>
        public /*static*/ double dblHour
        {
            get {
                return OpenDSS.Lib.Solution_Get_dblHour();
            }
            set {
                OpenDSS.Lib.Solution_Set_dblHour(value);
            }
        }

        /// <summary>
        /// Percent default  annual load growth rate
        /// </summary>
        public /*static*/ double pctGrowth
        {
            get {
                return OpenDSS.Lib.Solution_Get_pctGrowth();
            }
            set {
                OpenDSS.Lib.Solution_Set_pctGrowth(value);
            }
        }
        /// <summary>
        /// (write-only) Set Stepsize in Hr
        /// </summary>

        public /*static*/ double StepsizeHr
        {
            set {
                OpenDSS.Lib.Solution_Set_StepsizeHr(value);

            }
        }
        /// <summary>
        /// (write-only) Set Stepsize in minutes
        /// </summary>

        public /*static*/ double StepsizeMin
        {
            set {
                OpenDSS.Lib.Solution_Set_StepsizeMin(value);

            }
        }
    } // Solution 


    public class SwtControls
    {

        public /*static*/ void Reset()
        {
            OpenDSS.Lib.SwtControls_Reset();
        }

        /// <summary>
        /// Open or Close the switch. No effect if switch is locked.  However, Reset removes any lock and then closes the switch (shelf state).
        /// </summary>
        public /*static*/ int Action
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_Action();
            }
            set {
                OpenDSS.Lib.SwtControls_Set_Action(value);
            }
        }

        /// <summary>
        /// (read-only) Array of strings with all SwtControl names in the active circuit.
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.SwtControls_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_Count();
            }
        }

        /// <summary>
        /// Time delay [s] betwen arming and opening or closing the switch.  Control may reset before actually operating the switch.
        /// </summary>
        public /*static*/ double Delay
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_Delay();
            }
            set {
                OpenDSS.Lib.SwtControls_Set_Delay(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the first SwtControl active. Returns 0 if no more.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_First();
            }
        }

        /// <summary>
        /// The lock prevents both manual and automatic switch operation.
        /// </summary>
        public /*static*/ bool IsLocked
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_IsLocked() != 0;
            }
            set {
                OpenDSS.Lib.SwtControls_Set_IsLocked(value);
            }
        }

        /// <summary>
        /// Sets a SwtControl active by Name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_Name();
            }
            set {
                OpenDSS.Lib.SwtControls_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the next SwtControl active. Returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_Next();
            }
        }

        /// <summary>
        /// (read) Get Normal state of switch
        /// (write) set Normal state of switch  (see actioncodes) dssActionOpen or dssActionClose
        /// </summary>
        public /*static*/ int NormalState
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_NormalState();
            }
            set {
                OpenDSS.Lib.SwtControls_Set_NormalState(value);
            }
        }

        /// <summary>
        /// (read) Force switch to specified state
        /// (write) Get Present state of switch
        /// </summary>
        public /*static*/ int State
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_State();
            }
            set {
                OpenDSS.Lib.SwtControls_Set_State(value);
            }
        }

        /// <summary>
        /// Full name of the switched element.
        /// </summary>
        public /*static*/ string SwitchedObj
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_SwitchedObj();
            }
            set {
                OpenDSS.Lib.SwtControls_Set_SwitchedObj(value);
            }
        }

        /// <summary>
        /// Terminal number where the switch is located on the SwitchedObj
        /// </summary>
        public /*static*/ int SwitchedTerm
        {
            get {
                return OpenDSS.Lib.SwtControls_Get_SwitchedTerm();
            }
            set {
                OpenDSS.Lib.SwtControls_Set_SwitchedTerm(value);
            }
        }
    } // SwtControls 


    public class Text
    {

        /// <summary>
        /// Input command string for the DSS.
        /// </summary>
        public /*static*/ string Command
        {
            get {
                return OpenDSS.Lib.Text_Get_Command();
            }
            set {
                OpenDSS.Lib.Text_Set_Command(value);
                OpenDSS.CheckForError();
            }
        }

        /// <summary>
        /// (read-only) Result string for the last command.
        /// </summary>
        public /*static*/ string Result
        {
            get {
                return OpenDSS.Lib.Text_Get_Result();
            }
        }
    } // Text 


    public class Topology
    {

        /// <summary>
        /// (read-only) Returns index of the active branch
        /// </summary>
        public /*static*/ int ActiveBranch
        {
            get {
                return OpenDSS.Lib.Topology_Get_ActiveBranch();
            }
        }

        /// <summary>
        /// (read-only) Topological depth of the active branch
        /// </summary>
        public /*static*/ int ActiveLevel
        {
            get {
                return OpenDSS.Lib.Topology_Get_ActiveLevel();
            }
        }

        /// <summary>
        /// (read-only) Array of all isolated branch names.
        /// </summary>
        public /*static*/ string[] AllIsolatedBranches
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Topology_Get_AllIsolatedBranches(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of all isolated load names.
        /// </summary>
        public /*static*/ string[] AllIsolatedLoads
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Topology_Get_AllIsolatedLoads(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of all looped element names, by pairs.
        /// </summary>
        public /*static*/ string[] AllLoopedPairs
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Topology_Get_AllLoopedPairs(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) MOve back toward the source, return index of new active branch, or 0 if no more.
        /// </summary>
        public /*static*/ int BackwardBranch
        {
            get {
                return OpenDSS.Lib.Topology_Get_BackwardBranch();
            }
        }

        /// <summary>
        /// (read-only) Name of the active branch.
        /// </summary>
        public /*static*/ string BranchName
        {
            get {
                return OpenDSS.Lib.Topology_Get_BranchName();
            }
            set {
                OpenDSS.Lib.Topology_Set_BranchName(value);
            }
        }

        /// <summary>
        /// (write-only) Set the active branch to one containing this bus, return index or 0 if not found
        /// </summary>
        public /*static*/ string BusName
        {
            get {
                return OpenDSS.Lib.Topology_Get_BusName();
            }
            set {
                OpenDSS.Lib.Topology_Set_BusName(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the first branch active, returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Topology_Get_First();
            }
        }

        /// <summary>
        /// (read-only) First load at the active branch, return index or 0 if none.
        /// </summary>
        public /*static*/ int FirstLoad
        {
            get {
                return OpenDSS.Lib.Topology_Get_FirstLoad();
            }
        }

        /// <summary>
        /// (read-only) Move forward in the tree, return index of new active branch or 0 if no more
        /// </summary>
        public /*static*/ int ForwardBranch
        {
            get {
                return OpenDSS.Lib.Topology_Get_ForwardBranch();
            }
        }

        /// <summary>
        /// (read-only) Move to looped branch, return index or 0 if none.
        /// </summary>
        public /*static*/ int LoopedBranch
        {
            get {
                return OpenDSS.Lib.Topology_Get_LoopedBranch();
            }
        }

        /// <summary>
        /// (read-only) Sets the next branch active, returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Topology_Get_Next();
            }
        }

        /// <summary>
        /// (read-only) Next load at the active branch, return index or 0 if no more.
        /// </summary>
        public /*static*/ int NextLoad
        {
            get {
                return OpenDSS.Lib.Topology_Get_NextLoad();
            }
        }

        /// <summary>
        /// (read-only) Number of isolated branches (PD elements and capacitors).
        /// </summary>
        public /*static*/ int NumIsolatedBranches
        {
            get {
                return OpenDSS.Lib.Topology_Get_NumIsolatedBranches();
            }
        }

        /// <summary>
        /// (read-only) Number of isolated loads
        /// </summary>
        public /*static*/ int NumIsolatedLoads
        {
            get {
                return OpenDSS.Lib.Topology_Get_NumIsolatedLoads();
            }
        }

        /// <summary>
        /// (read-only) Number of loops
        /// </summary>
        public /*static*/ int NumLoops
        {
            get {
                return OpenDSS.Lib.Topology_Get_NumLoops();
            }
        }

        /// <summary>
        /// (read-only) Move to directly parallel branch, return index or 0 if none.
        /// </summary>
        public /*static*/ int ParallelBranch
        {
            get {
                return OpenDSS.Lib.Topology_Get_ParallelBranch();
            }
        }
    } // Topology 


    public class Transformers
    {

        /// <summary>
        /// (read-only) Array of strings with all Transformer names in the active circuit.
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Transformers_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Sets the first Transformer active. Returns 0 if no more.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Transformers_Get_First();
            }
        }

        /// <summary>
        /// Active Winding delta or wye connection?
        /// </summary>
        public /*static*/ bool IsDelta
        {
            get {
                return OpenDSS.Lib.Transformers_Get_IsDelta() != 0;
            }
            set {
                OpenDSS.Lib.Transformers_Set_IsDelta(value);
            }
        }

        /// <summary>
        /// Active Winding maximum tap in per-unit.
        /// </summary>
        public /*static*/ double MaxTap
        {
            get {
                return OpenDSS.Lib.Transformers_Get_MaxTap();
            }
            set {
                OpenDSS.Lib.Transformers_Set_MaxTap(value);
            }
        }

        /// <summary>
        /// Active Winding minimum tap in per-unit.
        /// </summary>
        public /*static*/ double MinTap
        {
            get {
                return OpenDSS.Lib.Transformers_Get_MinTap();
            }
            set {
                OpenDSS.Lib.Transformers_Set_MinTap(value);
            }
        }

        /// <summary>
        /// Sets a Transformer active by Name.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Name();
            }
            set {
                OpenDSS.Lib.Transformers_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the next Transformer active. Returns 0 if no more.
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Next();
            }
        }

        /// <summary>
        /// Active Winding number of tap steps betwein MinTap and MaxTap.
        /// </summary>
        public /*static*/ int NumTaps
        {
            get {
                return OpenDSS.Lib.Transformers_Get_NumTaps();
            }
            set {
                OpenDSS.Lib.Transformers_Set_NumTaps(value);
            }
        }

        /// <summary>
        /// Number of windings on this transformer. Allocates memory; set or change this property first.
        /// </summary>
        public /*static*/ int NumWindings
        {
            get {
                return OpenDSS.Lib.Transformers_Get_NumWindings();
            }
            set {
                OpenDSS.Lib.Transformers_Set_NumWindings(value);
            }
        }

        /// <summary>
        /// Active Winding resistance in %
        /// </summary>
        public /*static*/ double R
        {
            get {
                return OpenDSS.Lib.Transformers_Get_R();
            }
            set {
                OpenDSS.Lib.Transformers_Set_R(value);
            }
        }

        /// <summary>
        /// Active Winding neutral resistance [ohms] for wye connections. Set less than zero for ungrounded wye.
        /// </summary>
        public /*static*/ double Rneut
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Rneut();
            }
            set {
                OpenDSS.Lib.Transformers_Set_Rneut(value);
            }
        }

        /// <summary>
        /// Active Winding tap in per-unit.
        /// </summary>
        public /*static*/ double Tap
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Tap();
            }
            set {
                OpenDSS.Lib.Transformers_Set_Tap(value);
            }
        }

        /// <summary>
        /// Active Winding Number from 1..NumWindings. Update this before reading or setting a sequence of winding properties (R, Tap, kV, kVA, etc.)
        /// </summary>
        public /*static*/ int Wdg
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Wdg();
            }
            set {
                OpenDSS.Lib.Transformers_Set_Wdg(value);
            }
        }

        /// <summary>
        /// Name of an XfrmCode that supplies electircal parameters for this Transformer.
        /// </summary>
        public /*static*/ string XfmrCode
        {
            get {
                return OpenDSS.Lib.Transformers_Get_XfmrCode();
            }
            set {
                OpenDSS.Lib.Transformers_Set_XfmrCode(value);
            }
        }

        /// <summary>
        /// Percent reactance between windings 1 and 2, on winding 1 kVA base. Use for 2-winding or 3-winding transformers.
        /// </summary>
        public /*static*/ double Xhl
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Xhl();
            }
            set {
                OpenDSS.Lib.Transformers_Set_Xhl(value);
            }
        }

        /// <summary>
        /// Percent reactance between windigns 1 and 3, on winding 1 kVA base.  Use for 3-winding transformers only.
        /// </summary>
        public /*static*/ double Xht
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Xht();
            }
            set {
                OpenDSS.Lib.Transformers_Set_Xht(value);
            }
        }

        /// <summary>
        /// Percent reactance between windings 2 and 3, on winding 1 kVA base. Use for 3-winding transformers only.
        /// </summary>
        public /*static*/ double Xlt
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Xlt();
            }
            set {
                OpenDSS.Lib.Transformers_Set_Xlt(value);
            }
        }

        /// <summary>
        /// Active Winding neutral reactance [ohms] for wye connections.
        /// </summary>
        public /*static*/ double Xneut
        {
            get {
                return OpenDSS.Lib.Transformers_Get_Xneut();
            }
            set {
                OpenDSS.Lib.Transformers_Set_Xneut(value);
            }
        }

        /// <summary>
        /// Active Winding kV rating.  Phase-phase for 2 or 3 phases, actual winding kV for 1 phase transformer.
        /// </summary>
        public /*static*/ double kV
        {
            get {
                return OpenDSS.Lib.Transformers_Get_kV();
            }
            set {
                OpenDSS.Lib.Transformers_Set_kV(value);
            }
        }

        /// <summary>
        /// Active Winding kVA rating. On winding 1, this also determines normal and emergency current ratings for all windings.
        /// </summary>
        public /*static*/ double kVA
        {
            get {
                return OpenDSS.Lib.Transformers_Get_kVA();
            }
            set {
                OpenDSS.Lib.Transformers_Set_kVA(value);
            }
        }
    } // Transformers 


    public class Vsources
    {

        /// <summary>
        /// (read-only) Names of all Vsource objects in the circuit
        /// </summary>
        public /*static*/ string[] AllNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Vsources_Get_AllNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read) Phase angle of first phase in degrees
        /// (write) phase angle in degrees
        /// </summary>
        public /*static*/ double AngleDeg
        {
            get {
                return OpenDSS.Lib.Vsources_Get_AngleDeg();
            }
            set {
                OpenDSS.Lib.Vsources_Set_AngleDeg(value);
            }
        }

        /// <summary>
        /// (read) Source Voltage in kV
        /// (write) Source voltage in kV
        /// </summary>
        public /*static*/ double BasekV
        {
            get {
                return OpenDSS.Lib.Vsources_Get_BasekV();
            }
            set {
                OpenDSS.Lib.Vsources_Set_BasekV(value);
            }
        }

        /// <summary>
        /// (read-only) Number of Vsource Object
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.Vsources_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Sets the first VSOURCE to be active; Returns 0 if none
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.Vsources_Get_First();
            }
        }

        /// <summary>
        /// (read) Source Frequency in Hz
        /// (write) Source frequency in Hz
        /// </summary>
        public /*static*/ double Frequency
        {
            get {
                return OpenDSS.Lib.Vsources_Get_Frequency();
            }
            set {
                OpenDSS.Lib.Vsources_Set_Frequency(value);
            }
        }

        /// <summary>
        /// (read) Get Active VSOURCE name
        /// (write) Set Active VSOURCE by Name
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Vsources_Get_Name();
            }
            set {
                OpenDSS.Lib.Vsources_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Sets the next VSOURCE object to be active; returns zero if no more
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.Vsources_Get_Next();
            }
        }

        /// <summary>
        /// (read) Number of Phases
        /// (write) Number of phases
        /// </summary>
        public /*static*/ int Phases
        {
            get {
                return OpenDSS.Lib.Vsources_Get_Phases();
            }
            set {
                OpenDSS.Lib.Vsources_Set_Phases(value);
            }
        }

        /// <summary>
        /// (read) Source pu voltage.
        /// (write) Per-unit value of source voltage based on kV
        /// </summary>
        public /*static*/ double pu
        {
            get {
                return OpenDSS.Lib.Vsources_Get_pu();
            }
            set {
                OpenDSS.Lib.Vsources_Set_pu(value);
            }
        }
    } // Vsources 


    public class XYCurves
    {

        /// <summary>
        /// (read-only) Number of XYCurve Objects
        /// </summary>
        public /*static*/ int Count
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_Count();
            }
        }

        /// <summary>
        /// (read-only) Sets first XYcurve object active; returns 0 if none.
        /// </summary>
        public /*static*/ int First
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_First();
            }
        }

        /// <summary>
        /// (read) Name of active XYCurve Object
        /// (write) Get Name of active XYCurve Object
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_Name();
            }
            set {
                OpenDSS.Lib.XYCurves_Set_Name(value);
            }
        }

        /// <summary>
        /// (read-only) Advances to next XYCurve object; returns 0 if no more objects of this class
        /// </summary>
        public /*static*/ int Next
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_Next();
            }
        }

        /// <summary>
        /// (read) Get/Set Number of points in X-Y curve
        /// (write) Get/Set Number of Points in X-Y curve
        /// </summary>
        public /*static*/ int Npts
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_Npts();
            }
            set {
                OpenDSS.Lib.XYCurves_Set_Npts(value);
            }
        }

        /// <summary>
        /// Get/Set X values as a Array of doubles. Set Npts to max number expected if setting
        /// </summary>
        public /*static*/ double[] Xarray
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.XYCurves_Get_Xarray(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.XYCurves_Set_Xarray(value, value.Length);
            }
        }

        /// <summary>
        /// Factor to scale X values from original curve
        /// </summary>
        public /*static*/ double Xscale
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_Xscale();
            }
            set {
                OpenDSS.Lib.XYCurves_Set_Xscale(value);
            }
        }

        /// <summary>
        /// (read-only) Amount to shift X value from original curve
        /// </summary>
        public /*static*/ double Xshift
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_Xshift();
            }
            set {
                OpenDSS.Lib.XYCurves_Set_Xshift(value);
            }
        }

        /// <summary>
        /// Get/Set Y values in curve; Set Npts to max number expected if setting
        /// </summary>
        public /*static*/ double[] Yarray
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.XYCurves_Get_Yarray(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read) Factor to scale Y values from original curve
        /// (write) Amount to scale Y values from original curve. Represents a curve shift.
        /// </summary>
        public /*static*/ double Yscale
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_Yscale();
            }
            set {
                OpenDSS.Lib.XYCurves_Set_Yscale(value);
            }
        }

        /// <summary>
        /// (read-only) amount to shift Y valiue from original curve
        /// </summary>
        public /*static*/ double Yshift
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_Yshift();
            }
            set {
                OpenDSS.Lib.XYCurves_Set_Yshift(value);
            }
        }

        /// <summary>
        /// (read-only) Set X value or get interpolated value after setting Y
        /// </summary>
        public /*static*/ double x
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_x();
            }
            set {
                OpenDSS.Lib.XYCurves_Set_x(value);
            }
        }

        /// <summary>
        /// (read) Y value for present X or set this value then get corresponding X
        /// (write) Set Y value or get interpolated Y value after setting X
        /// </summary>
        public /*static*/ double y
        {
            get {
                return OpenDSS.Lib.XYCurves_Get_y();
            }
            set {
                OpenDSS.Lib.XYCurves_Set_y(value);
            }
        }
    } // XYCurves 


    public class CktElement
    {
        public /*static*/ DSSProperty Properties = new dss_sharp.DSSProperty();

        public /*static*/ void Close(int Term, int Phs)
        {
            OpenDSS.Lib.CktElement_Close(Term, Phs);
        }

        /// <summary>
        /// (read-only) Full name of the i-th controller attached to this element. Ex: str = Controller(2).  See NumControls to determine valid index range
        /// </summary>
        public /*static*/ string Controller(int idx)
        {
            return OpenDSS.Lib.CktElement_Get_Controller(idx);
        }

        /// <summary>
        /// (read-only) For PCElement, get the value of a variable by name. If Code>0 Then no variable by this name or not a PCelement.
        /// </summary>
        public /*static*/ double Variable(string MyVarName, int Code)
        {
            return OpenDSS.Lib.CktElement_Get_Variable(MyVarName, Code);
        }

        /// <summary>
        /// (read-only) For PCElement, get the value of a variable by integer index.
        /// </summary>
        public /*static*/ double Variablei(int Idx, int Code)
        {
            return OpenDSS.Lib.CktElement_Get_Variablei(Idx, Code);
        }

        public /*static*/ bool IsOpen(int Term, int Phs)
        {
            return OpenDSS.Lib.CktElement_IsOpen(Term, Phs) != 0;
        }

        public /*static*/ void Open(int Term, int Phs)
        {
            OpenDSS.Lib.CktElement_Open(Term, Phs);
        }

        /// <summary>
        /// (read-only) Array containing all property names of the active device.
        /// </summary>
        public /*static*/ string[] AllPropertyNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_AllPropertyNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of strings listing all the published variable names, if a PCElement. Otherwise, null string.
        /// </summary>
        public /*static*/ string[] AllVariableNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_AllVariableNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of doubles. Values of state variables of active element if PC element.
        /// </summary>
        public /*static*/ double[] AllVariableValues
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_AllVariableValues(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read) Array of strings. Get  Bus definitions to which each terminal is connected. 0-based array.
        /// (write) Array of strings. Set Bus definitions for each terminal is connected.
        /// </summary>
        public /*static*/ string[] BusNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_BusNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
            set {
                    OpenDSS.Lib.CktElement_Set_BusNames(value, value.Length);
            }
        }

        /// <summary>
        /// (read-only) Complex double array of Sequence Currents for all conductors of all terminals of active circuit element.
        /// </summary>
        public /*static*/ double[] CplxSeqCurrents
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_CplxSeqCurrents(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex double array of Sequence Voltage for all terminals of active circuit element.
        /// </summary>
        public /*static*/ double[] CplxSeqVoltages
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_CplxSeqVoltages(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex array of currents into each conductor of each terminal
        /// </summary>
        public /*static*/ double[] Currents
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_Currents(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Currents in magnitude, angle format as a array of doubles.
        /// </summary>
        public /*static*/ double[] CurrentsMagAng
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_CurrentsMagAng(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// Display name of the object (not necessarily unique)
        /// </summary>
        public /*static*/ string DisplayName
        {
            get {
                return OpenDSS.Lib.CktElement_Get_DisplayName();
            }
            set {
                OpenDSS.Lib.CktElement_Set_DisplayName(value);
            }
        }

        /// <summary>
        /// (read) Emergency Ampere Rating for PD elements
        /// (write) Emergency Ampere Rating
        /// </summary>
        public /*static*/ double EmergAmps
        {
            get {
                return OpenDSS.Lib.CktElement_Get_EmergAmps();
            }
            set {
                OpenDSS.Lib.CktElement_Set_EmergAmps(value);
            }
        }

        /// <summary>
        /// Boolean indicating that element is currently in the circuit.
        /// </summary>
        public /*static*/ bool Enabled
        {
            get {
                return OpenDSS.Lib.CktElement_Get_Enabled() != 0;
            }
            set {
                OpenDSS.Lib.CktElement_Set_Enabled(value);
            }
        }

        /// <summary>
        /// (read-only) Name of the Energy Meter this element is assigned to.
        /// </summary>
        public /*static*/ string EnergyMeter
        {
            get {
                return OpenDSS.Lib.CktElement_Get_EnergyMeter();
            }
        }

        /// <summary>
        /// (read-only) globally unique identifier for this object
        /// </summary>
        public /*static*/ string GUID
        {
            get {
                return OpenDSS.Lib.CktElement_Get_GUID();
            }
        }

        /// <summary>
        /// (read-only) Pointer to this object
        /// </summary>
        public /*static*/ int Handle
        {
            get {
                return OpenDSS.Lib.CktElement_Get_Handle();
            }
        }

        /// <summary>
        /// (read-only) True if a recloser, relay, or fuse controlling this ckt element. OCP = Overcurrent Protection 
        /// </summary>
        public /*static*/ bool HasOCPDevice
        {
            get {
                return OpenDSS.Lib.CktElement_Get_HasOCPDevice() != 0;
            }
        }

        /// <summary>
        /// (read-only) This element has a SwtControl attached.
        /// </summary>
        public /*static*/ bool HasSwitchControl
        {
            get {
                return OpenDSS.Lib.CktElement_Get_HasSwitchControl() != 0;
            }
        }

        /// <summary>
        /// (read-only) This element has a CapControl or RegControl attached.
        /// </summary>
        public /*static*/ bool HasVoltControl
        {
            get {
                return OpenDSS.Lib.CktElement_Get_HasVoltControl() != 0;
            }
        }

        /// <summary>
        /// (read-only) Total losses in the element: two-element complex array
        /// </summary>
        public /*static*/ double[] Losses
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_Losses(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Full Name of Active Circuit Element
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.CktElement_Get_Name();
            }
        }

        /// <summary>
        /// (read-only) Array of integer containing the node numbers (representing phases, for example) for each conductor of each terminal. 
        /// </summary>
        public /*static*/ int[] NodeOrder
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_NodeOrder(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_int32_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read) Normal ampere rating for PD Elements
        /// (write) Normal ampere rating
        /// </summary>
        public /*static*/ double NormalAmps
        {
            get {
                return OpenDSS.Lib.CktElement_Get_NormalAmps();
            }
            set {
                OpenDSS.Lib.CktElement_Set_NormalAmps(value);
            }
        }

        /// <summary>
        /// (read-only) Number of Conductors per Terminal
        /// </summary>
        public /*static*/ int NumConductors
        {
            get {
                return OpenDSS.Lib.CktElement_Get_NumConductors();
            }
        }

        /// <summary>
        /// (read-only) Number of controls connected to this device. Use to determine valid range for index into Controller array.
        /// </summary>
        public /*static*/ int NumControls
        {
            get {
                return OpenDSS.Lib.CktElement_Get_NumControls();
            }
        }

        /// <summary>
        /// (read-only) Number of Phases
        /// </summary>
        public /*static*/ int NumPhases
        {
            get {
                return OpenDSS.Lib.CktElement_Get_NumPhases();
            }
        }

        /// <summary>
        /// (read-only) Number of Properties this Circuit Element.
        /// </summary>
        public /*static*/ int NumProperties
        {
            get {
                return OpenDSS.Lib.CktElement_Get_NumProperties();
            }
        }

        /// <summary>
        /// (read-only) Number of Terminals this Circuit Element
        /// </summary>
        public /*static*/ int NumTerminals
        {
            get {
                return OpenDSS.Lib.CktElement_Get_NumTerminals();
            }
        }

        /// <summary>
        /// (read-only) Index into Controller list of OCP Device controlling this CktElement
        /// </summary>
        public /*static*/ int OCPDevIndex
        {
            get {
                return OpenDSS.Lib.CktElement_Get_OCPDevIndex();
            }
        }

        /// <summary>
        /// (read-only) 0=None; 1=Fuse; 2=Recloser; 3=Relay;  Type of OCP controller device
        /// </summary>
        public /*static*/ int OCPDevType
        {
            get {
                return OpenDSS.Lib.CktElement_Get_OCPDevType();
            }
        }

        /// <summary>
        /// (read-only) Complex array of losses by phase
        /// </summary>
        public /*static*/ double[] PhaseLosses
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_PhaseLosses(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex array of powers into each conductor of each terminal
        /// </summary>
        public /*static*/ double[] Powers
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_Powers(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Residual currents for each terminal: (mag, angle)
        /// </summary>
        public /*static*/ double[] Residuals
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_Residuals(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Double array of symmetrical component currents into each 3-phase terminal
        /// </summary>
        public /*static*/ double[] SeqCurrents
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_SeqCurrents(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Double array of sequence powers into each 3-phase teminal
        /// </summary>
        public /*static*/ double[] SeqPowers
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_SeqPowers(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Double array of symmetrical component voltages at each 3-phase terminal
        /// </summary>
        public /*static*/ double[] SeqVoltages
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_SeqVoltages(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex array of voltages at terminals
        /// </summary>
        public /*static*/ double[] Voltages
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_Voltages(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Voltages at each conductor in magnitude, angle form as array of doubles.
        /// </summary>
        public /*static*/ double[] VoltagesMagAng
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_VoltagesMagAng(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) YPrim matrix, column order, complex numbers (paired)
        /// </summary>
        public /*static*/ double[] Yprim
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.CktElement_Get_Yprim(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }
    } // CktElement 


    public class DSSElement
    {
        public /*static*/ DSSProperty Properties = new dss_sharp.DSSProperty();

        /// <summary>
        /// (read-only) Array of strings containing the names of all properties for the active DSS object.
        /// </summary>
        public /*static*/ string[] AllPropertyNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.DSSElement_Get_AllPropertyNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Full Name of Active DSS Object (general element or circuit element).
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.DSSElement_Get_Name();
            }
        }

        /// <summary>
        /// (read-only) Number of Properties for the active DSS object.
        /// </summary>
        public /*static*/ int NumProperties
        {
            get {
                return OpenDSS.Lib.DSSElement_Get_NumProperties();
            }
        }
    } // DSSElement 


    public class Circuit
    {
        public /*static*/ Bus Buses = new dss_sharp.Bus();
        public /*static*/ CktElement CktElements = new dss_sharp.CktElement();
        public /*static*/ CktElement ActiveElement = new dss_sharp.CktElement();
        public /*static*/ Solution Solution = new dss_sharp.Solution();
        public /*static*/ Bus ActiveBus = new dss_sharp.Bus();
        public /*static*/ Generators Generators = new dss_sharp.Generators();
        public /*static*/ Meters Meters = new dss_sharp.Meters();
        public /*static*/ Monitors Monitors = new dss_sharp.Monitors();
        public /*static*/ Settings Settings = new dss_sharp.Settings();
        public /*static*/ Lines Lines = new dss_sharp.Lines();
        public /*static*/ CtrlQueue CtrlQueue = new dss_sharp.CtrlQueue();
        public /*static*/ Loads Loads = new dss_sharp.Loads();
        public /*static*/ CktElement ActiveCktElement = new dss_sharp.CktElement();
        public /*static*/ DSSElement ActiveDSSElement = new dss_sharp.DSSElement();
        public /*static*/ ActiveClass ActiveClass = new dss_sharp.ActiveClass();
        public /*static*/ CapControls CapControls = new dss_sharp.CapControls();
        public /*static*/ RegControls RegControls = new dss_sharp.RegControls();
        public /*static*/ SwtControls SwtControls = new dss_sharp.SwtControls();
        public /*static*/ Transformers Transformers = new dss_sharp.Transformers();
        public /*static*/ Capacitors Capacitors = new dss_sharp.Capacitors();
        public /*static*/ Topology Topology = new dss_sharp.Topology();
        public /*static*/ Sensors Sensors = new dss_sharp.Sensors();
        public /*static*/ XYCurves XYCurves = new dss_sharp.XYCurves();
        public /*static*/ PDElements PDElements = new dss_sharp.PDElements();
        public /*static*/ Reclosers Reclosers = new dss_sharp.Reclosers();
        public /*static*/ Relays Relays = new dss_sharp.Relays();
        public /*static*/ LoadShapes LoadShapes = new dss_sharp.LoadShapes();
        public /*static*/ Fuses Fuses = new dss_sharp.Fuses();
        public /*static*/ ISources Isources = new dss_sharp.ISources();
        public /*static*/ DSSimComs DSSim_Coms = new dss_sharp.DSSimComs();
        public /*static*/ PVSystems PVSystems = new dss_sharp.PVSystems();
        public /*static*/ Vsources Vsources = new dss_sharp.Vsources();
        public /*static*/ LineCodes LineCodes = new dss_sharp.LineCodes();

        public /*static*/ double Capacity(double Start, double Increment)
        {
            return OpenDSS.Lib.Circuit_Capacity(Start, Increment);
        }

        public /*static*/ void Disable(string Name)
        {
            OpenDSS.Lib.Circuit_Disable(Name);
        }

        public /*static*/ void Enable(string Name)
        {
            OpenDSS.Lib.Circuit_Enable(Name);
        }

        public /*static*/ void EndOfTimeStepUpdate()
        {
            OpenDSS.Lib.Circuit_EndOfTimeStepUpdate();
        }

        public /*static*/ int FirstElement()
        {
            return OpenDSS.Lib.Circuit_FirstElement();
        }

        public /*static*/ int FirstPCElement()
        {
            return OpenDSS.Lib.Circuit_FirstPCElement();
        }

        public /*static*/ int FirstPDElement()
        {
            return OpenDSS.Lib.Circuit_FirstPDElement();
        }

        /// <summary>
        /// (read-only) Returns an array of doubles representing the distances to parent EnergyMeter. Sequence of array corresponds to other node ByPhase properties.
        /// </summary>
        public /*static*/ double[] AllNodeDistancesByPhase(int Phase)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.Circuit_Get_AllNodeDistancesByPhase(ref resultPtr, ref resultCount, Phase);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        /// <summary>
        /// (read-only) Return array of strings of the node names for the By Phase criteria. Sequence corresponds to other ByPhase properties.
        /// </summary>
        public /*static*/ string[] AllNodeNamesByPhase(int Phase)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.Circuit_Get_AllNodeNamesByPhase(ref resultPtr, ref resultCount, Phase);
            return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
        }

        /// <summary>
        /// (read-only) Returns Array of doubles represent voltage magnitudes for nodes on the specified phase.
        /// </summary>
        public /*static*/ double[] AllNodeVmagByPhase(int Phase)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.Circuit_Get_AllNodeVmagByPhase(ref resultPtr, ref resultCount, Phase);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        /// <summary>
        /// (read-only) Returns array of per unit voltage magnitudes for each node by phase
        /// </summary>
        public /*static*/ double[] AllNodeVmagPUByPhase(int Phase)
        {
            IntPtr resultPtr = new IntPtr();
            int resultCount = 0;
            OpenDSS.Lib.Circuit_Get_AllNodeVmagPUByPhase(ref resultPtr, ref resultCount, Phase);
            return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
        }

        public /*static*/ int NextElement()
        {
            return OpenDSS.Lib.Circuit_NextElement();
        }

        public /*static*/ int NextPCElement()
        {
            return OpenDSS.Lib.Circuit_NextPCElement();
        }

        public /*static*/ int NextPDElement()
        {
            return OpenDSS.Lib.Circuit_NextPDElement();
        }

        public /*static*/ void Sample()
        {
            OpenDSS.Lib.Circuit_Sample();
        }

        public /*static*/ void SaveSample()
        {
            OpenDSS.Lib.Circuit_SaveSample();
        }

        public /*static*/ int SetActiveBus(string BusName)
        {
            return OpenDSS.Lib.Circuit_SetActiveBus(BusName);
        }

        public /*static*/ int SetActiveBusi(int BusIndex)
        {
            return OpenDSS.Lib.Circuit_SetActiveBusi(BusIndex);
        }

        public /*static*/ int SetActiveClass(string ClassName)
        {
            return OpenDSS.Lib.Circuit_SetActiveClass(ClassName);
        }

        public /*static*/ int SetActiveElement(string FullName)
        {
            return OpenDSS.Lib.Circuit_SetActiveElement(FullName);
        }

        public /*static*/ void UpdateStorage()
        {
            OpenDSS.Lib.Circuit_UpdateStorage();
        }

        /// <summary>
        /// (read-only) Returns distance from each bus to parent EnergyMeter. Corresponds to sequence in AllBusNames.
        /// </summary>
        public /*static*/ double[] AllBusDistances
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_AllBusDistances(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of strings containing names of all buses in circuit (see AllNodeNames).
        /// </summary>
        public /*static*/ string[] AllBusNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_AllBusNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of magnitudes (doubles) of voltages at all buses
        /// </summary>
        public /*static*/ double[] AllBusVmag
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_AllBusVmag(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Double Array of all bus voltages (each node) magnitudes in Per unit
        /// </summary>
        public /*static*/ double[] AllBusVmagPu
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_AllBusVmagPu(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex array of all bus, node voltages from most recent solution
        /// </summary>
        public /*static*/ double[] AllBusVolts
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_AllBusVolts(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of total losses (complex) in each circuit element
        /// </summary>
        public /*static*/ double[] AllElementLosses
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_AllElementLosses(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of strings containing Full Name of all elements.
        /// </summary>
        public /*static*/ string[] AllElementNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_AllElementNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Returns an array of distances from parent EnergyMeter for each Node. Corresponds to AllBusVMag sequence.
        /// </summary>
        public /*static*/ double[] AllNodeDistances
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_AllNodeDistances(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of strings containing full name of each node in system in same order as returned by AllBusVolts, etc.
        /// </summary>
        public /*static*/ string[] AllNodeNames
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_AllNodeNames(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex total line losses in the circuit
        /// </summary>
        public /*static*/ double[] LineLosses
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_LineLosses(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Total losses in active circuit, complex number (two-element array of double).
        /// </summary>
        public /*static*/ double[] Losses
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_Losses(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Name of the active circuit.
        /// </summary>
        public /*static*/ string Name
        {
            get {
                return OpenDSS.Lib.Circuit_Get_Name();
            }
        }

        /// <summary>
        /// (read-only) Total number of Buses in the circuit.
        /// </summary>
        public /*static*/ int NumBuses
        {
            get {
                return OpenDSS.Lib.Circuit_Get_NumBuses();
            }
        }

        /// <summary>
        /// (read-only) Number of CktElements in the circuit.
        /// </summary>
        public /*static*/ int NumCktElements
        {
            get {
                return OpenDSS.Lib.Circuit_Get_NumCktElements();
            }
        }

        /// <summary>
        /// (read-only) Total number of nodes in the circuit.
        /// </summary>
        public /*static*/ int NumNodes
        {
            get {
                return OpenDSS.Lib.Circuit_Get_NumNodes();
            }
        }

        /// <summary>
        /// (read-only) Sets Parent PD element, if any, to be the active circuit element and returns index>0; Returns 0 if it fails or not applicable.
        /// </summary>
        public /*static*/ int ParentPDElement
        {
            get {
                return OpenDSS.Lib.Circuit_Get_ParentPDElement();
            }
        }

        /// <summary>
        /// (read-only) Complex losses in all transformers designated to substations.
        /// </summary>
        public /*static*/ double[] SubstationLosses
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_SubstationLosses(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) System Y matrix (after a solution has been performed)
        /// </summary>
        public /*static*/ double[] SystemY
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_SystemY(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Total power, watts delivered to the circuit
        /// </summary>
        public /*static*/ double[] TotalPower
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_TotalPower(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of doubles containing complex injection currents for the present solution. Is is the "I" vector of I=YV
        /// </summary>
        public /*static*/ double[] YCurrents
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_YCurrents(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Array of strings containing the names of the nodes in the same order as the Y matrix
        /// </summary>
        public /*static*/ string[] YNodeOrder
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_YNodeOrder(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Complex array of actual node voltages in same order as SystemY matrix.
        /// </summary>
        public /*static*/ double[] YNodeVarray
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.Circuit_Get_YNodeVarray(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_float64_array(ref resultPtr, resultCount);
            }
        }
    } // Circuit 


    public class DSS
    {
        public /*static*/ Circuit ActiveCircuit = new dss_sharp.Circuit();
        public /*static*/ Circuit Circuits = new dss_sharp.Circuit();
        public /*static*/ Error Error = new dss_sharp.Error();
        public /*static*/ Text Text = new dss_sharp.Text();
        public /*static*/ Circuit NewCircuit = new dss_sharp.Circuit();
        public /*static*/ DSSProgress DSSProgress = new dss_sharp.DSSProgress();
        public /*static*/ ActiveClass ActiveClass = new dss_sharp.ActiveClass();
        public /*static*/ DSS_Executive Executive = new dss_sharp.DSS_Executive();
        public /*static*/ DSSEvents Events = new dss_sharp.DSSEvents();
        public /*static*/ CmathLib CmathLib = new dss_sharp.CmathLib();
        public /*static*/ Parser Parser = new dss_sharp.Parser();
        public /*static*/ DSSimComs DSSim_Coms = new dss_sharp.DSSimComs();

        public /*static*/ void ClearAll()
        {
            OpenDSS.Lib.DSS_ClearAll();
        }

        public /*static*/ void Reset()
        {
            OpenDSS.Lib.DSS_Reset();
        }

        public /*static*/ int SetActiveClass(string ClassName)
        {
            return OpenDSS.Lib.DSS_SetActiveClass(ClassName);
        }

        public /*static*/ bool Start(int code)
        {
            return OpenDSS.Lib.DSS_Start(code) != 0;
        }

        /// <summary>
        /// (read-only) List of DSS intrinsic classes (names of the classes)
        /// </summary>
        public /*static*/ string[] Classes
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.DSS_Get_Classes(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// DSS Data File Path.  Default path for reports, etc. from DSS
        /// </summary>
        public /*static*/ string DataPath
        {
            get {
                return OpenDSS.Lib.DSS_Get_DataPath();
            }
            set {
                OpenDSS.Lib.DSS_Set_DataPath(value);
            }
        }

        /// <summary>
        /// (read-only) Returns the path name for the default text editor.
        /// </summary>
        public /*static*/ string DefaultEditor
        {
            get {
                return OpenDSS.Lib.DSS_Get_DefaultEditor();
            }
        }

        /// <summary>
        /// (read-only) Number of Circuits currently defined
        /// </summary>
        public /*static*/ int NumCircuits
        {
            get {
                return OpenDSS.Lib.DSS_Get_NumCircuits();
            }
        }

        /// <summary>
        /// (read-only) Number of DSS intrinsic classes
        /// </summary>
        public /*static*/ int NumClasses
        {
            get {
                return OpenDSS.Lib.DSS_Get_NumClasses();
            }
        }

        /// <summary>
        /// (read-only) Number of user-defined classes
        /// </summary>
        public /*static*/ int NumUserClasses
        {
            get {
                return OpenDSS.Lib.DSS_Get_NumUserClasses();
            }
        }

        /// <summary>
        /// (read-only) List of user-defined classes
        /// </summary>
        public /*static*/ string[] UserClasses
        {
            get {
                IntPtr resultPtr = new IntPtr();
                int resultCount = 0;
                OpenDSS.Lib.DSS_Get_UserClasses(ref resultPtr, ref resultCount);
                return OpenDSS.Lib.get_string_array(ref resultPtr, resultCount);
            }
        }

        /// <summary>
        /// (read-only) Get version string for the DSS.
        /// </summary>
        public /*static*/ string Version
        {
            get {
                return OpenDSS.Lib.DSS_Get_Version();
            }
        }

    
        public /*static*/ bool AllowForms
        {
            get {
                //#warning AllowForms is not implemented.
                return false;
            }
            set {
                //#warning AllowForms is not implemented.
            }
        }

        public /*static*/ void ShowPanel()
        {
            //#warning ShowPanel is not implemented.
        }

    } // DSS 



}     
