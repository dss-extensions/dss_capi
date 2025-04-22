

/* this ALWAYS GENERATED file contains the definitions for the interfaces */


 /* File created by MIDL compiler version 8.01.0628 */
/* at Tue Jan 19 00:14:07 2038
 */
/* Compiler settings for DSSExtensions.idl:
    Oicf, W1, Zp8, env=Win64 (32b run), target_arch=AMD64 8.01.0628 
    protocol : all , ms_ext, c_ext, robust
    error checks: allocation ref bounds_check enum stub_data 
    VC __declspec() decoration level: 
         __declspec(uuid()), __declspec(selectany), __declspec(novtable)
         DECLSPEC_UUID(), MIDL_INTERFACE()
*/
/* @@MIDL_FILE_HEADING(  ) */



/* verify that the <rpcndr.h> version is high enough to compile this file*/
#ifndef __REQUIRED_RPCNDR_H_VERSION__
#define __REQUIRED_RPCNDR_H_VERSION__ 500
#endif

#include "rpc.h"
#include "rpcndr.h"

#ifndef __RPCNDR_H_VERSION__
#error this stub requires an updated version of <rpcndr.h>
#endif /* __RPCNDR_H_VERSION__ */


#ifndef __DSSExtensions_i_h__
#define __DSSExtensions_i_h__

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
#pragma once
#endif

#ifndef DECLSPEC_XFGVIRT
#if defined(_CONTROL_FLOW_GUARD_XFG)
#define DECLSPEC_XFGVIRT(base, func) __declspec(xfg_virtual(base, func))
#else
#define DECLSPEC_XFGVIRT(base, func)
#endif
#endif

/* Forward Declarations */ 

#ifndef __IText_FWD_DEFINED__
#define __IText_FWD_DEFINED__
typedef interface IText IText;

#endif 	/* __IText_FWD_DEFINED__ */


#ifndef __IDSSProperty_FWD_DEFINED__
#define __IDSSProperty_FWD_DEFINED__
typedef interface IDSSProperty IDSSProperty;

#endif 	/* __IDSSProperty_FWD_DEFINED__ */


#ifndef __ICktElement_FWD_DEFINED__
#define __ICktElement_FWD_DEFINED__
typedef interface ICktElement ICktElement;

#endif 	/* __ICktElement_FWD_DEFINED__ */


#ifndef __IError_FWD_DEFINED__
#define __IError_FWD_DEFINED__
typedef interface IError IError;

#endif 	/* __IError_FWD_DEFINED__ */


#ifndef __ICircuit_FWD_DEFINED__
#define __ICircuit_FWD_DEFINED__
typedef interface ICircuit ICircuit;

#endif 	/* __ICircuit_FWD_DEFINED__ */


#ifndef __IBus_FWD_DEFINED__
#define __IBus_FWD_DEFINED__
typedef interface IBus IBus;

#endif 	/* __IBus_FWD_DEFINED__ */


#ifndef __IDSS_FWD_DEFINED__
#define __IDSS_FWD_DEFINED__
typedef interface IDSS IDSS;

#endif 	/* __IDSS_FWD_DEFINED__ */


#ifndef __ISolution_FWD_DEFINED__
#define __ISolution_FWD_DEFINED__
typedef interface ISolution ISolution;

#endif 	/* __ISolution_FWD_DEFINED__ */


#ifndef __IMonitors_FWD_DEFINED__
#define __IMonitors_FWD_DEFINED__
typedef interface IMonitors IMonitors;

#endif 	/* __IMonitors_FWD_DEFINED__ */


#ifndef __IMeters_FWD_DEFINED__
#define __IMeters_FWD_DEFINED__
typedef interface IMeters IMeters;

#endif 	/* __IMeters_FWD_DEFINED__ */


#ifndef __IGenerators_FWD_DEFINED__
#define __IGenerators_FWD_DEFINED__
typedef interface IGenerators IGenerators;

#endif 	/* __IGenerators_FWD_DEFINED__ */


#ifndef __IDSSProgress_FWD_DEFINED__
#define __IDSSProgress_FWD_DEFINED__
typedef interface IDSSProgress IDSSProgress;

#endif 	/* __IDSSProgress_FWD_DEFINED__ */


#ifndef __ISettings_FWD_DEFINED__
#define __ISettings_FWD_DEFINED__
typedef interface ISettings ISettings;

#endif 	/* __ISettings_FWD_DEFINED__ */


#ifndef __ILines_FWD_DEFINED__
#define __ILines_FWD_DEFINED__
typedef interface ILines ILines;

#endif 	/* __ILines_FWD_DEFINED__ */


#ifndef __ICtrlQueue_FWD_DEFINED__
#define __ICtrlQueue_FWD_DEFINED__
typedef interface ICtrlQueue ICtrlQueue;

#endif 	/* __ICtrlQueue_FWD_DEFINED__ */


#ifndef __ILoads_FWD_DEFINED__
#define __ILoads_FWD_DEFINED__
typedef interface ILoads ILoads;

#endif 	/* __ILoads_FWD_DEFINED__ */


#ifndef __IDSSElement_FWD_DEFINED__
#define __IDSSElement_FWD_DEFINED__
typedef interface IDSSElement IDSSElement;

#endif 	/* __IDSSElement_FWD_DEFINED__ */


#ifndef __IActiveClass_FWD_DEFINED__
#define __IActiveClass_FWD_DEFINED__
typedef interface IActiveClass IActiveClass;

#endif 	/* __IActiveClass_FWD_DEFINED__ */


#ifndef __ICapacitors_FWD_DEFINED__
#define __ICapacitors_FWD_DEFINED__
typedef interface ICapacitors ICapacitors;

#endif 	/* __ICapacitors_FWD_DEFINED__ */


#ifndef __ITransformers_FWD_DEFINED__
#define __ITransformers_FWD_DEFINED__
typedef interface ITransformers ITransformers;

#endif 	/* __ITransformers_FWD_DEFINED__ */


#ifndef __ISwtControls_FWD_DEFINED__
#define __ISwtControls_FWD_DEFINED__
typedef interface ISwtControls ISwtControls;

#endif 	/* __ISwtControls_FWD_DEFINED__ */


#ifndef __ICapControls_FWD_DEFINED__
#define __ICapControls_FWD_DEFINED__
typedef interface ICapControls ICapControls;

#endif 	/* __ICapControls_FWD_DEFINED__ */


#ifndef __IRegControls_FWD_DEFINED__
#define __IRegControls_FWD_DEFINED__
typedef interface IRegControls IRegControls;

#endif 	/* __IRegControls_FWD_DEFINED__ */


#ifndef __ITopology_FWD_DEFINED__
#define __ITopology_FWD_DEFINED__
typedef interface ITopology ITopology;

#endif 	/* __ITopology_FWD_DEFINED__ */


#ifndef __IDSS_Executive_FWD_DEFINED__
#define __IDSS_Executive_FWD_DEFINED__
typedef interface IDSS_Executive IDSS_Executive;

#endif 	/* __IDSS_Executive_FWD_DEFINED__ */


#ifndef __ISensors_FWD_DEFINED__
#define __ISensors_FWD_DEFINED__
typedef interface ISensors ISensors;

#endif 	/* __ISensors_FWD_DEFINED__ */


#ifndef __IXYCurves_FWD_DEFINED__
#define __IXYCurves_FWD_DEFINED__
typedef interface IXYCurves IXYCurves;

#endif 	/* __IXYCurves_FWD_DEFINED__ */


#ifndef __IPDElements_FWD_DEFINED__
#define __IPDElements_FWD_DEFINED__
typedef interface IPDElements IPDElements;

#endif 	/* __IPDElements_FWD_DEFINED__ */


#ifndef __IReclosers_FWD_DEFINED__
#define __IReclosers_FWD_DEFINED__
typedef interface IReclosers IReclosers;

#endif 	/* __IReclosers_FWD_DEFINED__ */


#ifndef __IRelays_FWD_DEFINED__
#define __IRelays_FWD_DEFINED__
typedef interface IRelays IRelays;

#endif 	/* __IRelays_FWD_DEFINED__ */


#ifndef __ICmathLib_FWD_DEFINED__
#define __ICmathLib_FWD_DEFINED__
typedef interface ICmathLib ICmathLib;

#endif 	/* __ICmathLib_FWD_DEFINED__ */


#ifndef __IParser_FWD_DEFINED__
#define __IParser_FWD_DEFINED__
typedef interface IParser IParser;

#endif 	/* __IParser_FWD_DEFINED__ */


#ifndef __ILoadShapes_FWD_DEFINED__
#define __ILoadShapes_FWD_DEFINED__
typedef interface ILoadShapes ILoadShapes;

#endif 	/* __ILoadShapes_FWD_DEFINED__ */


#ifndef __IFuses_FWD_DEFINED__
#define __IFuses_FWD_DEFINED__
typedef interface IFuses IFuses;

#endif 	/* __IFuses_FWD_DEFINED__ */


#ifndef __IISources_FWD_DEFINED__
#define __IISources_FWD_DEFINED__
typedef interface IISources IISources;

#endif 	/* __IISources_FWD_DEFINED__ */


#ifndef __IDSSimComs_FWD_DEFINED__
#define __IDSSimComs_FWD_DEFINED__
typedef interface IDSSimComs IDSSimComs;

#endif 	/* __IDSSimComs_FWD_DEFINED__ */


#ifndef __IPVSystems_FWD_DEFINED__
#define __IPVSystems_FWD_DEFINED__
typedef interface IPVSystems IPVSystems;

#endif 	/* __IPVSystems_FWD_DEFINED__ */


#ifndef __IVsources_FWD_DEFINED__
#define __IVsources_FWD_DEFINED__
typedef interface IVsources IVsources;

#endif 	/* __IVsources_FWD_DEFINED__ */


#ifndef __IParallel_FWD_DEFINED__
#define __IParallel_FWD_DEFINED__
typedef interface IParallel IParallel;

#endif 	/* __IParallel_FWD_DEFINED__ */


#ifndef __ILineCodes_FWD_DEFINED__
#define __ILineCodes_FWD_DEFINED__
typedef interface ILineCodes ILineCodes;

#endif 	/* __ILineCodes_FWD_DEFINED__ */


#ifndef __IGICSources_FWD_DEFINED__
#define __IGICSources_FWD_DEFINED__
typedef interface IGICSources IGICSources;

#endif 	/* __IGICSources_FWD_DEFINED__ */


#ifndef __IReduceCkt_FWD_DEFINED__
#define __IReduceCkt_FWD_DEFINED__
typedef interface IReduceCkt IReduceCkt;

#endif 	/* __IReduceCkt_FWD_DEFINED__ */


#ifndef __IStorages_FWD_DEFINED__
#define __IStorages_FWD_DEFINED__
typedef interface IStorages IStorages;

#endif 	/* __IStorages_FWD_DEFINED__ */


#ifndef __IWindGens_FWD_DEFINED__
#define __IWindGens_FWD_DEFINED__
typedef interface IWindGens IWindGens;

#endif 	/* __IWindGens_FWD_DEFINED__ */


#ifndef __IReactors_FWD_DEFINED__
#define __IReactors_FWD_DEFINED__
typedef interface IReactors IReactors;

#endif 	/* __IReactors_FWD_DEFINED__ */


#ifndef __IYMatrix_FWD_DEFINED__
#define __IYMatrix_FWD_DEFINED__
typedef interface IYMatrix IYMatrix;

#endif 	/* __IYMatrix_FWD_DEFINED__ */


#ifndef __IZIP_FWD_DEFINED__
#define __IZIP_FWD_DEFINED__
typedef interface IZIP IZIP;

#endif 	/* __IZIP_FWD_DEFINED__ */


#ifndef __Text_FWD_DEFINED__
#define __Text_FWD_DEFINED__

#ifdef __cplusplus
typedef class Text Text;
#else
typedef struct Text Text;
#endif /* __cplusplus */

#endif 	/* __Text_FWD_DEFINED__ */


#ifndef __DSSProperty_FWD_DEFINED__
#define __DSSProperty_FWD_DEFINED__

#ifdef __cplusplus
typedef class DSSProperty DSSProperty;
#else
typedef struct DSSProperty DSSProperty;
#endif /* __cplusplus */

#endif 	/* __DSSProperty_FWD_DEFINED__ */


#ifndef __CktElement_FWD_DEFINED__
#define __CktElement_FWD_DEFINED__

#ifdef __cplusplus
typedef class CktElement CktElement;
#else
typedef struct CktElement CktElement;
#endif /* __cplusplus */

#endif 	/* __CktElement_FWD_DEFINED__ */


#ifndef __Error_FWD_DEFINED__
#define __Error_FWD_DEFINED__

#ifdef __cplusplus
typedef class Error Error;
#else
typedef struct Error Error;
#endif /* __cplusplus */

#endif 	/* __Error_FWD_DEFINED__ */


#ifndef __Circuit_FWD_DEFINED__
#define __Circuit_FWD_DEFINED__

#ifdef __cplusplus
typedef class Circuit Circuit;
#else
typedef struct Circuit Circuit;
#endif /* __cplusplus */

#endif 	/* __Circuit_FWD_DEFINED__ */


#ifndef __Bus_FWD_DEFINED__
#define __Bus_FWD_DEFINED__

#ifdef __cplusplus
typedef class Bus Bus;
#else
typedef struct Bus Bus;
#endif /* __cplusplus */

#endif 	/* __Bus_FWD_DEFINED__ */


#ifndef __DSS_FWD_DEFINED__
#define __DSS_FWD_DEFINED__

#ifdef __cplusplus
typedef class DSS DSS;
#else
typedef struct DSS DSS;
#endif /* __cplusplus */

#endif 	/* __DSS_FWD_DEFINED__ */


#ifndef __Solution_FWD_DEFINED__
#define __Solution_FWD_DEFINED__

#ifdef __cplusplus
typedef class Solution Solution;
#else
typedef struct Solution Solution;
#endif /* __cplusplus */

#endif 	/* __Solution_FWD_DEFINED__ */


#ifndef __Monitors_FWD_DEFINED__
#define __Monitors_FWD_DEFINED__

#ifdef __cplusplus
typedef class Monitors Monitors;
#else
typedef struct Monitors Monitors;
#endif /* __cplusplus */

#endif 	/* __Monitors_FWD_DEFINED__ */


#ifndef __Meters_FWD_DEFINED__
#define __Meters_FWD_DEFINED__

#ifdef __cplusplus
typedef class Meters Meters;
#else
typedef struct Meters Meters;
#endif /* __cplusplus */

#endif 	/* __Meters_FWD_DEFINED__ */


#ifndef __Generators_FWD_DEFINED__
#define __Generators_FWD_DEFINED__

#ifdef __cplusplus
typedef class Generators Generators;
#else
typedef struct Generators Generators;
#endif /* __cplusplus */

#endif 	/* __Generators_FWD_DEFINED__ */


#ifndef __DSSProgress_FWD_DEFINED__
#define __DSSProgress_FWD_DEFINED__

#ifdef __cplusplus
typedef class DSSProgress DSSProgress;
#else
typedef struct DSSProgress DSSProgress;
#endif /* __cplusplus */

#endif 	/* __DSSProgress_FWD_DEFINED__ */


#ifndef __Settings_FWD_DEFINED__
#define __Settings_FWD_DEFINED__

#ifdef __cplusplus
typedef class Settings Settings;
#else
typedef struct Settings Settings;
#endif /* __cplusplus */

#endif 	/* __Settings_FWD_DEFINED__ */


#ifndef __Lines_FWD_DEFINED__
#define __Lines_FWD_DEFINED__

#ifdef __cplusplus
typedef class Lines Lines;
#else
typedef struct Lines Lines;
#endif /* __cplusplus */

#endif 	/* __Lines_FWD_DEFINED__ */


#ifndef __CtrlQueue_FWD_DEFINED__
#define __CtrlQueue_FWD_DEFINED__

#ifdef __cplusplus
typedef class CtrlQueue CtrlQueue;
#else
typedef struct CtrlQueue CtrlQueue;
#endif /* __cplusplus */

#endif 	/* __CtrlQueue_FWD_DEFINED__ */


#ifndef __Loads_FWD_DEFINED__
#define __Loads_FWD_DEFINED__

#ifdef __cplusplus
typedef class Loads Loads;
#else
typedef struct Loads Loads;
#endif /* __cplusplus */

#endif 	/* __Loads_FWD_DEFINED__ */


#ifndef __DSSElement_FWD_DEFINED__
#define __DSSElement_FWD_DEFINED__

#ifdef __cplusplus
typedef class DSSElement DSSElement;
#else
typedef struct DSSElement DSSElement;
#endif /* __cplusplus */

#endif 	/* __DSSElement_FWD_DEFINED__ */


#ifndef __ActiveClass_FWD_DEFINED__
#define __ActiveClass_FWD_DEFINED__

#ifdef __cplusplus
typedef class ActiveClass ActiveClass;
#else
typedef struct ActiveClass ActiveClass;
#endif /* __cplusplus */

#endif 	/* __ActiveClass_FWD_DEFINED__ */


#ifndef __Capacitors_FWD_DEFINED__
#define __Capacitors_FWD_DEFINED__

#ifdef __cplusplus
typedef class Capacitors Capacitors;
#else
typedef struct Capacitors Capacitors;
#endif /* __cplusplus */

#endif 	/* __Capacitors_FWD_DEFINED__ */


#ifndef __Transformers_FWD_DEFINED__
#define __Transformers_FWD_DEFINED__

#ifdef __cplusplus
typedef class Transformers Transformers;
#else
typedef struct Transformers Transformers;
#endif /* __cplusplus */

#endif 	/* __Transformers_FWD_DEFINED__ */


#ifndef __SwtControls_FWD_DEFINED__
#define __SwtControls_FWD_DEFINED__

#ifdef __cplusplus
typedef class SwtControls SwtControls;
#else
typedef struct SwtControls SwtControls;
#endif /* __cplusplus */

#endif 	/* __SwtControls_FWD_DEFINED__ */


#ifndef __CapControls_FWD_DEFINED__
#define __CapControls_FWD_DEFINED__

#ifdef __cplusplus
typedef class CapControls CapControls;
#else
typedef struct CapControls CapControls;
#endif /* __cplusplus */

#endif 	/* __CapControls_FWD_DEFINED__ */


#ifndef __RegControls_FWD_DEFINED__
#define __RegControls_FWD_DEFINED__

#ifdef __cplusplus
typedef class RegControls RegControls;
#else
typedef struct RegControls RegControls;
#endif /* __cplusplus */

#endif 	/* __RegControls_FWD_DEFINED__ */


#ifndef __Topology_FWD_DEFINED__
#define __Topology_FWD_DEFINED__

#ifdef __cplusplus
typedef class Topology Topology;
#else
typedef struct Topology Topology;
#endif /* __cplusplus */

#endif 	/* __Topology_FWD_DEFINED__ */


#ifndef __DSS_Executive_FWD_DEFINED__
#define __DSS_Executive_FWD_DEFINED__

#ifdef __cplusplus
typedef class DSS_Executive DSS_Executive;
#else
typedef struct DSS_Executive DSS_Executive;
#endif /* __cplusplus */

#endif 	/* __DSS_Executive_FWD_DEFINED__ */


#ifndef __Sensors_FWD_DEFINED__
#define __Sensors_FWD_DEFINED__

#ifdef __cplusplus
typedef class Sensors Sensors;
#else
typedef struct Sensors Sensors;
#endif /* __cplusplus */

#endif 	/* __Sensors_FWD_DEFINED__ */


#ifndef __XYCurves_FWD_DEFINED__
#define __XYCurves_FWD_DEFINED__

#ifdef __cplusplus
typedef class XYCurves XYCurves;
#else
typedef struct XYCurves XYCurves;
#endif /* __cplusplus */

#endif 	/* __XYCurves_FWD_DEFINED__ */


#ifndef __PDElements_FWD_DEFINED__
#define __PDElements_FWD_DEFINED__

#ifdef __cplusplus
typedef class PDElements PDElements;
#else
typedef struct PDElements PDElements;
#endif /* __cplusplus */

#endif 	/* __PDElements_FWD_DEFINED__ */


#ifndef __Reclosers_FWD_DEFINED__
#define __Reclosers_FWD_DEFINED__

#ifdef __cplusplus
typedef class Reclosers Reclosers;
#else
typedef struct Reclosers Reclosers;
#endif /* __cplusplus */

#endif 	/* __Reclosers_FWD_DEFINED__ */


#ifndef __Relays_FWD_DEFINED__
#define __Relays_FWD_DEFINED__

#ifdef __cplusplus
typedef class Relays Relays;
#else
typedef struct Relays Relays;
#endif /* __cplusplus */

#endif 	/* __Relays_FWD_DEFINED__ */


#ifndef __CmathLib_FWD_DEFINED__
#define __CmathLib_FWD_DEFINED__

#ifdef __cplusplus
typedef class CmathLib CmathLib;
#else
typedef struct CmathLib CmathLib;
#endif /* __cplusplus */

#endif 	/* __CmathLib_FWD_DEFINED__ */


#ifndef __Parser_FWD_DEFINED__
#define __Parser_FWD_DEFINED__

#ifdef __cplusplus
typedef class Parser Parser;
#else
typedef struct Parser Parser;
#endif /* __cplusplus */

#endif 	/* __Parser_FWD_DEFINED__ */


#ifndef __LoadShapes_FWD_DEFINED__
#define __LoadShapes_FWD_DEFINED__

#ifdef __cplusplus
typedef class LoadShapes LoadShapes;
#else
typedef struct LoadShapes LoadShapes;
#endif /* __cplusplus */

#endif 	/* __LoadShapes_FWD_DEFINED__ */


#ifndef __Fuses_FWD_DEFINED__
#define __Fuses_FWD_DEFINED__

#ifdef __cplusplus
typedef class Fuses Fuses;
#else
typedef struct Fuses Fuses;
#endif /* __cplusplus */

#endif 	/* __Fuses_FWD_DEFINED__ */


#ifndef __ISources_FWD_DEFINED__
#define __ISources_FWD_DEFINED__

#ifdef __cplusplus
typedef class ISources ISources;
#else
typedef struct ISources ISources;
#endif /* __cplusplus */

#endif 	/* __ISources_FWD_DEFINED__ */


#ifndef __DSSimComs_FWD_DEFINED__
#define __DSSimComs_FWD_DEFINED__

#ifdef __cplusplus
typedef class DSSimComs DSSimComs;
#else
typedef struct DSSimComs DSSimComs;
#endif /* __cplusplus */

#endif 	/* __DSSimComs_FWD_DEFINED__ */


#ifndef __PVSystems_FWD_DEFINED__
#define __PVSystems_FWD_DEFINED__

#ifdef __cplusplus
typedef class PVSystems PVSystems;
#else
typedef struct PVSystems PVSystems;
#endif /* __cplusplus */

#endif 	/* __PVSystems_FWD_DEFINED__ */


#ifndef __Vsources_FWD_DEFINED__
#define __Vsources_FWD_DEFINED__

#ifdef __cplusplus
typedef class Vsources Vsources;
#else
typedef struct Vsources Vsources;
#endif /* __cplusplus */

#endif 	/* __Vsources_FWD_DEFINED__ */


#ifndef __Parallel_FWD_DEFINED__
#define __Parallel_FWD_DEFINED__

#ifdef __cplusplus
typedef class Parallel Parallel;
#else
typedef struct Parallel Parallel;
#endif /* __cplusplus */

#endif 	/* __Parallel_FWD_DEFINED__ */


#ifndef __LineCodes_FWD_DEFINED__
#define __LineCodes_FWD_DEFINED__

#ifdef __cplusplus
typedef class LineCodes LineCodes;
#else
typedef struct LineCodes LineCodes;
#endif /* __cplusplus */

#endif 	/* __LineCodes_FWD_DEFINED__ */


#ifndef __GICSources_FWD_DEFINED__
#define __GICSources_FWD_DEFINED__

#ifdef __cplusplus
typedef class GICSources GICSources;
#else
typedef struct GICSources GICSources;
#endif /* __cplusplus */

#endif 	/* __GICSources_FWD_DEFINED__ */


#ifndef __ReduceCkt_FWD_DEFINED__
#define __ReduceCkt_FWD_DEFINED__

#ifdef __cplusplus
typedef class ReduceCkt ReduceCkt;
#else
typedef struct ReduceCkt ReduceCkt;
#endif /* __cplusplus */

#endif 	/* __ReduceCkt_FWD_DEFINED__ */


#ifndef __Storages_FWD_DEFINED__
#define __Storages_FWD_DEFINED__

#ifdef __cplusplus
typedef class Storages Storages;
#else
typedef struct Storages Storages;
#endif /* __cplusplus */

#endif 	/* __Storages_FWD_DEFINED__ */


#ifndef __WindGens_FWD_DEFINED__
#define __WindGens_FWD_DEFINED__

#ifdef __cplusplus
typedef class WindGens WindGens;
#else
typedef struct WindGens WindGens;
#endif /* __cplusplus */

#endif 	/* __WindGens_FWD_DEFINED__ */


#ifndef __Reactors_FWD_DEFINED__
#define __Reactors_FWD_DEFINED__

#ifdef __cplusplus
typedef class Reactors Reactors;
#else
typedef struct Reactors Reactors;
#endif /* __cplusplus */

#endif 	/* __Reactors_FWD_DEFINED__ */


#ifndef __ZIP_FWD_DEFINED__
#define __ZIP_FWD_DEFINED__

#ifdef __cplusplus
typedef class ZIP ZIP;
#else
typedef struct ZIP ZIP;
#endif /* __cplusplus */

#endif 	/* __ZIP_FWD_DEFINED__ */


#ifndef __YMatrix_FWD_DEFINED__
#define __YMatrix_FWD_DEFINED__

#ifdef __cplusplus
typedef class YMatrix YMatrix;
#else
typedef struct YMatrix YMatrix;
#endif /* __cplusplus */

#endif 	/* __YMatrix_FWD_DEFINED__ */


#ifdef __cplusplus
extern "C"{
#endif 



#ifndef __DSSExtensions_LIBRARY_DEFINED__
#define __DSSExtensions_LIBRARY_DEFINED__

/* library DSSExtensions */
/* [helpstring][version][uuid] */ 
















































typedef /* [public][helpstring][version][uuid] */  DECLSPEC_UUID("C924BE2C-18C2-11F0-A417-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0001
    {
        dssVI	= 0,
        dssPower	= 1,
        dssSequence	= 16,
        dssMagnitude	= 32,
        dssPosOnly	= 64,
        dssTaps	= 2,
        dssStates	= 3
    } 	MonitorModes;

typedef /* [public][version][uuid] */  DECLSPEC_UUID("C924BE40-18C2-11F0-A417-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0002
    {
        dssSnapShot	= 0,
        dssDutyCycle	= 6,
        dssDirect	= 7,
        dssDaily	= 1,
        dssMonte1	= 3,
        dssMonte2	= 10,
        dssMonte3	= 11,
        dssFaultStudy	= 9,
        dssYearly	= 2,
        dssMonteFault	= 8,
        dssPeakDay	= 5,
        dssLD1	= 4,
        dssLD2	= 12,
        dssAutoAdd	= 13,
        dssHarmonic	= 15,
        dssDynamic	= 14
    } 	SolveModes;

typedef /* [public][version][uuid] */  DECLSPEC_UUID("C924BE54-18C2-11F0-A417-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0003
    {
        dssPowerFlow	= 1,
        dssAdmittance	= 2,
        dssNormalSolve	= 0,
        dssNewtonSolve	= 1,
        dssStatic	= 0,
        dssEvent	= 1,
        dssTime	= 2,
        dssMultiphase	= 0,
        dssPositiveSeq	= 1,
        dssGaussian	= 1,
        dssUniform	= 2,
        dssLogNormal	= 3,
        dssAddGen	= 1,
        dssAddCap	= 2,
        dssControlOFF	= 0xffffffff
    } 	Options;

typedef /* [public][public][public][version][uuid] */  DECLSPEC_UUID("C924BE68-18C2-11F0-A417-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0004
    {
        dssCapControlVoltage	= 1,
        dssCapControlKVAR	= 2,
        dssCapControlCurrent	= 0,
        dssCapControlPF	= 4,
        dssCapControlTime	= 3
    } 	CapControlModes;

typedef /* [public][public][public][public][public][public][public][public][public][public][public][public][public][public][public][version][uuid] */  DECLSPEC_UUID("C924BE7C-18C2-11F0-A417-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0005
    {
        dssActionNone	= 0,
        dssActionOpen	= 1,
        dssActionClose	= 2,
        dssActionReset	= 3,
        dssActionLock	= 4,
        dssActionUnlock	= 5,
        dssActionTapUp	= 6,
        dssActionTapDown	= 7
    } 	ActionCodes;

typedef /* [public][public][public][public][public][uuid] */  DECLSPEC_UUID("C924BE90-18C2-11F0-A417-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0006
    {
        dssLoadVariable	= 0,
        dssLoadFixed	= 1,
        dssLoadExempt	= 2
    } 	LoadStatus;

typedef /* [public][public][public][uuid] */  DECLSPEC_UUID("C924BEA4-18C2-11F0-A417-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0007
    {
        dssLoadConstPQ	= 1,
        dssLoadConstZ	= 2,
        dssLoadMotor	= 3,
        dssLoadCVR	= 4,
        dssLoadConstI	= 5,
        dssLoadConstPFixedQ	= 6,
        dssLoadConstPFixedX	= 7,
        dssLoadZIPV	= 8
    } 	LoadModels;

typedef /* [public][uuid] */  DECLSPEC_UUID("C924BEB8-18C2-11F0-A417-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0008
    {
        dssLineUnitsNone	= 0,
        dssLineUnitsMiles	= 1,
        dssLineUnitskFt	= 2,
        dssLineUnitskm	= 3,
        dssLineUnitsmeter	= 4,
        dssLineUnitsft	= 5,
        dssLineUnitsinch	= 6,
        dssLineUnitscm	= 7,
        dssLineUnitsmm	= 8,
        dssLineUnitsMaxnum	= 9
    } 	LineUnits;

typedef /* [public][public][helpstring][uuid] */  DECLSPEC_UUID("04968192-1D6A-11F0-BC79-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0009
    {
        dssModernStyle	= 0,
        dssLowercaseStyle	= 1,
        dssLegacyStyle	= 2
    } 	DSSPropertyNameStyle;

typedef /* [public][helpstring][uuid] */  DECLSPEC_UUID("8150D456-1D8F-11F0-9029-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0010
    {
        dssCompatNoSolverFloatChecks	= 0x1,
        dssCompatBadPrecision	= 0x2,
        dssCompatInvControl9611	= 0x4,
        dssCompatSaveCalcVoltageBases	= 0x8,
        dssCompatActiveLine	= 0x10,
        dssCompatNoPropertyTracking	= 0x20,
        dssCompatSkipSideEffects	= 0x40,
        dssCompatMonitorHeader	= 0x80,
        dssCompatInvControlDeltaV	= 0x100,
        dssCompatPermissiveProperties	= 0x200
    } 	DSSCompatFlags;

typedef /* [public][helpstring][uuid] */  DECLSPEC_UUID("3D5FA0C8-1D90-11F0-AF38-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0011
    {
        dssSaveCalcVoltageBases	= 0x1,
        dssSaveSetVoltageBases	= 0x2,
        dssSaveIncludeOptions	= 0x4,
        dssSaveIncludeDisabled	= 0x8,
        dssSaveExcludeDefault	= 0x10,
        dssSaveSingleFile	= 0x20,
        dssSaveKeepOrder	= 0x40,
        dssSaveExcludeMeterZones	= 0x80,
        dssSaveIsOpen	= 0x100,
        dssSaveToString	= 0x200
    } 	DSSSaveFlags;

typedef /* [public][helpstring][uuid] */  DECLSPEC_UUID("2593AFE6-1D97-11F0-9EE5-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0012
    {
        dssJSONFull	= 0x1,
        dssJSONSkipRedundant	= 0x2,
        dssJSONEnumAsInt	= 0x4,
        dssJSONFullNames	= 0x8,
        dssJSONPretty	= 0x10,
        dssJSONExcludeDisabled	= 0x20,
        dssJSONIncludeDSSClass	= 0x40,
        dssJSONLowercaseKeys	= 0x80,
        dssJSONIncludeDefaultObjs	= 0x100,
        dssJSONSkipTimestamp	= 0x200,
        dssJSONSkipBuses	= 0x400
    } 	DSSJSONFlags;

typedef /* [public][helpstring][uuid] */  DECLSPEC_UUID("2D752EB0-1D97-11F0-9EE5-C87F5452571C") 
enum __MIDL___MIDL_itf_DSSExtensions_0000_0000_0013
    {
        dssSolverReuseNothing	= 0,
        dssSolverReuseCompressedMatrix	= 1,
        dssSolverReuseSymbolicFactorization	= 2,
        dssSolverReuseNumericFactorization	= 3,
        dssSolverAlwaysResetYPrimInvalid	= 0x10000000
    } 	SparseSolverOptions;


EXTERN_C const IID LIBID_DSSExtensions;

#ifndef __IText_INTERFACE_DEFINED__
#define __IText_INTERFACE_DEFINED__

/* interface IText */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IText;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B15C-18C2-11F0-A417-C87F5452571C")
    IText : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Command( 
            /* [retval][out] */ BSTR *Command) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Command( 
            /* [in] */ BSTR Command) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Result( 
            /* [retval][out] */ BSTR *Result) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ITextVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IText * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IText * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IText * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IText * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IText * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IText * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IText * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IText, get_Command)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Command )( 
            IText * This,
            /* [retval][out] */ BSTR *Command);
        
        DECLSPEC_XFGVIRT(IText, put_Command)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Command )( 
            IText * This,
            /* [in] */ BSTR Command);
        
        DECLSPEC_XFGVIRT(IText, get_Result)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Result )( 
            IText * This,
            /* [retval][out] */ BSTR *Result);
        
        END_INTERFACE
    } ITextVtbl;

    interface IText
    {
        CONST_VTBL struct ITextVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IText_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IText_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IText_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IText_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IText_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IText_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IText_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IText_get_Command(This,Command)	\
    ( (This)->lpVtbl -> get_Command(This,Command) ) 

#define IText_put_Command(This,Command)	\
    ( (This)->lpVtbl -> put_Command(This,Command) ) 

#define IText_get_Result(This,Result)	\
    ( (This)->lpVtbl -> get_Result(This,Result) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IText_INTERFACE_DEFINED__ */


#ifndef __IDSSProperty_INTERFACE_DEFINED__
#define __IDSSProperty_INTERFACE_DEFINED__

/* interface IDSSProperty */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IDSSProperty;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B1B6-18C2-11F0-A417-C87F5452571C")
    IDSSProperty : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Description( 
            /* [retval][out] */ BSTR *Description) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Val( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Val( 
            /* [in] */ BSTR Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IDSSPropertyVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IDSSProperty * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IDSSProperty * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IDSSProperty * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IDSSProperty * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IDSSProperty * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IDSSProperty * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IDSSProperty * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IDSSProperty, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IDSSProperty * This,
            /* [retval][out] */ BSTR *Name);
        
        DECLSPEC_XFGVIRT(IDSSProperty, get_Description)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Description )( 
            IDSSProperty * This,
            /* [retval][out] */ BSTR *Description);
        
        DECLSPEC_XFGVIRT(IDSSProperty, get_Val)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Val )( 
            IDSSProperty * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IDSSProperty, put_Val)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Val )( 
            IDSSProperty * This,
            /* [in] */ BSTR Value);
        
        END_INTERFACE
    } IDSSPropertyVtbl;

    interface IDSSProperty
    {
        CONST_VTBL struct IDSSPropertyVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IDSSProperty_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IDSSProperty_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IDSSProperty_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IDSSProperty_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IDSSProperty_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IDSSProperty_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IDSSProperty_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IDSSProperty_get_Name(This,Name)	\
    ( (This)->lpVtbl -> get_Name(This,Name) ) 

#define IDSSProperty_get_Description(This,Description)	\
    ( (This)->lpVtbl -> get_Description(This,Description) ) 

#define IDSSProperty_get_Val(This,Value)	\
    ( (This)->lpVtbl -> get_Val(This,Value) ) 

#define IDSSProperty_put_Val(This,Value)	\
    ( (This)->lpVtbl -> put_Val(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IDSSProperty_INTERFACE_DEFINED__ */


#ifndef __ICktElement_INTERFACE_DEFINED__
#define __ICktElement_INTERFACE_DEFINED__

/* interface ICktElement */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ICktElement;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B206-18C2-11F0-A417-C87F5452571C")
    ICktElement : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumTerminals( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumConductors( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumPhases( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_BusNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_BusNames( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Properties( 
            /* [in] */ VARIANT Indx,
            /* [retval][out] */ IDSSProperty **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Voltages( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Currents( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Powers( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Losses( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PhaseLosses( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SeqVoltages( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SeqCurrents( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SeqPowers( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Enabled( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Enabled( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NormalAmps( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NormalAmps( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EmergAmps( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EmergAmps( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Open( 
            /* [in] */ long Term,
            /* [in] */ long Phs) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Close( 
            /* [in] */ long Term,
            /* [in] */ long Phs) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IsOpen( 
            /* [in] */ long Term,
            /* [in] */ long Phs,
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumProperties( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllPropertyNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Residuals( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Yprim( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DisplayName( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_DisplayName( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Handle( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_GUID( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_HasSwitchControl( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_HasVoltControl( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EnergyMeter( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Controller( 
            /* [in] */ long idx,
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CplxSeqVoltages( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CplxSeqCurrents( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllVariableNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllVariableValues( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Variable( 
            /* [in] */ BSTR MyVarName,
            /* [out] */ long *Code,
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Variablei( 
            /* [in] */ long idx,
            /* [out] */ long *Code,
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NodeOrder( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_HasOCPDevice( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumControls( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_OCPDevIndex( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_OCPDevType( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CurrentsMagAng( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VoltagesMagAng( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TotalPowers( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VariableByName( 
            /* [in] */ BSTR MyVarName,
            /* [out] */ long *Code,
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VariableByName( 
            /* [in] */ BSTR MyVarName,
            /* [out] */ long *Code,
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VariableByIndex( 
            /* [in] */ long idx,
            /* [out] */ long *Code,
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VariableByIndex( 
            /* [in] */ long idx,
            /* [out] */ long *Code,
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VariableName( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VariableName( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VariableValue( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VariableValue( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VariableIdx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VariableIdx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsIsolated( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NodeRef( 
            /* [retval][out] */ VARIANT *Nodes) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ICktElementVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ICktElement * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ICktElement * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ICktElement * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ICktElement * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ICktElement * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ICktElement * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ICktElement * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ICktElement * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_NumTerminals)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumTerminals )( 
            ICktElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_NumConductors)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumConductors )( 
            ICktElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_NumPhases)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumPhases )( 
            ICktElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_BusNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_BusNames )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_BusNames)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_BusNames )( 
            ICktElement * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Properties)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Properties )( 
            ICktElement * This,
            /* [in] */ VARIANT Indx,
            /* [retval][out] */ IDSSProperty **Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Voltages)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Voltages )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Currents)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Currents )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Powers)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Powers )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Losses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Losses )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_PhaseLosses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PhaseLosses )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_SeqVoltages)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SeqVoltages )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_SeqCurrents)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SeqCurrents )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_SeqPowers)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SeqPowers )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Enabled)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Enabled )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_Enabled)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Enabled )( 
            ICktElement * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_NormalAmps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NormalAmps )( 
            ICktElement * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_NormalAmps)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NormalAmps )( 
            ICktElement * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_EmergAmps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EmergAmps )( 
            ICktElement * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_EmergAmps)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EmergAmps )( 
            ICktElement * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICktElement, Open)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Open )( 
            ICktElement * This,
            /* [in] */ long Term,
            /* [in] */ long Phs);
        
        DECLSPEC_XFGVIRT(ICktElement, Close)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Close )( 
            ICktElement * This,
            /* [in] */ long Term,
            /* [in] */ long Phs);
        
        DECLSPEC_XFGVIRT(ICktElement, IsOpen)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *IsOpen )( 
            ICktElement * This,
            /* [in] */ long Term,
            /* [in] */ long Phs,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_NumProperties)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumProperties )( 
            ICktElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_AllPropertyNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllPropertyNames )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Residuals)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Residuals )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Yprim)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Yprim )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_DisplayName)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DisplayName )( 
            ICktElement * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_DisplayName)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_DisplayName )( 
            ICktElement * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Handle)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Handle )( 
            ICktElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_GUID)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_GUID )( 
            ICktElement * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_HasSwitchControl)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_HasSwitchControl )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_HasVoltControl)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_HasVoltControl )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_EnergyMeter)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EnergyMeter )( 
            ICktElement * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Controller)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Controller )( 
            ICktElement * This,
            /* [in] */ long idx,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_CplxSeqVoltages)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CplxSeqVoltages )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_CplxSeqCurrents)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CplxSeqCurrents )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_AllVariableNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllVariableNames )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_AllVariableValues)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllVariableValues )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Variable)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Variable )( 
            ICktElement * This,
            /* [in] */ BSTR MyVarName,
            /* [out] */ long *Code,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_Variablei)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Variablei )( 
            ICktElement * This,
            /* [in] */ long idx,
            /* [out] */ long *Code,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_NodeOrder)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NodeOrder )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_HasOCPDevice)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_HasOCPDevice )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_NumControls)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumControls )( 
            ICktElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_OCPDevIndex)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_OCPDevIndex )( 
            ICktElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_OCPDevType)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_OCPDevType )( 
            ICktElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_CurrentsMagAng)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CurrentsMagAng )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_VoltagesMagAng)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VoltagesMagAng )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_TotalPowers)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TotalPowers )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_VariableByName)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VariableByName )( 
            ICktElement * This,
            /* [in] */ BSTR MyVarName,
            /* [out] */ long *Code,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_VariableByName)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VariableByName )( 
            ICktElement * This,
            /* [in] */ BSTR MyVarName,
            /* [out] */ long *Code,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_VariableByIndex)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VariableByIndex )( 
            ICktElement * This,
            /* [in] */ long idx,
            /* [out] */ long *Code,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_VariableByIndex)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VariableByIndex )( 
            ICktElement * This,
            /* [in] */ long idx,
            /* [out] */ long *Code,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_VariableName)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VariableName )( 
            ICktElement * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_VariableName)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VariableName )( 
            ICktElement * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_VariableValue)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VariableValue )( 
            ICktElement * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_VariableValue)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VariableValue )( 
            ICktElement * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_VariableIdx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VariableIdx )( 
            ICktElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, put_VariableIdx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VariableIdx )( 
            ICktElement * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_IsIsolated)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsIsolated )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICktElement, get_NodeRef)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NodeRef )( 
            ICktElement * This,
            /* [retval][out] */ VARIANT *Nodes);
        
        END_INTERFACE
    } ICktElementVtbl;

    interface ICktElement
    {
        CONST_VTBL struct ICktElementVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ICktElement_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ICktElement_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ICktElement_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ICktElement_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ICktElement_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ICktElement_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ICktElement_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ICktElement_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ICktElement_get_NumTerminals(This,Value)	\
    ( (This)->lpVtbl -> get_NumTerminals(This,Value) ) 

#define ICktElement_get_NumConductors(This,Value)	\
    ( (This)->lpVtbl -> get_NumConductors(This,Value) ) 

#define ICktElement_get_NumPhases(This,Value)	\
    ( (This)->lpVtbl -> get_NumPhases(This,Value) ) 

#define ICktElement_get_BusNames(This,Value)	\
    ( (This)->lpVtbl -> get_BusNames(This,Value) ) 

#define ICktElement_put_BusNames(This,Value)	\
    ( (This)->lpVtbl -> put_BusNames(This,Value) ) 

#define ICktElement_get_Properties(This,Indx,Value)	\
    ( (This)->lpVtbl -> get_Properties(This,Indx,Value) ) 

#define ICktElement_get_Voltages(This,Value)	\
    ( (This)->lpVtbl -> get_Voltages(This,Value) ) 

#define ICktElement_get_Currents(This,Value)	\
    ( (This)->lpVtbl -> get_Currents(This,Value) ) 

#define ICktElement_get_Powers(This,Value)	\
    ( (This)->lpVtbl -> get_Powers(This,Value) ) 

#define ICktElement_get_Losses(This,Value)	\
    ( (This)->lpVtbl -> get_Losses(This,Value) ) 

#define ICktElement_get_PhaseLosses(This,Value)	\
    ( (This)->lpVtbl -> get_PhaseLosses(This,Value) ) 

#define ICktElement_get_SeqVoltages(This,Value)	\
    ( (This)->lpVtbl -> get_SeqVoltages(This,Value) ) 

#define ICktElement_get_SeqCurrents(This,Value)	\
    ( (This)->lpVtbl -> get_SeqCurrents(This,Value) ) 

#define ICktElement_get_SeqPowers(This,Value)	\
    ( (This)->lpVtbl -> get_SeqPowers(This,Value) ) 

#define ICktElement_get_Enabled(This,Value)	\
    ( (This)->lpVtbl -> get_Enabled(This,Value) ) 

#define ICktElement_put_Enabled(This,Value)	\
    ( (This)->lpVtbl -> put_Enabled(This,Value) ) 

#define ICktElement_get_NormalAmps(This,Value)	\
    ( (This)->lpVtbl -> get_NormalAmps(This,Value) ) 

#define ICktElement_put_NormalAmps(This,Value)	\
    ( (This)->lpVtbl -> put_NormalAmps(This,Value) ) 

#define ICktElement_get_EmergAmps(This,Value)	\
    ( (This)->lpVtbl -> get_EmergAmps(This,Value) ) 

#define ICktElement_put_EmergAmps(This,Value)	\
    ( (This)->lpVtbl -> put_EmergAmps(This,Value) ) 

#define ICktElement_Open(This,Term,Phs)	\
    ( (This)->lpVtbl -> Open(This,Term,Phs) ) 

#define ICktElement_Close(This,Term,Phs)	\
    ( (This)->lpVtbl -> Close(This,Term,Phs) ) 

#define ICktElement_IsOpen(This,Term,Phs,Value)	\
    ( (This)->lpVtbl -> IsOpen(This,Term,Phs,Value) ) 

#define ICktElement_get_NumProperties(This,Value)	\
    ( (This)->lpVtbl -> get_NumProperties(This,Value) ) 

#define ICktElement_get_AllPropertyNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllPropertyNames(This,Value) ) 

#define ICktElement_get_Residuals(This,Value)	\
    ( (This)->lpVtbl -> get_Residuals(This,Value) ) 

#define ICktElement_get_Yprim(This,Value)	\
    ( (This)->lpVtbl -> get_Yprim(This,Value) ) 

#define ICktElement_get_DisplayName(This,Value)	\
    ( (This)->lpVtbl -> get_DisplayName(This,Value) ) 

#define ICktElement_put_DisplayName(This,Value)	\
    ( (This)->lpVtbl -> put_DisplayName(This,Value) ) 

#define ICktElement_get_Handle(This,Value)	\
    ( (This)->lpVtbl -> get_Handle(This,Value) ) 

#define ICktElement_get_GUID(This,Value)	\
    ( (This)->lpVtbl -> get_GUID(This,Value) ) 

#define ICktElement_get_HasSwitchControl(This,Value)	\
    ( (This)->lpVtbl -> get_HasSwitchControl(This,Value) ) 

#define ICktElement_get_HasVoltControl(This,Value)	\
    ( (This)->lpVtbl -> get_HasVoltControl(This,Value) ) 

#define ICktElement_get_EnergyMeter(This,Value)	\
    ( (This)->lpVtbl -> get_EnergyMeter(This,Value) ) 

#define ICktElement_get_Controller(This,idx,Value)	\
    ( (This)->lpVtbl -> get_Controller(This,idx,Value) ) 

#define ICktElement_get_CplxSeqVoltages(This,Value)	\
    ( (This)->lpVtbl -> get_CplxSeqVoltages(This,Value) ) 

#define ICktElement_get_CplxSeqCurrents(This,Value)	\
    ( (This)->lpVtbl -> get_CplxSeqCurrents(This,Value) ) 

#define ICktElement_get_AllVariableNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllVariableNames(This,Value) ) 

#define ICktElement_get_AllVariableValues(This,Value)	\
    ( (This)->lpVtbl -> get_AllVariableValues(This,Value) ) 

#define ICktElement_get_Variable(This,MyVarName,Code,Value)	\
    ( (This)->lpVtbl -> get_Variable(This,MyVarName,Code,Value) ) 

#define ICktElement_get_Variablei(This,idx,Code,Value)	\
    ( (This)->lpVtbl -> get_Variablei(This,idx,Code,Value) ) 

#define ICktElement_get_NodeOrder(This,Value)	\
    ( (This)->lpVtbl -> get_NodeOrder(This,Value) ) 

#define ICktElement_get_HasOCPDevice(This,Value)	\
    ( (This)->lpVtbl -> get_HasOCPDevice(This,Value) ) 

#define ICktElement_get_NumControls(This,Value)	\
    ( (This)->lpVtbl -> get_NumControls(This,Value) ) 

#define ICktElement_get_OCPDevIndex(This,Value)	\
    ( (This)->lpVtbl -> get_OCPDevIndex(This,Value) ) 

#define ICktElement_get_OCPDevType(This,Value)	\
    ( (This)->lpVtbl -> get_OCPDevType(This,Value) ) 

#define ICktElement_get_CurrentsMagAng(This,Value)	\
    ( (This)->lpVtbl -> get_CurrentsMagAng(This,Value) ) 

#define ICktElement_get_VoltagesMagAng(This,Value)	\
    ( (This)->lpVtbl -> get_VoltagesMagAng(This,Value) ) 

#define ICktElement_get_TotalPowers(This,Value)	\
    ( (This)->lpVtbl -> get_TotalPowers(This,Value) ) 

#define ICktElement_get_VariableByName(This,MyVarName,Code,Value)	\
    ( (This)->lpVtbl -> get_VariableByName(This,MyVarName,Code,Value) ) 

#define ICktElement_put_VariableByName(This,MyVarName,Code,Value)	\
    ( (This)->lpVtbl -> put_VariableByName(This,MyVarName,Code,Value) ) 

#define ICktElement_get_VariableByIndex(This,idx,Code,Value)	\
    ( (This)->lpVtbl -> get_VariableByIndex(This,idx,Code,Value) ) 

#define ICktElement_put_VariableByIndex(This,idx,Code,Value)	\
    ( (This)->lpVtbl -> put_VariableByIndex(This,idx,Code,Value) ) 

#define ICktElement_get_VariableName(This,Value)	\
    ( (This)->lpVtbl -> get_VariableName(This,Value) ) 

#define ICktElement_put_VariableName(This,Value)	\
    ( (This)->lpVtbl -> put_VariableName(This,Value) ) 

#define ICktElement_get_VariableValue(This,Value)	\
    ( (This)->lpVtbl -> get_VariableValue(This,Value) ) 

#define ICktElement_put_VariableValue(This,Value)	\
    ( (This)->lpVtbl -> put_VariableValue(This,Value) ) 

#define ICktElement_get_VariableIdx(This,Value)	\
    ( (This)->lpVtbl -> get_VariableIdx(This,Value) ) 

#define ICktElement_put_VariableIdx(This,Value)	\
    ( (This)->lpVtbl -> put_VariableIdx(This,Value) ) 

#define ICktElement_get_IsIsolated(This,Value)	\
    ( (This)->lpVtbl -> get_IsIsolated(This,Value) ) 

#define ICktElement_get_NodeRef(This,Nodes)	\
    ( (This)->lpVtbl -> get_NodeRef(This,Nodes) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ICktElement_INTERFACE_DEFINED__ */


#ifndef __IError_INTERFACE_DEFINED__
#define __IError_INTERFACE_DEFINED__

/* interface IError */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IError;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B26A-18C2-11F0-A417-C87F5452571C")
    IError : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Number( 
            /* [retval][out] */ long *Number) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Description( 
            /* [retval][out] */ BSTR *Description) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EarlyAbort( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EarlyAbort( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_UseExceptions( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_UseExceptions( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IErrorVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IError * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IError * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IError * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IError * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IError * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IError * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IError * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IError, get_Number)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Number )( 
            IError * This,
            /* [retval][out] */ long *Number);
        
        DECLSPEC_XFGVIRT(IError, get_Description)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Description )( 
            IError * This,
            /* [retval][out] */ BSTR *Description);
        
        DECLSPEC_XFGVIRT(IError, get_EarlyAbort)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EarlyAbort )( 
            IError * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IError, put_EarlyAbort)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EarlyAbort )( 
            IError * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IError, get_UseExceptions)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_UseExceptions )( 
            IError * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IError, put_UseExceptions)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_UseExceptions )( 
            IError * This,
            /* [in] */ VARIANT_BOOL Value);
        
        END_INTERFACE
    } IErrorVtbl;

    interface IError
    {
        CONST_VTBL struct IErrorVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IError_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IError_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IError_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IError_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IError_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IError_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IError_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IError_get_Number(This,Number)	\
    ( (This)->lpVtbl -> get_Number(This,Number) ) 

#define IError_get_Description(This,Description)	\
    ( (This)->lpVtbl -> get_Description(This,Description) ) 

#define IError_get_EarlyAbort(This,Value)	\
    ( (This)->lpVtbl -> get_EarlyAbort(This,Value) ) 

#define IError_put_EarlyAbort(This,Value)	\
    ( (This)->lpVtbl -> put_EarlyAbort(This,Value) ) 

#define IError_get_UseExceptions(This,Value)	\
    ( (This)->lpVtbl -> get_UseExceptions(This,Value) ) 

#define IError_put_UseExceptions(This,Value)	\
    ( (This)->lpVtbl -> put_UseExceptions(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IError_INTERFACE_DEFINED__ */


#ifndef __ICircuit_INTERFACE_DEFINED__
#define __ICircuit_INTERFACE_DEFINED__

/* interface ICircuit */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ICircuit;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B2BA-18C2-11F0-A417-C87F5452571C")
    ICircuit : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumCktElements( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumBuses( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumNodes( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Buses( 
            /* [in] */ VARIANT Index,
            /* [retval][out] */ IBus **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CktElements( 
            /* [in] */ VARIANT idx,
            /* [retval][out] */ ICktElement **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Losses( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LineLosses( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SubstationLosses( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TotalPower( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllBusVolts( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllBusVmag( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllElementNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveElement( 
            /* [retval][out] */ ICktElement **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Disable( 
            /* [in] */ BSTR myName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Enable( 
            /* [in] */ BSTR myName) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Solution( 
            /* [retval][out] */ ISolution **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveBus( 
            /* [retval][out] */ IBus **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FirstPCElement( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE NextPCElement( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FirstPDElement( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE NextPDElement( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllBusNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllElementLosses( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Sample( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SaveSample( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Monitors( 
            /* [retval][out] */ IMonitors **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Meters( 
            /* [retval][out] */ IMeters **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Generators( 
            /* [retval][out] */ IGenerators **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Settings( 
            /* [retval][out] */ ISettings **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Lines( 
            /* [retval][out] */ ILines **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetActiveElement( 
            /* [in] */ BSTR FullName,
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE Capacity( 
            /* [in] */ double Start,
            /* [in] */ double Increment,
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetActiveBus( 
            /* [in] */ BSTR BusName,
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetActiveBusi( 
            /* [in] */ long BusIndex,
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllBusVmagPu( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNodeNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SystemY( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CtrlQueue( 
            /* [retval][out] */ ICtrlQueue **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllBusDistances( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNodeDistances( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNodeVmagByPhase( 
            /* [in] */ long Phase,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNodeVmagPUByPhase( 
            /* [in] */ long Phase,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNodeDistancesByPhase( 
            /* [in] */ long Phase,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNodeNamesByPhase( 
            /* [in] */ long Phase,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Loads( 
            /* [retval][out] */ ILoads **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FirstElement( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE NextElement( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetActiveClass( 
            /* [in] */ BSTR ClassName,
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveDSSElement( 
            /* [retval][out] */ IDSSElement **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveCktElement( 
            /* [retval][out] */ ICktElement **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveClass( 
            /* [retval][out] */ IActiveClass **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Transformers( 
            /* [retval][out] */ ITransformers **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SwtControls( 
            /* [retval][out] */ ISwtControls **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CapControls( 
            /* [retval][out] */ ICapControls **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegControls( 
            /* [retval][out] */ IRegControls **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Capacitors( 
            /* [retval][out] */ ICapacitors **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Topology( 
            /* [retval][out] */ ITopology **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Sensors( 
            /* [retval][out] */ ISensors **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE UpdateStorage( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ParentPDElement( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_XYCurves( 
            /* [retval][out] */ IXYCurves **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PDElements( 
            /* [retval][out] */ IPDElements **Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Reclosers( 
            /* [retval][out] */ IReclosers **Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Relays( 
            /* [retval][out] */ IRelays **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LoadShapes( 
            /* [retval][out] */ ILoadShapes **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Fuses( 
            /* [retval][out] */ IFuses **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ISources( 
            /* [retval][out] */ IISources **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_YNodeVarray( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE EndOfTimeStepUpdate( void) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_DSSim_Coms( 
            /* [retval][out] */ IDSSimComs **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_YNodeOrder( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_YCurrents( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PVSystems( 
            /* [retval][out] */ IPVSystems **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vsources( 
            /* [retval][out] */ IVsources **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Parallel( 
            /* [retval][out] */ IParallel **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LineCodes( 
            /* [retval][out] */ ILineCodes **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_GICSources( 
            /* [retval][out] */ IGICSources **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ReduceCkt( 
            /* [retval][out] */ IReduceCkt **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Storages( 
            /* [retval][out] */ IStorages **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_WindGens( 
            /* [retval][out] */ IWindGens **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Reactors( 
            /* [retval][out] */ IReactors **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ElementLosses( 
            /* [in] */ VARIANT Elements,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ToJSON( 
            /* [defaultvalue][in] */ long options,
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FromJSON( 
            /* [in] */ BSTR data,
            /* [in] */ long options) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Save( 
            /* [in] */ BSTR dirOrFilePath,
            /* [defaultvalue][in] */ long saveFlags,
            /* [retval][out] */ BSTR *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ICircuitVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ICircuit * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ICircuit * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ICircuit * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ICircuit * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ICircuit * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ICircuit * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ICircuit * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ICircuit * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_NumCktElements)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumCktElements )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_NumBuses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumBuses )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_NumNodes)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumNodes )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Buses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Buses )( 
            ICircuit * This,
            /* [in] */ VARIANT Index,
            /* [retval][out] */ IBus **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_CktElements)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CktElements )( 
            ICircuit * This,
            /* [in] */ VARIANT idx,
            /* [retval][out] */ ICktElement **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Losses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Losses )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_LineLosses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LineLosses )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_SubstationLosses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SubstationLosses )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_TotalPower)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TotalPower )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllBusVolts)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllBusVolts )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllBusVmag)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllBusVmag )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllElementNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllElementNames )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_ActiveElement)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveElement )( 
            ICircuit * This,
            /* [retval][out] */ ICktElement **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, Disable)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Disable )( 
            ICircuit * This,
            /* [in] */ BSTR myName);
        
        DECLSPEC_XFGVIRT(ICircuit, Enable)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Enable )( 
            ICircuit * This,
            /* [in] */ BSTR myName);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Solution)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Solution )( 
            ICircuit * This,
            /* [retval][out] */ ISolution **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_ActiveBus)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveBus )( 
            ICircuit * This,
            /* [retval][out] */ IBus **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, FirstPCElement)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FirstPCElement )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, NextPCElement)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *NextPCElement )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, FirstPDElement)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FirstPDElement )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, NextPDElement)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *NextPDElement )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllBusNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllBusNames )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllElementLosses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllElementLosses )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, Sample)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Sample )( 
            ICircuit * This);
        
        DECLSPEC_XFGVIRT(ICircuit, SaveSample)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SaveSample )( 
            ICircuit * This);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Monitors)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Monitors )( 
            ICircuit * This,
            /* [retval][out] */ IMonitors **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Meters)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Meters )( 
            ICircuit * This,
            /* [retval][out] */ IMeters **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Generators)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Generators )( 
            ICircuit * This,
            /* [retval][out] */ IGenerators **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Settings)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Settings )( 
            ICircuit * This,
            /* [retval][out] */ ISettings **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Lines)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Lines )( 
            ICircuit * This,
            /* [retval][out] */ ILines **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, SetActiveElement)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetActiveElement )( 
            ICircuit * This,
            /* [in] */ BSTR FullName,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, Capacity)
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *Capacity )( 
            ICircuit * This,
            /* [in] */ double Start,
            /* [in] */ double Increment,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, SetActiveBus)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetActiveBus )( 
            ICircuit * This,
            /* [in] */ BSTR BusName,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, SetActiveBusi)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetActiveBusi )( 
            ICircuit * This,
            /* [in] */ long BusIndex,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllBusVmagPu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllBusVmagPu )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllNodeNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNodeNames )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_SystemY)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SystemY )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_CtrlQueue)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CtrlQueue )( 
            ICircuit * This,
            /* [retval][out] */ ICtrlQueue **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllBusDistances)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllBusDistances )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllNodeDistances)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNodeDistances )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllNodeVmagByPhase)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNodeVmagByPhase )( 
            ICircuit * This,
            /* [in] */ long Phase,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllNodeVmagPUByPhase)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNodeVmagPUByPhase )( 
            ICircuit * This,
            /* [in] */ long Phase,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllNodeDistancesByPhase)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNodeDistancesByPhase )( 
            ICircuit * This,
            /* [in] */ long Phase,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_AllNodeNamesByPhase)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNodeNamesByPhase )( 
            ICircuit * This,
            /* [in] */ long Phase,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Loads)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Loads )( 
            ICircuit * This,
            /* [retval][out] */ ILoads **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, FirstElement)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FirstElement )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, NextElement)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *NextElement )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, SetActiveClass)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetActiveClass )( 
            ICircuit * This,
            /* [in] */ BSTR ClassName,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_ActiveDSSElement)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveDSSElement )( 
            ICircuit * This,
            /* [retval][out] */ IDSSElement **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_ActiveCktElement)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveCktElement )( 
            ICircuit * This,
            /* [retval][out] */ ICktElement **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_ActiveClass)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveClass )( 
            ICircuit * This,
            /* [retval][out] */ IActiveClass **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Transformers)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Transformers )( 
            ICircuit * This,
            /* [retval][out] */ ITransformers **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_SwtControls)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwtControls )( 
            ICircuit * This,
            /* [retval][out] */ ISwtControls **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_CapControls)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CapControls )( 
            ICircuit * This,
            /* [retval][out] */ ICapControls **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_RegControls)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegControls )( 
            ICircuit * This,
            /* [retval][out] */ IRegControls **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Capacitors)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Capacitors )( 
            ICircuit * This,
            /* [retval][out] */ ICapacitors **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Topology)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Topology )( 
            ICircuit * This,
            /* [retval][out] */ ITopology **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Sensors)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Sensors )( 
            ICircuit * This,
            /* [retval][out] */ ISensors **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, UpdateStorage)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *UpdateStorage )( 
            ICircuit * This);
        
        DECLSPEC_XFGVIRT(ICircuit, get_ParentPDElement)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ParentPDElement )( 
            ICircuit * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_XYCurves)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_XYCurves )( 
            ICircuit * This,
            /* [retval][out] */ IXYCurves **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_PDElements)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PDElements )( 
            ICircuit * This,
            /* [retval][out] */ IPDElements **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Reclosers)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Reclosers )( 
            ICircuit * This,
            /* [retval][out] */ IReclosers **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Relays)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Relays )( 
            ICircuit * This,
            /* [retval][out] */ IRelays **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_LoadShapes)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LoadShapes )( 
            ICircuit * This,
            /* [retval][out] */ ILoadShapes **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Fuses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Fuses )( 
            ICircuit * This,
            /* [retval][out] */ IFuses **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_ISources)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ISources )( 
            ICircuit * This,
            /* [retval][out] */ IISources **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_YNodeVarray)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_YNodeVarray )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, EndOfTimeStepUpdate)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *EndOfTimeStepUpdate )( 
            ICircuit * This);
        
        DECLSPEC_XFGVIRT(ICircuit, get_DSSim_Coms)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DSSim_Coms )( 
            ICircuit * This,
            /* [retval][out] */ IDSSimComs **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_YNodeOrder)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_YNodeOrder )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_YCurrents)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_YCurrents )( 
            ICircuit * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_PVSystems)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PVSystems )( 
            ICircuit * This,
            /* [retval][out] */ IPVSystems **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Vsources)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vsources )( 
            ICircuit * This,
            /* [retval][out] */ IVsources **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Parallel)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Parallel )( 
            ICircuit * This,
            /* [retval][out] */ IParallel **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_LineCodes)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LineCodes )( 
            ICircuit * This,
            /* [retval][out] */ ILineCodes **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_GICSources)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_GICSources )( 
            ICircuit * This,
            /* [retval][out] */ IGICSources **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_ReduceCkt)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ReduceCkt )( 
            ICircuit * This,
            /* [retval][out] */ IReduceCkt **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Storages)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Storages )( 
            ICircuit * This,
            /* [retval][out] */ IStorages **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_WindGens)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_WindGens )( 
            ICircuit * This,
            /* [retval][out] */ IWindGens **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, get_Reactors)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Reactors )( 
            ICircuit * This,
            /* [retval][out] */ IReactors **Value);
        
        DECLSPEC_XFGVIRT(ICircuit, ElementLosses)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ElementLosses )( 
            ICircuit * This,
            /* [in] */ VARIANT Elements,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, ToJSON)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ToJSON )( 
            ICircuit * This,
            /* [defaultvalue][in] */ long options,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICircuit, FromJSON)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FromJSON )( 
            ICircuit * This,
            /* [in] */ BSTR data,
            /* [in] */ long options);
        
        DECLSPEC_XFGVIRT(ICircuit, Save)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Save )( 
            ICircuit * This,
            /* [in] */ BSTR dirOrFilePath,
            /* [defaultvalue][in] */ long saveFlags,
            /* [retval][out] */ BSTR *Value);
        
        END_INTERFACE
    } ICircuitVtbl;

    interface ICircuit
    {
        CONST_VTBL struct ICircuitVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ICircuit_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ICircuit_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ICircuit_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ICircuit_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ICircuit_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ICircuit_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ICircuit_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ICircuit_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ICircuit_get_NumCktElements(This,Value)	\
    ( (This)->lpVtbl -> get_NumCktElements(This,Value) ) 

#define ICircuit_get_NumBuses(This,Value)	\
    ( (This)->lpVtbl -> get_NumBuses(This,Value) ) 

#define ICircuit_get_NumNodes(This,Value)	\
    ( (This)->lpVtbl -> get_NumNodes(This,Value) ) 

#define ICircuit_get_Buses(This,Index,Value)	\
    ( (This)->lpVtbl -> get_Buses(This,Index,Value) ) 

#define ICircuit_get_CktElements(This,idx,Value)	\
    ( (This)->lpVtbl -> get_CktElements(This,idx,Value) ) 

#define ICircuit_get_Losses(This,Value)	\
    ( (This)->lpVtbl -> get_Losses(This,Value) ) 

#define ICircuit_get_LineLosses(This,Value)	\
    ( (This)->lpVtbl -> get_LineLosses(This,Value) ) 

#define ICircuit_get_SubstationLosses(This,Value)	\
    ( (This)->lpVtbl -> get_SubstationLosses(This,Value) ) 

#define ICircuit_get_TotalPower(This,Value)	\
    ( (This)->lpVtbl -> get_TotalPower(This,Value) ) 

#define ICircuit_get_AllBusVolts(This,Value)	\
    ( (This)->lpVtbl -> get_AllBusVolts(This,Value) ) 

#define ICircuit_get_AllBusVmag(This,Value)	\
    ( (This)->lpVtbl -> get_AllBusVmag(This,Value) ) 

#define ICircuit_get_AllElementNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllElementNames(This,Value) ) 

#define ICircuit_get_ActiveElement(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveElement(This,Value) ) 

#define ICircuit_Disable(This,myName)	\
    ( (This)->lpVtbl -> Disable(This,myName) ) 

#define ICircuit_Enable(This,myName)	\
    ( (This)->lpVtbl -> Enable(This,myName) ) 

#define ICircuit_get_Solution(This,Value)	\
    ( (This)->lpVtbl -> get_Solution(This,Value) ) 

#define ICircuit_get_ActiveBus(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveBus(This,Value) ) 

#define ICircuit_FirstPCElement(This,Value)	\
    ( (This)->lpVtbl -> FirstPCElement(This,Value) ) 

#define ICircuit_NextPCElement(This,Value)	\
    ( (This)->lpVtbl -> NextPCElement(This,Value) ) 

#define ICircuit_FirstPDElement(This,Value)	\
    ( (This)->lpVtbl -> FirstPDElement(This,Value) ) 

#define ICircuit_NextPDElement(This,Value)	\
    ( (This)->lpVtbl -> NextPDElement(This,Value) ) 

#define ICircuit_get_AllBusNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllBusNames(This,Value) ) 

#define ICircuit_get_AllElementLosses(This,Value)	\
    ( (This)->lpVtbl -> get_AllElementLosses(This,Value) ) 

#define ICircuit_Sample(This)	\
    ( (This)->lpVtbl -> Sample(This) ) 

#define ICircuit_SaveSample(This)	\
    ( (This)->lpVtbl -> SaveSample(This) ) 

#define ICircuit_get_Monitors(This,Value)	\
    ( (This)->lpVtbl -> get_Monitors(This,Value) ) 

#define ICircuit_get_Meters(This,Value)	\
    ( (This)->lpVtbl -> get_Meters(This,Value) ) 

#define ICircuit_get_Generators(This,Value)	\
    ( (This)->lpVtbl -> get_Generators(This,Value) ) 

#define ICircuit_get_Settings(This,Value)	\
    ( (This)->lpVtbl -> get_Settings(This,Value) ) 

#define ICircuit_get_Lines(This,Value)	\
    ( (This)->lpVtbl -> get_Lines(This,Value) ) 

#define ICircuit_SetActiveElement(This,FullName,Value)	\
    ( (This)->lpVtbl -> SetActiveElement(This,FullName,Value) ) 

#define ICircuit_Capacity(This,Start,Increment,Value)	\
    ( (This)->lpVtbl -> Capacity(This,Start,Increment,Value) ) 

#define ICircuit_SetActiveBus(This,BusName,Value)	\
    ( (This)->lpVtbl -> SetActiveBus(This,BusName,Value) ) 

#define ICircuit_SetActiveBusi(This,BusIndex,Value)	\
    ( (This)->lpVtbl -> SetActiveBusi(This,BusIndex,Value) ) 

#define ICircuit_get_AllBusVmagPu(This,Value)	\
    ( (This)->lpVtbl -> get_AllBusVmagPu(This,Value) ) 

#define ICircuit_get_AllNodeNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNodeNames(This,Value) ) 

#define ICircuit_get_SystemY(This,Value)	\
    ( (This)->lpVtbl -> get_SystemY(This,Value) ) 

#define ICircuit_get_CtrlQueue(This,Value)	\
    ( (This)->lpVtbl -> get_CtrlQueue(This,Value) ) 

#define ICircuit_get_AllBusDistances(This,Value)	\
    ( (This)->lpVtbl -> get_AllBusDistances(This,Value) ) 

#define ICircuit_get_AllNodeDistances(This,Value)	\
    ( (This)->lpVtbl -> get_AllNodeDistances(This,Value) ) 

#define ICircuit_get_AllNodeVmagByPhase(This,Phase,Value)	\
    ( (This)->lpVtbl -> get_AllNodeVmagByPhase(This,Phase,Value) ) 

#define ICircuit_get_AllNodeVmagPUByPhase(This,Phase,Value)	\
    ( (This)->lpVtbl -> get_AllNodeVmagPUByPhase(This,Phase,Value) ) 

#define ICircuit_get_AllNodeDistancesByPhase(This,Phase,Value)	\
    ( (This)->lpVtbl -> get_AllNodeDistancesByPhase(This,Phase,Value) ) 

#define ICircuit_get_AllNodeNamesByPhase(This,Phase,Value)	\
    ( (This)->lpVtbl -> get_AllNodeNamesByPhase(This,Phase,Value) ) 

#define ICircuit_get_Loads(This,Value)	\
    ( (This)->lpVtbl -> get_Loads(This,Value) ) 

#define ICircuit_FirstElement(This,Value)	\
    ( (This)->lpVtbl -> FirstElement(This,Value) ) 

#define ICircuit_NextElement(This,Value)	\
    ( (This)->lpVtbl -> NextElement(This,Value) ) 

#define ICircuit_SetActiveClass(This,ClassName,Value)	\
    ( (This)->lpVtbl -> SetActiveClass(This,ClassName,Value) ) 

#define ICircuit_get_ActiveDSSElement(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveDSSElement(This,Value) ) 

#define ICircuit_get_ActiveCktElement(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveCktElement(This,Value) ) 

#define ICircuit_get_ActiveClass(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveClass(This,Value) ) 

#define ICircuit_get_Transformers(This,Value)	\
    ( (This)->lpVtbl -> get_Transformers(This,Value) ) 

#define ICircuit_get_SwtControls(This,Value)	\
    ( (This)->lpVtbl -> get_SwtControls(This,Value) ) 

#define ICircuit_get_CapControls(This,Value)	\
    ( (This)->lpVtbl -> get_CapControls(This,Value) ) 

#define ICircuit_get_RegControls(This,Value)	\
    ( (This)->lpVtbl -> get_RegControls(This,Value) ) 

#define ICircuit_get_Capacitors(This,Value)	\
    ( (This)->lpVtbl -> get_Capacitors(This,Value) ) 

#define ICircuit_get_Topology(This,Value)	\
    ( (This)->lpVtbl -> get_Topology(This,Value) ) 

#define ICircuit_get_Sensors(This,Value)	\
    ( (This)->lpVtbl -> get_Sensors(This,Value) ) 

#define ICircuit_UpdateStorage(This)	\
    ( (This)->lpVtbl -> UpdateStorage(This) ) 

#define ICircuit_get_ParentPDElement(This,Value)	\
    ( (This)->lpVtbl -> get_ParentPDElement(This,Value) ) 

#define ICircuit_get_XYCurves(This,Value)	\
    ( (This)->lpVtbl -> get_XYCurves(This,Value) ) 

#define ICircuit_get_PDElements(This,Value)	\
    ( (This)->lpVtbl -> get_PDElements(This,Value) ) 

#define ICircuit_get_Reclosers(This,Value)	\
    ( (This)->lpVtbl -> get_Reclosers(This,Value) ) 

#define ICircuit_get_Relays(This,Value)	\
    ( (This)->lpVtbl -> get_Relays(This,Value) ) 

#define ICircuit_get_LoadShapes(This,Value)	\
    ( (This)->lpVtbl -> get_LoadShapes(This,Value) ) 

#define ICircuit_get_Fuses(This,Value)	\
    ( (This)->lpVtbl -> get_Fuses(This,Value) ) 

#define ICircuit_get_ISources(This,Value)	\
    ( (This)->lpVtbl -> get_ISources(This,Value) ) 

#define ICircuit_get_YNodeVarray(This,Value)	\
    ( (This)->lpVtbl -> get_YNodeVarray(This,Value) ) 

#define ICircuit_EndOfTimeStepUpdate(This)	\
    ( (This)->lpVtbl -> EndOfTimeStepUpdate(This) ) 

#define ICircuit_get_DSSim_Coms(This,Value)	\
    ( (This)->lpVtbl -> get_DSSim_Coms(This,Value) ) 

#define ICircuit_get_YNodeOrder(This,Value)	\
    ( (This)->lpVtbl -> get_YNodeOrder(This,Value) ) 

#define ICircuit_get_YCurrents(This,Value)	\
    ( (This)->lpVtbl -> get_YCurrents(This,Value) ) 

#define ICircuit_get_PVSystems(This,Value)	\
    ( (This)->lpVtbl -> get_PVSystems(This,Value) ) 

#define ICircuit_get_Vsources(This,Value)	\
    ( (This)->lpVtbl -> get_Vsources(This,Value) ) 

#define ICircuit_get_Parallel(This,Value)	\
    ( (This)->lpVtbl -> get_Parallel(This,Value) ) 

#define ICircuit_get_LineCodes(This,Value)	\
    ( (This)->lpVtbl -> get_LineCodes(This,Value) ) 

#define ICircuit_get_GICSources(This,Value)	\
    ( (This)->lpVtbl -> get_GICSources(This,Value) ) 

#define ICircuit_get_ReduceCkt(This,Value)	\
    ( (This)->lpVtbl -> get_ReduceCkt(This,Value) ) 

#define ICircuit_get_Storages(This,Value)	\
    ( (This)->lpVtbl -> get_Storages(This,Value) ) 

#define ICircuit_get_WindGens(This,Value)	\
    ( (This)->lpVtbl -> get_WindGens(This,Value) ) 

#define ICircuit_get_Reactors(This,Value)	\
    ( (This)->lpVtbl -> get_Reactors(This,Value) ) 

#define ICircuit_ElementLosses(This,Elements,Value)	\
    ( (This)->lpVtbl -> ElementLosses(This,Elements,Value) ) 

#define ICircuit_ToJSON(This,options,Value)	\
    ( (This)->lpVtbl -> ToJSON(This,options,Value) ) 

#define ICircuit_FromJSON(This,data,options)	\
    ( (This)->lpVtbl -> FromJSON(This,data,options) ) 

#define ICircuit_Save(This,dirOrFilePath,saveFlags,Value)	\
    ( (This)->lpVtbl -> Save(This,dirOrFilePath,saveFlags,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ICircuit_INTERFACE_DEFINED__ */


#ifndef __IBus_INTERFACE_DEFINED__
#define __IBus_INTERFACE_DEFINED__

/* interface IBus */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IBus;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B31E-18C2-11F0-A417-C87F5452571C")
    IBus : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Name) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumNodes( 
            /* [retval][out] */ long *NumNodes) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Voltages( 
            /* [retval][out] */ VARIANT *Voltages) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SeqVoltages( 
            /* [retval][out] */ VARIANT *SeqVoltages) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Nodes( 
            /* [retval][out] */ VARIANT *Nodes) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Voc( 
            /* [retval][out] */ VARIANT *Voc) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Isc( 
            /* [retval][out] */ VARIANT *Isc) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_puVoltages( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kVBase( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ZscMatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Zsc1( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Zsc0( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ZscRefresh( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_YscMatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Coorddefined( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_x( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_x( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_y( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_y( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Distance( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetUniqueNodeNumber( 
            /* [in] */ long StartNumber,
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CplxSeqVoltages( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Lambda( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_N_interrupts( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Int_Duration( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Cust_Interrupts( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Cust_Duration( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_N_Customers( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VLL( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_puVLL( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VMagAngle( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_puVmagAngle( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TotalMiles( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SectionID( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LineList( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LoadList( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ZSC012Matrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Latitude( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Latitude( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Longitude( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Longitude( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllPCEatBus( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllPDEatBus( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IBusVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IBus * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IBus * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IBus * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IBus * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IBus * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IBus * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IBus * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IBus, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IBus * This,
            /* [retval][out] */ BSTR *Name);
        
        DECLSPEC_XFGVIRT(IBus, get_NumNodes)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumNodes )( 
            IBus * This,
            /* [retval][out] */ long *NumNodes);
        
        DECLSPEC_XFGVIRT(IBus, get_Voltages)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Voltages )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Voltages);
        
        DECLSPEC_XFGVIRT(IBus, get_SeqVoltages)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SeqVoltages )( 
            IBus * This,
            /* [retval][out] */ VARIANT *SeqVoltages);
        
        DECLSPEC_XFGVIRT(IBus, get_Nodes)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Nodes )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Nodes);
        
        DECLSPEC_XFGVIRT(IBus, get_Voc)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Voc )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Voc);
        
        DECLSPEC_XFGVIRT(IBus, get_Isc)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Isc )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Isc);
        
        DECLSPEC_XFGVIRT(IBus, get_puVoltages)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_puVoltages )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_kVBase)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kVBase )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_ZscMatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ZscMatrix )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Zsc1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Zsc1 )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Zsc0)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Zsc0 )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, ZscRefresh)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ZscRefresh )( 
            IBus * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_YscMatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_YscMatrix )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Coorddefined)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Coorddefined )( 
            IBus * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_x)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_x )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, put_x)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_x )( 
            IBus * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IBus, get_y)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_y )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, put_y)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_y )( 
            IBus * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Distance)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Distance )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, GetUniqueNodeNumber)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetUniqueNodeNumber )( 
            IBus * This,
            /* [in] */ long StartNumber,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_CplxSeqVoltages)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CplxSeqVoltages )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Lambda)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Lambda )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_N_interrupts)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_N_interrupts )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Int_Duration)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Int_Duration )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Cust_Interrupts)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Cust_Interrupts )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Cust_Duration)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Cust_Duration )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_N_Customers)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_N_Customers )( 
            IBus * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_VLL)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VLL )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_puVLL)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_puVLL )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_VMagAngle)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VMagAngle )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_puVmagAngle)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_puVmagAngle )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_TotalMiles)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TotalMiles )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_SectionID)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SectionID )( 
            IBus * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_LineList)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LineList )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_LoadList)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LoadList )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_ZSC012Matrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ZSC012Matrix )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Latitude)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Latitude )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, put_Latitude)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Latitude )( 
            IBus * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IBus, get_Longitude)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Longitude )( 
            IBus * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IBus, put_Longitude)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Longitude )( 
            IBus * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IBus, get_AllPCEatBus)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllPCEatBus )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IBus, get_AllPDEatBus)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllPDEatBus )( 
            IBus * This,
            /* [retval][out] */ VARIANT *Value);
        
        END_INTERFACE
    } IBusVtbl;

    interface IBus
    {
        CONST_VTBL struct IBusVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IBus_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IBus_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IBus_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IBus_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IBus_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IBus_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IBus_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IBus_get_Name(This,Name)	\
    ( (This)->lpVtbl -> get_Name(This,Name) ) 

#define IBus_get_NumNodes(This,NumNodes)	\
    ( (This)->lpVtbl -> get_NumNodes(This,NumNodes) ) 

#define IBus_get_Voltages(This,Voltages)	\
    ( (This)->lpVtbl -> get_Voltages(This,Voltages) ) 

#define IBus_get_SeqVoltages(This,SeqVoltages)	\
    ( (This)->lpVtbl -> get_SeqVoltages(This,SeqVoltages) ) 

#define IBus_get_Nodes(This,Nodes)	\
    ( (This)->lpVtbl -> get_Nodes(This,Nodes) ) 

#define IBus_get_Voc(This,Voc)	\
    ( (This)->lpVtbl -> get_Voc(This,Voc) ) 

#define IBus_get_Isc(This,Isc)	\
    ( (This)->lpVtbl -> get_Isc(This,Isc) ) 

#define IBus_get_puVoltages(This,Value)	\
    ( (This)->lpVtbl -> get_puVoltages(This,Value) ) 

#define IBus_get_kVBase(This,Value)	\
    ( (This)->lpVtbl -> get_kVBase(This,Value) ) 

#define IBus_get_ZscMatrix(This,Value)	\
    ( (This)->lpVtbl -> get_ZscMatrix(This,Value) ) 

#define IBus_get_Zsc1(This,Value)	\
    ( (This)->lpVtbl -> get_Zsc1(This,Value) ) 

#define IBus_get_Zsc0(This,Value)	\
    ( (This)->lpVtbl -> get_Zsc0(This,Value) ) 

#define IBus_ZscRefresh(This,Value)	\
    ( (This)->lpVtbl -> ZscRefresh(This,Value) ) 

#define IBus_get_YscMatrix(This,Value)	\
    ( (This)->lpVtbl -> get_YscMatrix(This,Value) ) 

#define IBus_get_Coorddefined(This,Value)	\
    ( (This)->lpVtbl -> get_Coorddefined(This,Value) ) 

#define IBus_get_x(This,Value)	\
    ( (This)->lpVtbl -> get_x(This,Value) ) 

#define IBus_put_x(This,Value)	\
    ( (This)->lpVtbl -> put_x(This,Value) ) 

#define IBus_get_y(This,Value)	\
    ( (This)->lpVtbl -> get_y(This,Value) ) 

#define IBus_put_y(This,Value)	\
    ( (This)->lpVtbl -> put_y(This,Value) ) 

#define IBus_get_Distance(This,Value)	\
    ( (This)->lpVtbl -> get_Distance(This,Value) ) 

#define IBus_GetUniqueNodeNumber(This,StartNumber,Value)	\
    ( (This)->lpVtbl -> GetUniqueNodeNumber(This,StartNumber,Value) ) 

#define IBus_get_CplxSeqVoltages(This,Value)	\
    ( (This)->lpVtbl -> get_CplxSeqVoltages(This,Value) ) 

#define IBus_get_Lambda(This,Value)	\
    ( (This)->lpVtbl -> get_Lambda(This,Value) ) 

#define IBus_get_N_interrupts(This,Value)	\
    ( (This)->lpVtbl -> get_N_interrupts(This,Value) ) 

#define IBus_get_Int_Duration(This,Value)	\
    ( (This)->lpVtbl -> get_Int_Duration(This,Value) ) 

#define IBus_get_Cust_Interrupts(This,Value)	\
    ( (This)->lpVtbl -> get_Cust_Interrupts(This,Value) ) 

#define IBus_get_Cust_Duration(This,Value)	\
    ( (This)->lpVtbl -> get_Cust_Duration(This,Value) ) 

#define IBus_get_N_Customers(This,Value)	\
    ( (This)->lpVtbl -> get_N_Customers(This,Value) ) 

#define IBus_get_VLL(This,Value)	\
    ( (This)->lpVtbl -> get_VLL(This,Value) ) 

#define IBus_get_puVLL(This,Value)	\
    ( (This)->lpVtbl -> get_puVLL(This,Value) ) 

#define IBus_get_VMagAngle(This,Value)	\
    ( (This)->lpVtbl -> get_VMagAngle(This,Value) ) 

#define IBus_get_puVmagAngle(This,Value)	\
    ( (This)->lpVtbl -> get_puVmagAngle(This,Value) ) 

#define IBus_get_TotalMiles(This,Value)	\
    ( (This)->lpVtbl -> get_TotalMiles(This,Value) ) 

#define IBus_get_SectionID(This,Value)	\
    ( (This)->lpVtbl -> get_SectionID(This,Value) ) 

#define IBus_get_LineList(This,Value)	\
    ( (This)->lpVtbl -> get_LineList(This,Value) ) 

#define IBus_get_LoadList(This,Value)	\
    ( (This)->lpVtbl -> get_LoadList(This,Value) ) 

#define IBus_get_ZSC012Matrix(This,Value)	\
    ( (This)->lpVtbl -> get_ZSC012Matrix(This,Value) ) 

#define IBus_get_Latitude(This,Value)	\
    ( (This)->lpVtbl -> get_Latitude(This,Value) ) 

#define IBus_put_Latitude(This,Value)	\
    ( (This)->lpVtbl -> put_Latitude(This,Value) ) 

#define IBus_get_Longitude(This,Value)	\
    ( (This)->lpVtbl -> get_Longitude(This,Value) ) 

#define IBus_put_Longitude(This,Value)	\
    ( (This)->lpVtbl -> put_Longitude(This,Value) ) 

#define IBus_get_AllPCEatBus(This,Value)	\
    ( (This)->lpVtbl -> get_AllPCEatBus(This,Value) ) 

#define IBus_get_AllPDEatBus(This,Value)	\
    ( (This)->lpVtbl -> get_AllPDEatBus(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IBus_INTERFACE_DEFINED__ */


#ifndef __IDSS_INTERFACE_DEFINED__
#define __IDSS_INTERFACE_DEFINED__

/* interface IDSS */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IDSS;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B364-18C2-11F0-A417-C87F5452571C")
    IDSS : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumCircuits( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Circuits( 
            /* [in] */ VARIANT idx,
            /* [retval][out] */ ICircuit **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveCircuit( 
            /* [retval][out] */ ICircuit **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Text( 
            /* [retval][out] */ IText **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Error( 
            /* [retval][out] */ IError **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE NewCircuit( 
            /* [in] */ BSTR Name,
            /* [retval][out] */ ICircuit **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ClearAll( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ShowPanel( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Start( 
            /* [in] */ long Code,
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Version( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DSSProgress( 
            /* [retval][out] */ IDSSProgress **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Classes( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_UserClasses( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumClasses( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumUserClasses( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DataPath( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_DataPath( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllowForms( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AllowForms( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DefaultEditor( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveClass( 
            /* [retval][out] */ IActiveClass **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetActiveClass( 
            /* [in] */ BSTR ClassName,
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Executive( 
            /* [retval][out] */ IDSS_Executive **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CmathLib( 
            /* [retval][out] */ ICmathLib **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Parser( 
            /* [retval][out] */ IParser **Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_DSSim_Coms( 
            /* [retval][out] */ IDSSimComs **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ZIP( 
            /* [retval][out] */ IZIP **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_YMatrix( 
            /* [retval][out] */ IYMatrix **Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE LoadOpenDSS( 
            /* [defaultvalue][in] */ BSTR dllpath = (BSTR)L"",
            /* [defaultvalue][in] */ hyper liboptions = 0) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE LoadAltDSS( 
            /* [defaultvalue][in] */ BSTR dllpath = (BSTR)L"",
            /* [defaultvalue][in] */ hyper liboptions = 0) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IDSSVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IDSS * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IDSS * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IDSS * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IDSS * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IDSS * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IDSS * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IDSS * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IDSS, get_NumCircuits)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumCircuits )( 
            IDSS * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_Circuits)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Circuits )( 
            IDSS * This,
            /* [in] */ VARIANT idx,
            /* [retval][out] */ ICircuit **Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_ActiveCircuit)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveCircuit )( 
            IDSS * This,
            /* [retval][out] */ ICircuit **Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_Text)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Text )( 
            IDSS * This,
            /* [retval][out] */ IText **Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_Error)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Error )( 
            IDSS * This,
            /* [retval][out] */ IError **Value);
        
        DECLSPEC_XFGVIRT(IDSS, NewCircuit)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *NewCircuit )( 
            IDSS * This,
            /* [in] */ BSTR Name,
            /* [retval][out] */ ICircuit **Value);
        
        DECLSPEC_XFGVIRT(IDSS, ClearAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ClearAll )( 
            IDSS * This);
        
        DECLSPEC_XFGVIRT(IDSS, ShowPanel)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ShowPanel )( 
            IDSS * This);
        
        DECLSPEC_XFGVIRT(IDSS, Start)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Start )( 
            IDSS * This,
            /* [in] */ long Code,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_Version)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Version )( 
            IDSS * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_DSSProgress)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DSSProgress )( 
            IDSS * This,
            /* [retval][out] */ IDSSProgress **Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_Classes)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Classes )( 
            IDSS * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_UserClasses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_UserClasses )( 
            IDSS * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_NumClasses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumClasses )( 
            IDSS * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_NumUserClasses)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumUserClasses )( 
            IDSS * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_DataPath)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DataPath )( 
            IDSS * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IDSS, put_DataPath)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_DataPath )( 
            IDSS * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IDSS, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            IDSS * This);
        
        DECLSPEC_XFGVIRT(IDSS, get_AllowForms)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllowForms )( 
            IDSS * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IDSS, put_AllowForms)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AllowForms )( 
            IDSS * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_DefaultEditor)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DefaultEditor )( 
            IDSS * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_ActiveClass)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveClass )( 
            IDSS * This,
            /* [retval][out] */ IActiveClass **Value);
        
        DECLSPEC_XFGVIRT(IDSS, SetActiveClass)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetActiveClass )( 
            IDSS * This,
            /* [in] */ BSTR ClassName,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_Executive)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Executive )( 
            IDSS * This,
            /* [retval][out] */ IDSS_Executive **Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_CmathLib)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CmathLib )( 
            IDSS * This,
            /* [retval][out] */ ICmathLib **Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_Parser)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Parser )( 
            IDSS * This,
            /* [retval][out] */ IParser **Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_DSSim_Coms)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DSSim_Coms )( 
            IDSS * This,
            /* [retval][out] */ IDSSimComs **Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_ZIP)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ZIP )( 
            IDSS * This,
            /* [retval][out] */ IZIP **Value);
        
        DECLSPEC_XFGVIRT(IDSS, get_YMatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_YMatrix )( 
            IDSS * This,
            /* [retval][out] */ IYMatrix **Value);
        
        DECLSPEC_XFGVIRT(IDSS, LoadOpenDSS)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *LoadOpenDSS )( 
            IDSS * This,
            /* [defaultvalue][in] */ BSTR dllpath,
            /* [defaultvalue][in] */ hyper liboptions);
        
        DECLSPEC_XFGVIRT(IDSS, LoadAltDSS)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *LoadAltDSS )( 
            IDSS * This,
            /* [defaultvalue][in] */ BSTR dllpath,
            /* [defaultvalue][in] */ hyper liboptions);
        
        END_INTERFACE
    } IDSSVtbl;

    interface IDSS
    {
        CONST_VTBL struct IDSSVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IDSS_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IDSS_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IDSS_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IDSS_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IDSS_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IDSS_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IDSS_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IDSS_get_NumCircuits(This,Value)	\
    ( (This)->lpVtbl -> get_NumCircuits(This,Value) ) 

#define IDSS_get_Circuits(This,idx,Value)	\
    ( (This)->lpVtbl -> get_Circuits(This,idx,Value) ) 

#define IDSS_get_ActiveCircuit(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveCircuit(This,Value) ) 

#define IDSS_get_Text(This,Value)	\
    ( (This)->lpVtbl -> get_Text(This,Value) ) 

#define IDSS_get_Error(This,Value)	\
    ( (This)->lpVtbl -> get_Error(This,Value) ) 

#define IDSS_NewCircuit(This,Name,Value)	\
    ( (This)->lpVtbl -> NewCircuit(This,Name,Value) ) 

#define IDSS_ClearAll(This)	\
    ( (This)->lpVtbl -> ClearAll(This) ) 

#define IDSS_ShowPanel(This)	\
    ( (This)->lpVtbl -> ShowPanel(This) ) 

#define IDSS_Start(This,Code,Value)	\
    ( (This)->lpVtbl -> Start(This,Code,Value) ) 

#define IDSS_get_Version(This,Value)	\
    ( (This)->lpVtbl -> get_Version(This,Value) ) 

#define IDSS_get_DSSProgress(This,Value)	\
    ( (This)->lpVtbl -> get_DSSProgress(This,Value) ) 

#define IDSS_get_Classes(This,Value)	\
    ( (This)->lpVtbl -> get_Classes(This,Value) ) 

#define IDSS_get_UserClasses(This,Value)	\
    ( (This)->lpVtbl -> get_UserClasses(This,Value) ) 

#define IDSS_get_NumClasses(This,Value)	\
    ( (This)->lpVtbl -> get_NumClasses(This,Value) ) 

#define IDSS_get_NumUserClasses(This,Value)	\
    ( (This)->lpVtbl -> get_NumUserClasses(This,Value) ) 

#define IDSS_get_DataPath(This,Value)	\
    ( (This)->lpVtbl -> get_DataPath(This,Value) ) 

#define IDSS_put_DataPath(This,Value)	\
    ( (This)->lpVtbl -> put_DataPath(This,Value) ) 

#define IDSS_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define IDSS_get_AllowForms(This,Value)	\
    ( (This)->lpVtbl -> get_AllowForms(This,Value) ) 

#define IDSS_put_AllowForms(This,Value)	\
    ( (This)->lpVtbl -> put_AllowForms(This,Value) ) 

#define IDSS_get_DefaultEditor(This,Value)	\
    ( (This)->lpVtbl -> get_DefaultEditor(This,Value) ) 

#define IDSS_get_ActiveClass(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveClass(This,Value) ) 

#define IDSS_SetActiveClass(This,ClassName,Value)	\
    ( (This)->lpVtbl -> SetActiveClass(This,ClassName,Value) ) 

#define IDSS_get_Executive(This,Value)	\
    ( (This)->lpVtbl -> get_Executive(This,Value) ) 

#define IDSS_get_CmathLib(This,Value)	\
    ( (This)->lpVtbl -> get_CmathLib(This,Value) ) 

#define IDSS_get_Parser(This,Value)	\
    ( (This)->lpVtbl -> get_Parser(This,Value) ) 

#define IDSS_get_DSSim_Coms(This,Value)	\
    ( (This)->lpVtbl -> get_DSSim_Coms(This,Value) ) 

#define IDSS_get_ZIP(This,Value)	\
    ( (This)->lpVtbl -> get_ZIP(This,Value) ) 

#define IDSS_get_YMatrix(This,Value)	\
    ( (This)->lpVtbl -> get_YMatrix(This,Value) ) 

#define IDSS_LoadOpenDSS(This,dllpath,liboptions)	\
    ( (This)->lpVtbl -> LoadOpenDSS(This,dllpath,liboptions) ) 

#define IDSS_LoadAltDSS(This,dllpath,liboptions)	\
    ( (This)->lpVtbl -> LoadAltDSS(This,dllpath,liboptions) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IDSS_INTERFACE_DEFINED__ */


#ifndef __ISolution_INTERFACE_DEFINED__
#define __ISolution_INTERFACE_DEFINED__

/* interface ISolution */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ISolution;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B3A0-18C2-11F0-A417-C87F5452571C")
    ISolution : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Solve( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Mode( 
            /* [retval][out] */ long *Mode) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Mode( 
            /* [in] */ long Mode) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Frequency( 
            /* [retval][out] */ double *Frequency) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Frequency( 
            /* [in] */ double Frequency) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Hour( 
            /* [retval][out] */ long *Hour) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Hour( 
            /* [in] */ long Hour) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Seconds( 
            /* [retval][out] */ double *Seconds) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Seconds( 
            /* [in] */ double Seconds) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_StepSize( 
            /* [retval][out] */ double *StepSize) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_StepSize( 
            /* [in] */ double StepSize) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Year( 
            /* [retval][out] */ long *Year) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Year( 
            /* [in] */ long Year) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LoadMult( 
            /* [retval][out] */ double *LoadMult) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LoadMult( 
            /* [in] */ double LoadMult) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Iterations( 
            /* [retval][out] */ long *Iterations) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MaxIterations( 
            /* [retval][out] */ long *MaxIterations) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MaxIterations( 
            /* [in] */ long MaxIterations) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Tolerance( 
            /* [retval][out] */ double *Tolerance) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Tolerance( 
            /* [in] */ double Tolerance) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Number( 
            /* [retval][out] */ long *Number) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Number( 
            /* [in] */ long Number) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Random( 
            /* [retval][out] */ long *Random) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Random( 
            /* [in] */ long Random) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ModeID( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LoadModel( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LoadModel( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LDCurve( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LDCurve( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_pctGrowth( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_pctGrowth( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AddType( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AddType( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_GenkW( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_GenkW( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_GenPF( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_GenPF( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Capkvar( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Capkvar( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Algorithm( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Algorithm( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ControlMode( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ControlMode( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_GenMult( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_GenMult( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DefaultDaily( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_DefaultDaily( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DefaultYearly( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_DefaultYearly( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EventLog( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_dblHour( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_dblHour( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_StepsizeMin( 
            /* [in] */ double rhs) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_StepsizeHr( 
            /* [in] */ double rhs) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ControlIterations( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ControlIterations( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MaxControlIterations( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MaxControlIterations( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Sample_DoControlActions( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE CheckFaultStatus( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SolveSnap( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SolveDirect( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SolvePflow( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SolveNoControl( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SolvePlusControl( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE InitSnap( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE CheckControls( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SampleControlDevices( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DoControlActions( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE BuildYMatrix( 
            /* [in] */ long BuildOption,
            /* [in] */ long AllocateVI) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SystemYChanged( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Converged( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Converged( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Totaliterations( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MostIterationsDone( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ControlActionsDone( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ControlActionsDone( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE FinishTimeStep( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Cleanup( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Total_Time( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Total_Time( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Process_Time( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Time_of_Step( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IntervalHrs( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IntervalHrs( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SolveAll( void) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_IncMatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_IncMatrixRows( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_IncMatrixCols( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_BusLevels( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Laplacian( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MinIterations( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MinIterations( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ISolutionVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ISolution * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ISolution * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ISolution * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ISolution * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ISolution * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ISolution, Solve)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Solve )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, get_Mode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Mode )( 
            ISolution * This,
            /* [retval][out] */ long *Mode);
        
        DECLSPEC_XFGVIRT(ISolution, put_Mode)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Mode )( 
            ISolution * This,
            /* [in] */ long Mode);
        
        DECLSPEC_XFGVIRT(ISolution, get_Frequency)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Frequency )( 
            ISolution * This,
            /* [retval][out] */ double *Frequency);
        
        DECLSPEC_XFGVIRT(ISolution, put_Frequency)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Frequency )( 
            ISolution * This,
            /* [in] */ double Frequency);
        
        DECLSPEC_XFGVIRT(ISolution, get_Hour)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Hour )( 
            ISolution * This,
            /* [retval][out] */ long *Hour);
        
        DECLSPEC_XFGVIRT(ISolution, put_Hour)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Hour )( 
            ISolution * This,
            /* [in] */ long Hour);
        
        DECLSPEC_XFGVIRT(ISolution, get_Seconds)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Seconds )( 
            ISolution * This,
            /* [retval][out] */ double *Seconds);
        
        DECLSPEC_XFGVIRT(ISolution, put_Seconds)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Seconds )( 
            ISolution * This,
            /* [in] */ double Seconds);
        
        DECLSPEC_XFGVIRT(ISolution, get_StepSize)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_StepSize )( 
            ISolution * This,
            /* [retval][out] */ double *StepSize);
        
        DECLSPEC_XFGVIRT(ISolution, put_StepSize)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_StepSize )( 
            ISolution * This,
            /* [in] */ double StepSize);
        
        DECLSPEC_XFGVIRT(ISolution, get_Year)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Year )( 
            ISolution * This,
            /* [retval][out] */ long *Year);
        
        DECLSPEC_XFGVIRT(ISolution, put_Year)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Year )( 
            ISolution * This,
            /* [in] */ long Year);
        
        DECLSPEC_XFGVIRT(ISolution, get_LoadMult)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LoadMult )( 
            ISolution * This,
            /* [retval][out] */ double *LoadMult);
        
        DECLSPEC_XFGVIRT(ISolution, put_LoadMult)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LoadMult )( 
            ISolution * This,
            /* [in] */ double LoadMult);
        
        DECLSPEC_XFGVIRT(ISolution, get_Iterations)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Iterations )( 
            ISolution * This,
            /* [retval][out] */ long *Iterations);
        
        DECLSPEC_XFGVIRT(ISolution, get_MaxIterations)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MaxIterations )( 
            ISolution * This,
            /* [retval][out] */ long *MaxIterations);
        
        DECLSPEC_XFGVIRT(ISolution, put_MaxIterations)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MaxIterations )( 
            ISolution * This,
            /* [in] */ long MaxIterations);
        
        DECLSPEC_XFGVIRT(ISolution, get_Tolerance)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Tolerance )( 
            ISolution * This,
            /* [retval][out] */ double *Tolerance);
        
        DECLSPEC_XFGVIRT(ISolution, put_Tolerance)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Tolerance )( 
            ISolution * This,
            /* [in] */ double Tolerance);
        
        DECLSPEC_XFGVIRT(ISolution, get_Number)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Number )( 
            ISolution * This,
            /* [retval][out] */ long *Number);
        
        DECLSPEC_XFGVIRT(ISolution, put_Number)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Number )( 
            ISolution * This,
            /* [in] */ long Number);
        
        DECLSPEC_XFGVIRT(ISolution, get_Random)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Random )( 
            ISolution * This,
            /* [retval][out] */ long *Random);
        
        DECLSPEC_XFGVIRT(ISolution, put_Random)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Random )( 
            ISolution * This,
            /* [in] */ long Random);
        
        DECLSPEC_XFGVIRT(ISolution, get_ModeID)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ModeID )( 
            ISolution * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_LoadModel)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LoadModel )( 
            ISolution * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_LoadModel)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LoadModel )( 
            ISolution * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_LDCurve)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LDCurve )( 
            ISolution * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_LDCurve)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LDCurve )( 
            ISolution * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_pctGrowth)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_pctGrowth )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_pctGrowth)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_pctGrowth )( 
            ISolution * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_AddType)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AddType )( 
            ISolution * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_AddType)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AddType )( 
            ISolution * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_GenkW)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_GenkW )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_GenkW)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_GenkW )( 
            ISolution * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_GenPF)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_GenPF )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_GenPF)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_GenPF )( 
            ISolution * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_Capkvar)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Capkvar )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_Capkvar)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Capkvar )( 
            ISolution * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_Algorithm)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Algorithm )( 
            ISolution * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_Algorithm)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Algorithm )( 
            ISolution * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_ControlMode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ControlMode )( 
            ISolution * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_ControlMode)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ControlMode )( 
            ISolution * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_GenMult)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_GenMult )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_GenMult)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_GenMult )( 
            ISolution * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_DefaultDaily)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DefaultDaily )( 
            ISolution * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_DefaultDaily)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_DefaultDaily )( 
            ISolution * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_DefaultYearly)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DefaultYearly )( 
            ISolution * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_DefaultYearly)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_DefaultYearly )( 
            ISolution * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_EventLog)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EventLog )( 
            ISolution * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_dblHour)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_dblHour )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_dblHour)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_dblHour )( 
            ISolution * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_StepsizeMin)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_StepsizeMin )( 
            ISolution * This,
            /* [in] */ double rhs);
        
        DECLSPEC_XFGVIRT(ISolution, put_StepsizeHr)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_StepsizeHr )( 
            ISolution * This,
            /* [in] */ double rhs);
        
        DECLSPEC_XFGVIRT(ISolution, get_ControlIterations)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ControlIterations )( 
            ISolution * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_ControlIterations)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ControlIterations )( 
            ISolution * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_MaxControlIterations)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MaxControlIterations )( 
            ISolution * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_MaxControlIterations)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MaxControlIterations )( 
            ISolution * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISolution, Sample_DoControlActions)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Sample_DoControlActions )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, CheckFaultStatus)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *CheckFaultStatus )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, SolveSnap)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SolveSnap )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, SolveDirect)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SolveDirect )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, SolvePflow)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SolvePflow )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, SolveNoControl)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SolveNoControl )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, SolvePlusControl)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SolvePlusControl )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, InitSnap)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *InitSnap )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, CheckControls)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *CheckControls )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, SampleControlDevices)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SampleControlDevices )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, DoControlActions)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DoControlActions )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, BuildYMatrix)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *BuildYMatrix )( 
            ISolution * This,
            /* [in] */ long BuildOption,
            /* [in] */ long AllocateVI);
        
        DECLSPEC_XFGVIRT(ISolution, get_SystemYChanged)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SystemYChanged )( 
            ISolution * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_Converged)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Converged )( 
            ISolution * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_Converged)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Converged )( 
            ISolution * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_Totaliterations)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Totaliterations )( 
            ISolution * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_MostIterationsDone)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MostIterationsDone )( 
            ISolution * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_ControlActionsDone)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ControlActionsDone )( 
            ISolution * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_ControlActionsDone)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ControlActionsDone )( 
            ISolution * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISolution, FinishTimeStep)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *FinishTimeStep )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, Cleanup)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Cleanup )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, get_Total_Time)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Total_Time )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_Total_Time)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Total_Time )( 
            ISolution * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_Process_Time)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Process_Time )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_Time_of_Step)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Time_of_Step )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_IntervalHrs)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IntervalHrs )( 
            ISolution * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_IntervalHrs)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IntervalHrs )( 
            ISolution * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISolution, SolveAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SolveAll )( 
            ISolution * This);
        
        DECLSPEC_XFGVIRT(ISolution, get_IncMatrix)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IncMatrix )( 
            ISolution * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_IncMatrixRows)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IncMatrixRows )( 
            ISolution * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_IncMatrixCols)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IncMatrixCols )( 
            ISolution * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_BusLevels)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_BusLevels )( 
            ISolution * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_Laplacian)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Laplacian )( 
            ISolution * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISolution, get_MinIterations)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MinIterations )( 
            ISolution * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISolution, put_MinIterations)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MinIterations )( 
            ISolution * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } ISolutionVtbl;

    interface ISolution
    {
        CONST_VTBL struct ISolutionVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ISolution_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ISolution_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ISolution_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ISolution_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ISolution_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ISolution_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ISolution_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ISolution_Solve(This)	\
    ( (This)->lpVtbl -> Solve(This) ) 

#define ISolution_get_Mode(This,Mode)	\
    ( (This)->lpVtbl -> get_Mode(This,Mode) ) 

#define ISolution_put_Mode(This,Mode)	\
    ( (This)->lpVtbl -> put_Mode(This,Mode) ) 

#define ISolution_get_Frequency(This,Frequency)	\
    ( (This)->lpVtbl -> get_Frequency(This,Frequency) ) 

#define ISolution_put_Frequency(This,Frequency)	\
    ( (This)->lpVtbl -> put_Frequency(This,Frequency) ) 

#define ISolution_get_Hour(This,Hour)	\
    ( (This)->lpVtbl -> get_Hour(This,Hour) ) 

#define ISolution_put_Hour(This,Hour)	\
    ( (This)->lpVtbl -> put_Hour(This,Hour) ) 

#define ISolution_get_Seconds(This,Seconds)	\
    ( (This)->lpVtbl -> get_Seconds(This,Seconds) ) 

#define ISolution_put_Seconds(This,Seconds)	\
    ( (This)->lpVtbl -> put_Seconds(This,Seconds) ) 

#define ISolution_get_StepSize(This,StepSize)	\
    ( (This)->lpVtbl -> get_StepSize(This,StepSize) ) 

#define ISolution_put_StepSize(This,StepSize)	\
    ( (This)->lpVtbl -> put_StepSize(This,StepSize) ) 

#define ISolution_get_Year(This,Year)	\
    ( (This)->lpVtbl -> get_Year(This,Year) ) 

#define ISolution_put_Year(This,Year)	\
    ( (This)->lpVtbl -> put_Year(This,Year) ) 

#define ISolution_get_LoadMult(This,LoadMult)	\
    ( (This)->lpVtbl -> get_LoadMult(This,LoadMult) ) 

#define ISolution_put_LoadMult(This,LoadMult)	\
    ( (This)->lpVtbl -> put_LoadMult(This,LoadMult) ) 

#define ISolution_get_Iterations(This,Iterations)	\
    ( (This)->lpVtbl -> get_Iterations(This,Iterations) ) 

#define ISolution_get_MaxIterations(This,MaxIterations)	\
    ( (This)->lpVtbl -> get_MaxIterations(This,MaxIterations) ) 

#define ISolution_put_MaxIterations(This,MaxIterations)	\
    ( (This)->lpVtbl -> put_MaxIterations(This,MaxIterations) ) 

#define ISolution_get_Tolerance(This,Tolerance)	\
    ( (This)->lpVtbl -> get_Tolerance(This,Tolerance) ) 

#define ISolution_put_Tolerance(This,Tolerance)	\
    ( (This)->lpVtbl -> put_Tolerance(This,Tolerance) ) 

#define ISolution_get_Number(This,Number)	\
    ( (This)->lpVtbl -> get_Number(This,Number) ) 

#define ISolution_put_Number(This,Number)	\
    ( (This)->lpVtbl -> put_Number(This,Number) ) 

#define ISolution_get_Random(This,Random)	\
    ( (This)->lpVtbl -> get_Random(This,Random) ) 

#define ISolution_put_Random(This,Random)	\
    ( (This)->lpVtbl -> put_Random(This,Random) ) 

#define ISolution_get_ModeID(This,Value)	\
    ( (This)->lpVtbl -> get_ModeID(This,Value) ) 

#define ISolution_get_LoadModel(This,Value)	\
    ( (This)->lpVtbl -> get_LoadModel(This,Value) ) 

#define ISolution_put_LoadModel(This,Value)	\
    ( (This)->lpVtbl -> put_LoadModel(This,Value) ) 

#define ISolution_get_LDCurve(This,Value)	\
    ( (This)->lpVtbl -> get_LDCurve(This,Value) ) 

#define ISolution_put_LDCurve(This,Value)	\
    ( (This)->lpVtbl -> put_LDCurve(This,Value) ) 

#define ISolution_get_pctGrowth(This,Value)	\
    ( (This)->lpVtbl -> get_pctGrowth(This,Value) ) 

#define ISolution_put_pctGrowth(This,Value)	\
    ( (This)->lpVtbl -> put_pctGrowth(This,Value) ) 

#define ISolution_get_AddType(This,Value)	\
    ( (This)->lpVtbl -> get_AddType(This,Value) ) 

#define ISolution_put_AddType(This,Value)	\
    ( (This)->lpVtbl -> put_AddType(This,Value) ) 

#define ISolution_get_GenkW(This,Value)	\
    ( (This)->lpVtbl -> get_GenkW(This,Value) ) 

#define ISolution_put_GenkW(This,Value)	\
    ( (This)->lpVtbl -> put_GenkW(This,Value) ) 

#define ISolution_get_GenPF(This,Value)	\
    ( (This)->lpVtbl -> get_GenPF(This,Value) ) 

#define ISolution_put_GenPF(This,Value)	\
    ( (This)->lpVtbl -> put_GenPF(This,Value) ) 

#define ISolution_get_Capkvar(This,Value)	\
    ( (This)->lpVtbl -> get_Capkvar(This,Value) ) 

#define ISolution_put_Capkvar(This,Value)	\
    ( (This)->lpVtbl -> put_Capkvar(This,Value) ) 

#define ISolution_get_Algorithm(This,Value)	\
    ( (This)->lpVtbl -> get_Algorithm(This,Value) ) 

#define ISolution_put_Algorithm(This,Value)	\
    ( (This)->lpVtbl -> put_Algorithm(This,Value) ) 

#define ISolution_get_ControlMode(This,Value)	\
    ( (This)->lpVtbl -> get_ControlMode(This,Value) ) 

#define ISolution_put_ControlMode(This,Value)	\
    ( (This)->lpVtbl -> put_ControlMode(This,Value) ) 

#define ISolution_get_GenMult(This,Value)	\
    ( (This)->lpVtbl -> get_GenMult(This,Value) ) 

#define ISolution_put_GenMult(This,Value)	\
    ( (This)->lpVtbl -> put_GenMult(This,Value) ) 

#define ISolution_get_DefaultDaily(This,Value)	\
    ( (This)->lpVtbl -> get_DefaultDaily(This,Value) ) 

#define ISolution_put_DefaultDaily(This,Value)	\
    ( (This)->lpVtbl -> put_DefaultDaily(This,Value) ) 

#define ISolution_get_DefaultYearly(This,Value)	\
    ( (This)->lpVtbl -> get_DefaultYearly(This,Value) ) 

#define ISolution_put_DefaultYearly(This,Value)	\
    ( (This)->lpVtbl -> put_DefaultYearly(This,Value) ) 

#define ISolution_get_EventLog(This,Value)	\
    ( (This)->lpVtbl -> get_EventLog(This,Value) ) 

#define ISolution_get_dblHour(This,Value)	\
    ( (This)->lpVtbl -> get_dblHour(This,Value) ) 

#define ISolution_put_dblHour(This,Value)	\
    ( (This)->lpVtbl -> put_dblHour(This,Value) ) 

#define ISolution_put_StepsizeMin(This,rhs)	\
    ( (This)->lpVtbl -> put_StepsizeMin(This,rhs) ) 

#define ISolution_put_StepsizeHr(This,rhs)	\
    ( (This)->lpVtbl -> put_StepsizeHr(This,rhs) ) 

#define ISolution_get_ControlIterations(This,Value)	\
    ( (This)->lpVtbl -> get_ControlIterations(This,Value) ) 

#define ISolution_put_ControlIterations(This,Value)	\
    ( (This)->lpVtbl -> put_ControlIterations(This,Value) ) 

#define ISolution_get_MaxControlIterations(This,Value)	\
    ( (This)->lpVtbl -> get_MaxControlIterations(This,Value) ) 

#define ISolution_put_MaxControlIterations(This,Value)	\
    ( (This)->lpVtbl -> put_MaxControlIterations(This,Value) ) 

#define ISolution_Sample_DoControlActions(This)	\
    ( (This)->lpVtbl -> Sample_DoControlActions(This) ) 

#define ISolution_CheckFaultStatus(This)	\
    ( (This)->lpVtbl -> CheckFaultStatus(This) ) 

#define ISolution_SolveSnap(This)	\
    ( (This)->lpVtbl -> SolveSnap(This) ) 

#define ISolution_SolveDirect(This)	\
    ( (This)->lpVtbl -> SolveDirect(This) ) 

#define ISolution_SolvePflow(This)	\
    ( (This)->lpVtbl -> SolvePflow(This) ) 

#define ISolution_SolveNoControl(This)	\
    ( (This)->lpVtbl -> SolveNoControl(This) ) 

#define ISolution_SolvePlusControl(This)	\
    ( (This)->lpVtbl -> SolvePlusControl(This) ) 

#define ISolution_InitSnap(This)	\
    ( (This)->lpVtbl -> InitSnap(This) ) 

#define ISolution_CheckControls(This)	\
    ( (This)->lpVtbl -> CheckControls(This) ) 

#define ISolution_SampleControlDevices(This)	\
    ( (This)->lpVtbl -> SampleControlDevices(This) ) 

#define ISolution_DoControlActions(This)	\
    ( (This)->lpVtbl -> DoControlActions(This) ) 

#define ISolution_BuildYMatrix(This,BuildOption,AllocateVI)	\
    ( (This)->lpVtbl -> BuildYMatrix(This,BuildOption,AllocateVI) ) 

#define ISolution_get_SystemYChanged(This,Value)	\
    ( (This)->lpVtbl -> get_SystemYChanged(This,Value) ) 

#define ISolution_get_Converged(This,Value)	\
    ( (This)->lpVtbl -> get_Converged(This,Value) ) 

#define ISolution_put_Converged(This,Value)	\
    ( (This)->lpVtbl -> put_Converged(This,Value) ) 

#define ISolution_get_Totaliterations(This,Value)	\
    ( (This)->lpVtbl -> get_Totaliterations(This,Value) ) 

#define ISolution_get_MostIterationsDone(This,Value)	\
    ( (This)->lpVtbl -> get_MostIterationsDone(This,Value) ) 

#define ISolution_get_ControlActionsDone(This,Value)	\
    ( (This)->lpVtbl -> get_ControlActionsDone(This,Value) ) 

#define ISolution_put_ControlActionsDone(This,Value)	\
    ( (This)->lpVtbl -> put_ControlActionsDone(This,Value) ) 

#define ISolution_FinishTimeStep(This)	\
    ( (This)->lpVtbl -> FinishTimeStep(This) ) 

#define ISolution_Cleanup(This)	\
    ( (This)->lpVtbl -> Cleanup(This) ) 

#define ISolution_get_Total_Time(This,Value)	\
    ( (This)->lpVtbl -> get_Total_Time(This,Value) ) 

#define ISolution_put_Total_Time(This,Value)	\
    ( (This)->lpVtbl -> put_Total_Time(This,Value) ) 

#define ISolution_get_Process_Time(This,Value)	\
    ( (This)->lpVtbl -> get_Process_Time(This,Value) ) 

#define ISolution_get_Time_of_Step(This,Value)	\
    ( (This)->lpVtbl -> get_Time_of_Step(This,Value) ) 

#define ISolution_get_IntervalHrs(This,Value)	\
    ( (This)->lpVtbl -> get_IntervalHrs(This,Value) ) 

#define ISolution_put_IntervalHrs(This,Value)	\
    ( (This)->lpVtbl -> put_IntervalHrs(This,Value) ) 

#define ISolution_SolveAll(This)	\
    ( (This)->lpVtbl -> SolveAll(This) ) 

#define ISolution_get_IncMatrix(This,Value)	\
    ( (This)->lpVtbl -> get_IncMatrix(This,Value) ) 

#define ISolution_get_IncMatrixRows(This,Value)	\
    ( (This)->lpVtbl -> get_IncMatrixRows(This,Value) ) 

#define ISolution_get_IncMatrixCols(This,Value)	\
    ( (This)->lpVtbl -> get_IncMatrixCols(This,Value) ) 

#define ISolution_get_BusLevels(This,Value)	\
    ( (This)->lpVtbl -> get_BusLevels(This,Value) ) 

#define ISolution_get_Laplacian(This,Value)	\
    ( (This)->lpVtbl -> get_Laplacian(This,Value) ) 

#define ISolution_get_MinIterations(This,Value)	\
    ( (This)->lpVtbl -> get_MinIterations(This,Value) ) 

#define ISolution_put_MinIterations(This,Value)	\
    ( (This)->lpVtbl -> put_MinIterations(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ISolution_INTERFACE_DEFINED__ */


#ifndef __IMonitors_INTERFACE_DEFINED__
#define __IMonitors_INTERFACE_DEFINED__

/* interface IMonitors */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IMonitors;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B3FA-18C2-11F0-A417-C87F5452571C")
    IMonitors : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ResetAll( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Sample( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Save( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Show( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_FileName( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Mode( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Mode( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ByteStream( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SampleCount( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SampleAll( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SaveAll( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Process( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ProcessAll( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_FileVersion( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RecordSize( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Header( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_dblHour( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_dblFreq( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Channel( 
            /* [in] */ long Index,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumChannels( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Element( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Element( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Terminal( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Terminal( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IMonitorsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IMonitors * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IMonitors * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IMonitors * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IMonitors * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IMonitors * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IMonitors, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IMonitors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IMonitors, ResetAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ResetAll )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IMonitors, Sample)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Sample )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IMonitors, Save)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Save )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IMonitors, Show)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Show )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IMonitors, get_FileName)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_FileName )( 
            IMonitors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_Mode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Mode )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, put_Mode)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Mode )( 
            IMonitors * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IMonitors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IMonitors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_ByteStream)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ByteStream )( 
            IMonitors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_SampleCount)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SampleCount )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, SampleAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SampleAll )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IMonitors, SaveAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SaveAll )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IMonitors, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, Process)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Process )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IMonitors, ProcessAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ProcessAll )( 
            IMonitors * This);
        
        DECLSPEC_XFGVIRT(IMonitors, get_FileVersion)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_FileVersion )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_RecordSize)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RecordSize )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_Header)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Header )( 
            IMonitors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_dblHour)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_dblHour )( 
            IMonitors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_dblFreq)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_dblFreq )( 
            IMonitors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_Channel)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Channel )( 
            IMonitors * This,
            /* [in] */ long Index,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_NumChannels)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumChannels )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_Element)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Element )( 
            IMonitors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, put_Element)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Element )( 
            IMonitors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_Terminal)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Terminal )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, put_Terminal)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Terminal )( 
            IMonitors * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IMonitors, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IMonitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMonitors, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IMonitors * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } IMonitorsVtbl;

    interface IMonitors
    {
        CONST_VTBL struct IMonitorsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IMonitors_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IMonitors_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IMonitors_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IMonitors_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IMonitors_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IMonitors_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IMonitors_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IMonitors_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IMonitors_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IMonitors_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IMonitors_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define IMonitors_ResetAll(This)	\
    ( (This)->lpVtbl -> ResetAll(This) ) 

#define IMonitors_Sample(This)	\
    ( (This)->lpVtbl -> Sample(This) ) 

#define IMonitors_Save(This)	\
    ( (This)->lpVtbl -> Save(This) ) 

#define IMonitors_Show(This)	\
    ( (This)->lpVtbl -> Show(This) ) 

#define IMonitors_get_FileName(This,Value)	\
    ( (This)->lpVtbl -> get_FileName(This,Value) ) 

#define IMonitors_get_Mode(This,Value)	\
    ( (This)->lpVtbl -> get_Mode(This,Value) ) 

#define IMonitors_put_Mode(This,Value)	\
    ( (This)->lpVtbl -> put_Mode(This,Value) ) 

#define IMonitors_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IMonitors_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IMonitors_get_ByteStream(This,Value)	\
    ( (This)->lpVtbl -> get_ByteStream(This,Value) ) 

#define IMonitors_get_SampleCount(This,Value)	\
    ( (This)->lpVtbl -> get_SampleCount(This,Value) ) 

#define IMonitors_SampleAll(This)	\
    ( (This)->lpVtbl -> SampleAll(This) ) 

#define IMonitors_SaveAll(This)	\
    ( (This)->lpVtbl -> SaveAll(This) ) 

#define IMonitors_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IMonitors_Process(This)	\
    ( (This)->lpVtbl -> Process(This) ) 

#define IMonitors_ProcessAll(This)	\
    ( (This)->lpVtbl -> ProcessAll(This) ) 

#define IMonitors_get_FileVersion(This,Value)	\
    ( (This)->lpVtbl -> get_FileVersion(This,Value) ) 

#define IMonitors_get_RecordSize(This,Value)	\
    ( (This)->lpVtbl -> get_RecordSize(This,Value) ) 

#define IMonitors_get_Header(This,Value)	\
    ( (This)->lpVtbl -> get_Header(This,Value) ) 

#define IMonitors_get_dblHour(This,Value)	\
    ( (This)->lpVtbl -> get_dblHour(This,Value) ) 

#define IMonitors_get_dblFreq(This,Value)	\
    ( (This)->lpVtbl -> get_dblFreq(This,Value) ) 

#define IMonitors_get_Channel(This,Index,Value)	\
    ( (This)->lpVtbl -> get_Channel(This,Index,Value) ) 

#define IMonitors_get_NumChannels(This,Value)	\
    ( (This)->lpVtbl -> get_NumChannels(This,Value) ) 

#define IMonitors_get_Element(This,Value)	\
    ( (This)->lpVtbl -> get_Element(This,Value) ) 

#define IMonitors_put_Element(This,Value)	\
    ( (This)->lpVtbl -> put_Element(This,Value) ) 

#define IMonitors_get_Terminal(This,Value)	\
    ( (This)->lpVtbl -> get_Terminal(This,Value) ) 

#define IMonitors_put_Terminal(This,Value)	\
    ( (This)->lpVtbl -> put_Terminal(This,Value) ) 

#define IMonitors_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IMonitors_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IMonitors_INTERFACE_DEFINED__ */


#ifndef __IMeters_INTERFACE_DEFINED__
#define __IMeters_INTERFACE_DEFINED__

/* interface IMeters */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IMeters;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B436-18C2-11F0-A417-C87F5452571C")
    IMeters : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterValues( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ResetAll( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Sample( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Save( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Totals( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Peakcurrent( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Peakcurrent( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CalcCurrent( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CalcCurrent( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllocFactors( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AllocFactors( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MeteredElement( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MeteredElement( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MeteredTerminal( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MeteredTerminal( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DIFilesAreOpen( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SampleAll( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SaveAll( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE OpenAllDIFiles( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE CloseAllDIFiles( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CountEndElements( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllEndElements( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllBranchesInZone( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CountBranches( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SAIFI( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SequenceIndex( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SequenceIndex( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SAIFIKW( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DoReliabilityCalc( 
            /* [in] */ VARIANT_BOOL AssumeRestoration) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SeqListSize( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TotalCustomers( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SAIDI( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CustInterrupts( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumSections( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetActiveSection( 
            /* [in] */ long SectIdx) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_OCPDeviceType( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumSectionCustomers( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumSectionBranches( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AvgRepairTime( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_FaultRateXRepairHrs( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SumBranchFltRates( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SectSeqIdx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SectTotalCust( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ZonePCE( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IMetersVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IMeters * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IMeters * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IMeters * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IMeters * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IMeters * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IMeters, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_RegisterNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterNames )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_RegisterValues)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterValues )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IMeters, ResetAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ResetAll )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IMeters, Sample)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Sample )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IMeters, Save)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Save )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IMeters, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IMeters * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IMeters, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IMeters * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_Totals)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Totals )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_Peakcurrent)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Peakcurrent )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, put_Peakcurrent)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Peakcurrent )( 
            IMeters * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_CalcCurrent)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CalcCurrent )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, put_CalcCurrent)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CalcCurrent )( 
            IMeters * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_AllocFactors)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllocFactors )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, put_AllocFactors)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AllocFactors )( 
            IMeters * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_MeteredElement)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MeteredElement )( 
            IMeters * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IMeters, put_MeteredElement)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MeteredElement )( 
            IMeters * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_MeteredTerminal)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MeteredTerminal )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, put_MeteredTerminal)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MeteredTerminal )( 
            IMeters * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_DIFilesAreOpen)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DIFilesAreOpen )( 
            IMeters * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IMeters, SampleAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SampleAll )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IMeters, SaveAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SaveAll )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IMeters, OpenAllDIFiles)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *OpenAllDIFiles )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IMeters, CloseAllDIFiles)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *CloseAllDIFiles )( 
            IMeters * This);
        
        DECLSPEC_XFGVIRT(IMeters, get_CountEndElements)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CountEndElements )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_AllEndElements)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllEndElements )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_AllBranchesInZone)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllBranchesInZone )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_CountBranches)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CountBranches )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_SAIFI)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SAIFI )( 
            IMeters * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_SequenceIndex)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SequenceIndex )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, put_SequenceIndex)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SequenceIndex )( 
            IMeters * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_SAIFIKW)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SAIFIKW )( 
            IMeters * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IMeters, DoReliabilityCalc)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DoReliabilityCalc )( 
            IMeters * This,
            /* [in] */ VARIANT_BOOL AssumeRestoration);
        
        DECLSPEC_XFGVIRT(IMeters, get_SeqListSize)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SeqListSize )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_TotalCustomers)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TotalCustomers )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_SAIDI)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SAIDI )( 
            IMeters * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_CustInterrupts)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CustInterrupts )( 
            IMeters * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_NumSections)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumSections )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, SetActiveSection)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetActiveSection )( 
            IMeters * This,
            /* [in] */ long SectIdx);
        
        DECLSPEC_XFGVIRT(IMeters, get_OCPDeviceType)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_OCPDeviceType )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_NumSectionCustomers)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumSectionCustomers )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_NumSectionBranches)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumSectionBranches )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_AvgRepairTime)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AvgRepairTime )( 
            IMeters * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_FaultRateXRepairHrs)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_FaultRateXRepairHrs )( 
            IMeters * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_SumBranchFltRates)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SumBranchFltRates )( 
            IMeters * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_SectSeqIdx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SectSeqIdx )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_SectTotalCust)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SectTotalCust )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_ZonePCE)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ZonePCE )( 
            IMeters * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IMeters, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IMeters * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IMeters, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IMeters * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } IMetersVtbl;

    interface IMeters
    {
        CONST_VTBL struct IMetersVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IMeters_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IMeters_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IMeters_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IMeters_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IMeters_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IMeters_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IMeters_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IMeters_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IMeters_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IMeters_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IMeters_get_RegisterNames(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterNames(This,Value) ) 

#define IMeters_get_RegisterValues(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterValues(This,Value) ) 

#define IMeters_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define IMeters_ResetAll(This)	\
    ( (This)->lpVtbl -> ResetAll(This) ) 

#define IMeters_Sample(This)	\
    ( (This)->lpVtbl -> Sample(This) ) 

#define IMeters_Save(This)	\
    ( (This)->lpVtbl -> Save(This) ) 

#define IMeters_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IMeters_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IMeters_get_Totals(This,Value)	\
    ( (This)->lpVtbl -> get_Totals(This,Value) ) 

#define IMeters_get_Peakcurrent(This,Value)	\
    ( (This)->lpVtbl -> get_Peakcurrent(This,Value) ) 

#define IMeters_put_Peakcurrent(This,Value)	\
    ( (This)->lpVtbl -> put_Peakcurrent(This,Value) ) 

#define IMeters_get_CalcCurrent(This,Value)	\
    ( (This)->lpVtbl -> get_CalcCurrent(This,Value) ) 

#define IMeters_put_CalcCurrent(This,Value)	\
    ( (This)->lpVtbl -> put_CalcCurrent(This,Value) ) 

#define IMeters_get_AllocFactors(This,Value)	\
    ( (This)->lpVtbl -> get_AllocFactors(This,Value) ) 

#define IMeters_put_AllocFactors(This,Value)	\
    ( (This)->lpVtbl -> put_AllocFactors(This,Value) ) 

#define IMeters_get_MeteredElement(This,Value)	\
    ( (This)->lpVtbl -> get_MeteredElement(This,Value) ) 

#define IMeters_put_MeteredElement(This,Value)	\
    ( (This)->lpVtbl -> put_MeteredElement(This,Value) ) 

#define IMeters_get_MeteredTerminal(This,Value)	\
    ( (This)->lpVtbl -> get_MeteredTerminal(This,Value) ) 

#define IMeters_put_MeteredTerminal(This,Value)	\
    ( (This)->lpVtbl -> put_MeteredTerminal(This,Value) ) 

#define IMeters_get_DIFilesAreOpen(This,Value)	\
    ( (This)->lpVtbl -> get_DIFilesAreOpen(This,Value) ) 

#define IMeters_SampleAll(This)	\
    ( (This)->lpVtbl -> SampleAll(This) ) 

#define IMeters_SaveAll(This)	\
    ( (This)->lpVtbl -> SaveAll(This) ) 

#define IMeters_OpenAllDIFiles(This)	\
    ( (This)->lpVtbl -> OpenAllDIFiles(This) ) 

#define IMeters_CloseAllDIFiles(This)	\
    ( (This)->lpVtbl -> CloseAllDIFiles(This) ) 

#define IMeters_get_CountEndElements(This,Value)	\
    ( (This)->lpVtbl -> get_CountEndElements(This,Value) ) 

#define IMeters_get_AllEndElements(This,Value)	\
    ( (This)->lpVtbl -> get_AllEndElements(This,Value) ) 

#define IMeters_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IMeters_get_AllBranchesInZone(This,Value)	\
    ( (This)->lpVtbl -> get_AllBranchesInZone(This,Value) ) 

#define IMeters_get_CountBranches(This,Value)	\
    ( (This)->lpVtbl -> get_CountBranches(This,Value) ) 

#define IMeters_get_SAIFI(This,Value)	\
    ( (This)->lpVtbl -> get_SAIFI(This,Value) ) 

#define IMeters_get_SequenceIndex(This,Value)	\
    ( (This)->lpVtbl -> get_SequenceIndex(This,Value) ) 

#define IMeters_put_SequenceIndex(This,Value)	\
    ( (This)->lpVtbl -> put_SequenceIndex(This,Value) ) 

#define IMeters_get_SAIFIKW(This,Value)	\
    ( (This)->lpVtbl -> get_SAIFIKW(This,Value) ) 

#define IMeters_DoReliabilityCalc(This,AssumeRestoration)	\
    ( (This)->lpVtbl -> DoReliabilityCalc(This,AssumeRestoration) ) 

#define IMeters_get_SeqListSize(This,Value)	\
    ( (This)->lpVtbl -> get_SeqListSize(This,Value) ) 

#define IMeters_get_TotalCustomers(This,Value)	\
    ( (This)->lpVtbl -> get_TotalCustomers(This,Value) ) 

#define IMeters_get_SAIDI(This,Value)	\
    ( (This)->lpVtbl -> get_SAIDI(This,Value) ) 

#define IMeters_get_CustInterrupts(This,Value)	\
    ( (This)->lpVtbl -> get_CustInterrupts(This,Value) ) 

#define IMeters_get_NumSections(This,Value)	\
    ( (This)->lpVtbl -> get_NumSections(This,Value) ) 

#define IMeters_SetActiveSection(This,SectIdx)	\
    ( (This)->lpVtbl -> SetActiveSection(This,SectIdx) ) 

#define IMeters_get_OCPDeviceType(This,Value)	\
    ( (This)->lpVtbl -> get_OCPDeviceType(This,Value) ) 

#define IMeters_get_NumSectionCustomers(This,Value)	\
    ( (This)->lpVtbl -> get_NumSectionCustomers(This,Value) ) 

#define IMeters_get_NumSectionBranches(This,Value)	\
    ( (This)->lpVtbl -> get_NumSectionBranches(This,Value) ) 

#define IMeters_get_AvgRepairTime(This,Value)	\
    ( (This)->lpVtbl -> get_AvgRepairTime(This,Value) ) 

#define IMeters_get_FaultRateXRepairHrs(This,Value)	\
    ( (This)->lpVtbl -> get_FaultRateXRepairHrs(This,Value) ) 

#define IMeters_get_SumBranchFltRates(This,Value)	\
    ( (This)->lpVtbl -> get_SumBranchFltRates(This,Value) ) 

#define IMeters_get_SectSeqIdx(This,Value)	\
    ( (This)->lpVtbl -> get_SectSeqIdx(This,Value) ) 

#define IMeters_get_SectTotalCust(This,Value)	\
    ( (This)->lpVtbl -> get_SectTotalCust(This,Value) ) 

#define IMeters_get_ZonePCE(This,Value)	\
    ( (This)->lpVtbl -> get_ZonePCE(This,Value) ) 

#define IMeters_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IMeters_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IMeters_INTERFACE_DEFINED__ */


#ifndef __IGenerators_INTERFACE_DEFINED__
#define __IGenerators_INTERFACE_DEFINED__

/* interface IGenerators */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IGenerators;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B490-18C2-11F0-A417-C87F5452571C")
    IGenerators : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterValues( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ForcedON( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ForcedON( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kV( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kV( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kW( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kW( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kvar( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kvar( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PF( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PF( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Phases( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Phases( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Model( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Model( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kVArated( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kVArated( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vmaxpu( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Vmaxpu( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vminpu( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Vminpu( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Bus1( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Bus1( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Class( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Class( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_daily( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_daily( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_duty( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_duty( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsDelta( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsDelta( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kva( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kva( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Status( 
            /* [retval][out] */ LoadStatus *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Status( 
            /* [in] */ LoadStatus Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Yearly( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Yearly( 
            /* [in] */ BSTR Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IGeneratorsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IGenerators * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IGenerators * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IGenerators * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IGenerators * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IGenerators * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IGenerators * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IGenerators * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IGenerators, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IGenerators * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_RegisterNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterNames )( 
            IGenerators * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_RegisterValues)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterValues )( 
            IGenerators * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IGenerators * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IGenerators * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_ForcedON)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ForcedON )( 
            IGenerators * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_ForcedON)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ForcedON )( 
            IGenerators * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IGenerators * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IGenerators * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_kV)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kV )( 
            IGenerators * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_kV)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kV )( 
            IGenerators * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_kW)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kW )( 
            IGenerators * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_kW)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kW )( 
            IGenerators * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_kvar)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kvar )( 
            IGenerators * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_kvar)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kvar )( 
            IGenerators * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_PF)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PF )( 
            IGenerators * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_PF)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PF )( 
            IGenerators * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Phases)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Phases )( 
            IGenerators * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_Phases)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Phases )( 
            IGenerators * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IGenerators * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IGenerators * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IGenerators * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Model)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Model )( 
            IGenerators * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_Model)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Model )( 
            IGenerators * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_kVArated)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kVArated )( 
            IGenerators * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_kVArated)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kVArated )( 
            IGenerators * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Vmaxpu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vmaxpu )( 
            IGenerators * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_Vmaxpu)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Vmaxpu )( 
            IGenerators * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Vminpu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vminpu )( 
            IGenerators * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_Vminpu)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Vminpu )( 
            IGenerators * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Bus1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Bus1 )( 
            IGenerators * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_Bus1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Bus1 )( 
            IGenerators * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Class)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Class )( 
            IGenerators * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_Class)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Class )( 
            IGenerators * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_daily)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_daily )( 
            IGenerators * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_daily)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_daily )( 
            IGenerators * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_duty)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_duty )( 
            IGenerators * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_duty)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_duty )( 
            IGenerators * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_IsDelta)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsDelta )( 
            IGenerators * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_IsDelta)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsDelta )( 
            IGenerators * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_kva)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kva )( 
            IGenerators * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_kva)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kva )( 
            IGenerators * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Status)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Status )( 
            IGenerators * This,
            /* [retval][out] */ LoadStatus *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_Status)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Status )( 
            IGenerators * This,
            /* [in] */ LoadStatus Value);
        
        DECLSPEC_XFGVIRT(IGenerators, get_Yearly)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Yearly )( 
            IGenerators * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IGenerators, put_Yearly)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Yearly )( 
            IGenerators * This,
            /* [in] */ BSTR Value);
        
        END_INTERFACE
    } IGeneratorsVtbl;

    interface IGenerators
    {
        CONST_VTBL struct IGeneratorsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IGenerators_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IGenerators_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IGenerators_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IGenerators_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IGenerators_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IGenerators_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IGenerators_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IGenerators_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IGenerators_get_RegisterNames(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterNames(This,Value) ) 

#define IGenerators_get_RegisterValues(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterValues(This,Value) ) 

#define IGenerators_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IGenerators_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IGenerators_get_ForcedON(This,Value)	\
    ( (This)->lpVtbl -> get_ForcedON(This,Value) ) 

#define IGenerators_put_ForcedON(This,Value)	\
    ( (This)->lpVtbl -> put_ForcedON(This,Value) ) 

#define IGenerators_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IGenerators_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IGenerators_get_kV(This,Value)	\
    ( (This)->lpVtbl -> get_kV(This,Value) ) 

#define IGenerators_put_kV(This,Value)	\
    ( (This)->lpVtbl -> put_kV(This,Value) ) 

#define IGenerators_get_kW(This,Value)	\
    ( (This)->lpVtbl -> get_kW(This,Value) ) 

#define IGenerators_put_kW(This,Value)	\
    ( (This)->lpVtbl -> put_kW(This,Value) ) 

#define IGenerators_get_kvar(This,Value)	\
    ( (This)->lpVtbl -> get_kvar(This,Value) ) 

#define IGenerators_put_kvar(This,Value)	\
    ( (This)->lpVtbl -> put_kvar(This,Value) ) 

#define IGenerators_get_PF(This,Value)	\
    ( (This)->lpVtbl -> get_PF(This,Value) ) 

#define IGenerators_put_PF(This,Value)	\
    ( (This)->lpVtbl -> put_PF(This,Value) ) 

#define IGenerators_get_Phases(This,Value)	\
    ( (This)->lpVtbl -> get_Phases(This,Value) ) 

#define IGenerators_put_Phases(This,Value)	\
    ( (This)->lpVtbl -> put_Phases(This,Value) ) 

#define IGenerators_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IGenerators_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IGenerators_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define IGenerators_get_Model(This,Value)	\
    ( (This)->lpVtbl -> get_Model(This,Value) ) 

#define IGenerators_put_Model(This,Value)	\
    ( (This)->lpVtbl -> put_Model(This,Value) ) 

#define IGenerators_get_kVArated(This,Value)	\
    ( (This)->lpVtbl -> get_kVArated(This,Value) ) 

#define IGenerators_put_kVArated(This,Value)	\
    ( (This)->lpVtbl -> put_kVArated(This,Value) ) 

#define IGenerators_get_Vmaxpu(This,Value)	\
    ( (This)->lpVtbl -> get_Vmaxpu(This,Value) ) 

#define IGenerators_put_Vmaxpu(This,Value)	\
    ( (This)->lpVtbl -> put_Vmaxpu(This,Value) ) 

#define IGenerators_get_Vminpu(This,Value)	\
    ( (This)->lpVtbl -> get_Vminpu(This,Value) ) 

#define IGenerators_put_Vminpu(This,Value)	\
    ( (This)->lpVtbl -> put_Vminpu(This,Value) ) 

#define IGenerators_get_Bus1(This,Value)	\
    ( (This)->lpVtbl -> get_Bus1(This,Value) ) 

#define IGenerators_put_Bus1(This,Value)	\
    ( (This)->lpVtbl -> put_Bus1(This,Value) ) 

#define IGenerators_get_Class(This,Value)	\
    ( (This)->lpVtbl -> get_Class(This,Value) ) 

#define IGenerators_put_Class(This,Value)	\
    ( (This)->lpVtbl -> put_Class(This,Value) ) 

#define IGenerators_get_daily(This,Value)	\
    ( (This)->lpVtbl -> get_daily(This,Value) ) 

#define IGenerators_put_daily(This,Value)	\
    ( (This)->lpVtbl -> put_daily(This,Value) ) 

#define IGenerators_get_duty(This,Value)	\
    ( (This)->lpVtbl -> get_duty(This,Value) ) 

#define IGenerators_put_duty(This,Value)	\
    ( (This)->lpVtbl -> put_duty(This,Value) ) 

#define IGenerators_get_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> get_IsDelta(This,Value) ) 

#define IGenerators_put_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> put_IsDelta(This,Value) ) 

#define IGenerators_get_kva(This,Value)	\
    ( (This)->lpVtbl -> get_kva(This,Value) ) 

#define IGenerators_put_kva(This,Value)	\
    ( (This)->lpVtbl -> put_kva(This,Value) ) 

#define IGenerators_get_Status(This,Value)	\
    ( (This)->lpVtbl -> get_Status(This,Value) ) 

#define IGenerators_put_Status(This,Value)	\
    ( (This)->lpVtbl -> put_Status(This,Value) ) 

#define IGenerators_get_Yearly(This,Value)	\
    ( (This)->lpVtbl -> get_Yearly(This,Value) ) 

#define IGenerators_put_Yearly(This,Value)	\
    ( (This)->lpVtbl -> put_Yearly(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IGenerators_INTERFACE_DEFINED__ */


#ifndef __IDSSProgress_INTERFACE_DEFINED__
#define __IDSSProgress_INTERFACE_DEFINED__

/* interface IDSSProgress */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IDSSProgress;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B4CC-18C2-11F0-A417-C87F5452571C")
    IDSSProgress : public IDispatch
    {
    public:
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PctProgress( 
            /* [in] */ long rhs) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Caption( 
            /* [in] */ BSTR rhs) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Show( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Close( void) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IDSSProgressVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IDSSProgress * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IDSSProgress * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IDSSProgress * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IDSSProgress * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IDSSProgress * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IDSSProgress * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IDSSProgress * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IDSSProgress, put_PctProgress)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PctProgress )( 
            IDSSProgress * This,
            /* [in] */ long rhs);
        
        DECLSPEC_XFGVIRT(IDSSProgress, put_Caption)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Caption )( 
            IDSSProgress * This,
            /* [in] */ BSTR rhs);
        
        DECLSPEC_XFGVIRT(IDSSProgress, Show)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Show )( 
            IDSSProgress * This);
        
        DECLSPEC_XFGVIRT(IDSSProgress, Close)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Close )( 
            IDSSProgress * This);
        
        END_INTERFACE
    } IDSSProgressVtbl;

    interface IDSSProgress
    {
        CONST_VTBL struct IDSSProgressVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IDSSProgress_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IDSSProgress_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IDSSProgress_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IDSSProgress_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IDSSProgress_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IDSSProgress_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IDSSProgress_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IDSSProgress_put_PctProgress(This,rhs)	\
    ( (This)->lpVtbl -> put_PctProgress(This,rhs) ) 

#define IDSSProgress_put_Caption(This,rhs)	\
    ( (This)->lpVtbl -> put_Caption(This,rhs) ) 

#define IDSSProgress_Show(This)	\
    ( (This)->lpVtbl -> Show(This) ) 

#define IDSSProgress_Close(This)	\
    ( (This)->lpVtbl -> Close(This) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IDSSProgress_INTERFACE_DEFINED__ */


#ifndef __ISettings_INTERFACE_DEFINED__
#define __ISettings_INTERFACE_DEFINED__

/* interface ISettings */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ISettings;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B4F4-18C2-11F0-A417-C87F5452571C")
    ISettings : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllowDuplicates( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AllowDuplicates( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ZoneLock( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ZoneLock( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AllocationFactors( 
            /* [in] */ double rhs) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AutoBusList( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AutoBusList( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CktModel( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CktModel( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NormVminpu( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NormVminpu( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NormVmaxpu( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NormVmaxpu( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EmergVminpu( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EmergVminpu( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EmergVmaxpu( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EmergVmaxpu( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_UEweight( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_UEweight( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LossWeight( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LossWeight( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_UEregs( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_UEregs( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LossRegs( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LossRegs( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Trapezoidal( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Trapezoidal( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VoltageBases( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VoltageBases( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ControlTrace( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ControlTrace( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PriceSignal( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PriceSignal( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PriceCurve( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PriceCurve( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllowChangeDir( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AllowChangeDir( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllowDOScmd( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AllowDOScmd( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllowEditor( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AllowEditor( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_COMErrorResults( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_COMErrorResults( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CompatFlags( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CompatFlags( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IterateDisabled( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IterateDisabled( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LoadsTerminalCheck( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LoadsTerminalCheck( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetPropertyNameStyle( 
            /* [in] */ DSSPropertyNameStyle Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SkipFileRegExp( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SkipFileRegExp( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SkipCommands( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SkipCommands( 
            /* [in] */ VARIANT Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ISettingsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ISettings * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ISettings * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ISettings * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ISettings * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ISettings * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ISettings * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ISettings * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ISettings, get_AllowDuplicates)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllowDuplicates )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_AllowDuplicates)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AllowDuplicates )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_ZoneLock)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ZoneLock )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_ZoneLock)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ZoneLock )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_AllocationFactors)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AllocationFactors )( 
            ISettings * This,
            /* [in] */ double rhs);
        
        DECLSPEC_XFGVIRT(ISettings, get_AutoBusList)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AutoBusList )( 
            ISettings * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_AutoBusList)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AutoBusList )( 
            ISettings * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_CktModel)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CktModel )( 
            ISettings * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_CktModel)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CktModel )( 
            ISettings * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_NormVminpu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NormVminpu )( 
            ISettings * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_NormVminpu)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NormVminpu )( 
            ISettings * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_NormVmaxpu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NormVmaxpu )( 
            ISettings * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_NormVmaxpu)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NormVmaxpu )( 
            ISettings * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_EmergVminpu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EmergVminpu )( 
            ISettings * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_EmergVminpu)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EmergVminpu )( 
            ISettings * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_EmergVmaxpu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EmergVmaxpu )( 
            ISettings * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_EmergVmaxpu)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EmergVmaxpu )( 
            ISettings * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_UEweight)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_UEweight )( 
            ISettings * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_UEweight)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_UEweight )( 
            ISettings * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_LossWeight)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LossWeight )( 
            ISettings * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_LossWeight)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LossWeight )( 
            ISettings * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_UEregs)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_UEregs )( 
            ISettings * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_UEregs)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_UEregs )( 
            ISettings * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_LossRegs)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LossRegs )( 
            ISettings * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_LossRegs)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LossRegs )( 
            ISettings * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_Trapezoidal)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Trapezoidal )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_Trapezoidal)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Trapezoidal )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_VoltageBases)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VoltageBases )( 
            ISettings * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_VoltageBases)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VoltageBases )( 
            ISettings * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_ControlTrace)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ControlTrace )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_ControlTrace)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ControlTrace )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_PriceSignal)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PriceSignal )( 
            ISettings * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_PriceSignal)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PriceSignal )( 
            ISettings * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_PriceCurve)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PriceCurve )( 
            ISettings * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_PriceCurve)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PriceCurve )( 
            ISettings * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_AllowChangeDir)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllowChangeDir )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_AllowChangeDir)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AllowChangeDir )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_AllowDOScmd)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllowDOScmd )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_AllowDOScmd)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AllowDOScmd )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_AllowEditor)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllowEditor )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_AllowEditor)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AllowEditor )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_COMErrorResults)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_COMErrorResults )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_COMErrorResults)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_COMErrorResults )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_CompatFlags)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CompatFlags )( 
            ISettings * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_CompatFlags)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CompatFlags )( 
            ISettings * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_IterateDisabled)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IterateDisabled )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_IterateDisabled)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IterateDisabled )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_LoadsTerminalCheck)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LoadsTerminalCheck )( 
            ISettings * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_LoadsTerminalCheck)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LoadsTerminalCheck )( 
            ISettings * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISettings, SetPropertyNameStyle)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetPropertyNameStyle )( 
            ISettings * This,
            /* [in] */ DSSPropertyNameStyle Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_SkipFileRegExp)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SkipFileRegExp )( 
            ISettings * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_SkipFileRegExp)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SkipFileRegExp )( 
            ISettings * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISettings, get_SkipCommands)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SkipCommands )( 
            ISettings * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISettings, put_SkipCommands)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SkipCommands )( 
            ISettings * This,
            /* [in] */ VARIANT Value);
        
        END_INTERFACE
    } ISettingsVtbl;

    interface ISettings
    {
        CONST_VTBL struct ISettingsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ISettings_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ISettings_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ISettings_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ISettings_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ISettings_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ISettings_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ISettings_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ISettings_get_AllowDuplicates(This,Value)	\
    ( (This)->lpVtbl -> get_AllowDuplicates(This,Value) ) 

#define ISettings_put_AllowDuplicates(This,Value)	\
    ( (This)->lpVtbl -> put_AllowDuplicates(This,Value) ) 

#define ISettings_get_ZoneLock(This,Value)	\
    ( (This)->lpVtbl -> get_ZoneLock(This,Value) ) 

#define ISettings_put_ZoneLock(This,Value)	\
    ( (This)->lpVtbl -> put_ZoneLock(This,Value) ) 

#define ISettings_put_AllocationFactors(This,rhs)	\
    ( (This)->lpVtbl -> put_AllocationFactors(This,rhs) ) 

#define ISettings_get_AutoBusList(This,Value)	\
    ( (This)->lpVtbl -> get_AutoBusList(This,Value) ) 

#define ISettings_put_AutoBusList(This,Value)	\
    ( (This)->lpVtbl -> put_AutoBusList(This,Value) ) 

#define ISettings_get_CktModel(This,Value)	\
    ( (This)->lpVtbl -> get_CktModel(This,Value) ) 

#define ISettings_put_CktModel(This,Value)	\
    ( (This)->lpVtbl -> put_CktModel(This,Value) ) 

#define ISettings_get_NormVminpu(This,Value)	\
    ( (This)->lpVtbl -> get_NormVminpu(This,Value) ) 

#define ISettings_put_NormVminpu(This,Value)	\
    ( (This)->lpVtbl -> put_NormVminpu(This,Value) ) 

#define ISettings_get_NormVmaxpu(This,Value)	\
    ( (This)->lpVtbl -> get_NormVmaxpu(This,Value) ) 

#define ISettings_put_NormVmaxpu(This,Value)	\
    ( (This)->lpVtbl -> put_NormVmaxpu(This,Value) ) 

#define ISettings_get_EmergVminpu(This,Value)	\
    ( (This)->lpVtbl -> get_EmergVminpu(This,Value) ) 

#define ISettings_put_EmergVminpu(This,Value)	\
    ( (This)->lpVtbl -> put_EmergVminpu(This,Value) ) 

#define ISettings_get_EmergVmaxpu(This,Value)	\
    ( (This)->lpVtbl -> get_EmergVmaxpu(This,Value) ) 

#define ISettings_put_EmergVmaxpu(This,Value)	\
    ( (This)->lpVtbl -> put_EmergVmaxpu(This,Value) ) 

#define ISettings_get_UEweight(This,Value)	\
    ( (This)->lpVtbl -> get_UEweight(This,Value) ) 

#define ISettings_put_UEweight(This,Value)	\
    ( (This)->lpVtbl -> put_UEweight(This,Value) ) 

#define ISettings_get_LossWeight(This,Value)	\
    ( (This)->lpVtbl -> get_LossWeight(This,Value) ) 

#define ISettings_put_LossWeight(This,Value)	\
    ( (This)->lpVtbl -> put_LossWeight(This,Value) ) 

#define ISettings_get_UEregs(This,Value)	\
    ( (This)->lpVtbl -> get_UEregs(This,Value) ) 

#define ISettings_put_UEregs(This,Value)	\
    ( (This)->lpVtbl -> put_UEregs(This,Value) ) 

#define ISettings_get_LossRegs(This,Value)	\
    ( (This)->lpVtbl -> get_LossRegs(This,Value) ) 

#define ISettings_put_LossRegs(This,Value)	\
    ( (This)->lpVtbl -> put_LossRegs(This,Value) ) 

#define ISettings_get_Trapezoidal(This,Value)	\
    ( (This)->lpVtbl -> get_Trapezoidal(This,Value) ) 

#define ISettings_put_Trapezoidal(This,Value)	\
    ( (This)->lpVtbl -> put_Trapezoidal(This,Value) ) 

#define ISettings_get_VoltageBases(This,Value)	\
    ( (This)->lpVtbl -> get_VoltageBases(This,Value) ) 

#define ISettings_put_VoltageBases(This,Value)	\
    ( (This)->lpVtbl -> put_VoltageBases(This,Value) ) 

#define ISettings_get_ControlTrace(This,Value)	\
    ( (This)->lpVtbl -> get_ControlTrace(This,Value) ) 

#define ISettings_put_ControlTrace(This,Value)	\
    ( (This)->lpVtbl -> put_ControlTrace(This,Value) ) 

#define ISettings_get_PriceSignal(This,Value)	\
    ( (This)->lpVtbl -> get_PriceSignal(This,Value) ) 

#define ISettings_put_PriceSignal(This,Value)	\
    ( (This)->lpVtbl -> put_PriceSignal(This,Value) ) 

#define ISettings_get_PriceCurve(This,Value)	\
    ( (This)->lpVtbl -> get_PriceCurve(This,Value) ) 

#define ISettings_put_PriceCurve(This,Value)	\
    ( (This)->lpVtbl -> put_PriceCurve(This,Value) ) 

#define ISettings_get_AllowChangeDir(This,Value)	\
    ( (This)->lpVtbl -> get_AllowChangeDir(This,Value) ) 

#define ISettings_put_AllowChangeDir(This,Value)	\
    ( (This)->lpVtbl -> put_AllowChangeDir(This,Value) ) 

#define ISettings_get_AllowDOScmd(This,Value)	\
    ( (This)->lpVtbl -> get_AllowDOScmd(This,Value) ) 

#define ISettings_put_AllowDOScmd(This,Value)	\
    ( (This)->lpVtbl -> put_AllowDOScmd(This,Value) ) 

#define ISettings_get_AllowEditor(This,Value)	\
    ( (This)->lpVtbl -> get_AllowEditor(This,Value) ) 

#define ISettings_put_AllowEditor(This,Value)	\
    ( (This)->lpVtbl -> put_AllowEditor(This,Value) ) 

#define ISettings_get_COMErrorResults(This,Value)	\
    ( (This)->lpVtbl -> get_COMErrorResults(This,Value) ) 

#define ISettings_put_COMErrorResults(This,Value)	\
    ( (This)->lpVtbl -> put_COMErrorResults(This,Value) ) 

#define ISettings_get_CompatFlags(This,Value)	\
    ( (This)->lpVtbl -> get_CompatFlags(This,Value) ) 

#define ISettings_put_CompatFlags(This,Value)	\
    ( (This)->lpVtbl -> put_CompatFlags(This,Value) ) 

#define ISettings_get_IterateDisabled(This,Value)	\
    ( (This)->lpVtbl -> get_IterateDisabled(This,Value) ) 

#define ISettings_put_IterateDisabled(This,Value)	\
    ( (This)->lpVtbl -> put_IterateDisabled(This,Value) ) 

#define ISettings_get_LoadsTerminalCheck(This,Value)	\
    ( (This)->lpVtbl -> get_LoadsTerminalCheck(This,Value) ) 

#define ISettings_put_LoadsTerminalCheck(This,Value)	\
    ( (This)->lpVtbl -> put_LoadsTerminalCheck(This,Value) ) 

#define ISettings_SetPropertyNameStyle(This,Value)	\
    ( (This)->lpVtbl -> SetPropertyNameStyle(This,Value) ) 

#define ISettings_get_SkipFileRegExp(This,Value)	\
    ( (This)->lpVtbl -> get_SkipFileRegExp(This,Value) ) 

#define ISettings_put_SkipFileRegExp(This,Value)	\
    ( (This)->lpVtbl -> put_SkipFileRegExp(This,Value) ) 

#define ISettings_get_SkipCommands(This,Value)	\
    ( (This)->lpVtbl -> get_SkipCommands(This,Value) ) 

#define ISettings_put_SkipCommands(This,Value)	\
    ( (This)->lpVtbl -> put_SkipCommands(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ISettings_INTERFACE_DEFINED__ */


#ifndef __ILines_INTERFACE_DEFINED__
#define __ILines_INTERFACE_DEFINED__

/* interface ILines */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ILines;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B54E-18C2-11F0-A417-C87F5452571C")
    ILines : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE New( 
            /* [in] */ BSTR Name,
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Bus1( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Bus1( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Bus2( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Bus2( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LineCode( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LineCode( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Length( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Length( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Phases( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Phases( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_R1( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_R1( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_X1( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_X1( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_R0( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_R0( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_X0( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_X0( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_C1( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_C1( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_C0( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_C0( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Rmatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Rmatrix( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Xmatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Xmatrix( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Cmatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Cmatrix( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NormAmps( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NormAmps( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EmergAmps( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EmergAmps( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Geometry( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Geometry( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Rg( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Rg( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xg( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xg( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Rho( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Rho( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Yprim( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Yprim( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumCust( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TotalCust( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Parent( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Spacing( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Spacing( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Units( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Units( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SeasonRating( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsSwitch( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsSwitch( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ILinesVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ILines * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ILines * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ILines * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ILines * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ILines * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ILines * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ILines * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ILines, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ILines * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            ILines * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILines, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            ILines * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILines, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ILines * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ILines * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, New)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *New )( 
            ILines * This,
            /* [in] */ BSTR Name,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Bus1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Bus1 )( 
            ILines * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Bus1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Bus1 )( 
            ILines * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Bus2)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Bus2 )( 
            ILines * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Bus2)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Bus2 )( 
            ILines * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILines, get_LineCode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LineCode )( 
            ILines * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_LineCode)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LineCode )( 
            ILines * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Length)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Length )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Length)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Length )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Phases)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Phases )( 
            ILines * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Phases)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Phases )( 
            ILines * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ILines, get_R1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_R1 )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_R1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_R1 )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_X1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_X1 )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_X1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_X1 )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_R0)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_R0 )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_R0)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_R0 )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_X0)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_X0 )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_X0)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_X0 )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_C1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_C1 )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_C1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_C1 )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_C0)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_C0 )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_C0)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_C0 )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Rmatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Rmatrix )( 
            ILines * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Rmatrix)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Rmatrix )( 
            ILines * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Xmatrix)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xmatrix )( 
            ILines * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Xmatrix)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xmatrix )( 
            ILines * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Cmatrix)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Cmatrix )( 
            ILines * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Cmatrix)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Cmatrix )( 
            ILines * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILines, get_NormAmps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NormAmps )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_NormAmps)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NormAmps )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_EmergAmps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EmergAmps )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_EmergAmps)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EmergAmps )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Geometry)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Geometry )( 
            ILines * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Geometry)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Geometry )( 
            ILines * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Rg)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Rg )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Rg)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Rg )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Xg)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xg )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Xg)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xg )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Rho)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Rho )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Rho)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Rho )( 
            ILines * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Yprim)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Yprim )( 
            ILines * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Yprim)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Yprim )( 
            ILines * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILines, get_NumCust)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumCust )( 
            ILines * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, get_TotalCust)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TotalCust )( 
            ILines * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Parent)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Parent )( 
            ILines * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            ILines * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Spacing)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Spacing )( 
            ILines * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Spacing)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Spacing )( 
            ILines * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILines, get_Units)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Units )( 
            ILines * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_Units)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Units )( 
            ILines * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ILines, get_SeasonRating)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SeasonRating )( 
            ILines * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILines, get_IsSwitch)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsSwitch )( 
            ILines * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_IsSwitch)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsSwitch )( 
            ILines * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ILines, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            ILines * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILines, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            ILines * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } ILinesVtbl;

    interface ILines
    {
        CONST_VTBL struct ILinesVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ILines_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ILines_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ILines_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ILines_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ILines_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ILines_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ILines_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ILines_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ILines_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define ILines_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define ILines_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ILines_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ILines_New(This,Name,Value)	\
    ( (This)->lpVtbl -> New(This,Name,Value) ) 

#define ILines_get_Bus1(This,Value)	\
    ( (This)->lpVtbl -> get_Bus1(This,Value) ) 

#define ILines_put_Bus1(This,Value)	\
    ( (This)->lpVtbl -> put_Bus1(This,Value) ) 

#define ILines_get_Bus2(This,Value)	\
    ( (This)->lpVtbl -> get_Bus2(This,Value) ) 

#define ILines_put_Bus2(This,Value)	\
    ( (This)->lpVtbl -> put_Bus2(This,Value) ) 

#define ILines_get_LineCode(This,Value)	\
    ( (This)->lpVtbl -> get_LineCode(This,Value) ) 

#define ILines_put_LineCode(This,Value)	\
    ( (This)->lpVtbl -> put_LineCode(This,Value) ) 

#define ILines_get_Length(This,Value)	\
    ( (This)->lpVtbl -> get_Length(This,Value) ) 

#define ILines_put_Length(This,Value)	\
    ( (This)->lpVtbl -> put_Length(This,Value) ) 

#define ILines_get_Phases(This,Value)	\
    ( (This)->lpVtbl -> get_Phases(This,Value) ) 

#define ILines_put_Phases(This,Value)	\
    ( (This)->lpVtbl -> put_Phases(This,Value) ) 

#define ILines_get_R1(This,Value)	\
    ( (This)->lpVtbl -> get_R1(This,Value) ) 

#define ILines_put_R1(This,Value)	\
    ( (This)->lpVtbl -> put_R1(This,Value) ) 

#define ILines_get_X1(This,Value)	\
    ( (This)->lpVtbl -> get_X1(This,Value) ) 

#define ILines_put_X1(This,Value)	\
    ( (This)->lpVtbl -> put_X1(This,Value) ) 

#define ILines_get_R0(This,Value)	\
    ( (This)->lpVtbl -> get_R0(This,Value) ) 

#define ILines_put_R0(This,Value)	\
    ( (This)->lpVtbl -> put_R0(This,Value) ) 

#define ILines_get_X0(This,Value)	\
    ( (This)->lpVtbl -> get_X0(This,Value) ) 

#define ILines_put_X0(This,Value)	\
    ( (This)->lpVtbl -> put_X0(This,Value) ) 

#define ILines_get_C1(This,Value)	\
    ( (This)->lpVtbl -> get_C1(This,Value) ) 

#define ILines_put_C1(This,Value)	\
    ( (This)->lpVtbl -> put_C1(This,Value) ) 

#define ILines_get_C0(This,Value)	\
    ( (This)->lpVtbl -> get_C0(This,Value) ) 

#define ILines_put_C0(This,Value)	\
    ( (This)->lpVtbl -> put_C0(This,Value) ) 

#define ILines_get_Rmatrix(This,Value)	\
    ( (This)->lpVtbl -> get_Rmatrix(This,Value) ) 

#define ILines_put_Rmatrix(This,Value)	\
    ( (This)->lpVtbl -> put_Rmatrix(This,Value) ) 

#define ILines_get_Xmatrix(This,Value)	\
    ( (This)->lpVtbl -> get_Xmatrix(This,Value) ) 

#define ILines_put_Xmatrix(This,Value)	\
    ( (This)->lpVtbl -> put_Xmatrix(This,Value) ) 

#define ILines_get_Cmatrix(This,Value)	\
    ( (This)->lpVtbl -> get_Cmatrix(This,Value) ) 

#define ILines_put_Cmatrix(This,Value)	\
    ( (This)->lpVtbl -> put_Cmatrix(This,Value) ) 

#define ILines_get_NormAmps(This,Value)	\
    ( (This)->lpVtbl -> get_NormAmps(This,Value) ) 

#define ILines_put_NormAmps(This,Value)	\
    ( (This)->lpVtbl -> put_NormAmps(This,Value) ) 

#define ILines_get_EmergAmps(This,Value)	\
    ( (This)->lpVtbl -> get_EmergAmps(This,Value) ) 

#define ILines_put_EmergAmps(This,Value)	\
    ( (This)->lpVtbl -> put_EmergAmps(This,Value) ) 

#define ILines_get_Geometry(This,Value)	\
    ( (This)->lpVtbl -> get_Geometry(This,Value) ) 

#define ILines_put_Geometry(This,Value)	\
    ( (This)->lpVtbl -> put_Geometry(This,Value) ) 

#define ILines_get_Rg(This,Value)	\
    ( (This)->lpVtbl -> get_Rg(This,Value) ) 

#define ILines_put_Rg(This,Value)	\
    ( (This)->lpVtbl -> put_Rg(This,Value) ) 

#define ILines_get_Xg(This,Value)	\
    ( (This)->lpVtbl -> get_Xg(This,Value) ) 

#define ILines_put_Xg(This,Value)	\
    ( (This)->lpVtbl -> put_Xg(This,Value) ) 

#define ILines_get_Rho(This,Value)	\
    ( (This)->lpVtbl -> get_Rho(This,Value) ) 

#define ILines_put_Rho(This,Value)	\
    ( (This)->lpVtbl -> put_Rho(This,Value) ) 

#define ILines_get_Yprim(This,Value)	\
    ( (This)->lpVtbl -> get_Yprim(This,Value) ) 

#define ILines_put_Yprim(This,Value)	\
    ( (This)->lpVtbl -> put_Yprim(This,Value) ) 

#define ILines_get_NumCust(This,Value)	\
    ( (This)->lpVtbl -> get_NumCust(This,Value) ) 

#define ILines_get_TotalCust(This,Value)	\
    ( (This)->lpVtbl -> get_TotalCust(This,Value) ) 

#define ILines_get_Parent(This,Value)	\
    ( (This)->lpVtbl -> get_Parent(This,Value) ) 

#define ILines_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define ILines_get_Spacing(This,Value)	\
    ( (This)->lpVtbl -> get_Spacing(This,Value) ) 

#define ILines_put_Spacing(This,Value)	\
    ( (This)->lpVtbl -> put_Spacing(This,Value) ) 

#define ILines_get_Units(This,Value)	\
    ( (This)->lpVtbl -> get_Units(This,Value) ) 

#define ILines_put_Units(This,Value)	\
    ( (This)->lpVtbl -> put_Units(This,Value) ) 

#define ILines_get_SeasonRating(This,Value)	\
    ( (This)->lpVtbl -> get_SeasonRating(This,Value) ) 

#define ILines_get_IsSwitch(This,Value)	\
    ( (This)->lpVtbl -> get_IsSwitch(This,Value) ) 

#define ILines_put_IsSwitch(This,Value)	\
    ( (This)->lpVtbl -> put_IsSwitch(This,Value) ) 

#define ILines_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define ILines_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ILines_INTERFACE_DEFINED__ */


#ifndef __ICtrlQueue_INTERFACE_DEFINED__
#define __ICtrlQueue_INTERFACE_DEFINED__

/* interface ICtrlQueue */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ICtrlQueue;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B5A8-18C2-11F0-A417-C87F5452571C")
    ICtrlQueue : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ClearQueue( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Delete( 
            /* [in] */ long ActionHandle) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumActions( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Action( 
            /* [in] */ long rhs) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActionCode( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DeviceHandle( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Push( 
            /* [in] */ long Hour,
            /* [in] */ double Seconds,
            /* [in] */ long ActionCode,
            /* [in] */ long DeviceHandle,
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Show( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ClearActions( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PopAction( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_QueueSize( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DoAllQueue( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Queue( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ICtrlQueueVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ICtrlQueue * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ICtrlQueue * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ICtrlQueue * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ICtrlQueue * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ICtrlQueue * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ICtrlQueue * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ICtrlQueue * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, ClearQueue)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ClearQueue )( 
            ICtrlQueue * This);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, Delete)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Delete )( 
            ICtrlQueue * This,
            /* [in] */ long ActionHandle);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, get_NumActions)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumActions )( 
            ICtrlQueue * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, put_Action)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Action )( 
            ICtrlQueue * This,
            /* [in] */ long rhs);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, get_ActionCode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActionCode )( 
            ICtrlQueue * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, get_DeviceHandle)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DeviceHandle )( 
            ICtrlQueue * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, Push)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Push )( 
            ICtrlQueue * This,
            /* [in] */ long Hour,
            /* [in] */ double Seconds,
            /* [in] */ long ActionCode,
            /* [in] */ long DeviceHandle,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, Show)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Show )( 
            ICtrlQueue * This);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, ClearActions)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ClearActions )( 
            ICtrlQueue * This);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, get_PopAction)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PopAction )( 
            ICtrlQueue * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, get_QueueSize)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_QueueSize )( 
            ICtrlQueue * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, DoAllQueue)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DoAllQueue )( 
            ICtrlQueue * This);
        
        DECLSPEC_XFGVIRT(ICtrlQueue, get_Queue)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Queue )( 
            ICtrlQueue * This,
            /* [retval][out] */ VARIANT *Value);
        
        END_INTERFACE
    } ICtrlQueueVtbl;

    interface ICtrlQueue
    {
        CONST_VTBL struct ICtrlQueueVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ICtrlQueue_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ICtrlQueue_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ICtrlQueue_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ICtrlQueue_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ICtrlQueue_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ICtrlQueue_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ICtrlQueue_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ICtrlQueue_ClearQueue(This)	\
    ( (This)->lpVtbl -> ClearQueue(This) ) 

#define ICtrlQueue_Delete(This,ActionHandle)	\
    ( (This)->lpVtbl -> Delete(This,ActionHandle) ) 

#define ICtrlQueue_get_NumActions(This,Value)	\
    ( (This)->lpVtbl -> get_NumActions(This,Value) ) 

#define ICtrlQueue_put_Action(This,rhs)	\
    ( (This)->lpVtbl -> put_Action(This,rhs) ) 

#define ICtrlQueue_get_ActionCode(This,Value)	\
    ( (This)->lpVtbl -> get_ActionCode(This,Value) ) 

#define ICtrlQueue_get_DeviceHandle(This,Value)	\
    ( (This)->lpVtbl -> get_DeviceHandle(This,Value) ) 

#define ICtrlQueue_Push(This,Hour,Seconds,ActionCode,DeviceHandle,Value)	\
    ( (This)->lpVtbl -> Push(This,Hour,Seconds,ActionCode,DeviceHandle,Value) ) 

#define ICtrlQueue_Show(This)	\
    ( (This)->lpVtbl -> Show(This) ) 

#define ICtrlQueue_ClearActions(This)	\
    ( (This)->lpVtbl -> ClearActions(This) ) 

#define ICtrlQueue_get_PopAction(This,Value)	\
    ( (This)->lpVtbl -> get_PopAction(This,Value) ) 

#define ICtrlQueue_get_QueueSize(This,Value)	\
    ( (This)->lpVtbl -> get_QueueSize(This,Value) ) 

#define ICtrlQueue_DoAllQueue(This)	\
    ( (This)->lpVtbl -> DoAllQueue(This) ) 

#define ICtrlQueue_get_Queue(This,Value)	\
    ( (This)->lpVtbl -> get_Queue(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ICtrlQueue_INTERFACE_DEFINED__ */


#ifndef __ILoads_INTERFACE_DEFINED__
#define __ILoads_INTERFACE_DEFINED__

/* interface ILoads */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ILoads;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B5DA-18C2-11F0-A417-C87F5452571C")
    ILoads : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kW( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kW( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kV( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kV( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kvar( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kvar( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PF( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PF( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PctMean( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PctMean( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PctStdDev( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PctStdDev( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllocationFactor( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AllocationFactor( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Cfactor( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Cfactor( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Class( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Class( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsDelta( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsDelta( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CVRcurve( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CVRcurve( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CVRwatts( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CVRwatts( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CVRvars( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CVRvars( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_daily( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_daily( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_duty( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_duty( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kva( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kva( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kwh( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kwh( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kwhdays( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kwhdays( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Model( 
            /* [retval][out] */ LoadModels *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Model( 
            /* [in] */ LoadModels Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumCust( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NumCust( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Rneut( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Rneut( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Spectrum( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Spectrum( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vmaxpu( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Vmaxpu( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vminemerg( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Vminemerg( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vminnorm( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Vminnorm( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vminpu( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Vminpu( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_xfkVA( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_xfkVA( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xneut( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xneut( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Yearly( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Yearly( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Status( 
            /* [retval][out] */ LoadStatus *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Status( 
            /* [in] */ LoadStatus Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Growth( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Growth( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ZIPV( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ZIPV( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_pctSeriesRL( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_pctSeriesRL( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RelWeight( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_RelWeight( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Sensor( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Phases( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Phases( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ILoadsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ILoads * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ILoads * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ILoads * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ILoads * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ILoads * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ILoads * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ILoads * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ILoads, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            ILoads * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ILoads * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ILoads * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ILoads * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            ILoads * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            ILoads * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            ILoads * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_kW)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kW )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_kW)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kW )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_kV)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kV )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_kV)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kV )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_kvar)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kvar )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_kvar)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kvar )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_PF)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PF )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_PF)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PF )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            ILoads * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_PctMean)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PctMean )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_PctMean)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PctMean )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_PctStdDev)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PctStdDev )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_PctStdDev)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PctStdDev )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_AllocationFactor)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllocationFactor )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_AllocationFactor)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AllocationFactor )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Cfactor)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Cfactor )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Cfactor)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Cfactor )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Class)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Class )( 
            ILoads * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Class)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Class )( 
            ILoads * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_IsDelta)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsDelta )( 
            ILoads * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_IsDelta)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsDelta )( 
            ILoads * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_CVRcurve)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CVRcurve )( 
            ILoads * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_CVRcurve)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CVRcurve )( 
            ILoads * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_CVRwatts)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CVRwatts )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_CVRwatts)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CVRwatts )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_CVRvars)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CVRvars )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_CVRvars)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CVRvars )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_daily)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_daily )( 
            ILoads * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_daily)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_daily )( 
            ILoads * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_duty)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_duty )( 
            ILoads * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_duty)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_duty )( 
            ILoads * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_kva)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kva )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_kva)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kva )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_kwh)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kwh )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_kwh)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kwh )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_kwhdays)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kwhdays )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_kwhdays)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kwhdays )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Model)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Model )( 
            ILoads * This,
            /* [retval][out] */ LoadModels *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Model)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Model )( 
            ILoads * This,
            /* [in] */ LoadModels Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_NumCust)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumCust )( 
            ILoads * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_NumCust)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NumCust )( 
            ILoads * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Rneut)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Rneut )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Rneut)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Rneut )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Spectrum)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Spectrum )( 
            ILoads * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Spectrum)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Spectrum )( 
            ILoads * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Vmaxpu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vmaxpu )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Vmaxpu)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Vmaxpu )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Vminemerg)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vminemerg )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Vminemerg)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Vminemerg )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Vminnorm)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vminnorm )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Vminnorm)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Vminnorm )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Vminpu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vminpu )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Vminpu)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Vminpu )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_xfkVA)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_xfkVA )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_xfkVA)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_xfkVA )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Xneut)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xneut )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Xneut)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xneut )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Yearly)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Yearly )( 
            ILoads * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Yearly)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Yearly )( 
            ILoads * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Status)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Status )( 
            ILoads * This,
            /* [retval][out] */ LoadStatus *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Status)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Status )( 
            ILoads * This,
            /* [in] */ LoadStatus Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Growth)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Growth )( 
            ILoads * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Growth)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Growth )( 
            ILoads * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_ZIPV)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ZIPV )( 
            ILoads * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_ZIPV)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ZIPV )( 
            ILoads * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_pctSeriesRL)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_pctSeriesRL )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_pctSeriesRL)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_pctSeriesRL )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_RelWeight)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RelWeight )( 
            ILoads * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_RelWeight)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_RelWeight )( 
            ILoads * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Sensor)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Sensor )( 
            ILoads * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILoads, get_Phases)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Phases )( 
            ILoads * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoads, put_Phases)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Phases )( 
            ILoads * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } ILoadsVtbl;

    interface ILoads
    {
        CONST_VTBL struct ILoadsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ILoads_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ILoads_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ILoads_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ILoads_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ILoads_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ILoads_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ILoads_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ILoads_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define ILoads_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ILoads_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ILoads_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ILoads_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define ILoads_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define ILoads_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define ILoads_get_kW(This,Value)	\
    ( (This)->lpVtbl -> get_kW(This,Value) ) 

#define ILoads_put_kW(This,Value)	\
    ( (This)->lpVtbl -> put_kW(This,Value) ) 

#define ILoads_get_kV(This,Value)	\
    ( (This)->lpVtbl -> get_kV(This,Value) ) 

#define ILoads_put_kV(This,Value)	\
    ( (This)->lpVtbl -> put_kV(This,Value) ) 

#define ILoads_get_kvar(This,Value)	\
    ( (This)->lpVtbl -> get_kvar(This,Value) ) 

#define ILoads_put_kvar(This,Value)	\
    ( (This)->lpVtbl -> put_kvar(This,Value) ) 

#define ILoads_get_PF(This,Value)	\
    ( (This)->lpVtbl -> get_PF(This,Value) ) 

#define ILoads_put_PF(This,Value)	\
    ( (This)->lpVtbl -> put_PF(This,Value) ) 

#define ILoads_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define ILoads_get_PctMean(This,Value)	\
    ( (This)->lpVtbl -> get_PctMean(This,Value) ) 

#define ILoads_put_PctMean(This,Value)	\
    ( (This)->lpVtbl -> put_PctMean(This,Value) ) 

#define ILoads_get_PctStdDev(This,Value)	\
    ( (This)->lpVtbl -> get_PctStdDev(This,Value) ) 

#define ILoads_put_PctStdDev(This,Value)	\
    ( (This)->lpVtbl -> put_PctStdDev(This,Value) ) 

#define ILoads_get_AllocationFactor(This,Value)	\
    ( (This)->lpVtbl -> get_AllocationFactor(This,Value) ) 

#define ILoads_put_AllocationFactor(This,Value)	\
    ( (This)->lpVtbl -> put_AllocationFactor(This,Value) ) 

#define ILoads_get_Cfactor(This,Value)	\
    ( (This)->lpVtbl -> get_Cfactor(This,Value) ) 

#define ILoads_put_Cfactor(This,Value)	\
    ( (This)->lpVtbl -> put_Cfactor(This,Value) ) 

#define ILoads_get_Class(This,Value)	\
    ( (This)->lpVtbl -> get_Class(This,Value) ) 

#define ILoads_put_Class(This,Value)	\
    ( (This)->lpVtbl -> put_Class(This,Value) ) 

#define ILoads_get_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> get_IsDelta(This,Value) ) 

#define ILoads_put_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> put_IsDelta(This,Value) ) 

#define ILoads_get_CVRcurve(This,Value)	\
    ( (This)->lpVtbl -> get_CVRcurve(This,Value) ) 

#define ILoads_put_CVRcurve(This,Value)	\
    ( (This)->lpVtbl -> put_CVRcurve(This,Value) ) 

#define ILoads_get_CVRwatts(This,Value)	\
    ( (This)->lpVtbl -> get_CVRwatts(This,Value) ) 

#define ILoads_put_CVRwatts(This,Value)	\
    ( (This)->lpVtbl -> put_CVRwatts(This,Value) ) 

#define ILoads_get_CVRvars(This,Value)	\
    ( (This)->lpVtbl -> get_CVRvars(This,Value) ) 

#define ILoads_put_CVRvars(This,Value)	\
    ( (This)->lpVtbl -> put_CVRvars(This,Value) ) 

#define ILoads_get_daily(This,Value)	\
    ( (This)->lpVtbl -> get_daily(This,Value) ) 

#define ILoads_put_daily(This,Value)	\
    ( (This)->lpVtbl -> put_daily(This,Value) ) 

#define ILoads_get_duty(This,Value)	\
    ( (This)->lpVtbl -> get_duty(This,Value) ) 

#define ILoads_put_duty(This,Value)	\
    ( (This)->lpVtbl -> put_duty(This,Value) ) 

#define ILoads_get_kva(This,Value)	\
    ( (This)->lpVtbl -> get_kva(This,Value) ) 

#define ILoads_put_kva(This,Value)	\
    ( (This)->lpVtbl -> put_kva(This,Value) ) 

#define ILoads_get_kwh(This,Value)	\
    ( (This)->lpVtbl -> get_kwh(This,Value) ) 

#define ILoads_put_kwh(This,Value)	\
    ( (This)->lpVtbl -> put_kwh(This,Value) ) 

#define ILoads_get_kwhdays(This,Value)	\
    ( (This)->lpVtbl -> get_kwhdays(This,Value) ) 

#define ILoads_put_kwhdays(This,Value)	\
    ( (This)->lpVtbl -> put_kwhdays(This,Value) ) 

#define ILoads_get_Model(This,Value)	\
    ( (This)->lpVtbl -> get_Model(This,Value) ) 

#define ILoads_put_Model(This,Value)	\
    ( (This)->lpVtbl -> put_Model(This,Value) ) 

#define ILoads_get_NumCust(This,Value)	\
    ( (This)->lpVtbl -> get_NumCust(This,Value) ) 

#define ILoads_put_NumCust(This,Value)	\
    ( (This)->lpVtbl -> put_NumCust(This,Value) ) 

#define ILoads_get_Rneut(This,Value)	\
    ( (This)->lpVtbl -> get_Rneut(This,Value) ) 

#define ILoads_put_Rneut(This,Value)	\
    ( (This)->lpVtbl -> put_Rneut(This,Value) ) 

#define ILoads_get_Spectrum(This,Value)	\
    ( (This)->lpVtbl -> get_Spectrum(This,Value) ) 

#define ILoads_put_Spectrum(This,Value)	\
    ( (This)->lpVtbl -> put_Spectrum(This,Value) ) 

#define ILoads_get_Vmaxpu(This,Value)	\
    ( (This)->lpVtbl -> get_Vmaxpu(This,Value) ) 

#define ILoads_put_Vmaxpu(This,Value)	\
    ( (This)->lpVtbl -> put_Vmaxpu(This,Value) ) 

#define ILoads_get_Vminemerg(This,Value)	\
    ( (This)->lpVtbl -> get_Vminemerg(This,Value) ) 

#define ILoads_put_Vminemerg(This,Value)	\
    ( (This)->lpVtbl -> put_Vminemerg(This,Value) ) 

#define ILoads_get_Vminnorm(This,Value)	\
    ( (This)->lpVtbl -> get_Vminnorm(This,Value) ) 

#define ILoads_put_Vminnorm(This,Value)	\
    ( (This)->lpVtbl -> put_Vminnorm(This,Value) ) 

#define ILoads_get_Vminpu(This,Value)	\
    ( (This)->lpVtbl -> get_Vminpu(This,Value) ) 

#define ILoads_put_Vminpu(This,Value)	\
    ( (This)->lpVtbl -> put_Vminpu(This,Value) ) 

#define ILoads_get_xfkVA(This,Value)	\
    ( (This)->lpVtbl -> get_xfkVA(This,Value) ) 

#define ILoads_put_xfkVA(This,Value)	\
    ( (This)->lpVtbl -> put_xfkVA(This,Value) ) 

#define ILoads_get_Xneut(This,Value)	\
    ( (This)->lpVtbl -> get_Xneut(This,Value) ) 

#define ILoads_put_Xneut(This,Value)	\
    ( (This)->lpVtbl -> put_Xneut(This,Value) ) 

#define ILoads_get_Yearly(This,Value)	\
    ( (This)->lpVtbl -> get_Yearly(This,Value) ) 

#define ILoads_put_Yearly(This,Value)	\
    ( (This)->lpVtbl -> put_Yearly(This,Value) ) 

#define ILoads_get_Status(This,Value)	\
    ( (This)->lpVtbl -> get_Status(This,Value) ) 

#define ILoads_put_Status(This,Value)	\
    ( (This)->lpVtbl -> put_Status(This,Value) ) 

#define ILoads_get_Growth(This,Value)	\
    ( (This)->lpVtbl -> get_Growth(This,Value) ) 

#define ILoads_put_Growth(This,Value)	\
    ( (This)->lpVtbl -> put_Growth(This,Value) ) 

#define ILoads_get_ZIPV(This,Value)	\
    ( (This)->lpVtbl -> get_ZIPV(This,Value) ) 

#define ILoads_put_ZIPV(This,Value)	\
    ( (This)->lpVtbl -> put_ZIPV(This,Value) ) 

#define ILoads_get_pctSeriesRL(This,Value)	\
    ( (This)->lpVtbl -> get_pctSeriesRL(This,Value) ) 

#define ILoads_put_pctSeriesRL(This,Value)	\
    ( (This)->lpVtbl -> put_pctSeriesRL(This,Value) ) 

#define ILoads_get_RelWeight(This,Value)	\
    ( (This)->lpVtbl -> get_RelWeight(This,Value) ) 

#define ILoads_put_RelWeight(This,Value)	\
    ( (This)->lpVtbl -> put_RelWeight(This,Value) ) 

#define ILoads_get_Sensor(This,Value)	\
    ( (This)->lpVtbl -> get_Sensor(This,Value) ) 

#define ILoads_get_Phases(This,Value)	\
    ( (This)->lpVtbl -> get_Phases(This,Value) ) 

#define ILoads_put_Phases(This,Value)	\
    ( (This)->lpVtbl -> put_Phases(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ILoads_INTERFACE_DEFINED__ */


#ifndef __IDSSElement_INTERFACE_DEFINED__
#define __IDSSElement_INTERFACE_DEFINED__

/* interface IDSSElement */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IDSSElement;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B648-18C2-11F0-A417-C87F5452571C")
    IDSSElement : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Properties( 
            /* [in] */ VARIANT Indx,
            /* [retval][out] */ IDSSProperty **Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumProperties( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllPropertyNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ToJSON( 
            /* [defaultvalue][in] */ long options,
            /* [retval][out] */ BSTR *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IDSSElementVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IDSSElement * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IDSSElement * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IDSSElement * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IDSSElement * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IDSSElement * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IDSSElement * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IDSSElement * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IDSSElement, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IDSSElement * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IDSSElement, get_Properties)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Properties )( 
            IDSSElement * This,
            /* [in] */ VARIANT Indx,
            /* [retval][out] */ IDSSProperty **Value);
        
        DECLSPEC_XFGVIRT(IDSSElement, get_NumProperties)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumProperties )( 
            IDSSElement * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IDSSElement, get_AllPropertyNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllPropertyNames )( 
            IDSSElement * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IDSSElement, ToJSON)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ToJSON )( 
            IDSSElement * This,
            /* [defaultvalue][in] */ long options,
            /* [retval][out] */ BSTR *Value);
        
        END_INTERFACE
    } IDSSElementVtbl;

    interface IDSSElement
    {
        CONST_VTBL struct IDSSElementVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IDSSElement_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IDSSElement_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IDSSElement_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IDSSElement_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IDSSElement_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IDSSElement_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IDSSElement_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IDSSElement_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IDSSElement_get_Properties(This,Indx,Value)	\
    ( (This)->lpVtbl -> get_Properties(This,Indx,Value) ) 

#define IDSSElement_get_NumProperties(This,Value)	\
    ( (This)->lpVtbl -> get_NumProperties(This,Value) ) 

#define IDSSElement_get_AllPropertyNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllPropertyNames(This,Value) ) 

#define IDSSElement_ToJSON(This,options,Value)	\
    ( (This)->lpVtbl -> ToJSON(This,options,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IDSSElement_INTERFACE_DEFINED__ */


#ifndef __IActiveClass_INTERFACE_DEFINED__
#define __IActiveClass_INTERFACE_DEFINED__

/* interface IActiveClass */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IActiveClass;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B670-18C2-11F0-A417-C87F5452571C")
    IActiveClass : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumElements( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveClassName( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveClassParent( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ToJSON( 
            /* [defaultvalue][in] */ long options,
            /* [retval][out] */ BSTR *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IActiveClassVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IActiveClass * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IActiveClass * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IActiveClass * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IActiveClass * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IActiveClass * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IActiveClass * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IActiveClass * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IActiveClass, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IActiveClass * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IActiveClass, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IActiveClass * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IActiveClass, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IActiveClass * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IActiveClass, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IActiveClass * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IActiveClass, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IActiveClass * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IActiveClass, get_NumElements)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumElements )( 
            IActiveClass * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IActiveClass, get_ActiveClassName)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveClassName )( 
            IActiveClass * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IActiveClass, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IActiveClass * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IActiveClass, get_ActiveClassParent)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveClassParent )( 
            IActiveClass * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IActiveClass, ToJSON)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ToJSON )( 
            IActiveClass * This,
            /* [defaultvalue][in] */ long options,
            /* [retval][out] */ BSTR *Value);
        
        END_INTERFACE
    } IActiveClassVtbl;

    interface IActiveClass
    {
        CONST_VTBL struct IActiveClassVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IActiveClass_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IActiveClass_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IActiveClass_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IActiveClass_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IActiveClass_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IActiveClass_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IActiveClass_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IActiveClass_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IActiveClass_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IActiveClass_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IActiveClass_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IActiveClass_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IActiveClass_get_NumElements(This,Value)	\
    ( (This)->lpVtbl -> get_NumElements(This,Value) ) 

#define IActiveClass_get_ActiveClassName(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveClassName(This,Value) ) 

#define IActiveClass_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IActiveClass_get_ActiveClassParent(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveClassParent(This,Value) ) 

#define IActiveClass_ToJSON(This,options,Value)	\
    ( (This)->lpVtbl -> ToJSON(This,options,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IActiveClass_INTERFACE_DEFINED__ */


#ifndef __ICapacitors_INTERFACE_DEFINED__
#define __ICapacitors_INTERFACE_DEFINED__

/* interface ICapacitors */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ICapacitors;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B6A2-18C2-11F0-A417-C87F5452571C")
    ICapacitors : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kV( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kV( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kvar( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kvar( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumSteps( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NumSteps( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsDelta( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsDelta( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE AddStep( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SubtractStep( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AvailableSteps( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_States( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_States( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Open( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Close( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ICapacitorsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ICapacitors * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ICapacitors * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ICapacitors * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ICapacitors * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ICapacitors * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ICapacitors * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ICapacitors * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_kV)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kV )( 
            ICapacitors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, put_kV)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kV )( 
            ICapacitors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_kvar)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kvar )( 
            ICapacitors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, put_kvar)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kvar )( 
            ICapacitors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_NumSteps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumSteps )( 
            ICapacitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, put_NumSteps)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NumSteps )( 
            ICapacitors * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_IsDelta)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsDelta )( 
            ICapacitors * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, put_IsDelta)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsDelta )( 
            ICapacitors * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            ICapacitors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ICapacitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ICapacitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ICapacitors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            ICapacitors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            ICapacitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, AddStep)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *AddStep )( 
            ICapacitors * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, SubtractStep)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SubtractStep )( 
            ICapacitors * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_AvailableSteps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AvailableSteps )( 
            ICapacitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_States)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_States )( 
            ICapacitors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, put_States)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_States )( 
            ICapacitors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, Open)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Open )( 
            ICapacitors * This);
        
        DECLSPEC_XFGVIRT(ICapacitors, Close)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Close )( 
            ICapacitors * This);
        
        DECLSPEC_XFGVIRT(ICapacitors, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            ICapacitors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapacitors, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            ICapacitors * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } ICapacitorsVtbl;

    interface ICapacitors
    {
        CONST_VTBL struct ICapacitorsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ICapacitors_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ICapacitors_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ICapacitors_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ICapacitors_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ICapacitors_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ICapacitors_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ICapacitors_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ICapacitors_get_kV(This,Value)	\
    ( (This)->lpVtbl -> get_kV(This,Value) ) 

#define ICapacitors_put_kV(This,Value)	\
    ( (This)->lpVtbl -> put_kV(This,Value) ) 

#define ICapacitors_get_kvar(This,Value)	\
    ( (This)->lpVtbl -> get_kvar(This,Value) ) 

#define ICapacitors_put_kvar(This,Value)	\
    ( (This)->lpVtbl -> put_kvar(This,Value) ) 

#define ICapacitors_get_NumSteps(This,Value)	\
    ( (This)->lpVtbl -> get_NumSteps(This,Value) ) 

#define ICapacitors_put_NumSteps(This,Value)	\
    ( (This)->lpVtbl -> put_NumSteps(This,Value) ) 

#define ICapacitors_get_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> get_IsDelta(This,Value) ) 

#define ICapacitors_put_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> put_IsDelta(This,Value) ) 

#define ICapacitors_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define ICapacitors_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ICapacitors_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ICapacitors_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ICapacitors_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define ICapacitors_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define ICapacitors_AddStep(This,Value)	\
    ( (This)->lpVtbl -> AddStep(This,Value) ) 

#define ICapacitors_SubtractStep(This,Value)	\
    ( (This)->lpVtbl -> SubtractStep(This,Value) ) 

#define ICapacitors_get_AvailableSteps(This,Value)	\
    ( (This)->lpVtbl -> get_AvailableSteps(This,Value) ) 

#define ICapacitors_get_States(This,Value)	\
    ( (This)->lpVtbl -> get_States(This,Value) ) 

#define ICapacitors_put_States(This,Value)	\
    ( (This)->lpVtbl -> put_States(This,Value) ) 

#define ICapacitors_Open(This)	\
    ( (This)->lpVtbl -> Open(This) ) 

#define ICapacitors_Close(This)	\
    ( (This)->lpVtbl -> Close(This) ) 

#define ICapacitors_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define ICapacitors_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ICapacitors_INTERFACE_DEFINED__ */


#ifndef __ITransformers_INTERFACE_DEFINED__
#define __ITransformers_INTERFACE_DEFINED__

/* interface ITransformers */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ITransformers;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B6E8-18C2-11F0-A417-C87F5452571C")
    ITransformers : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumWindings( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NumWindings( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_XfmrCode( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_XfmrCode( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Wdg( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Wdg( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_R( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_R( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Tap( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Tap( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MinTap( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MinTap( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MaxTap( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MaxTap( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumTaps( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NumTaps( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kV( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kV( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kva( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kva( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xneut( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xneut( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Rneut( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Rneut( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsDelta( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsDelta( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xhl( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xhl( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xht( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xht( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xlt( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xlt( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_WdgVoltages( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_WdgCurrents( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_strWdgCurrents( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CoreType( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CoreType( 
            /* [in] */ long Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_RdcOhms( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_RdcOhms( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LossesByType( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllLossesByType( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ITransformersVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ITransformers * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ITransformers * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ITransformers * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ITransformers * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ITransformers * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ITransformers * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ITransformers * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ITransformers, get_NumWindings)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumWindings )( 
            ITransformers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_NumWindings)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NumWindings )( 
            ITransformers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_XfmrCode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_XfmrCode )( 
            ITransformers * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_XfmrCode)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_XfmrCode )( 
            ITransformers * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Wdg)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Wdg )( 
            ITransformers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_Wdg)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Wdg )( 
            ITransformers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_R)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_R )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_R)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_R )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Tap)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Tap )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_Tap)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Tap )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_MinTap)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MinTap )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_MinTap)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MinTap )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_MaxTap)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MaxTap )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_MaxTap)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MaxTap )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_NumTaps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumTaps )( 
            ITransformers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_NumTaps)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NumTaps )( 
            ITransformers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_kV)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kV )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_kV)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kV )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_kva)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kva )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_kva)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kva )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Xneut)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xneut )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_Xneut)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xneut )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Rneut)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Rneut )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_Rneut)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Rneut )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_IsDelta)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsDelta )( 
            ITransformers * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_IsDelta)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsDelta )( 
            ITransformers * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Xhl)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xhl )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_Xhl)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xhl )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Xht)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xht )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_Xht)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xht )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Xlt)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xlt )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_Xlt)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xlt )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ITransformers * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            ITransformers * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ITransformers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ITransformers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            ITransformers * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_Count)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            ITransformers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_WdgVoltages)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_WdgVoltages )( 
            ITransformers * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_WdgCurrents)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_WdgCurrents )( 
            ITransformers * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_strWdgCurrents)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_strWdgCurrents )( 
            ITransformers * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_CoreType)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CoreType )( 
            ITransformers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_CoreType)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CoreType )( 
            ITransformers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_RdcOhms)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RdcOhms )( 
            ITransformers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_RdcOhms)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_RdcOhms )( 
            ITransformers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            ITransformers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            ITransformers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_LossesByType)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LossesByType )( 
            ITransformers * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ITransformers, get_AllLossesByType)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllLossesByType )( 
            ITransformers * This,
            /* [retval][out] */ VARIANT *Value);
        
        END_INTERFACE
    } ITransformersVtbl;

    interface ITransformers
    {
        CONST_VTBL struct ITransformersVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ITransformers_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ITransformers_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ITransformers_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ITransformers_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ITransformers_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ITransformers_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ITransformers_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ITransformers_get_NumWindings(This,Value)	\
    ( (This)->lpVtbl -> get_NumWindings(This,Value) ) 

#define ITransformers_put_NumWindings(This,Value)	\
    ( (This)->lpVtbl -> put_NumWindings(This,Value) ) 

#define ITransformers_get_XfmrCode(This,Value)	\
    ( (This)->lpVtbl -> get_XfmrCode(This,Value) ) 

#define ITransformers_put_XfmrCode(This,Value)	\
    ( (This)->lpVtbl -> put_XfmrCode(This,Value) ) 

#define ITransformers_get_Wdg(This,Value)	\
    ( (This)->lpVtbl -> get_Wdg(This,Value) ) 

#define ITransformers_put_Wdg(This,Value)	\
    ( (This)->lpVtbl -> put_Wdg(This,Value) ) 

#define ITransformers_get_R(This,Value)	\
    ( (This)->lpVtbl -> get_R(This,Value) ) 

#define ITransformers_put_R(This,Value)	\
    ( (This)->lpVtbl -> put_R(This,Value) ) 

#define ITransformers_get_Tap(This,Value)	\
    ( (This)->lpVtbl -> get_Tap(This,Value) ) 

#define ITransformers_put_Tap(This,Value)	\
    ( (This)->lpVtbl -> put_Tap(This,Value) ) 

#define ITransformers_get_MinTap(This,Value)	\
    ( (This)->lpVtbl -> get_MinTap(This,Value) ) 

#define ITransformers_put_MinTap(This,Value)	\
    ( (This)->lpVtbl -> put_MinTap(This,Value) ) 

#define ITransformers_get_MaxTap(This,Value)	\
    ( (This)->lpVtbl -> get_MaxTap(This,Value) ) 

#define ITransformers_put_MaxTap(This,Value)	\
    ( (This)->lpVtbl -> put_MaxTap(This,Value) ) 

#define ITransformers_get_NumTaps(This,Value)	\
    ( (This)->lpVtbl -> get_NumTaps(This,Value) ) 

#define ITransformers_put_NumTaps(This,Value)	\
    ( (This)->lpVtbl -> put_NumTaps(This,Value) ) 

#define ITransformers_get_kV(This,Value)	\
    ( (This)->lpVtbl -> get_kV(This,Value) ) 

#define ITransformers_put_kV(This,Value)	\
    ( (This)->lpVtbl -> put_kV(This,Value) ) 

#define ITransformers_get_kva(This,Value)	\
    ( (This)->lpVtbl -> get_kva(This,Value) ) 

#define ITransformers_put_kva(This,Value)	\
    ( (This)->lpVtbl -> put_kva(This,Value) ) 

#define ITransformers_get_Xneut(This,Value)	\
    ( (This)->lpVtbl -> get_Xneut(This,Value) ) 

#define ITransformers_put_Xneut(This,Value)	\
    ( (This)->lpVtbl -> put_Xneut(This,Value) ) 

#define ITransformers_get_Rneut(This,Value)	\
    ( (This)->lpVtbl -> get_Rneut(This,Value) ) 

#define ITransformers_put_Rneut(This,Value)	\
    ( (This)->lpVtbl -> put_Rneut(This,Value) ) 

#define ITransformers_get_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> get_IsDelta(This,Value) ) 

#define ITransformers_put_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> put_IsDelta(This,Value) ) 

#define ITransformers_get_Xhl(This,Value)	\
    ( (This)->lpVtbl -> get_Xhl(This,Value) ) 

#define ITransformers_put_Xhl(This,Value)	\
    ( (This)->lpVtbl -> put_Xhl(This,Value) ) 

#define ITransformers_get_Xht(This,Value)	\
    ( (This)->lpVtbl -> get_Xht(This,Value) ) 

#define ITransformers_put_Xht(This,Value)	\
    ( (This)->lpVtbl -> put_Xht(This,Value) ) 

#define ITransformers_get_Xlt(This,Value)	\
    ( (This)->lpVtbl -> get_Xlt(This,Value) ) 

#define ITransformers_put_Xlt(This,Value)	\
    ( (This)->lpVtbl -> put_Xlt(This,Value) ) 

#define ITransformers_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ITransformers_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define ITransformers_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ITransformers_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ITransformers_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define ITransformers_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define ITransformers_get_WdgVoltages(This,Value)	\
    ( (This)->lpVtbl -> get_WdgVoltages(This,Value) ) 

#define ITransformers_get_WdgCurrents(This,Value)	\
    ( (This)->lpVtbl -> get_WdgCurrents(This,Value) ) 

#define ITransformers_get_strWdgCurrents(This,Value)	\
    ( (This)->lpVtbl -> get_strWdgCurrents(This,Value) ) 

#define ITransformers_get_CoreType(This,Value)	\
    ( (This)->lpVtbl -> get_CoreType(This,Value) ) 

#define ITransformers_put_CoreType(This,Value)	\
    ( (This)->lpVtbl -> put_CoreType(This,Value) ) 

#define ITransformers_get_RdcOhms(This,Value)	\
    ( (This)->lpVtbl -> get_RdcOhms(This,Value) ) 

#define ITransformers_put_RdcOhms(This,Value)	\
    ( (This)->lpVtbl -> put_RdcOhms(This,Value) ) 

#define ITransformers_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define ITransformers_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define ITransformers_get_LossesByType(This,Value)	\
    ( (This)->lpVtbl -> get_LossesByType(This,Value) ) 

#define ITransformers_get_AllLossesByType(This,Value)	\
    ( (This)->lpVtbl -> get_AllLossesByType(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ITransformers_INTERFACE_DEFINED__ */


#ifndef __ISwtControls_INTERFACE_DEFINED__
#define __ISwtControls_INTERFACE_DEFINED__

/* interface ISwtControls */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ISwtControls;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B724-18C2-11F0-A417-C87F5452571C")
    ISwtControls : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Action( 
            /* [retval][out] */ ActionCodes *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Action( 
            /* [in] */ ActionCodes Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsLocked( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsLocked( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Delay( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Delay( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SwitchedObj( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SwitchedObj( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SwitchedTerm( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SwitchedTerm( 
            /* [in] */ long Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NormalState( 
            /* [retval][out] */ ActionCodes *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NormalState( 
            /* [in] */ ActionCodes Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_State( 
            /* [retval][out] */ ActionCodes *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_State( 
            /* [in] */ ActionCodes Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ISwtControlsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ISwtControls * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ISwtControls * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ISwtControls * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ISwtControls * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ISwtControls * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ISwtControls * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ISwtControls * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            ISwtControls * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ISwtControls * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            ISwtControls * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ISwtControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ISwtControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_Action)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Action )( 
            ISwtControls * This,
            /* [retval][out] */ ActionCodes *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, put_Action)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Action )( 
            ISwtControls * This,
            /* [in] */ ActionCodes Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_IsLocked)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsLocked )( 
            ISwtControls * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, put_IsLocked)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsLocked )( 
            ISwtControls * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_Delay)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Delay )( 
            ISwtControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, put_Delay)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Delay )( 
            ISwtControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_SwitchedObj)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwitchedObj )( 
            ISwtControls * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, put_SwitchedObj)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SwitchedObj )( 
            ISwtControls * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_SwitchedTerm)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwitchedTerm )( 
            ISwtControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, put_SwitchedTerm)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SwitchedTerm )( 
            ISwtControls * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_Count)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            ISwtControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_NormalState)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NormalState )( 
            ISwtControls * This,
            /* [retval][out] */ ActionCodes *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, put_NormalState)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NormalState )( 
            ISwtControls * This,
            /* [in] */ ActionCodes Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_State)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_State )( 
            ISwtControls * This,
            /* [retval][out] */ ActionCodes *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, put_State)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_State )( 
            ISwtControls * This,
            /* [in] */ ActionCodes Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            ISwtControls * This);
        
        DECLSPEC_XFGVIRT(ISwtControls, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            ISwtControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISwtControls, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            ISwtControls * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } ISwtControlsVtbl;

    interface ISwtControls
    {
        CONST_VTBL struct ISwtControlsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ISwtControls_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ISwtControls_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ISwtControls_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ISwtControls_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ISwtControls_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ISwtControls_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ISwtControls_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ISwtControls_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define ISwtControls_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ISwtControls_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define ISwtControls_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ISwtControls_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ISwtControls_get_Action(This,Value)	\
    ( (This)->lpVtbl -> get_Action(This,Value) ) 

#define ISwtControls_put_Action(This,Value)	\
    ( (This)->lpVtbl -> put_Action(This,Value) ) 

#define ISwtControls_get_IsLocked(This,Value)	\
    ( (This)->lpVtbl -> get_IsLocked(This,Value) ) 

#define ISwtControls_put_IsLocked(This,Value)	\
    ( (This)->lpVtbl -> put_IsLocked(This,Value) ) 

#define ISwtControls_get_Delay(This,Value)	\
    ( (This)->lpVtbl -> get_Delay(This,Value) ) 

#define ISwtControls_put_Delay(This,Value)	\
    ( (This)->lpVtbl -> put_Delay(This,Value) ) 

#define ISwtControls_get_SwitchedObj(This,Value)	\
    ( (This)->lpVtbl -> get_SwitchedObj(This,Value) ) 

#define ISwtControls_put_SwitchedObj(This,Value)	\
    ( (This)->lpVtbl -> put_SwitchedObj(This,Value) ) 

#define ISwtControls_get_SwitchedTerm(This,Value)	\
    ( (This)->lpVtbl -> get_SwitchedTerm(This,Value) ) 

#define ISwtControls_put_SwitchedTerm(This,Value)	\
    ( (This)->lpVtbl -> put_SwitchedTerm(This,Value) ) 

#define ISwtControls_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define ISwtControls_get_NormalState(This,Value)	\
    ( (This)->lpVtbl -> get_NormalState(This,Value) ) 

#define ISwtControls_put_NormalState(This,Value)	\
    ( (This)->lpVtbl -> put_NormalState(This,Value) ) 

#define ISwtControls_get_State(This,Value)	\
    ( (This)->lpVtbl -> get_State(This,Value) ) 

#define ISwtControls_put_State(This,Value)	\
    ( (This)->lpVtbl -> put_State(This,Value) ) 

#define ISwtControls_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define ISwtControls_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define ISwtControls_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ISwtControls_INTERFACE_DEFINED__ */


#ifndef __ICapControls_INTERFACE_DEFINED__
#define __ICapControls_INTERFACE_DEFINED__

/* interface ICapControls */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ICapControls;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B74C-18C2-11F0-A417-C87F5452571C")
    ICapControls : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Mode( 
            /* [retval][out] */ CapControlModes *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Mode( 
            /* [in] */ CapControlModes Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Capacitor( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Capacitor( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MonitoredObj( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MonitoredObj( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MonitoredTerm( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MonitoredTerm( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CTratio( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CTratio( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PTratio( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PTratio( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ONSetting( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ONSetting( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_OFFSetting( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_OFFSetting( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vmax( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Vmax( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vmin( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Vmin( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_UseVoltOverride( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_UseVoltOverride( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Delay( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Delay( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DelayOff( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_DelayOff( 
            /* [in] */ double Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_DeadTime( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_DeadTime( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ICapControlsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ICapControls * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ICapControls * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ICapControls * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ICapControls * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ICapControls * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ICapControls * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ICapControls * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ICapControls, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            ICapControls * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ICapControls * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            ICapControls * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ICapControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ICapControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_Mode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Mode )( 
            ICapControls * This,
            /* [retval][out] */ CapControlModes *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_Mode)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Mode )( 
            ICapControls * This,
            /* [in] */ CapControlModes Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_Capacitor)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Capacitor )( 
            ICapControls * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_Capacitor)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Capacitor )( 
            ICapControls * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_MonitoredObj)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MonitoredObj )( 
            ICapControls * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_MonitoredObj)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MonitoredObj )( 
            ICapControls * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_MonitoredTerm)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MonitoredTerm )( 
            ICapControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_MonitoredTerm)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MonitoredTerm )( 
            ICapControls * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_CTratio)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CTratio )( 
            ICapControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_CTratio)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CTratio )( 
            ICapControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_PTratio)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PTratio )( 
            ICapControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_PTratio)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PTratio )( 
            ICapControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_ONSetting)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ONSetting )( 
            ICapControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_ONSetting)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ONSetting )( 
            ICapControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_OFFSetting)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_OFFSetting )( 
            ICapControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_OFFSetting)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_OFFSetting )( 
            ICapControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_Vmax)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vmax )( 
            ICapControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_Vmax)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Vmax )( 
            ICapControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_Vmin)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vmin )( 
            ICapControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_Vmin)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Vmin )( 
            ICapControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_UseVoltOverride)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_UseVoltOverride )( 
            ICapControls * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_UseVoltOverride)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_UseVoltOverride )( 
            ICapControls * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_Delay)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Delay )( 
            ICapControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_Delay)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Delay )( 
            ICapControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_DelayOff)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DelayOff )( 
            ICapControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_DelayOff)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_DelayOff )( 
            ICapControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_DeadTime)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DeadTime )( 
            ICapControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_DeadTime)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_DeadTime )( 
            ICapControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ICapControls, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            ICapControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            ICapControls * This);
        
        DECLSPEC_XFGVIRT(ICapControls, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            ICapControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ICapControls, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            ICapControls * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } ICapControlsVtbl;

    interface ICapControls
    {
        CONST_VTBL struct ICapControlsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ICapControls_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ICapControls_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ICapControls_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ICapControls_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ICapControls_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ICapControls_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ICapControls_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ICapControls_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define ICapControls_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ICapControls_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define ICapControls_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ICapControls_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ICapControls_get_Mode(This,Value)	\
    ( (This)->lpVtbl -> get_Mode(This,Value) ) 

#define ICapControls_put_Mode(This,Value)	\
    ( (This)->lpVtbl -> put_Mode(This,Value) ) 

#define ICapControls_get_Capacitor(This,Value)	\
    ( (This)->lpVtbl -> get_Capacitor(This,Value) ) 

#define ICapControls_put_Capacitor(This,Value)	\
    ( (This)->lpVtbl -> put_Capacitor(This,Value) ) 

#define ICapControls_get_MonitoredObj(This,Value)	\
    ( (This)->lpVtbl -> get_MonitoredObj(This,Value) ) 

#define ICapControls_put_MonitoredObj(This,Value)	\
    ( (This)->lpVtbl -> put_MonitoredObj(This,Value) ) 

#define ICapControls_get_MonitoredTerm(This,Value)	\
    ( (This)->lpVtbl -> get_MonitoredTerm(This,Value) ) 

#define ICapControls_put_MonitoredTerm(This,Value)	\
    ( (This)->lpVtbl -> put_MonitoredTerm(This,Value) ) 

#define ICapControls_get_CTratio(This,Value)	\
    ( (This)->lpVtbl -> get_CTratio(This,Value) ) 

#define ICapControls_put_CTratio(This,Value)	\
    ( (This)->lpVtbl -> put_CTratio(This,Value) ) 

#define ICapControls_get_PTratio(This,Value)	\
    ( (This)->lpVtbl -> get_PTratio(This,Value) ) 

#define ICapControls_put_PTratio(This,Value)	\
    ( (This)->lpVtbl -> put_PTratio(This,Value) ) 

#define ICapControls_get_ONSetting(This,Value)	\
    ( (This)->lpVtbl -> get_ONSetting(This,Value) ) 

#define ICapControls_put_ONSetting(This,Value)	\
    ( (This)->lpVtbl -> put_ONSetting(This,Value) ) 

#define ICapControls_get_OFFSetting(This,Value)	\
    ( (This)->lpVtbl -> get_OFFSetting(This,Value) ) 

#define ICapControls_put_OFFSetting(This,Value)	\
    ( (This)->lpVtbl -> put_OFFSetting(This,Value) ) 

#define ICapControls_get_Vmax(This,Value)	\
    ( (This)->lpVtbl -> get_Vmax(This,Value) ) 

#define ICapControls_put_Vmax(This,Value)	\
    ( (This)->lpVtbl -> put_Vmax(This,Value) ) 

#define ICapControls_get_Vmin(This,Value)	\
    ( (This)->lpVtbl -> get_Vmin(This,Value) ) 

#define ICapControls_put_Vmin(This,Value)	\
    ( (This)->lpVtbl -> put_Vmin(This,Value) ) 

#define ICapControls_get_UseVoltOverride(This,Value)	\
    ( (This)->lpVtbl -> get_UseVoltOverride(This,Value) ) 

#define ICapControls_put_UseVoltOverride(This,Value)	\
    ( (This)->lpVtbl -> put_UseVoltOverride(This,Value) ) 

#define ICapControls_get_Delay(This,Value)	\
    ( (This)->lpVtbl -> get_Delay(This,Value) ) 

#define ICapControls_put_Delay(This,Value)	\
    ( (This)->lpVtbl -> put_Delay(This,Value) ) 

#define ICapControls_get_DelayOff(This,Value)	\
    ( (This)->lpVtbl -> get_DelayOff(This,Value) ) 

#define ICapControls_put_DelayOff(This,Value)	\
    ( (This)->lpVtbl -> put_DelayOff(This,Value) ) 

#define ICapControls_get_DeadTime(This,Value)	\
    ( (This)->lpVtbl -> get_DeadTime(This,Value) ) 

#define ICapControls_put_DeadTime(This,Value)	\
    ( (This)->lpVtbl -> put_DeadTime(This,Value) ) 

#define ICapControls_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define ICapControls_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define ICapControls_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define ICapControls_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ICapControls_INTERFACE_DEFINED__ */


#ifndef __IRegControls_INTERFACE_DEFINED__
#define __IRegControls_INTERFACE_DEFINED__

/* interface IRegControls */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IRegControls;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B788-18C2-11F0-A417-C87F5452571C")
    IRegControls : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MonitoredBus( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MonitoredBus( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Transformer( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Transformer( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TapWinding( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_TapWinding( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Winding( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Winding( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CTPrimary( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CTPrimary( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PTratio( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PTratio( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ForwardR( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ForwardR( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ForwardX( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ForwardX( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ReverseR( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ReverseR( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ReverseX( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ReverseX( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsReversible( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsReversible( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsInverseTime( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsInverseTime( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Delay( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Delay( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TapDelay( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_TapDelay( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MaxTapChange( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MaxTapChange( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VoltageLimit( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VoltageLimit( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ForwardBand( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ForwardBand( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ForwardVreg( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ForwardVreg( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ReverseBand( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ReverseBand( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ReverseVreg( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ReverseVreg( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_TapNumber( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_TapNumber( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IRegControlsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IRegControls * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IRegControls * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IRegControls * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IRegControls * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IRegControls * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IRegControls * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IRegControls * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IRegControls, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IRegControls * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IRegControls * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IRegControls * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IRegControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IRegControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_MonitoredBus)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MonitoredBus )( 
            IRegControls * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_MonitoredBus)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MonitoredBus )( 
            IRegControls * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_Transformer)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Transformer )( 
            IRegControls * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_Transformer)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Transformer )( 
            IRegControls * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_TapWinding)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TapWinding )( 
            IRegControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_TapWinding)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_TapWinding )( 
            IRegControls * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_Winding)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Winding )( 
            IRegControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_Winding)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Winding )( 
            IRegControls * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_CTPrimary)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CTPrimary )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_CTPrimary)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CTPrimary )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_PTratio)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PTratio )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_PTratio)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PTratio )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_ForwardR)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ForwardR )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_ForwardR)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ForwardR )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_ForwardX)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ForwardX )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_ForwardX)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ForwardX )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_ReverseR)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ReverseR )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_ReverseR)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ReverseR )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_ReverseX)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ReverseX )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_ReverseX)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ReverseX )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_IsReversible)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsReversible )( 
            IRegControls * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_IsReversible)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsReversible )( 
            IRegControls * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_IsInverseTime)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsInverseTime )( 
            IRegControls * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_IsInverseTime)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsInverseTime )( 
            IRegControls * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_Delay)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Delay )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_Delay)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Delay )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_TapDelay)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TapDelay )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_TapDelay)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_TapDelay )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_MaxTapChange)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MaxTapChange )( 
            IRegControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_MaxTapChange)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MaxTapChange )( 
            IRegControls * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_VoltageLimit)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VoltageLimit )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_VoltageLimit)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VoltageLimit )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_ForwardBand)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ForwardBand )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_ForwardBand)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ForwardBand )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_ForwardVreg)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ForwardVreg )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_ForwardVreg)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ForwardVreg )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_ReverseBand)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ReverseBand )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_ReverseBand)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ReverseBand )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_ReverseVreg)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ReverseVreg )( 
            IRegControls * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_ReverseVreg)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ReverseVreg )( 
            IRegControls * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IRegControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, get_TapNumber)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TapNumber )( 
            IRegControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_TapNumber)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_TapNumber )( 
            IRegControls * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IRegControls, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            IRegControls * This);
        
        DECLSPEC_XFGVIRT(IRegControls, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IRegControls * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRegControls, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IRegControls * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } IRegControlsVtbl;

    interface IRegControls
    {
        CONST_VTBL struct IRegControlsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IRegControls_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IRegControls_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IRegControls_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IRegControls_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IRegControls_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IRegControls_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IRegControls_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IRegControls_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IRegControls_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IRegControls_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IRegControls_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IRegControls_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IRegControls_get_MonitoredBus(This,Value)	\
    ( (This)->lpVtbl -> get_MonitoredBus(This,Value) ) 

#define IRegControls_put_MonitoredBus(This,Value)	\
    ( (This)->lpVtbl -> put_MonitoredBus(This,Value) ) 

#define IRegControls_get_Transformer(This,Value)	\
    ( (This)->lpVtbl -> get_Transformer(This,Value) ) 

#define IRegControls_put_Transformer(This,Value)	\
    ( (This)->lpVtbl -> put_Transformer(This,Value) ) 

#define IRegControls_get_TapWinding(This,Value)	\
    ( (This)->lpVtbl -> get_TapWinding(This,Value) ) 

#define IRegControls_put_TapWinding(This,Value)	\
    ( (This)->lpVtbl -> put_TapWinding(This,Value) ) 

#define IRegControls_get_Winding(This,Value)	\
    ( (This)->lpVtbl -> get_Winding(This,Value) ) 

#define IRegControls_put_Winding(This,Value)	\
    ( (This)->lpVtbl -> put_Winding(This,Value) ) 

#define IRegControls_get_CTPrimary(This,Value)	\
    ( (This)->lpVtbl -> get_CTPrimary(This,Value) ) 

#define IRegControls_put_CTPrimary(This,Value)	\
    ( (This)->lpVtbl -> put_CTPrimary(This,Value) ) 

#define IRegControls_get_PTratio(This,Value)	\
    ( (This)->lpVtbl -> get_PTratio(This,Value) ) 

#define IRegControls_put_PTratio(This,Value)	\
    ( (This)->lpVtbl -> put_PTratio(This,Value) ) 

#define IRegControls_get_ForwardR(This,Value)	\
    ( (This)->lpVtbl -> get_ForwardR(This,Value) ) 

#define IRegControls_put_ForwardR(This,Value)	\
    ( (This)->lpVtbl -> put_ForwardR(This,Value) ) 

#define IRegControls_get_ForwardX(This,Value)	\
    ( (This)->lpVtbl -> get_ForwardX(This,Value) ) 

#define IRegControls_put_ForwardX(This,Value)	\
    ( (This)->lpVtbl -> put_ForwardX(This,Value) ) 

#define IRegControls_get_ReverseR(This,Value)	\
    ( (This)->lpVtbl -> get_ReverseR(This,Value) ) 

#define IRegControls_put_ReverseR(This,Value)	\
    ( (This)->lpVtbl -> put_ReverseR(This,Value) ) 

#define IRegControls_get_ReverseX(This,Value)	\
    ( (This)->lpVtbl -> get_ReverseX(This,Value) ) 

#define IRegControls_put_ReverseX(This,Value)	\
    ( (This)->lpVtbl -> put_ReverseX(This,Value) ) 

#define IRegControls_get_IsReversible(This,Value)	\
    ( (This)->lpVtbl -> get_IsReversible(This,Value) ) 

#define IRegControls_put_IsReversible(This,Value)	\
    ( (This)->lpVtbl -> put_IsReversible(This,Value) ) 

#define IRegControls_get_IsInverseTime(This,Value)	\
    ( (This)->lpVtbl -> get_IsInverseTime(This,Value) ) 

#define IRegControls_put_IsInverseTime(This,Value)	\
    ( (This)->lpVtbl -> put_IsInverseTime(This,Value) ) 

#define IRegControls_get_Delay(This,Value)	\
    ( (This)->lpVtbl -> get_Delay(This,Value) ) 

#define IRegControls_put_Delay(This,Value)	\
    ( (This)->lpVtbl -> put_Delay(This,Value) ) 

#define IRegControls_get_TapDelay(This,Value)	\
    ( (This)->lpVtbl -> get_TapDelay(This,Value) ) 

#define IRegControls_put_TapDelay(This,Value)	\
    ( (This)->lpVtbl -> put_TapDelay(This,Value) ) 

#define IRegControls_get_MaxTapChange(This,Value)	\
    ( (This)->lpVtbl -> get_MaxTapChange(This,Value) ) 

#define IRegControls_put_MaxTapChange(This,Value)	\
    ( (This)->lpVtbl -> put_MaxTapChange(This,Value) ) 

#define IRegControls_get_VoltageLimit(This,Value)	\
    ( (This)->lpVtbl -> get_VoltageLimit(This,Value) ) 

#define IRegControls_put_VoltageLimit(This,Value)	\
    ( (This)->lpVtbl -> put_VoltageLimit(This,Value) ) 

#define IRegControls_get_ForwardBand(This,Value)	\
    ( (This)->lpVtbl -> get_ForwardBand(This,Value) ) 

#define IRegControls_put_ForwardBand(This,Value)	\
    ( (This)->lpVtbl -> put_ForwardBand(This,Value) ) 

#define IRegControls_get_ForwardVreg(This,Value)	\
    ( (This)->lpVtbl -> get_ForwardVreg(This,Value) ) 

#define IRegControls_put_ForwardVreg(This,Value)	\
    ( (This)->lpVtbl -> put_ForwardVreg(This,Value) ) 

#define IRegControls_get_ReverseBand(This,Value)	\
    ( (This)->lpVtbl -> get_ReverseBand(This,Value) ) 

#define IRegControls_put_ReverseBand(This,Value)	\
    ( (This)->lpVtbl -> put_ReverseBand(This,Value) ) 

#define IRegControls_get_ReverseVreg(This,Value)	\
    ( (This)->lpVtbl -> get_ReverseVreg(This,Value) ) 

#define IRegControls_put_ReverseVreg(This,Value)	\
    ( (This)->lpVtbl -> put_ReverseVreg(This,Value) ) 

#define IRegControls_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IRegControls_get_TapNumber(This,Value)	\
    ( (This)->lpVtbl -> get_TapNumber(This,Value) ) 

#define IRegControls_put_TapNumber(This,Value)	\
    ( (This)->lpVtbl -> put_TapNumber(This,Value) ) 

#define IRegControls_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define IRegControls_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IRegControls_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IRegControls_INTERFACE_DEFINED__ */


#ifndef __ITopology_INTERFACE_DEFINED__
#define __ITopology_INTERFACE_DEFINED__

/* interface ITopology */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_ITopology;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B81E-18C2-11F0-A417-C87F5452571C")
    ITopology : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumLoops( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumIsolatedBranches( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllLoopedPairs( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllIsolatedBranches( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumIsolatedLoads( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllIsolatedLoads( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_BranchName( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_BranchName( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveBranch( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ForwardBranch( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_BackwardBranch( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LoopedBranch( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ParallelBranch( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_FirstLoad( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NextLoad( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveLevel( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_BusName( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_BusName( 
            /* [in] */ BSTR Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ITopologyVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ITopology * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ITopology * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ITopology * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ITopology * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ITopology * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ITopology * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ITopology * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ITopology, get_NumLoops)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumLoops )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_NumIsolatedBranches)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumIsolatedBranches )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_AllLoopedPairs)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllLoopedPairs )( 
            ITopology * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_AllIsolatedBranches)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllIsolatedBranches )( 
            ITopology * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_NumIsolatedLoads)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumIsolatedLoads )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_AllIsolatedLoads)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllIsolatedLoads )( 
            ITopology * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_BranchName)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_BranchName )( 
            ITopology * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ITopology, put_BranchName)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_BranchName )( 
            ITopology * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_ActiveBranch)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveBranch )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_ForwardBranch)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ForwardBranch )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_BackwardBranch)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_BackwardBranch )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_LoopedBranch)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LoopedBranch )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_ParallelBranch)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ParallelBranch )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_FirstLoad)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_FirstLoad )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_NextLoad)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NextLoad )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_ActiveLevel)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveLevel )( 
            ITopology * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ITopology, get_BusName)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_BusName )( 
            ITopology * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ITopology, put_BusName)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_BusName )( 
            ITopology * This,
            /* [in] */ BSTR Value);
        
        END_INTERFACE
    } ITopologyVtbl;

    interface ITopology
    {
        CONST_VTBL struct ITopologyVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ITopology_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ITopology_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ITopology_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ITopology_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ITopology_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ITopology_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ITopology_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ITopology_get_NumLoops(This,Value)	\
    ( (This)->lpVtbl -> get_NumLoops(This,Value) ) 

#define ITopology_get_NumIsolatedBranches(This,Value)	\
    ( (This)->lpVtbl -> get_NumIsolatedBranches(This,Value) ) 

#define ITopology_get_AllLoopedPairs(This,Value)	\
    ( (This)->lpVtbl -> get_AllLoopedPairs(This,Value) ) 

#define ITopology_get_AllIsolatedBranches(This,Value)	\
    ( (This)->lpVtbl -> get_AllIsolatedBranches(This,Value) ) 

#define ITopology_get_NumIsolatedLoads(This,Value)	\
    ( (This)->lpVtbl -> get_NumIsolatedLoads(This,Value) ) 

#define ITopology_get_AllIsolatedLoads(This,Value)	\
    ( (This)->lpVtbl -> get_AllIsolatedLoads(This,Value) ) 

#define ITopology_get_BranchName(This,Value)	\
    ( (This)->lpVtbl -> get_BranchName(This,Value) ) 

#define ITopology_put_BranchName(This,Value)	\
    ( (This)->lpVtbl -> put_BranchName(This,Value) ) 

#define ITopology_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ITopology_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ITopology_get_ActiveBranch(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveBranch(This,Value) ) 

#define ITopology_get_ForwardBranch(This,Value)	\
    ( (This)->lpVtbl -> get_ForwardBranch(This,Value) ) 

#define ITopology_get_BackwardBranch(This,Value)	\
    ( (This)->lpVtbl -> get_BackwardBranch(This,Value) ) 

#define ITopology_get_LoopedBranch(This,Value)	\
    ( (This)->lpVtbl -> get_LoopedBranch(This,Value) ) 

#define ITopology_get_ParallelBranch(This,Value)	\
    ( (This)->lpVtbl -> get_ParallelBranch(This,Value) ) 

#define ITopology_get_FirstLoad(This,Value)	\
    ( (This)->lpVtbl -> get_FirstLoad(This,Value) ) 

#define ITopology_get_NextLoad(This,Value)	\
    ( (This)->lpVtbl -> get_NextLoad(This,Value) ) 

#define ITopology_get_ActiveLevel(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveLevel(This,Value) ) 

#define ITopology_get_BusName(This,Value)	\
    ( (This)->lpVtbl -> get_BusName(This,Value) ) 

#define ITopology_put_BusName(This,Value)	\
    ( (This)->lpVtbl -> put_BusName(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ITopology_INTERFACE_DEFINED__ */


#ifndef __IDSS_Executive_INTERFACE_DEFINED__
#define __IDSS_Executive_INTERFACE_DEFINED__

/* interface IDSS_Executive */
/* [object][oleautomation][dual][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IDSS_Executive;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B846-18C2-11F0-A417-C87F5452571C")
    IDSS_Executive : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumCommands( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumOptions( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Command( 
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Option( 
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CommandHelp( 
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_OptionHelp( 
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_OptionValue( 
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IDSS_ExecutiveVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IDSS_Executive * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IDSS_Executive * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IDSS_Executive * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IDSS_Executive * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IDSS_Executive * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IDSS_Executive * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IDSS_Executive * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IDSS_Executive, get_NumCommands)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumCommands )( 
            IDSS_Executive * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IDSS_Executive, get_NumOptions)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumOptions )( 
            IDSS_Executive * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IDSS_Executive, get_Command)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Command )( 
            IDSS_Executive * This,
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IDSS_Executive, get_Option)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Option )( 
            IDSS_Executive * This,
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IDSS_Executive, get_CommandHelp)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CommandHelp )( 
            IDSS_Executive * This,
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IDSS_Executive, get_OptionHelp)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_OptionHelp )( 
            IDSS_Executive * This,
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IDSS_Executive, get_OptionValue)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_OptionValue )( 
            IDSS_Executive * This,
            /* [in] */ long i,
            /* [retval][out] */ BSTR *Value);
        
        END_INTERFACE
    } IDSS_ExecutiveVtbl;

    interface IDSS_Executive
    {
        CONST_VTBL struct IDSS_ExecutiveVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IDSS_Executive_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IDSS_Executive_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IDSS_Executive_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IDSS_Executive_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IDSS_Executive_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IDSS_Executive_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IDSS_Executive_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IDSS_Executive_get_NumCommands(This,Value)	\
    ( (This)->lpVtbl -> get_NumCommands(This,Value) ) 

#define IDSS_Executive_get_NumOptions(This,Value)	\
    ( (This)->lpVtbl -> get_NumOptions(This,Value) ) 

#define IDSS_Executive_get_Command(This,i,Value)	\
    ( (This)->lpVtbl -> get_Command(This,i,Value) ) 

#define IDSS_Executive_get_Option(This,i,Value)	\
    ( (This)->lpVtbl -> get_Option(This,i,Value) ) 

#define IDSS_Executive_get_CommandHelp(This,i,Value)	\
    ( (This)->lpVtbl -> get_CommandHelp(This,i,Value) ) 

#define IDSS_Executive_get_OptionHelp(This,i,Value)	\
    ( (This)->lpVtbl -> get_OptionHelp(This,i,Value) ) 

#define IDSS_Executive_get_OptionValue(This,i,Value)	\
    ( (This)->lpVtbl -> get_OptionValue(This,i,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IDSS_Executive_INTERFACE_DEFINED__ */


#ifndef __ISensors_INTERFACE_DEFINED__
#define __ISensors_INTERFACE_DEFINED__

/* interface ISensors */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_ISensors;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B8C8-18C2-11F0-A417-C87F5452571C")
    ISensors : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsDelta( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsDelta( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ReverseDelta( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ReverseDelta( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PctError( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PctError( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Weight( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Weight( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MeteredElement( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MeteredElement( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MeteredTerminal( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MeteredTerminal( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ResetAll( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kVBase( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kVBase( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Currents( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Currents( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kVS( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kVS( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kVARS( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kVARS( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kWS( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kWS( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllocationFactor( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ISensorsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ISensors * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ISensors * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ISensors * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ISensors * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ISensors * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ISensors * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ISensors * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ISensors, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ISensors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            ISensors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            ISensors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ISensors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ISensors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            ISensors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_IsDelta)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsDelta )( 
            ISensors * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_IsDelta)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsDelta )( 
            ISensors * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_ReverseDelta)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ReverseDelta )( 
            ISensors * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_ReverseDelta)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ReverseDelta )( 
            ISensors * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_PctError)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PctError )( 
            ISensors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_PctError)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PctError )( 
            ISensors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_Weight)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Weight )( 
            ISensors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_Weight)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Weight )( 
            ISensors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_MeteredElement)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MeteredElement )( 
            ISensors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_MeteredElement)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MeteredElement )( 
            ISensors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_MeteredTerminal)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MeteredTerminal )( 
            ISensors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_MeteredTerminal)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MeteredTerminal )( 
            ISensors * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ISensors, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            ISensors * This);
        
        DECLSPEC_XFGVIRT(ISensors, ResetAll)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ResetAll )( 
            ISensors * This);
        
        DECLSPEC_XFGVIRT(ISensors, get_kVBase)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kVBase )( 
            ISensors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_kVBase)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kVBase )( 
            ISensors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_Currents)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Currents )( 
            ISensors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_Currents)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Currents )( 
            ISensors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_kVS)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kVS )( 
            ISensors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_kVS)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kVS )( 
            ISensors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_kVARS)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kVARS )( 
            ISensors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_kVARS)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kVARS )( 
            ISensors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_kWS)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kWS )( 
            ISensors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_kWS)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kWS )( 
            ISensors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_AllocationFactor)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllocationFactor )( 
            ISensors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ISensors, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            ISensors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ISensors, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            ISensors * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } ISensorsVtbl;

    interface ISensors
    {
        CONST_VTBL struct ISensorsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ISensors_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ISensors_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ISensors_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ISensors_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ISensors_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ISensors_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ISensors_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ISensors_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ISensors_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define ISensors_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define ISensors_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ISensors_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ISensors_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define ISensors_get_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> get_IsDelta(This,Value) ) 

#define ISensors_put_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> put_IsDelta(This,Value) ) 

#define ISensors_get_ReverseDelta(This,Value)	\
    ( (This)->lpVtbl -> get_ReverseDelta(This,Value) ) 

#define ISensors_put_ReverseDelta(This,Value)	\
    ( (This)->lpVtbl -> put_ReverseDelta(This,Value) ) 

#define ISensors_get_PctError(This,Value)	\
    ( (This)->lpVtbl -> get_PctError(This,Value) ) 

#define ISensors_put_PctError(This,Value)	\
    ( (This)->lpVtbl -> put_PctError(This,Value) ) 

#define ISensors_get_Weight(This,Value)	\
    ( (This)->lpVtbl -> get_Weight(This,Value) ) 

#define ISensors_put_Weight(This,Value)	\
    ( (This)->lpVtbl -> put_Weight(This,Value) ) 

#define ISensors_get_MeteredElement(This,Value)	\
    ( (This)->lpVtbl -> get_MeteredElement(This,Value) ) 

#define ISensors_put_MeteredElement(This,Value)	\
    ( (This)->lpVtbl -> put_MeteredElement(This,Value) ) 

#define ISensors_get_MeteredTerminal(This,Value)	\
    ( (This)->lpVtbl -> get_MeteredTerminal(This,Value) ) 

#define ISensors_put_MeteredTerminal(This,Value)	\
    ( (This)->lpVtbl -> put_MeteredTerminal(This,Value) ) 

#define ISensors_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define ISensors_ResetAll(This)	\
    ( (This)->lpVtbl -> ResetAll(This) ) 

#define ISensors_get_kVBase(This,Value)	\
    ( (This)->lpVtbl -> get_kVBase(This,Value) ) 

#define ISensors_put_kVBase(This,Value)	\
    ( (This)->lpVtbl -> put_kVBase(This,Value) ) 

#define ISensors_get_Currents(This,Value)	\
    ( (This)->lpVtbl -> get_Currents(This,Value) ) 

#define ISensors_put_Currents(This,Value)	\
    ( (This)->lpVtbl -> put_Currents(This,Value) ) 

#define ISensors_get_kVS(This,Value)	\
    ( (This)->lpVtbl -> get_kVS(This,Value) ) 

#define ISensors_put_kVS(This,Value)	\
    ( (This)->lpVtbl -> put_kVS(This,Value) ) 

#define ISensors_get_kVARS(This,Value)	\
    ( (This)->lpVtbl -> get_kVARS(This,Value) ) 

#define ISensors_put_kVARS(This,Value)	\
    ( (This)->lpVtbl -> put_kVARS(This,Value) ) 

#define ISensors_get_kWS(This,Value)	\
    ( (This)->lpVtbl -> get_kWS(This,Value) ) 

#define ISensors_put_kWS(This,Value)	\
    ( (This)->lpVtbl -> put_kWS(This,Value) ) 

#define ISensors_get_AllocationFactor(This,Value)	\
    ( (This)->lpVtbl -> get_AllocationFactor(This,Value) ) 

#define ISensors_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define ISensors_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ISensors_INTERFACE_DEFINED__ */


#ifndef __IXYCurves_INTERFACE_DEFINED__
#define __IXYCurves_INTERFACE_DEFINED__

/* interface IXYCurves */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IXYCurves;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B90E-18C2-11F0-A417-C87F5452571C")
    IXYCurves : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Npts( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Npts( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xarray( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xarray( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Yarray( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Yarray( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_x( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_x( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_y( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_y( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xshift( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xshift( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Yshift( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Yshift( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xscale( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xscale( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Yscale( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Yscale( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IXYCurvesVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IXYCurves * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IXYCurves * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IXYCurves * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IXYCurves * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IXYCurves * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IXYCurves * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IXYCurves * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IXYCurves * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IXYCurves * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IXYCurves * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IXYCurves * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IXYCurves * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Npts)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Npts )( 
            IXYCurves * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_Npts)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Npts )( 
            IXYCurves * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Xarray)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xarray )( 
            IXYCurves * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_Xarray)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xarray )( 
            IXYCurves * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Yarray)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Yarray )( 
            IXYCurves * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_Yarray)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Yarray )( 
            IXYCurves * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_x)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_x )( 
            IXYCurves * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_x)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_x )( 
            IXYCurves * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_y)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_y )( 
            IXYCurves * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_y)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_y )( 
            IXYCurves * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Xshift)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xshift )( 
            IXYCurves * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_Xshift)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xshift )( 
            IXYCurves * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Yshift)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Yshift )( 
            IXYCurves * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_Yshift)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Yshift )( 
            IXYCurves * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Xscale)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xscale )( 
            IXYCurves * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_Xscale)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xscale )( 
            IXYCurves * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_Yscale)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Yscale )( 
            IXYCurves * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_Yscale)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Yscale )( 
            IXYCurves * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IXYCurves * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IXYCurves, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IXYCurves * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } IXYCurvesVtbl;

    interface IXYCurves
    {
        CONST_VTBL struct IXYCurvesVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IXYCurves_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IXYCurves_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IXYCurves_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IXYCurves_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IXYCurves_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IXYCurves_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IXYCurves_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IXYCurves_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IXYCurves_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IXYCurves_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IXYCurves_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IXYCurves_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IXYCurves_get_Npts(This,Value)	\
    ( (This)->lpVtbl -> get_Npts(This,Value) ) 

#define IXYCurves_put_Npts(This,Value)	\
    ( (This)->lpVtbl -> put_Npts(This,Value) ) 

#define IXYCurves_get_Xarray(This,Value)	\
    ( (This)->lpVtbl -> get_Xarray(This,Value) ) 

#define IXYCurves_put_Xarray(This,Value)	\
    ( (This)->lpVtbl -> put_Xarray(This,Value) ) 

#define IXYCurves_get_Yarray(This,Value)	\
    ( (This)->lpVtbl -> get_Yarray(This,Value) ) 

#define IXYCurves_put_Yarray(This,Value)	\
    ( (This)->lpVtbl -> put_Yarray(This,Value) ) 

#define IXYCurves_get_x(This,Value)	\
    ( (This)->lpVtbl -> get_x(This,Value) ) 

#define IXYCurves_put_x(This,Value)	\
    ( (This)->lpVtbl -> put_x(This,Value) ) 

#define IXYCurves_get_y(This,Value)	\
    ( (This)->lpVtbl -> get_y(This,Value) ) 

#define IXYCurves_put_y(This,Value)	\
    ( (This)->lpVtbl -> put_y(This,Value) ) 

#define IXYCurves_get_Xshift(This,Value)	\
    ( (This)->lpVtbl -> get_Xshift(This,Value) ) 

#define IXYCurves_put_Xshift(This,Value)	\
    ( (This)->lpVtbl -> put_Xshift(This,Value) ) 

#define IXYCurves_get_Yshift(This,Value)	\
    ( (This)->lpVtbl -> get_Yshift(This,Value) ) 

#define IXYCurves_put_Yshift(This,Value)	\
    ( (This)->lpVtbl -> put_Yshift(This,Value) ) 

#define IXYCurves_get_Xscale(This,Value)	\
    ( (This)->lpVtbl -> get_Xscale(This,Value) ) 

#define IXYCurves_put_Xscale(This,Value)	\
    ( (This)->lpVtbl -> put_Xscale(This,Value) ) 

#define IXYCurves_get_Yscale(This,Value)	\
    ( (This)->lpVtbl -> get_Yscale(This,Value) ) 

#define IXYCurves_put_Yscale(This,Value)	\
    ( (This)->lpVtbl -> put_Yscale(This,Value) ) 

#define IXYCurves_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IXYCurves_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IXYCurves_INTERFACE_DEFINED__ */


#ifndef __IPDElements_INTERFACE_DEFINED__
#define __IPDElements_INTERFACE_DEFINED__

/* interface IPDElements */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IPDElements;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B954-18C2-11F0-A417-C87F5452571C")
    IPDElements : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsShunt( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_FaultRate( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_FaultRate( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_pctPermanent( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_pctPermanent( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Lambda( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AccumulatedL( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Numcustomers( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TotalCustomers( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ParentPDElement( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_FromTerminal( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TotalMiles( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SectionID( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RepairTime( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_RepairTime( 
            /* [in] */ double Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IPDElementsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IPDElements * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IPDElements * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IPDElements * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IPDElements * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IPDElements * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IPDElements * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IPDElements * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IPDElements, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IPDElements * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IPDElements * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IPDElements * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_IsShunt)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsShunt )( 
            IPDElements * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_FaultRate)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_FaultRate )( 
            IPDElements * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, put_FaultRate)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_FaultRate )( 
            IPDElements * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_pctPermanent)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_pctPermanent )( 
            IPDElements * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, put_pctPermanent)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_pctPermanent )( 
            IPDElements * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IPDElements * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IPDElements * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_Lambda)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Lambda )( 
            IPDElements * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_AccumulatedL)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AccumulatedL )( 
            IPDElements * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_Numcustomers)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Numcustomers )( 
            IPDElements * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_TotalCustomers)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TotalCustomers )( 
            IPDElements * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_ParentPDElement)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ParentPDElement )( 
            IPDElements * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_FromTerminal)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_FromTerminal )( 
            IPDElements * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_TotalMiles)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TotalMiles )( 
            IPDElements * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_SectionID)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SectionID )( 
            IPDElements * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, get_RepairTime)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RepairTime )( 
            IPDElements * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPDElements, put_RepairTime)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_RepairTime )( 
            IPDElements * This,
            /* [in] */ double Value);
        
        END_INTERFACE
    } IPDElementsVtbl;

    interface IPDElements
    {
        CONST_VTBL struct IPDElementsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IPDElements_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IPDElements_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IPDElements_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IPDElements_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IPDElements_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IPDElements_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IPDElements_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IPDElements_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IPDElements_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IPDElements_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IPDElements_get_IsShunt(This,Value)	\
    ( (This)->lpVtbl -> get_IsShunt(This,Value) ) 

#define IPDElements_get_FaultRate(This,Value)	\
    ( (This)->lpVtbl -> get_FaultRate(This,Value) ) 

#define IPDElements_put_FaultRate(This,Value)	\
    ( (This)->lpVtbl -> put_FaultRate(This,Value) ) 

#define IPDElements_get_pctPermanent(This,Value)	\
    ( (This)->lpVtbl -> get_pctPermanent(This,Value) ) 

#define IPDElements_put_pctPermanent(This,Value)	\
    ( (This)->lpVtbl -> put_pctPermanent(This,Value) ) 

#define IPDElements_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IPDElements_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IPDElements_get_Lambda(This,Value)	\
    ( (This)->lpVtbl -> get_Lambda(This,Value) ) 

#define IPDElements_get_AccumulatedL(This,Value)	\
    ( (This)->lpVtbl -> get_AccumulatedL(This,Value) ) 

#define IPDElements_get_Numcustomers(This,Value)	\
    ( (This)->lpVtbl -> get_Numcustomers(This,Value) ) 

#define IPDElements_get_TotalCustomers(This,Value)	\
    ( (This)->lpVtbl -> get_TotalCustomers(This,Value) ) 

#define IPDElements_get_ParentPDElement(This,Value)	\
    ( (This)->lpVtbl -> get_ParentPDElement(This,Value) ) 

#define IPDElements_get_FromTerminal(This,Value)	\
    ( (This)->lpVtbl -> get_FromTerminal(This,Value) ) 

#define IPDElements_get_TotalMiles(This,Value)	\
    ( (This)->lpVtbl -> get_TotalMiles(This,Value) ) 

#define IPDElements_get_SectionID(This,Value)	\
    ( (This)->lpVtbl -> get_SectionID(This,Value) ) 

#define IPDElements_get_RepairTime(This,Value)	\
    ( (This)->lpVtbl -> get_RepairTime(This,Value) ) 

#define IPDElements_put_RepairTime(This,Value)	\
    ( (This)->lpVtbl -> put_RepairTime(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IPDElements_INTERFACE_DEFINED__ */


#ifndef __IReclosers_INTERFACE_DEFINED__
#define __IReclosers_INTERFACE_DEFINED__

/* interface IReclosers */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IReclosers;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B99A-18C2-11F0-A417-C87F5452571C")
    IReclosers : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MonitoredObj( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MonitoredObj( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MonitoredTerm( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MonitoredTerm( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SwitchedObj( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SwitchedObj( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SwitchedTerm( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SwitchedTerm( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumFast( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NumFast( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Shots( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Shots( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RecloseIntervals( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PhaseTrip( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PhaseTrip( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PhaseInst( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PhaseInst( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_GroundTrip( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_GroundTrip( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_GroundInst( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_GroundInst( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Open( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Close( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NormalState( 
            /* [retval][out] */ ActionCodes *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NormalState( 
            /* [in] */ ActionCodes Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_State( 
            /* [retval][out] */ ActionCodes *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_State( 
            /* [in] */ ActionCodes Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IReclosersVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IReclosers * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IReclosers * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IReclosers * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IReclosers * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IReclosers * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IReclosers * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IReclosers * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IReclosers, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IReclosers * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IReclosers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IReclosers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IReclosers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IReclosers * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IReclosers * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_MonitoredObj)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MonitoredObj )( 
            IReclosers * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_MonitoredObj)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MonitoredObj )( 
            IReclosers * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_MonitoredTerm)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MonitoredTerm )( 
            IReclosers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_MonitoredTerm)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MonitoredTerm )( 
            IReclosers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_SwitchedObj)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwitchedObj )( 
            IReclosers * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_SwitchedObj)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SwitchedObj )( 
            IReclosers * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_SwitchedTerm)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwitchedTerm )( 
            IReclosers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_SwitchedTerm)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SwitchedTerm )( 
            IReclosers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_NumFast)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumFast )( 
            IReclosers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_NumFast)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NumFast )( 
            IReclosers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_Shots)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Shots )( 
            IReclosers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_Shots)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Shots )( 
            IReclosers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_RecloseIntervals)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RecloseIntervals )( 
            IReclosers * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_PhaseTrip)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PhaseTrip )( 
            IReclosers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_PhaseTrip)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PhaseTrip )( 
            IReclosers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_PhaseInst)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PhaseInst )( 
            IReclosers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_PhaseInst)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PhaseInst )( 
            IReclosers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_GroundTrip)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_GroundTrip )( 
            IReclosers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_GroundTrip)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_GroundTrip )( 
            IReclosers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_GroundInst)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_GroundInst )( 
            IReclosers * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_GroundInst)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_GroundInst )( 
            IReclosers * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReclosers, Open)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Open )( 
            IReclosers * This);
        
        DECLSPEC_XFGVIRT(IReclosers, Close)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Close )( 
            IReclosers * This);
        
        DECLSPEC_XFGVIRT(IReclosers, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IReclosers * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IReclosers * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IReclosers, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            IReclosers * This);
        
        DECLSPEC_XFGVIRT(IReclosers, get_NormalState)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NormalState )( 
            IReclosers * This,
            /* [retval][out] */ ActionCodes *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_NormalState)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NormalState )( 
            IReclosers * This,
            /* [in] */ ActionCodes Value);
        
        DECLSPEC_XFGVIRT(IReclosers, get_State)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_State )( 
            IReclosers * This,
            /* [retval][out] */ ActionCodes *Value);
        
        DECLSPEC_XFGVIRT(IReclosers, put_State)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_State )( 
            IReclosers * This,
            /* [in] */ ActionCodes Value);
        
        END_INTERFACE
    } IReclosersVtbl;

    interface IReclosers
    {
        CONST_VTBL struct IReclosersVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IReclosers_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IReclosers_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IReclosers_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IReclosers_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IReclosers_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IReclosers_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IReclosers_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IReclosers_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IReclosers_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IReclosers_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IReclosers_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IReclosers_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IReclosers_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IReclosers_get_MonitoredObj(This,Value)	\
    ( (This)->lpVtbl -> get_MonitoredObj(This,Value) ) 

#define IReclosers_put_MonitoredObj(This,Value)	\
    ( (This)->lpVtbl -> put_MonitoredObj(This,Value) ) 

#define IReclosers_get_MonitoredTerm(This,Value)	\
    ( (This)->lpVtbl -> get_MonitoredTerm(This,Value) ) 

#define IReclosers_put_MonitoredTerm(This,Value)	\
    ( (This)->lpVtbl -> put_MonitoredTerm(This,Value) ) 

#define IReclosers_get_SwitchedObj(This,Value)	\
    ( (This)->lpVtbl -> get_SwitchedObj(This,Value) ) 

#define IReclosers_put_SwitchedObj(This,Value)	\
    ( (This)->lpVtbl -> put_SwitchedObj(This,Value) ) 

#define IReclosers_get_SwitchedTerm(This,Value)	\
    ( (This)->lpVtbl -> get_SwitchedTerm(This,Value) ) 

#define IReclosers_put_SwitchedTerm(This,Value)	\
    ( (This)->lpVtbl -> put_SwitchedTerm(This,Value) ) 

#define IReclosers_get_NumFast(This,Value)	\
    ( (This)->lpVtbl -> get_NumFast(This,Value) ) 

#define IReclosers_put_NumFast(This,Value)	\
    ( (This)->lpVtbl -> put_NumFast(This,Value) ) 

#define IReclosers_get_Shots(This,Value)	\
    ( (This)->lpVtbl -> get_Shots(This,Value) ) 

#define IReclosers_put_Shots(This,Value)	\
    ( (This)->lpVtbl -> put_Shots(This,Value) ) 

#define IReclosers_get_RecloseIntervals(This,Value)	\
    ( (This)->lpVtbl -> get_RecloseIntervals(This,Value) ) 

#define IReclosers_get_PhaseTrip(This,Value)	\
    ( (This)->lpVtbl -> get_PhaseTrip(This,Value) ) 

#define IReclosers_put_PhaseTrip(This,Value)	\
    ( (This)->lpVtbl -> put_PhaseTrip(This,Value) ) 

#define IReclosers_get_PhaseInst(This,Value)	\
    ( (This)->lpVtbl -> get_PhaseInst(This,Value) ) 

#define IReclosers_put_PhaseInst(This,Value)	\
    ( (This)->lpVtbl -> put_PhaseInst(This,Value) ) 

#define IReclosers_get_GroundTrip(This,Value)	\
    ( (This)->lpVtbl -> get_GroundTrip(This,Value) ) 

#define IReclosers_put_GroundTrip(This,Value)	\
    ( (This)->lpVtbl -> put_GroundTrip(This,Value) ) 

#define IReclosers_get_GroundInst(This,Value)	\
    ( (This)->lpVtbl -> get_GroundInst(This,Value) ) 

#define IReclosers_put_GroundInst(This,Value)	\
    ( (This)->lpVtbl -> put_GroundInst(This,Value) ) 

#define IReclosers_Open(This)	\
    ( (This)->lpVtbl -> Open(This) ) 

#define IReclosers_Close(This)	\
    ( (This)->lpVtbl -> Close(This) ) 

#define IReclosers_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IReclosers_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define IReclosers_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define IReclosers_get_NormalState(This,Value)	\
    ( (This)->lpVtbl -> get_NormalState(This,Value) ) 

#define IReclosers_put_NormalState(This,Value)	\
    ( (This)->lpVtbl -> put_NormalState(This,Value) ) 

#define IReclosers_get_State(This,Value)	\
    ( (This)->lpVtbl -> get_State(This,Value) ) 

#define IReclosers_put_State(This,Value)	\
    ( (This)->lpVtbl -> put_State(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IReclosers_INTERFACE_DEFINED__ */


#ifndef __IRelays_INTERFACE_DEFINED__
#define __IRelays_INTERFACE_DEFINED__

/* interface IRelays */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IRelays;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924B9FE-18C2-11F0-A417-C87F5452571C")
    IRelays : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MonitoredObj( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MonitoredObj( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MonitoredTerm( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MonitoredTerm( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SwitchedObj( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SwitchedObj( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_SwitchedTerm( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_SwitchedTerm( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Open( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Close( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_State( 
            /* [retval][out] */ ActionCodes *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_State( 
            /* [in] */ ActionCodes Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NormalState( 
            /* [retval][out] */ ActionCodes *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NormalState( 
            /* [in] */ ActionCodes Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IRelaysVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IRelays * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IRelays * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IRelays * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IRelays * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IRelays * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IRelays * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IRelays * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IRelays, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IRelays * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IRelays * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IRelays * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IRelays * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IRelays * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IRelays, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IRelays * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_MonitoredObj)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MonitoredObj )( 
            IRelays * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IRelays, put_MonitoredObj)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MonitoredObj )( 
            IRelays * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_MonitoredTerm)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MonitoredTerm )( 
            IRelays * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRelays, put_MonitoredTerm)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MonitoredTerm )( 
            IRelays * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_SwitchedObj)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwitchedObj )( 
            IRelays * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IRelays, put_SwitchedObj)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SwitchedObj )( 
            IRelays * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_SwitchedTerm)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwitchedTerm )( 
            IRelays * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRelays, put_SwitchedTerm)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SwitchedTerm )( 
            IRelays * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IRelays * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IRelays, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IRelays * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IRelays, Open)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Open )( 
            IRelays * This);
        
        DECLSPEC_XFGVIRT(IRelays, Close)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Close )( 
            IRelays * This);
        
        DECLSPEC_XFGVIRT(IRelays, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            IRelays * This);
        
        DECLSPEC_XFGVIRT(IRelays, get_State)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_State )( 
            IRelays * This,
            /* [retval][out] */ ActionCodes *Value);
        
        DECLSPEC_XFGVIRT(IRelays, put_State)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_State )( 
            IRelays * This,
            /* [in] */ ActionCodes Value);
        
        DECLSPEC_XFGVIRT(IRelays, get_NormalState)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NormalState )( 
            IRelays * This,
            /* [retval][out] */ ActionCodes *Value);
        
        DECLSPEC_XFGVIRT(IRelays, put_NormalState)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NormalState )( 
            IRelays * This,
            /* [in] */ ActionCodes Value);
        
        END_INTERFACE
    } IRelaysVtbl;

    interface IRelays
    {
        CONST_VTBL struct IRelaysVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IRelays_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IRelays_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IRelays_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IRelays_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IRelays_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IRelays_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IRelays_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IRelays_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IRelays_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IRelays_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IRelays_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IRelays_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IRelays_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IRelays_get_MonitoredObj(This,Value)	\
    ( (This)->lpVtbl -> get_MonitoredObj(This,Value) ) 

#define IRelays_put_MonitoredObj(This,Value)	\
    ( (This)->lpVtbl -> put_MonitoredObj(This,Value) ) 

#define IRelays_get_MonitoredTerm(This,Value)	\
    ( (This)->lpVtbl -> get_MonitoredTerm(This,Value) ) 

#define IRelays_put_MonitoredTerm(This,Value)	\
    ( (This)->lpVtbl -> put_MonitoredTerm(This,Value) ) 

#define IRelays_get_SwitchedObj(This,Value)	\
    ( (This)->lpVtbl -> get_SwitchedObj(This,Value) ) 

#define IRelays_put_SwitchedObj(This,Value)	\
    ( (This)->lpVtbl -> put_SwitchedObj(This,Value) ) 

#define IRelays_get_SwitchedTerm(This,Value)	\
    ( (This)->lpVtbl -> get_SwitchedTerm(This,Value) ) 

#define IRelays_put_SwitchedTerm(This,Value)	\
    ( (This)->lpVtbl -> put_SwitchedTerm(This,Value) ) 

#define IRelays_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IRelays_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define IRelays_Open(This)	\
    ( (This)->lpVtbl -> Open(This) ) 

#define IRelays_Close(This)	\
    ( (This)->lpVtbl -> Close(This) ) 

#define IRelays_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define IRelays_get_State(This,Value)	\
    ( (This)->lpVtbl -> get_State(This,Value) ) 

#define IRelays_put_State(This,Value)	\
    ( (This)->lpVtbl -> put_State(This,Value) ) 

#define IRelays_get_NormalState(This,Value)	\
    ( (This)->lpVtbl -> get_NormalState(This,Value) ) 

#define IRelays_put_NormalState(This,Value)	\
    ( (This)->lpVtbl -> put_NormalState(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IRelays_INTERFACE_DEFINED__ */


#ifndef __ICmathLib_INTERFACE_DEFINED__
#define __ICmathLib_INTERFACE_DEFINED__

/* interface ICmathLib */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_ICmathLib;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BA3A-18C2-11F0-A417-C87F5452571C")
    ICmathLib : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_cmplx( 
            /* [in] */ double RealPart,
            /* [in] */ double ImagPart,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_cabs( 
            /* [in] */ double RealPart,
            /* [in] */ double ImagPart,
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_cdang( 
            /* [in] */ double RealPart,
            /* [in] */ double ImagPart,
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ctopolardeg( 
            /* [in] */ double RealPart,
            /* [in] */ double ImagPart,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_pdegtocomplex( 
            /* [in] */ double magnitude,
            /* [in] */ double angle,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_cmul( 
            /* [in] */ double a1,
            /* [in] */ double b1,
            /* [in] */ double a2,
            /* [in] */ double b2,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_cdiv( 
            /* [in] */ double a1,
            /* [in] */ double b1,
            /* [in] */ double a2,
            /* [in] */ double b2,
            /* [retval][out] */ VARIANT *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ICmathLibVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ICmathLib * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ICmathLib * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ICmathLib * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ICmathLib * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ICmathLib * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ICmathLib * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ICmathLib * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ICmathLib, get_cmplx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_cmplx )( 
            ICmathLib * This,
            /* [in] */ double RealPart,
            /* [in] */ double ImagPart,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICmathLib, get_cabs)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_cabs )( 
            ICmathLib * This,
            /* [in] */ double RealPart,
            /* [in] */ double ImagPart,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICmathLib, get_cdang)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_cdang )( 
            ICmathLib * This,
            /* [in] */ double RealPart,
            /* [in] */ double ImagPart,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ICmathLib, get_ctopolardeg)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ctopolardeg )( 
            ICmathLib * This,
            /* [in] */ double RealPart,
            /* [in] */ double ImagPart,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICmathLib, get_pdegtocomplex)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_pdegtocomplex )( 
            ICmathLib * This,
            /* [in] */ double magnitude,
            /* [in] */ double angle,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICmathLib, get_cmul)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_cmul )( 
            ICmathLib * This,
            /* [in] */ double a1,
            /* [in] */ double b1,
            /* [in] */ double a2,
            /* [in] */ double b2,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ICmathLib, get_cdiv)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_cdiv )( 
            ICmathLib * This,
            /* [in] */ double a1,
            /* [in] */ double b1,
            /* [in] */ double a2,
            /* [in] */ double b2,
            /* [retval][out] */ VARIANT *Value);
        
        END_INTERFACE
    } ICmathLibVtbl;

    interface ICmathLib
    {
        CONST_VTBL struct ICmathLibVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ICmathLib_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ICmathLib_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ICmathLib_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ICmathLib_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ICmathLib_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ICmathLib_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ICmathLib_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ICmathLib_get_cmplx(This,RealPart,ImagPart,Value)	\
    ( (This)->lpVtbl -> get_cmplx(This,RealPart,ImagPart,Value) ) 

#define ICmathLib_get_cabs(This,RealPart,ImagPart,Value)	\
    ( (This)->lpVtbl -> get_cabs(This,RealPart,ImagPart,Value) ) 

#define ICmathLib_get_cdang(This,RealPart,ImagPart,Value)	\
    ( (This)->lpVtbl -> get_cdang(This,RealPart,ImagPart,Value) ) 

#define ICmathLib_get_ctopolardeg(This,RealPart,ImagPart,Value)	\
    ( (This)->lpVtbl -> get_ctopolardeg(This,RealPart,ImagPart,Value) ) 

#define ICmathLib_get_pdegtocomplex(This,magnitude,angle,Value)	\
    ( (This)->lpVtbl -> get_pdegtocomplex(This,magnitude,angle,Value) ) 

#define ICmathLib_get_cmul(This,a1,b1,a2,b2,Value)	\
    ( (This)->lpVtbl -> get_cmul(This,a1,b1,a2,b2,Value) ) 

#define ICmathLib_get_cdiv(This,a1,b1,a2,b2,Value)	\
    ( (This)->lpVtbl -> get_cdiv(This,a1,b1,a2,b2,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ICmathLib_INTERFACE_DEFINED__ */


#ifndef __IParser_INTERFACE_DEFINED__
#define __IParser_INTERFACE_DEFINED__

/* interface IParser */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IParser;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BA6C-18C2-11F0-A417-C87F5452571C")
    IParser : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_CmdString( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_CmdString( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NextParam( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AutoIncrement( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AutoIncrement( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DblValue( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IntValue( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_StrValue( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_WhiteSpace( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_WhiteSpace( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_BeginQuote( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_BeginQuote( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EndQuote( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EndQuote( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Delimiters( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Delimiters( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ResetDelimiters( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vector( 
            /* [in] */ long ExpectedSize,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Matrix( 
            /* [in] */ long ExpectedOrder,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SymMatrix( 
            /* [in] */ long ExpectedOrder,
            /* [retval][out] */ VARIANT *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IParserVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IParser * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IParser * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IParser * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IParser * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IParser * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IParser * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IParser * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IParser, get_CmdString)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_CmdString )( 
            IParser * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IParser, put_CmdString)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_CmdString )( 
            IParser * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IParser, get_NextParam)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NextParam )( 
            IParser * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IParser, get_AutoIncrement)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AutoIncrement )( 
            IParser * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IParser, put_AutoIncrement)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AutoIncrement )( 
            IParser * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IParser, get_DblValue)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DblValue )( 
            IParser * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IParser, get_IntValue)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IntValue )( 
            IParser * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IParser, get_StrValue)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_StrValue )( 
            IParser * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IParser, get_WhiteSpace)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_WhiteSpace )( 
            IParser * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IParser, put_WhiteSpace)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_WhiteSpace )( 
            IParser * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IParser, get_BeginQuote)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_BeginQuote )( 
            IParser * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IParser, put_BeginQuote)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_BeginQuote )( 
            IParser * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IParser, get_EndQuote)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EndQuote )( 
            IParser * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IParser, put_EndQuote)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EndQuote )( 
            IParser * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IParser, get_Delimiters)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Delimiters )( 
            IParser * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IParser, put_Delimiters)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Delimiters )( 
            IParser * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IParser, ResetDelimiters)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ResetDelimiters )( 
            IParser * This);
        
        DECLSPEC_XFGVIRT(IParser, get_Vector)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vector )( 
            IParser * This,
            /* [in] */ long ExpectedSize,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IParser, get_Matrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Matrix )( 
            IParser * This,
            /* [in] */ long ExpectedOrder,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IParser, get_SymMatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SymMatrix )( 
            IParser * This,
            /* [in] */ long ExpectedOrder,
            /* [retval][out] */ VARIANT *Value);
        
        END_INTERFACE
    } IParserVtbl;

    interface IParser
    {
        CONST_VTBL struct IParserVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IParser_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IParser_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IParser_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IParser_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IParser_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IParser_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IParser_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IParser_get_CmdString(This,Value)	\
    ( (This)->lpVtbl -> get_CmdString(This,Value) ) 

#define IParser_put_CmdString(This,Value)	\
    ( (This)->lpVtbl -> put_CmdString(This,Value) ) 

#define IParser_get_NextParam(This,Value)	\
    ( (This)->lpVtbl -> get_NextParam(This,Value) ) 

#define IParser_get_AutoIncrement(This,Value)	\
    ( (This)->lpVtbl -> get_AutoIncrement(This,Value) ) 

#define IParser_put_AutoIncrement(This,Value)	\
    ( (This)->lpVtbl -> put_AutoIncrement(This,Value) ) 

#define IParser_get_DblValue(This,Value)	\
    ( (This)->lpVtbl -> get_DblValue(This,Value) ) 

#define IParser_get_IntValue(This,Value)	\
    ( (This)->lpVtbl -> get_IntValue(This,Value) ) 

#define IParser_get_StrValue(This,Value)	\
    ( (This)->lpVtbl -> get_StrValue(This,Value) ) 

#define IParser_get_WhiteSpace(This,Value)	\
    ( (This)->lpVtbl -> get_WhiteSpace(This,Value) ) 

#define IParser_put_WhiteSpace(This,Value)	\
    ( (This)->lpVtbl -> put_WhiteSpace(This,Value) ) 

#define IParser_get_BeginQuote(This,Value)	\
    ( (This)->lpVtbl -> get_BeginQuote(This,Value) ) 

#define IParser_put_BeginQuote(This,Value)	\
    ( (This)->lpVtbl -> put_BeginQuote(This,Value) ) 

#define IParser_get_EndQuote(This,Value)	\
    ( (This)->lpVtbl -> get_EndQuote(This,Value) ) 

#define IParser_put_EndQuote(This,Value)	\
    ( (This)->lpVtbl -> put_EndQuote(This,Value) ) 

#define IParser_get_Delimiters(This,Value)	\
    ( (This)->lpVtbl -> get_Delimiters(This,Value) ) 

#define IParser_put_Delimiters(This,Value)	\
    ( (This)->lpVtbl -> put_Delimiters(This,Value) ) 

#define IParser_ResetDelimiters(This)	\
    ( (This)->lpVtbl -> ResetDelimiters(This) ) 

#define IParser_get_Vector(This,ExpectedSize,Value)	\
    ( (This)->lpVtbl -> get_Vector(This,ExpectedSize,Value) ) 

#define IParser_get_Matrix(This,ExpectedOrder,Value)	\
    ( (This)->lpVtbl -> get_Matrix(This,ExpectedOrder,Value) ) 

#define IParser_get_SymMatrix(This,ExpectedOrder,Value)	\
    ( (This)->lpVtbl -> get_SymMatrix(This,ExpectedOrder,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IParser_INTERFACE_DEFINED__ */


#ifndef __ILoadShapes_INTERFACE_DEFINED__
#define __ILoadShapes_INTERFACE_DEFINED__

/* interface ILoadShapes */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_ILoadShapes;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BAB2-18C2-11F0-A417-C87F5452571C")
    ILoadShapes : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Npts( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Npts( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Pmult( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Pmult( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Qmult( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Qmult( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Normalize( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TimeArray( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_TimeArray( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_HrInterval( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_HrInterval( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MinInterval( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MinInterval( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE New( 
            /* [in] */ BSTR Name,
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Pbase( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Pbase( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Qbase( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Qbase( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_UseActual( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_UseActual( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Sinterval( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Sinterval( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE UseFloat32( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE UseFloat64( void) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ILoadShapesVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ILoadShapes * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ILoadShapes * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ILoadShapes * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ILoadShapes * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ILoadShapes * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ILoadShapes * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ILoadShapes * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ILoadShapes * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            ILoadShapes * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            ILoadShapes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ILoadShapes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ILoadShapes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            ILoadShapes * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_Npts)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Npts )( 
            ILoadShapes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_Npts)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Npts )( 
            ILoadShapes * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_Pmult)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Pmult )( 
            ILoadShapes * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_Pmult)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Pmult )( 
            ILoadShapes * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_Qmult)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Qmult )( 
            ILoadShapes * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_Qmult)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Qmult )( 
            ILoadShapes * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, Normalize)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Normalize )( 
            ILoadShapes * This);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_TimeArray)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TimeArray )( 
            ILoadShapes * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_TimeArray)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_TimeArray )( 
            ILoadShapes * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_HrInterval)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_HrInterval )( 
            ILoadShapes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_HrInterval)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_HrInterval )( 
            ILoadShapes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_MinInterval)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MinInterval )( 
            ILoadShapes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_MinInterval)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MinInterval )( 
            ILoadShapes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, New)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *New )( 
            ILoadShapes * This,
            /* [in] */ BSTR Name,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_Pbase)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Pbase )( 
            ILoadShapes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_Pbase)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Pbase )( 
            ILoadShapes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_Qbase)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Qbase )( 
            ILoadShapes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_Qbase)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Qbase )( 
            ILoadShapes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_UseActual)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_UseActual )( 
            ILoadShapes * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_UseActual)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_UseActual )( 
            ILoadShapes * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_Sinterval)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Sinterval )( 
            ILoadShapes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_Sinterval)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Sinterval )( 
            ILoadShapes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            ILoadShapes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            ILoadShapes * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ILoadShapes, UseFloat32)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *UseFloat32 )( 
            ILoadShapes * This);
        
        DECLSPEC_XFGVIRT(ILoadShapes, UseFloat64)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *UseFloat64 )( 
            ILoadShapes * This);
        
        END_INTERFACE
    } ILoadShapesVtbl;

    interface ILoadShapes
    {
        CONST_VTBL struct ILoadShapesVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ILoadShapes_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ILoadShapes_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ILoadShapes_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ILoadShapes_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ILoadShapes_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ILoadShapes_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ILoadShapes_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ILoadShapes_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ILoadShapes_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define ILoadShapes_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define ILoadShapes_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ILoadShapes_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ILoadShapes_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define ILoadShapes_get_Npts(This,Value)	\
    ( (This)->lpVtbl -> get_Npts(This,Value) ) 

#define ILoadShapes_put_Npts(This,Value)	\
    ( (This)->lpVtbl -> put_Npts(This,Value) ) 

#define ILoadShapes_get_Pmult(This,Value)	\
    ( (This)->lpVtbl -> get_Pmult(This,Value) ) 

#define ILoadShapes_put_Pmult(This,Value)	\
    ( (This)->lpVtbl -> put_Pmult(This,Value) ) 

#define ILoadShapes_get_Qmult(This,Value)	\
    ( (This)->lpVtbl -> get_Qmult(This,Value) ) 

#define ILoadShapes_put_Qmult(This,Value)	\
    ( (This)->lpVtbl -> put_Qmult(This,Value) ) 

#define ILoadShapes_Normalize(This)	\
    ( (This)->lpVtbl -> Normalize(This) ) 

#define ILoadShapes_get_TimeArray(This,Value)	\
    ( (This)->lpVtbl -> get_TimeArray(This,Value) ) 

#define ILoadShapes_put_TimeArray(This,Value)	\
    ( (This)->lpVtbl -> put_TimeArray(This,Value) ) 

#define ILoadShapes_get_HrInterval(This,Value)	\
    ( (This)->lpVtbl -> get_HrInterval(This,Value) ) 

#define ILoadShapes_put_HrInterval(This,Value)	\
    ( (This)->lpVtbl -> put_HrInterval(This,Value) ) 

#define ILoadShapes_get_MinInterval(This,Value)	\
    ( (This)->lpVtbl -> get_MinInterval(This,Value) ) 

#define ILoadShapes_put_MinInterval(This,Value)	\
    ( (This)->lpVtbl -> put_MinInterval(This,Value) ) 

#define ILoadShapes_New(This,Name,Value)	\
    ( (This)->lpVtbl -> New(This,Name,Value) ) 

#define ILoadShapes_get_Pbase(This,Value)	\
    ( (This)->lpVtbl -> get_Pbase(This,Value) ) 

#define ILoadShapes_put_Pbase(This,Value)	\
    ( (This)->lpVtbl -> put_Pbase(This,Value) ) 

#define ILoadShapes_get_Qbase(This,Value)	\
    ( (This)->lpVtbl -> get_Qbase(This,Value) ) 

#define ILoadShapes_put_Qbase(This,Value)	\
    ( (This)->lpVtbl -> put_Qbase(This,Value) ) 

#define ILoadShapes_get_UseActual(This,Value)	\
    ( (This)->lpVtbl -> get_UseActual(This,Value) ) 

#define ILoadShapes_put_UseActual(This,Value)	\
    ( (This)->lpVtbl -> put_UseActual(This,Value) ) 

#define ILoadShapes_get_Sinterval(This,Value)	\
    ( (This)->lpVtbl -> get_Sinterval(This,Value) ) 

#define ILoadShapes_put_Sinterval(This,Value)	\
    ( (This)->lpVtbl -> put_Sinterval(This,Value) ) 

#define ILoadShapes_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define ILoadShapes_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define ILoadShapes_UseFloat32(This)	\
    ( (This)->lpVtbl -> UseFloat32(This) ) 

#define ILoadShapes_UseFloat64(This)	\
    ( (This)->lpVtbl -> UseFloat64(This) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ILoadShapes_INTERFACE_DEFINED__ */


#ifndef __IFuses_INTERFACE_DEFINED__
#define __IFuses_INTERFACE_DEFINED__

/* interface IFuses */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IFuses;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BAF8-18C2-11F0-A417-C87F5452571C")
    IFuses : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MonitoredObj( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MonitoredObj( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_MonitoredTerm( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_MonitoredTerm( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SwitchedObj( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SwitchedObj( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SwitchedTerm( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SwitchedTerm( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TCCcurve( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_TCCcurve( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RatedCurrent( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_RatedCurrent( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Delay( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Delay( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Open( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Close( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE IsBlown( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumPhases( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Reset( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_State( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_State( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NormalState( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NormalState( 
            /* [in] */ VARIANT Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IFusesVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IFuses * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IFuses * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IFuses * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IFuses * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IFuses * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IFuses * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IFuses * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IFuses, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IFuses * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IFuses * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IFuses * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IFuses * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IFuses * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IFuses * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_MonitoredObj)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MonitoredObj )( 
            IFuses * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_MonitoredObj)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MonitoredObj )( 
            IFuses * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_MonitoredTerm)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_MonitoredTerm )( 
            IFuses * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_MonitoredTerm)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_MonitoredTerm )( 
            IFuses * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_SwitchedObj)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwitchedObj )( 
            IFuses * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_SwitchedObj)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SwitchedObj )( 
            IFuses * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_SwitchedTerm)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SwitchedTerm )( 
            IFuses * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_SwitchedTerm)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SwitchedTerm )( 
            IFuses * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_TCCcurve)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TCCcurve )( 
            IFuses * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_TCCcurve)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_TCCcurve )( 
            IFuses * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_RatedCurrent)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RatedCurrent )( 
            IFuses * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_RatedCurrent)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_RatedCurrent )( 
            IFuses * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_Delay)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Delay )( 
            IFuses * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_Delay)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Delay )( 
            IFuses * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IFuses, Open)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Open )( 
            IFuses * This);
        
        DECLSPEC_XFGVIRT(IFuses, Close)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Close )( 
            IFuses * This);
        
        DECLSPEC_XFGVIRT(IFuses, IsBlown)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *IsBlown )( 
            IFuses * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IFuses * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IFuses * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_NumPhases)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumPhases )( 
            IFuses * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IFuses, Reset)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Reset )( 
            IFuses * This);
        
        DECLSPEC_XFGVIRT(IFuses, get_State)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_State )( 
            IFuses * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_State)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_State )( 
            IFuses * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IFuses, get_NormalState)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NormalState )( 
            IFuses * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IFuses, put_NormalState)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NormalState )( 
            IFuses * This,
            /* [in] */ VARIANT Value);
        
        END_INTERFACE
    } IFusesVtbl;

    interface IFuses
    {
        CONST_VTBL struct IFusesVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IFuses_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IFuses_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IFuses_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IFuses_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IFuses_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IFuses_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IFuses_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IFuses_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IFuses_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IFuses_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IFuses_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IFuses_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IFuses_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IFuses_get_MonitoredObj(This,Value)	\
    ( (This)->lpVtbl -> get_MonitoredObj(This,Value) ) 

#define IFuses_put_MonitoredObj(This,Value)	\
    ( (This)->lpVtbl -> put_MonitoredObj(This,Value) ) 

#define IFuses_get_MonitoredTerm(This,Value)	\
    ( (This)->lpVtbl -> get_MonitoredTerm(This,Value) ) 

#define IFuses_put_MonitoredTerm(This,Value)	\
    ( (This)->lpVtbl -> put_MonitoredTerm(This,Value) ) 

#define IFuses_get_SwitchedObj(This,Value)	\
    ( (This)->lpVtbl -> get_SwitchedObj(This,Value) ) 

#define IFuses_put_SwitchedObj(This,Value)	\
    ( (This)->lpVtbl -> put_SwitchedObj(This,Value) ) 

#define IFuses_get_SwitchedTerm(This,Value)	\
    ( (This)->lpVtbl -> get_SwitchedTerm(This,Value) ) 

#define IFuses_put_SwitchedTerm(This,Value)	\
    ( (This)->lpVtbl -> put_SwitchedTerm(This,Value) ) 

#define IFuses_get_TCCcurve(This,Value)	\
    ( (This)->lpVtbl -> get_TCCcurve(This,Value) ) 

#define IFuses_put_TCCcurve(This,Value)	\
    ( (This)->lpVtbl -> put_TCCcurve(This,Value) ) 

#define IFuses_get_RatedCurrent(This,Value)	\
    ( (This)->lpVtbl -> get_RatedCurrent(This,Value) ) 

#define IFuses_put_RatedCurrent(This,Value)	\
    ( (This)->lpVtbl -> put_RatedCurrent(This,Value) ) 

#define IFuses_get_Delay(This,Value)	\
    ( (This)->lpVtbl -> get_Delay(This,Value) ) 

#define IFuses_put_Delay(This,Value)	\
    ( (This)->lpVtbl -> put_Delay(This,Value) ) 

#define IFuses_Open(This)	\
    ( (This)->lpVtbl -> Open(This) ) 

#define IFuses_Close(This)	\
    ( (This)->lpVtbl -> Close(This) ) 

#define IFuses_IsBlown(This,Value)	\
    ( (This)->lpVtbl -> IsBlown(This,Value) ) 

#define IFuses_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IFuses_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define IFuses_get_NumPhases(This,Value)	\
    ( (This)->lpVtbl -> get_NumPhases(This,Value) ) 

#define IFuses_Reset(This)	\
    ( (This)->lpVtbl -> Reset(This) ) 

#define IFuses_get_State(This,Value)	\
    ( (This)->lpVtbl -> get_State(This,Value) ) 

#define IFuses_put_State(This,Value)	\
    ( (This)->lpVtbl -> put_State(This,Value) ) 

#define IFuses_get_NormalState(This,Value)	\
    ( (This)->lpVtbl -> get_NormalState(This,Value) ) 

#define IFuses_put_NormalState(This,Value)	\
    ( (This)->lpVtbl -> put_NormalState(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IFuses_INTERFACE_DEFINED__ */


#ifndef __IISources_INTERFACE_DEFINED__
#define __IISources_INTERFACE_DEFINED__

/* interface IISources */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IISources;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BB48-18C2-11F0-A417-C87F5452571C")
    IISources : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Amps( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Amps( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AngleDeg( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AngleDeg( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Frequency( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Frequency( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IISourcesVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IISources * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IISources * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IISources * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IISources * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IISources * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IISources * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IISources * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IISources, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IISources * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IISources, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IISources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IISources, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IISources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IISources, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IISources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IISources, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IISources * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IISources, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IISources * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IISources, get_Amps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Amps )( 
            IISources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IISources, put_Amps)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Amps )( 
            IISources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IISources, get_AngleDeg)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AngleDeg )( 
            IISources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IISources, put_AngleDeg)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AngleDeg )( 
            IISources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IISources, get_Frequency)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Frequency )( 
            IISources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IISources, put_Frequency)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Frequency )( 
            IISources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IISources, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IISources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IISources, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IISources * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } IISourcesVtbl;

    interface IISources
    {
        CONST_VTBL struct IISourcesVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IISources_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IISources_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IISources_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IISources_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IISources_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IISources_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IISources_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IISources_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IISources_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IISources_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IISources_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IISources_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IISources_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IISources_get_Amps(This,Value)	\
    ( (This)->lpVtbl -> get_Amps(This,Value) ) 

#define IISources_put_Amps(This,Value)	\
    ( (This)->lpVtbl -> put_Amps(This,Value) ) 

#define IISources_get_AngleDeg(This,Value)	\
    ( (This)->lpVtbl -> get_AngleDeg(This,Value) ) 

#define IISources_put_AngleDeg(This,Value)	\
    ( (This)->lpVtbl -> put_AngleDeg(This,Value) ) 

#define IISources_get_Frequency(This,Value)	\
    ( (This)->lpVtbl -> get_Frequency(This,Value) ) 

#define IISources_put_Frequency(This,Value)	\
    ( (This)->lpVtbl -> put_Frequency(This,Value) ) 

#define IISources_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IISources_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IISources_INTERFACE_DEFINED__ */


#ifndef __IDSSimComs_INTERFACE_DEFINED__
#define __IDSSimComs_INTERFACE_DEFINED__

/* interface IDSSimComs */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IDSSimComs;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BB84-18C2-11F0-A417-C87F5452571C")
    IDSSimComs : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE BusVoltagepu( 
            /* [in] */ unsigned int Index,
            /* [retval][out] */ VARIANT *Vpu) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE BusVoltage( 
            /* [in] */ unsigned int Index,
            /* [retval][out] */ VARIANT *Voltages) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IDSSimComsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IDSSimComs * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IDSSimComs * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IDSSimComs * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IDSSimComs * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IDSSimComs * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IDSSimComs * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IDSSimComs * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IDSSimComs, BusVoltagepu)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *BusVoltagepu )( 
            IDSSimComs * This,
            /* [in] */ unsigned int Index,
            /* [retval][out] */ VARIANT *Vpu);
        
        DECLSPEC_XFGVIRT(IDSSimComs, BusVoltage)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *BusVoltage )( 
            IDSSimComs * This,
            /* [in] */ unsigned int Index,
            /* [retval][out] */ VARIANT *Voltages);
        
        END_INTERFACE
    } IDSSimComsVtbl;

    interface IDSSimComs
    {
        CONST_VTBL struct IDSSimComsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IDSSimComs_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IDSSimComs_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IDSSimComs_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IDSSimComs_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IDSSimComs_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IDSSimComs_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IDSSimComs_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IDSSimComs_BusVoltagepu(This,Index,Vpu)	\
    ( (This)->lpVtbl -> BusVoltagepu(This,Index,Vpu) ) 

#define IDSSimComs_BusVoltage(This,Index,Voltages)	\
    ( (This)->lpVtbl -> BusVoltage(This,Index,Voltages) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IDSSimComs_INTERFACE_DEFINED__ */


#ifndef __IPVSystems_INTERFACE_DEFINED__
#define __IPVSystems_INTERFACE_DEFINED__

/* interface IPVSystems */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IPVSystems;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BBAC-18C2-11F0-A417-C87F5452571C")
    IPVSystems : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterValues( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Irradiance( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Irradiance( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kW( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kvar( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kvar( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PF( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PF( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kVArated( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kVArated( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Pmpp( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Pmpp( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IrradianceNow( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Sensor( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_daily( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_daily( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_duty( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_duty( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Yearly( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Yearly( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Tdaily( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Tdaily( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Tduty( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Tduty( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Tyearly( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Tyearly( 
            /* [in] */ BSTR Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IPVSystemsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IPVSystems * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IPVSystems * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IPVSystems * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IPVSystems * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IPVSystems * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IPVSystems * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IPVSystems * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IPVSystems * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_RegisterNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterNames )( 
            IPVSystems * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_RegisterValues)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterValues )( 
            IPVSystems * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IPVSystems * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IPVSystems * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IPVSystems * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IPVSystems * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IPVSystems * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IPVSystems * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IPVSystems * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Irradiance)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Irradiance )( 
            IPVSystems * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_Irradiance)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Irradiance )( 
            IPVSystems * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_kW)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kW )( 
            IPVSystems * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_kvar)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kvar )( 
            IPVSystems * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_kvar)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kvar )( 
            IPVSystems * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_PF)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PF )( 
            IPVSystems * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_PF)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PF )( 
            IPVSystems * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_kVArated)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kVArated )( 
            IPVSystems * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_kVArated)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kVArated )( 
            IPVSystems * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Pmpp)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Pmpp )( 
            IPVSystems * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_Pmpp)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Pmpp )( 
            IPVSystems * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_IrradianceNow)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IrradianceNow )( 
            IPVSystems * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Sensor)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Sensor )( 
            IPVSystems * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_daily)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_daily )( 
            IPVSystems * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_daily)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_daily )( 
            IPVSystems * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_duty)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_duty )( 
            IPVSystems * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_duty)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_duty )( 
            IPVSystems * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Yearly)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Yearly )( 
            IPVSystems * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_Yearly)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Yearly )( 
            IPVSystems * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Tdaily)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Tdaily )( 
            IPVSystems * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_Tdaily)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Tdaily )( 
            IPVSystems * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Tduty)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Tduty )( 
            IPVSystems * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_Tduty)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Tduty )( 
            IPVSystems * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, get_Tyearly)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Tyearly )( 
            IPVSystems * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IPVSystems, put_Tyearly)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Tyearly )( 
            IPVSystems * This,
            /* [in] */ BSTR Value);
        
        END_INTERFACE
    } IPVSystemsVtbl;

    interface IPVSystems
    {
        CONST_VTBL struct IPVSystemsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IPVSystems_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IPVSystems_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IPVSystems_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IPVSystems_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IPVSystems_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IPVSystems_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IPVSystems_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IPVSystems_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IPVSystems_get_RegisterNames(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterNames(This,Value) ) 

#define IPVSystems_get_RegisterValues(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterValues(This,Value) ) 

#define IPVSystems_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IPVSystems_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IPVSystems_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IPVSystems_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IPVSystems_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define IPVSystems_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IPVSystems_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IPVSystems_get_Irradiance(This,Value)	\
    ( (This)->lpVtbl -> get_Irradiance(This,Value) ) 

#define IPVSystems_put_Irradiance(This,Value)	\
    ( (This)->lpVtbl -> put_Irradiance(This,Value) ) 

#define IPVSystems_get_kW(This,Value)	\
    ( (This)->lpVtbl -> get_kW(This,Value) ) 

#define IPVSystems_get_kvar(This,Value)	\
    ( (This)->lpVtbl -> get_kvar(This,Value) ) 

#define IPVSystems_put_kvar(This,Value)	\
    ( (This)->lpVtbl -> put_kvar(This,Value) ) 

#define IPVSystems_get_PF(This,Value)	\
    ( (This)->lpVtbl -> get_PF(This,Value) ) 

#define IPVSystems_put_PF(This,Value)	\
    ( (This)->lpVtbl -> put_PF(This,Value) ) 

#define IPVSystems_get_kVArated(This,Value)	\
    ( (This)->lpVtbl -> get_kVArated(This,Value) ) 

#define IPVSystems_put_kVArated(This,Value)	\
    ( (This)->lpVtbl -> put_kVArated(This,Value) ) 

#define IPVSystems_get_Pmpp(This,Value)	\
    ( (This)->lpVtbl -> get_Pmpp(This,Value) ) 

#define IPVSystems_put_Pmpp(This,Value)	\
    ( (This)->lpVtbl -> put_Pmpp(This,Value) ) 

#define IPVSystems_get_IrradianceNow(This,Value)	\
    ( (This)->lpVtbl -> get_IrradianceNow(This,Value) ) 

#define IPVSystems_get_Sensor(This,Value)	\
    ( (This)->lpVtbl -> get_Sensor(This,Value) ) 

#define IPVSystems_get_daily(This,Value)	\
    ( (This)->lpVtbl -> get_daily(This,Value) ) 

#define IPVSystems_put_daily(This,Value)	\
    ( (This)->lpVtbl -> put_daily(This,Value) ) 

#define IPVSystems_get_duty(This,Value)	\
    ( (This)->lpVtbl -> get_duty(This,Value) ) 

#define IPVSystems_put_duty(This,Value)	\
    ( (This)->lpVtbl -> put_duty(This,Value) ) 

#define IPVSystems_get_Yearly(This,Value)	\
    ( (This)->lpVtbl -> get_Yearly(This,Value) ) 

#define IPVSystems_put_Yearly(This,Value)	\
    ( (This)->lpVtbl -> put_Yearly(This,Value) ) 

#define IPVSystems_get_Tdaily(This,Value)	\
    ( (This)->lpVtbl -> get_Tdaily(This,Value) ) 

#define IPVSystems_put_Tdaily(This,Value)	\
    ( (This)->lpVtbl -> put_Tdaily(This,Value) ) 

#define IPVSystems_get_Tduty(This,Value)	\
    ( (This)->lpVtbl -> get_Tduty(This,Value) ) 

#define IPVSystems_put_Tduty(This,Value)	\
    ( (This)->lpVtbl -> put_Tduty(This,Value) ) 

#define IPVSystems_get_Tyearly(This,Value)	\
    ( (This)->lpVtbl -> get_Tyearly(This,Value) ) 

#define IPVSystems_put_Tyearly(This,Value)	\
    ( (This)->lpVtbl -> put_Tyearly(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IPVSystems_INTERFACE_DEFINED__ */


#ifndef __IVsources_INTERFACE_DEFINED__
#define __IVsources_INTERFACE_DEFINED__

/* interface IVsources */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IVsources;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BC10-18C2-11F0-A417-C87F5452571C")
    IVsources : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_BasekV( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_BasekV( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_pu( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_pu( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AngleDeg( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AngleDeg( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Frequency( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Frequency( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Phases( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Phases( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IVsourcesVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IVsources * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IVsources * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IVsources * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IVsources * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IVsources * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IVsources * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IVsources * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IVsources, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IVsources * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IVsources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IVsources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IVsources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IVsources * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IVsources, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IVsources * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_BasekV)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_BasekV )( 
            IVsources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IVsources, put_BasekV)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_BasekV )( 
            IVsources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_pu)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_pu )( 
            IVsources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IVsources, put_pu)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_pu )( 
            IVsources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_AngleDeg)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AngleDeg )( 
            IVsources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IVsources, put_AngleDeg)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AngleDeg )( 
            IVsources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_Frequency)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Frequency )( 
            IVsources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IVsources, put_Frequency)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Frequency )( 
            IVsources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_Phases)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Phases )( 
            IVsources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IVsources, put_Phases)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Phases )( 
            IVsources * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IVsources, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IVsources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IVsources, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IVsources * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } IVsourcesVtbl;

    interface IVsources
    {
        CONST_VTBL struct IVsourcesVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IVsources_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IVsources_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IVsources_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IVsources_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IVsources_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IVsources_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IVsources_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IVsources_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IVsources_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IVsources_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IVsources_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IVsources_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IVsources_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IVsources_get_BasekV(This,Value)	\
    ( (This)->lpVtbl -> get_BasekV(This,Value) ) 

#define IVsources_put_BasekV(This,Value)	\
    ( (This)->lpVtbl -> put_BasekV(This,Value) ) 

#define IVsources_get_pu(This,Value)	\
    ( (This)->lpVtbl -> get_pu(This,Value) ) 

#define IVsources_put_pu(This,Value)	\
    ( (This)->lpVtbl -> put_pu(This,Value) ) 

#define IVsources_get_AngleDeg(This,Value)	\
    ( (This)->lpVtbl -> get_AngleDeg(This,Value) ) 

#define IVsources_put_AngleDeg(This,Value)	\
    ( (This)->lpVtbl -> put_AngleDeg(This,Value) ) 

#define IVsources_get_Frequency(This,Value)	\
    ( (This)->lpVtbl -> get_Frequency(This,Value) ) 

#define IVsources_put_Frequency(This,Value)	\
    ( (This)->lpVtbl -> put_Frequency(This,Value) ) 

#define IVsources_get_Phases(This,Value)	\
    ( (This)->lpVtbl -> get_Phases(This,Value) ) 

#define IVsources_put_Phases(This,Value)	\
    ( (This)->lpVtbl -> put_Phases(This,Value) ) 

#define IVsources_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IVsources_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IVsources_INTERFACE_DEFINED__ */


#ifndef __IParallel_INTERFACE_DEFINED__
#define __IParallel_INTERFACE_DEFINED__

/* interface IParallel */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IParallel;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BC42-18C2-11F0-A417-C87F5452571C")
    IParallel : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumCPUs( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumCores( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveActor( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ActiveActor( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE CreateActor( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActorCPU( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ActorCPU( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NumOfActors( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Wait( void) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActorProgress( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActorStatus( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ActiveParallel( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ActiveParallel( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ConcatenateReports( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ConcatenateReports( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IParallelVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IParallel * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IParallel * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IParallel * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IParallel * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IParallel * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IParallel * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IParallel * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IParallel, get_NumCPUs)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumCPUs )( 
            IParallel * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IParallel, get_NumCores)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumCores )( 
            IParallel * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IParallel, get_ActiveActor)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveActor )( 
            IParallel * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IParallel, put_ActiveActor)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ActiveActor )( 
            IParallel * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IParallel, CreateActor)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *CreateActor )( 
            IParallel * This);
        
        DECLSPEC_XFGVIRT(IParallel, get_ActorCPU)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActorCPU )( 
            IParallel * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IParallel, put_ActorCPU)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ActorCPU )( 
            IParallel * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IParallel, get_NumOfActors)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NumOfActors )( 
            IParallel * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IParallel, Wait)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Wait )( 
            IParallel * This);
        
        DECLSPEC_XFGVIRT(IParallel, get_ActorProgress)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActorProgress )( 
            IParallel * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IParallel, get_ActorStatus)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActorStatus )( 
            IParallel * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IParallel, get_ActiveParallel)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ActiveParallel )( 
            IParallel * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IParallel, put_ActiveParallel)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ActiveParallel )( 
            IParallel * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IParallel, get_ConcatenateReports)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ConcatenateReports )( 
            IParallel * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IParallel, put_ConcatenateReports)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ConcatenateReports )( 
            IParallel * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } IParallelVtbl;

    interface IParallel
    {
        CONST_VTBL struct IParallelVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IParallel_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IParallel_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IParallel_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IParallel_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IParallel_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IParallel_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IParallel_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IParallel_get_NumCPUs(This,Value)	\
    ( (This)->lpVtbl -> get_NumCPUs(This,Value) ) 

#define IParallel_get_NumCores(This,Value)	\
    ( (This)->lpVtbl -> get_NumCores(This,Value) ) 

#define IParallel_get_ActiveActor(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveActor(This,Value) ) 

#define IParallel_put_ActiveActor(This,Value)	\
    ( (This)->lpVtbl -> put_ActiveActor(This,Value) ) 

#define IParallel_CreateActor(This)	\
    ( (This)->lpVtbl -> CreateActor(This) ) 

#define IParallel_get_ActorCPU(This,Value)	\
    ( (This)->lpVtbl -> get_ActorCPU(This,Value) ) 

#define IParallel_put_ActorCPU(This,Value)	\
    ( (This)->lpVtbl -> put_ActorCPU(This,Value) ) 

#define IParallel_get_NumOfActors(This,Value)	\
    ( (This)->lpVtbl -> get_NumOfActors(This,Value) ) 

#define IParallel_Wait(This)	\
    ( (This)->lpVtbl -> Wait(This) ) 

#define IParallel_get_ActorProgress(This,Value)	\
    ( (This)->lpVtbl -> get_ActorProgress(This,Value) ) 

#define IParallel_get_ActorStatus(This,Value)	\
    ( (This)->lpVtbl -> get_ActorStatus(This,Value) ) 

#define IParallel_get_ActiveParallel(This,Value)	\
    ( (This)->lpVtbl -> get_ActiveParallel(This,Value) ) 

#define IParallel_put_ActiveParallel(This,Value)	\
    ( (This)->lpVtbl -> put_ActiveParallel(This,Value) ) 

#define IParallel_get_ConcatenateReports(This,Value)	\
    ( (This)->lpVtbl -> get_ConcatenateReports(This,Value) ) 

#define IParallel_put_ConcatenateReports(This,Value)	\
    ( (This)->lpVtbl -> put_ConcatenateReports(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IParallel_INTERFACE_DEFINED__ */


#ifndef __ILineCodes_INTERFACE_DEFINED__
#define __ILineCodes_INTERFACE_DEFINED__

/* interface ILineCodes */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_ILineCodes;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BC74-18C2-11F0-A417-C87F5452571C")
    ILineCodes : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsZ1Z0( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Units( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Units( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Phases( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Phases( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_R1( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_R1( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_X1( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_X1( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_R0( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_R0( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_X0( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_X0( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_C1( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_C1( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_C0( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_C0( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Rmatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Rmatrix( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xmatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xmatrix( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Cmatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Cmatrix( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NormAmps( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NormAmps( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EmergAmps( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EmergAmps( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct ILineCodesVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            ILineCodes * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            ILineCodes * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            ILineCodes * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            ILineCodes * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            ILineCodes * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            ILineCodes * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            ILineCodes * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            ILineCodes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_First)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            ILineCodes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_Next)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            ILineCodes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            ILineCodes * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            ILineCodes * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_IsZ1Z0)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsZ1Z0 )( 
            ILineCodes * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_Units)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Units )( 
            ILineCodes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_Units)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Units )( 
            ILineCodes * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_Phases)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Phases )( 
            ILineCodes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_Phases)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Phases )( 
            ILineCodes * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_R1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_R1 )( 
            ILineCodes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_R1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_R1 )( 
            ILineCodes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_X1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_X1 )( 
            ILineCodes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_X1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_X1 )( 
            ILineCodes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_R0)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_R0 )( 
            ILineCodes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_R0)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_R0 )( 
            ILineCodes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_X0)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_X0 )( 
            ILineCodes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_X0)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_X0 )( 
            ILineCodes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_C1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_C1 )( 
            ILineCodes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_C1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_C1 )( 
            ILineCodes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_C0)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_C0 )( 
            ILineCodes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_C0)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_C0 )( 
            ILineCodes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_Rmatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Rmatrix )( 
            ILineCodes * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_Rmatrix)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Rmatrix )( 
            ILineCodes * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_Xmatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xmatrix )( 
            ILineCodes * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_Xmatrix)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xmatrix )( 
            ILineCodes * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_Cmatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Cmatrix )( 
            ILineCodes * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_Cmatrix)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Cmatrix )( 
            ILineCodes * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_NormAmps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NormAmps )( 
            ILineCodes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_NormAmps)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NormAmps )( 
            ILineCodes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_EmergAmps)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EmergAmps )( 
            ILineCodes * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_EmergAmps)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EmergAmps )( 
            ILineCodes * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            ILineCodes * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            ILineCodes * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(ILineCodes, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            ILineCodes * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } ILineCodesVtbl;

    interface ILineCodes
    {
        CONST_VTBL struct ILineCodesVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define ILineCodes_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define ILineCodes_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define ILineCodes_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define ILineCodes_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define ILineCodes_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define ILineCodes_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define ILineCodes_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define ILineCodes_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define ILineCodes_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define ILineCodes_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define ILineCodes_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define ILineCodes_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define ILineCodes_get_IsZ1Z0(This,Value)	\
    ( (This)->lpVtbl -> get_IsZ1Z0(This,Value) ) 

#define ILineCodes_get_Units(This,Value)	\
    ( (This)->lpVtbl -> get_Units(This,Value) ) 

#define ILineCodes_put_Units(This,Value)	\
    ( (This)->lpVtbl -> put_Units(This,Value) ) 

#define ILineCodes_get_Phases(This,Value)	\
    ( (This)->lpVtbl -> get_Phases(This,Value) ) 

#define ILineCodes_put_Phases(This,Value)	\
    ( (This)->lpVtbl -> put_Phases(This,Value) ) 

#define ILineCodes_get_R1(This,Value)	\
    ( (This)->lpVtbl -> get_R1(This,Value) ) 

#define ILineCodes_put_R1(This,Value)	\
    ( (This)->lpVtbl -> put_R1(This,Value) ) 

#define ILineCodes_get_X1(This,Value)	\
    ( (This)->lpVtbl -> get_X1(This,Value) ) 

#define ILineCodes_put_X1(This,Value)	\
    ( (This)->lpVtbl -> put_X1(This,Value) ) 

#define ILineCodes_get_R0(This,Value)	\
    ( (This)->lpVtbl -> get_R0(This,Value) ) 

#define ILineCodes_put_R0(This,Value)	\
    ( (This)->lpVtbl -> put_R0(This,Value) ) 

#define ILineCodes_get_X0(This,Value)	\
    ( (This)->lpVtbl -> get_X0(This,Value) ) 

#define ILineCodes_put_X0(This,Value)	\
    ( (This)->lpVtbl -> put_X0(This,Value) ) 

#define ILineCodes_get_C1(This,Value)	\
    ( (This)->lpVtbl -> get_C1(This,Value) ) 

#define ILineCodes_put_C1(This,Value)	\
    ( (This)->lpVtbl -> put_C1(This,Value) ) 

#define ILineCodes_get_C0(This,Value)	\
    ( (This)->lpVtbl -> get_C0(This,Value) ) 

#define ILineCodes_put_C0(This,Value)	\
    ( (This)->lpVtbl -> put_C0(This,Value) ) 

#define ILineCodes_get_Rmatrix(This,Value)	\
    ( (This)->lpVtbl -> get_Rmatrix(This,Value) ) 

#define ILineCodes_put_Rmatrix(This,Value)	\
    ( (This)->lpVtbl -> put_Rmatrix(This,Value) ) 

#define ILineCodes_get_Xmatrix(This,Value)	\
    ( (This)->lpVtbl -> get_Xmatrix(This,Value) ) 

#define ILineCodes_put_Xmatrix(This,Value)	\
    ( (This)->lpVtbl -> put_Xmatrix(This,Value) ) 

#define ILineCodes_get_Cmatrix(This,Value)	\
    ( (This)->lpVtbl -> get_Cmatrix(This,Value) ) 

#define ILineCodes_put_Cmatrix(This,Value)	\
    ( (This)->lpVtbl -> put_Cmatrix(This,Value) ) 

#define ILineCodes_get_NormAmps(This,Value)	\
    ( (This)->lpVtbl -> get_NormAmps(This,Value) ) 

#define ILineCodes_put_NormAmps(This,Value)	\
    ( (This)->lpVtbl -> put_NormAmps(This,Value) ) 

#define ILineCodes_get_EmergAmps(This,Value)	\
    ( (This)->lpVtbl -> get_EmergAmps(This,Value) ) 

#define ILineCodes_put_EmergAmps(This,Value)	\
    ( (This)->lpVtbl -> put_EmergAmps(This,Value) ) 

#define ILineCodes_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define ILineCodes_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define ILineCodes_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __ILineCodes_INTERFACE_DEFINED__ */


#ifndef __IGICSources_INTERFACE_DEFINED__
#define __IGICSources_INTERFACE_DEFINED__

/* interface IGICSources */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IGICSources;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BCBA-18C2-11F0-A417-C87F5452571C")
    IGICSources : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Bus1( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Bus2( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Phases( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Phases( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EN( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EN( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EE( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EE( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Lat1( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Lat1( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Lat2( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Lat2( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Lon1( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Lon1( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Lon2( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Lon2( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Volts( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Volts( 
            /* [in] */ double Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IGICSourcesVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IGICSources * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IGICSources * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IGICSources * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IGICSources * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IGICSources * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IGICSources * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IGICSources * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IGICSources, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IGICSources * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Bus1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Bus1 )( 
            IGICSources * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Bus2)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Bus2 )( 
            IGICSources * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Name)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IGICSources * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, put_Name)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IGICSources * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Phases)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Phases )( 
            IGICSources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, put_Phases)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Phases )( 
            IGICSources * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_EN)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EN )( 
            IGICSources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, put_EN)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EN )( 
            IGICSources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_EE)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EE )( 
            IGICSources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, put_EE)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EE )( 
            IGICSources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Lat1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Lat1 )( 
            IGICSources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, put_Lat1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Lat1 )( 
            IGICSources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Lat2)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Lat2 )( 
            IGICSources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, put_Lat2)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Lat2 )( 
            IGICSources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Lon1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Lon1 )( 
            IGICSources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, put_Lon1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Lon1 )( 
            IGICSources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Lon2)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Lon2 )( 
            IGICSources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, put_Lon2)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Lon2 )( 
            IGICSources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Volts)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Volts )( 
            IGICSources * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, put_Volts)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Volts )( 
            IGICSources * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Count)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IGICSources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IGICSources * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IGICSources, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IGICSources * This,
            /* [retval][out] */ long *Value);
        
        END_INTERFACE
    } IGICSourcesVtbl;

    interface IGICSources
    {
        CONST_VTBL struct IGICSourcesVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IGICSources_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IGICSources_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IGICSources_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IGICSources_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IGICSources_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IGICSources_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IGICSources_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IGICSources_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IGICSources_get_Bus1(This,Value)	\
    ( (This)->lpVtbl -> get_Bus1(This,Value) ) 

#define IGICSources_get_Bus2(This,Value)	\
    ( (This)->lpVtbl -> get_Bus2(This,Value) ) 

#define IGICSources_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IGICSources_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IGICSources_get_Phases(This,Value)	\
    ( (This)->lpVtbl -> get_Phases(This,Value) ) 

#define IGICSources_put_Phases(This,Value)	\
    ( (This)->lpVtbl -> put_Phases(This,Value) ) 

#define IGICSources_get_EN(This,Value)	\
    ( (This)->lpVtbl -> get_EN(This,Value) ) 

#define IGICSources_put_EN(This,Value)	\
    ( (This)->lpVtbl -> put_EN(This,Value) ) 

#define IGICSources_get_EE(This,Value)	\
    ( (This)->lpVtbl -> get_EE(This,Value) ) 

#define IGICSources_put_EE(This,Value)	\
    ( (This)->lpVtbl -> put_EE(This,Value) ) 

#define IGICSources_get_Lat1(This,Value)	\
    ( (This)->lpVtbl -> get_Lat1(This,Value) ) 

#define IGICSources_put_Lat1(This,Value)	\
    ( (This)->lpVtbl -> put_Lat1(This,Value) ) 

#define IGICSources_get_Lat2(This,Value)	\
    ( (This)->lpVtbl -> get_Lat2(This,Value) ) 

#define IGICSources_put_Lat2(This,Value)	\
    ( (This)->lpVtbl -> put_Lat2(This,Value) ) 

#define IGICSources_get_Lon1(This,Value)	\
    ( (This)->lpVtbl -> get_Lon1(This,Value) ) 

#define IGICSources_put_Lon1(This,Value)	\
    ( (This)->lpVtbl -> put_Lon1(This,Value) ) 

#define IGICSources_get_Lon2(This,Value)	\
    ( (This)->lpVtbl -> get_Lon2(This,Value) ) 

#define IGICSources_put_Lon2(This,Value)	\
    ( (This)->lpVtbl -> put_Lon2(This,Value) ) 

#define IGICSources_get_Volts(This,Value)	\
    ( (This)->lpVtbl -> get_Volts(This,Value) ) 

#define IGICSources_put_Volts(This,Value)	\
    ( (This)->lpVtbl -> put_Volts(This,Value) ) 

#define IGICSources_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IGICSources_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IGICSources_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IGICSources_INTERFACE_DEFINED__ */


#ifndef __IReduceCkt_INTERFACE_DEFINED__
#define __IReduceCkt_INTERFACE_DEFINED__

/* interface IReduceCkt */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IReduceCkt;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BCF6-18C2-11F0-A417-C87F5452571C")
    IReduceCkt : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Zmag( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Zmag( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_KeepLoad( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_KeepLoad( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EditString( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EditString( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_StartPDElement( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_StartPDElement( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EnergyMeter( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EnergyMeter( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SaveCircuit( 
            /* [in] */ BSTR CktName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DoDefault( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DoShortLines( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE DoDangling( void) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE DoLoopBreak( void) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE DoParallelLines( void) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE DoSwitches( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Do1phLaterals( void) = 0;
        
        virtual /* [id] */ HRESULT STDMETHODCALLTYPE DoBranchRemove( void) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IReduceCktVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IReduceCkt * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IReduceCkt * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IReduceCkt * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IReduceCkt * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IReduceCkt * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IReduceCkt * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IReduceCkt * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IReduceCkt, get_Zmag)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Zmag )( 
            IReduceCkt * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, put_Zmag)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Zmag )( 
            IReduceCkt * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, get_KeepLoad)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_KeepLoad )( 
            IReduceCkt * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, put_KeepLoad)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_KeepLoad )( 
            IReduceCkt * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, get_EditString)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EditString )( 
            IReduceCkt * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, put_EditString)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EditString )( 
            IReduceCkt * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, get_StartPDElement)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_StartPDElement )( 
            IReduceCkt * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, put_StartPDElement)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_StartPDElement )( 
            IReduceCkt * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, get_EnergyMeter)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EnergyMeter )( 
            IReduceCkt * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, put_EnergyMeter)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EnergyMeter )( 
            IReduceCkt * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReduceCkt, SaveCircuit)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SaveCircuit )( 
            IReduceCkt * This,
            /* [in] */ BSTR CktName);
        
        DECLSPEC_XFGVIRT(IReduceCkt, DoDefault)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DoDefault )( 
            IReduceCkt * This);
        
        DECLSPEC_XFGVIRT(IReduceCkt, DoShortLines)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DoShortLines )( 
            IReduceCkt * This);
        
        DECLSPEC_XFGVIRT(IReduceCkt, DoDangling)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *DoDangling )( 
            IReduceCkt * This);
        
        DECLSPEC_XFGVIRT(IReduceCkt, DoLoopBreak)
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *DoLoopBreak )( 
            IReduceCkt * This);
        
        DECLSPEC_XFGVIRT(IReduceCkt, DoParallelLines)
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *DoParallelLines )( 
            IReduceCkt * This);
        
        DECLSPEC_XFGVIRT(IReduceCkt, DoSwitches)
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *DoSwitches )( 
            IReduceCkt * This);
        
        DECLSPEC_XFGVIRT(IReduceCkt, Do1phLaterals)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Do1phLaterals )( 
            IReduceCkt * This);
        
        DECLSPEC_XFGVIRT(IReduceCkt, DoBranchRemove)
        /* [id] */ HRESULT ( STDMETHODCALLTYPE *DoBranchRemove )( 
            IReduceCkt * This);
        
        END_INTERFACE
    } IReduceCktVtbl;

    interface IReduceCkt
    {
        CONST_VTBL struct IReduceCktVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IReduceCkt_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IReduceCkt_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IReduceCkt_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IReduceCkt_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IReduceCkt_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IReduceCkt_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IReduceCkt_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IReduceCkt_get_Zmag(This,Value)	\
    ( (This)->lpVtbl -> get_Zmag(This,Value) ) 

#define IReduceCkt_put_Zmag(This,Value)	\
    ( (This)->lpVtbl -> put_Zmag(This,Value) ) 

#define IReduceCkt_get_KeepLoad(This,Value)	\
    ( (This)->lpVtbl -> get_KeepLoad(This,Value) ) 

#define IReduceCkt_put_KeepLoad(This,Value)	\
    ( (This)->lpVtbl -> put_KeepLoad(This,Value) ) 

#define IReduceCkt_get_EditString(This,Value)	\
    ( (This)->lpVtbl -> get_EditString(This,Value) ) 

#define IReduceCkt_put_EditString(This,Value)	\
    ( (This)->lpVtbl -> put_EditString(This,Value) ) 

#define IReduceCkt_get_StartPDElement(This,Value)	\
    ( (This)->lpVtbl -> get_StartPDElement(This,Value) ) 

#define IReduceCkt_put_StartPDElement(This,Value)	\
    ( (This)->lpVtbl -> put_StartPDElement(This,Value) ) 

#define IReduceCkt_get_EnergyMeter(This,Value)	\
    ( (This)->lpVtbl -> get_EnergyMeter(This,Value) ) 

#define IReduceCkt_put_EnergyMeter(This,Value)	\
    ( (This)->lpVtbl -> put_EnergyMeter(This,Value) ) 

#define IReduceCkt_SaveCircuit(This,CktName)	\
    ( (This)->lpVtbl -> SaveCircuit(This,CktName) ) 

#define IReduceCkt_DoDefault(This)	\
    ( (This)->lpVtbl -> DoDefault(This) ) 

#define IReduceCkt_DoShortLines(This)	\
    ( (This)->lpVtbl -> DoShortLines(This) ) 

#define IReduceCkt_DoDangling(This)	\
    ( (This)->lpVtbl -> DoDangling(This) ) 

#define IReduceCkt_DoLoopBreak(This)	\
    ( (This)->lpVtbl -> DoLoopBreak(This) ) 

#define IReduceCkt_DoParallelLines(This)	\
    ( (This)->lpVtbl -> DoParallelLines(This) ) 

#define IReduceCkt_DoSwitches(This)	\
    ( (This)->lpVtbl -> DoSwitches(This) ) 

#define IReduceCkt_Do1phLaterals(This)	\
    ( (This)->lpVtbl -> Do1phLaterals(This) ) 

#define IReduceCkt_DoBranchRemove(This)	\
    ( (This)->lpVtbl -> DoBranchRemove(This) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IReduceCkt_INTERFACE_DEFINED__ */


#ifndef __IStorages_INTERFACE_DEFINED__
#define __IStorages_INTERFACE_DEFINED__

/* interface IStorages */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IStorages;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BD28-18C2-11F0-A417-C87F5452571C")
    IStorages : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterValues( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_State( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_State( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_puSOC( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_puSOC( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EffCharge( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EffCharge( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_EffDischarge( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_EffDischarge( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kWRated( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kWRated( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ControlMode( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ControlMode( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Kp( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Kp( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kV( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kV( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kva( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kva( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kvar( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kvar( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kWhRated( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kWhRated( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LimitCurrent( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LimitCurrent( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PF( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PF( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SafeMode( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SafeVoltage( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SafeVoltage( 
            /* [in] */ double Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_AmpLimit( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_AmpLimit( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AmpLimitGain( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_AmpLimitGain( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kVDC( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kVDC( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kW( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kW( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PITol( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PITol( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_ChargeTrigger( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_ChargeTrigger( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_DischargeTrigger( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_DischargeTrigger( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_TimeChargeTrig( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_TimeChargeTrig( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VarFollowInverter( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VarFollowInverter( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IStoragesVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IStorages * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IStorages * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IStorages * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IStorages * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IStorages * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IStorages * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IStorages * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IStorages, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IStorages * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_RegisterNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterNames )( 
            IStorages * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_RegisterValues)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterValues )( 
            IStorages * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IStorages * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IStorages * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IStorages * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IStorages * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IStorages * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IStorages * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IStorages * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_State)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_State )( 
            IStorages * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_State)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_State )( 
            IStorages * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_puSOC)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_puSOC )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_puSOC)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_puSOC )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_EffCharge)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EffCharge )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_EffCharge)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EffCharge )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_EffDischarge)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_EffDischarge )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_EffDischarge)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_EffDischarge )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_kWRated)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kWRated )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_kWRated)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kWRated )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_ControlMode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ControlMode )( 
            IStorages * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_ControlMode)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ControlMode )( 
            IStorages * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_Kp)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Kp )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_Kp)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Kp )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_kV)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kV )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_kV)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kV )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_kva)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kva )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_kva)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kva )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_kvar)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kvar )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_kvar)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kvar )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_kWhRated)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kWhRated )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_kWhRated)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kWhRated )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_LimitCurrent)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LimitCurrent )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_LimitCurrent)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LimitCurrent )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_PF)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PF )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_PF)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PF )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_SafeMode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SafeMode )( 
            IStorages * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_SafeVoltage)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SafeVoltage )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_SafeVoltage)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SafeVoltage )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_AmpLimit)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AmpLimit )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_AmpLimit)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AmpLimit )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_AmpLimitGain)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AmpLimitGain )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_AmpLimitGain)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_AmpLimitGain )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_kVDC)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kVDC )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_kVDC)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kVDC )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_kW)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kW )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_kW)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kW )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_PITol)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PITol )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_PITol)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PITol )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_ChargeTrigger)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_ChargeTrigger )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_ChargeTrigger)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_ChargeTrigger )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_DischargeTrigger)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_DischargeTrigger )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_DischargeTrigger)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_DischargeTrigger )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_TimeChargeTrig)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_TimeChargeTrig )( 
            IStorages * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_TimeChargeTrig)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_TimeChargeTrig )( 
            IStorages * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IStorages, get_VarFollowInverter)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VarFollowInverter )( 
            IStorages * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IStorages, put_VarFollowInverter)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VarFollowInverter )( 
            IStorages * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } IStoragesVtbl;

    interface IStorages
    {
        CONST_VTBL struct IStoragesVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IStorages_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IStorages_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IStorages_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IStorages_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IStorages_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IStorages_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IStorages_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IStorages_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IStorages_get_RegisterNames(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterNames(This,Value) ) 

#define IStorages_get_RegisterValues(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterValues(This,Value) ) 

#define IStorages_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IStorages_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IStorages_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IStorages_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IStorages_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define IStorages_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IStorages_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IStorages_get_State(This,Value)	\
    ( (This)->lpVtbl -> get_State(This,Value) ) 

#define IStorages_put_State(This,Value)	\
    ( (This)->lpVtbl -> put_State(This,Value) ) 

#define IStorages_get_puSOC(This,Value)	\
    ( (This)->lpVtbl -> get_puSOC(This,Value) ) 

#define IStorages_put_puSOC(This,Value)	\
    ( (This)->lpVtbl -> put_puSOC(This,Value) ) 

#define IStorages_get_EffCharge(This,Value)	\
    ( (This)->lpVtbl -> get_EffCharge(This,Value) ) 

#define IStorages_put_EffCharge(This,Value)	\
    ( (This)->lpVtbl -> put_EffCharge(This,Value) ) 

#define IStorages_get_EffDischarge(This,Value)	\
    ( (This)->lpVtbl -> get_EffDischarge(This,Value) ) 

#define IStorages_put_EffDischarge(This,Value)	\
    ( (This)->lpVtbl -> put_EffDischarge(This,Value) ) 

#define IStorages_get_kWRated(This,Value)	\
    ( (This)->lpVtbl -> get_kWRated(This,Value) ) 

#define IStorages_put_kWRated(This,Value)	\
    ( (This)->lpVtbl -> put_kWRated(This,Value) ) 

#define IStorages_get_ControlMode(This,Value)	\
    ( (This)->lpVtbl -> get_ControlMode(This,Value) ) 

#define IStorages_put_ControlMode(This,Value)	\
    ( (This)->lpVtbl -> put_ControlMode(This,Value) ) 

#define IStorages_get_Kp(This,Value)	\
    ( (This)->lpVtbl -> get_Kp(This,Value) ) 

#define IStorages_put_Kp(This,Value)	\
    ( (This)->lpVtbl -> put_Kp(This,Value) ) 

#define IStorages_get_kV(This,Value)	\
    ( (This)->lpVtbl -> get_kV(This,Value) ) 

#define IStorages_put_kV(This,Value)	\
    ( (This)->lpVtbl -> put_kV(This,Value) ) 

#define IStorages_get_kva(This,Value)	\
    ( (This)->lpVtbl -> get_kva(This,Value) ) 

#define IStorages_put_kva(This,Value)	\
    ( (This)->lpVtbl -> put_kva(This,Value) ) 

#define IStorages_get_kvar(This,Value)	\
    ( (This)->lpVtbl -> get_kvar(This,Value) ) 

#define IStorages_put_kvar(This,Value)	\
    ( (This)->lpVtbl -> put_kvar(This,Value) ) 

#define IStorages_get_kWhRated(This,Value)	\
    ( (This)->lpVtbl -> get_kWhRated(This,Value) ) 

#define IStorages_put_kWhRated(This,Value)	\
    ( (This)->lpVtbl -> put_kWhRated(This,Value) ) 

#define IStorages_get_LimitCurrent(This,Value)	\
    ( (This)->lpVtbl -> get_LimitCurrent(This,Value) ) 

#define IStorages_put_LimitCurrent(This,Value)	\
    ( (This)->lpVtbl -> put_LimitCurrent(This,Value) ) 

#define IStorages_get_PF(This,Value)	\
    ( (This)->lpVtbl -> get_PF(This,Value) ) 

#define IStorages_put_PF(This,Value)	\
    ( (This)->lpVtbl -> put_PF(This,Value) ) 

#define IStorages_get_SafeMode(This,Value)	\
    ( (This)->lpVtbl -> get_SafeMode(This,Value) ) 

#define IStorages_get_SafeVoltage(This,Value)	\
    ( (This)->lpVtbl -> get_SafeVoltage(This,Value) ) 

#define IStorages_put_SafeVoltage(This,Value)	\
    ( (This)->lpVtbl -> put_SafeVoltage(This,Value) ) 

#define IStorages_get_AmpLimit(This,Value)	\
    ( (This)->lpVtbl -> get_AmpLimit(This,Value) ) 

#define IStorages_put_AmpLimit(This,Value)	\
    ( (This)->lpVtbl -> put_AmpLimit(This,Value) ) 

#define IStorages_get_AmpLimitGain(This,Value)	\
    ( (This)->lpVtbl -> get_AmpLimitGain(This,Value) ) 

#define IStorages_put_AmpLimitGain(This,Value)	\
    ( (This)->lpVtbl -> put_AmpLimitGain(This,Value) ) 

#define IStorages_get_kVDC(This,Value)	\
    ( (This)->lpVtbl -> get_kVDC(This,Value) ) 

#define IStorages_put_kVDC(This,Value)	\
    ( (This)->lpVtbl -> put_kVDC(This,Value) ) 

#define IStorages_get_kW(This,Value)	\
    ( (This)->lpVtbl -> get_kW(This,Value) ) 

#define IStorages_put_kW(This,Value)	\
    ( (This)->lpVtbl -> put_kW(This,Value) ) 

#define IStorages_get_PITol(This,Value)	\
    ( (This)->lpVtbl -> get_PITol(This,Value) ) 

#define IStorages_put_PITol(This,Value)	\
    ( (This)->lpVtbl -> put_PITol(This,Value) ) 

#define IStorages_get_ChargeTrigger(This,Value)	\
    ( (This)->lpVtbl -> get_ChargeTrigger(This,Value) ) 

#define IStorages_put_ChargeTrigger(This,Value)	\
    ( (This)->lpVtbl -> put_ChargeTrigger(This,Value) ) 

#define IStorages_get_DischargeTrigger(This,Value)	\
    ( (This)->lpVtbl -> get_DischargeTrigger(This,Value) ) 

#define IStorages_put_DischargeTrigger(This,Value)	\
    ( (This)->lpVtbl -> put_DischargeTrigger(This,Value) ) 

#define IStorages_get_TimeChargeTrig(This,Value)	\
    ( (This)->lpVtbl -> get_TimeChargeTrig(This,Value) ) 

#define IStorages_put_TimeChargeTrig(This,Value)	\
    ( (This)->lpVtbl -> put_TimeChargeTrig(This,Value) ) 

#define IStorages_get_VarFollowInverter(This,Value)	\
    ( (This)->lpVtbl -> get_VarFollowInverter(This,Value) ) 

#define IStorages_put_VarFollowInverter(This,Value)	\
    ( (This)->lpVtbl -> put_VarFollowInverter(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IStorages_INTERFACE_DEFINED__ */


#ifndef __IWindGens_INTERFACE_DEFINED__
#define __IWindGens_INTERFACE_DEFINED__

/* interface IWindGens */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IWindGens;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BD8C-18C2-11F0-A417-C87F5452571C")
    IWindGens : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RegisterValues( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Ag( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Ag( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_idx( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_idx( 
            /* [in] */ long Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_Cp( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_Cp( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kV( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kV( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kva( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kva( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kvar( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kvar( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kW( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kW( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Lamda( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Lamda( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_N_WTG( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_N_WTG( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_NPoles( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_NPoles( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_pd( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_pd( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PF( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PF( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_PSS( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_PSS( 
            /* [in] */ double Value) = 0;
        
        virtual /* [propget][id] */ HRESULT STDMETHODCALLTYPE get_QFlag( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [propput][id] */ HRESULT STDMETHODCALLTYPE put_QFlag( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_QMode( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_QMode( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_QSS( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_QSS( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Rad( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Rad( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RThev( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_RThev( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VCutIn( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VCutIn( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_VCutOut( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_VCutOut( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Vss( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Vss( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_WindSpeed( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_WindSpeed( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_XThev( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_XThev( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IWindGensVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IWindGens * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IWindGens * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IWindGens * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IWindGens * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IWindGens * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IWindGens * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IWindGens * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IWindGens, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IWindGens * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_RegisterNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterNames )( 
            IWindGens * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_RegisterValues)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RegisterValues )( 
            IWindGens * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IWindGens * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IWindGens * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IWindGens * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_Ag)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Ag )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_Ag)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Ag )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_idx)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_idx )( 
            IWindGens * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_idx)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_idx )( 
            IWindGens * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_Cp)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Cp )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_Cp)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Cp )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_kV)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kV )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_kV)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kV )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_kva)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kva )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_kva)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kva )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_kvar)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kvar )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_kvar)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kvar )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_kW)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kW )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_kW)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kW )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_Lamda)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Lamda )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_Lamda)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Lamda )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_N_WTG)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_N_WTG )( 
            IWindGens * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_N_WTG)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_N_WTG )( 
            IWindGens * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_NPoles)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_NPoles )( 
            IWindGens * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_NPoles)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_NPoles )( 
            IWindGens * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_pd)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_pd )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_pd)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_pd )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_PF)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PF )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_PF)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PF )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_PSS)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_PSS )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_PSS)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_PSS )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_QFlag)
        /* [propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_QFlag )( 
            IWindGens * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_QFlag)
        /* [propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_QFlag )( 
            IWindGens * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_QMode)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_QMode )( 
            IWindGens * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_QMode)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_QMode )( 
            IWindGens * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_QSS)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_QSS )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_QSS)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_QSS )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_Rad)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Rad )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_Rad)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Rad )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_RThev)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RThev )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_RThev)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_RThev )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_VCutIn)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VCutIn )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_VCutIn)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VCutIn )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_VCutOut)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_VCutOut )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_VCutOut)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_VCutOut )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_Vss)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Vss )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_Vss)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Vss )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_WindSpeed)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_WindSpeed )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_WindSpeed)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_WindSpeed )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_XThev)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_XThev )( 
            IWindGens * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_XThev)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_XThev )( 
            IWindGens * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IWindGens, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IWindGens * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IWindGens, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IWindGens * This,
            /* [in] */ BSTR Value);
        
        END_INTERFACE
    } IWindGensVtbl;

    interface IWindGens
    {
        CONST_VTBL struct IWindGensVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IWindGens_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IWindGens_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IWindGens_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IWindGens_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IWindGens_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IWindGens_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IWindGens_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IWindGens_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IWindGens_get_RegisterNames(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterNames(This,Value) ) 

#define IWindGens_get_RegisterValues(This,Value)	\
    ( (This)->lpVtbl -> get_RegisterValues(This,Value) ) 

#define IWindGens_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IWindGens_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IWindGens_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IWindGens_get_Ag(This,Value)	\
    ( (This)->lpVtbl -> get_Ag(This,Value) ) 

#define IWindGens_put_Ag(This,Value)	\
    ( (This)->lpVtbl -> put_Ag(This,Value) ) 

#define IWindGens_get_idx(This,Value)	\
    ( (This)->lpVtbl -> get_idx(This,Value) ) 

#define IWindGens_put_idx(This,Value)	\
    ( (This)->lpVtbl -> put_idx(This,Value) ) 

#define IWindGens_get_Cp(This,Value)	\
    ( (This)->lpVtbl -> get_Cp(This,Value) ) 

#define IWindGens_put_Cp(This,Value)	\
    ( (This)->lpVtbl -> put_Cp(This,Value) ) 

#define IWindGens_get_kV(This,Value)	\
    ( (This)->lpVtbl -> get_kV(This,Value) ) 

#define IWindGens_put_kV(This,Value)	\
    ( (This)->lpVtbl -> put_kV(This,Value) ) 

#define IWindGens_get_kva(This,Value)	\
    ( (This)->lpVtbl -> get_kva(This,Value) ) 

#define IWindGens_put_kva(This,Value)	\
    ( (This)->lpVtbl -> put_kva(This,Value) ) 

#define IWindGens_get_kvar(This,Value)	\
    ( (This)->lpVtbl -> get_kvar(This,Value) ) 

#define IWindGens_put_kvar(This,Value)	\
    ( (This)->lpVtbl -> put_kvar(This,Value) ) 

#define IWindGens_get_kW(This,Value)	\
    ( (This)->lpVtbl -> get_kW(This,Value) ) 

#define IWindGens_put_kW(This,Value)	\
    ( (This)->lpVtbl -> put_kW(This,Value) ) 

#define IWindGens_get_Lamda(This,Value)	\
    ( (This)->lpVtbl -> get_Lamda(This,Value) ) 

#define IWindGens_put_Lamda(This,Value)	\
    ( (This)->lpVtbl -> put_Lamda(This,Value) ) 

#define IWindGens_get_N_WTG(This,Value)	\
    ( (This)->lpVtbl -> get_N_WTG(This,Value) ) 

#define IWindGens_put_N_WTG(This,Value)	\
    ( (This)->lpVtbl -> put_N_WTG(This,Value) ) 

#define IWindGens_get_NPoles(This,Value)	\
    ( (This)->lpVtbl -> get_NPoles(This,Value) ) 

#define IWindGens_put_NPoles(This,Value)	\
    ( (This)->lpVtbl -> put_NPoles(This,Value) ) 

#define IWindGens_get_pd(This,Value)	\
    ( (This)->lpVtbl -> get_pd(This,Value) ) 

#define IWindGens_put_pd(This,Value)	\
    ( (This)->lpVtbl -> put_pd(This,Value) ) 

#define IWindGens_get_PF(This,Value)	\
    ( (This)->lpVtbl -> get_PF(This,Value) ) 

#define IWindGens_put_PF(This,Value)	\
    ( (This)->lpVtbl -> put_PF(This,Value) ) 

#define IWindGens_get_PSS(This,Value)	\
    ( (This)->lpVtbl -> get_PSS(This,Value) ) 

#define IWindGens_put_PSS(This,Value)	\
    ( (This)->lpVtbl -> put_PSS(This,Value) ) 

#define IWindGens_get_QFlag(This,Value)	\
    ( (This)->lpVtbl -> get_QFlag(This,Value) ) 

#define IWindGens_put_QFlag(This,Value)	\
    ( (This)->lpVtbl -> put_QFlag(This,Value) ) 

#define IWindGens_get_QMode(This,Value)	\
    ( (This)->lpVtbl -> get_QMode(This,Value) ) 

#define IWindGens_put_QMode(This,Value)	\
    ( (This)->lpVtbl -> put_QMode(This,Value) ) 

#define IWindGens_get_QSS(This,Value)	\
    ( (This)->lpVtbl -> get_QSS(This,Value) ) 

#define IWindGens_put_QSS(This,Value)	\
    ( (This)->lpVtbl -> put_QSS(This,Value) ) 

#define IWindGens_get_Rad(This,Value)	\
    ( (This)->lpVtbl -> get_Rad(This,Value) ) 

#define IWindGens_put_Rad(This,Value)	\
    ( (This)->lpVtbl -> put_Rad(This,Value) ) 

#define IWindGens_get_RThev(This,Value)	\
    ( (This)->lpVtbl -> get_RThev(This,Value) ) 

#define IWindGens_put_RThev(This,Value)	\
    ( (This)->lpVtbl -> put_RThev(This,Value) ) 

#define IWindGens_get_VCutIn(This,Value)	\
    ( (This)->lpVtbl -> get_VCutIn(This,Value) ) 

#define IWindGens_put_VCutIn(This,Value)	\
    ( (This)->lpVtbl -> put_VCutIn(This,Value) ) 

#define IWindGens_get_VCutOut(This,Value)	\
    ( (This)->lpVtbl -> get_VCutOut(This,Value) ) 

#define IWindGens_put_VCutOut(This,Value)	\
    ( (This)->lpVtbl -> put_VCutOut(This,Value) ) 

#define IWindGens_get_Vss(This,Value)	\
    ( (This)->lpVtbl -> get_Vss(This,Value) ) 

#define IWindGens_put_Vss(This,Value)	\
    ( (This)->lpVtbl -> put_Vss(This,Value) ) 

#define IWindGens_get_WindSpeed(This,Value)	\
    ( (This)->lpVtbl -> get_WindSpeed(This,Value) ) 

#define IWindGens_put_WindSpeed(This,Value)	\
    ( (This)->lpVtbl -> put_WindSpeed(This,Value) ) 

#define IWindGens_get_XThev(This,Value)	\
    ( (This)->lpVtbl -> get_XThev(This,Value) ) 

#define IWindGens_put_XThev(This,Value)	\
    ( (This)->lpVtbl -> put_XThev(This,Value) ) 

#define IWindGens_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IWindGens_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IWindGens_INTERFACE_DEFINED__ */


#ifndef __IReactors_INTERFACE_DEFINED__
#define __IReactors_INTERFACE_DEFINED__

/* interface IReactors */
/* [object][oleautomation][dual][helpstring][uuid] */ 


EXTERN_C const IID IID_IReactors;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("C924BDE6-18C2-11F0-A417-C87F5452571C")
    IReactors : public IDispatch
    {
    public:
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_First( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Next( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Count( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Name( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Name( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kV( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kV( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_kvar( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_kvar( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LCurve( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LCurve( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_lmH( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_lmH( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Parallel( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Parallel( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_R( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_R( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_RCurve( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_RCurve( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Rmatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Rmatrix( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Rp( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Rp( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_x( 
            /* [retval][out] */ double *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_x( 
            /* [in] */ double Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Xmatrix( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Xmatrix( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Z( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Z( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Z0( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Z0( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Z1( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Z1( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Z2( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Z2( 
            /* [in] */ VARIANT Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_AllNames( 
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Bus1( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Bus1( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Bus2( 
            /* [retval][out] */ BSTR *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Bus2( 
            /* [in] */ BSTR Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_IsDelta( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_IsDelta( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Phases( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Phases( 
            /* [in] */ long Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IReactorsVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IReactors * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IReactors * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IReactors * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IReactors * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IReactors * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IReactors * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IReactors * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IReactors, get_First)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_First )( 
            IReactors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Next)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Next )( 
            IReactors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Count)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Count )( 
            IReactors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Name)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Name )( 
            IReactors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Name)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Name )( 
            IReactors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_kV)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kV )( 
            IReactors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_kV)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kV )( 
            IReactors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_kvar)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_kvar )( 
            IReactors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_kvar)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_kvar )( 
            IReactors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_LCurve)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LCurve )( 
            IReactors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_LCurve)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LCurve )( 
            IReactors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_lmH)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_lmH )( 
            IReactors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_lmH)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_lmH )( 
            IReactors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Parallel)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Parallel )( 
            IReactors * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Parallel)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Parallel )( 
            IReactors * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_R)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_R )( 
            IReactors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_R)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_R )( 
            IReactors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_RCurve)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_RCurve )( 
            IReactors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_RCurve)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_RCurve )( 
            IReactors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Rmatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Rmatrix )( 
            IReactors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Rmatrix)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Rmatrix )( 
            IReactors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Rp)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Rp )( 
            IReactors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Rp)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Rp )( 
            IReactors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_x)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_x )( 
            IReactors * This,
            /* [retval][out] */ double *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_x)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_x )( 
            IReactors * This,
            /* [in] */ double Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Xmatrix)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Xmatrix )( 
            IReactors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Xmatrix)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Xmatrix )( 
            IReactors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Z)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Z )( 
            IReactors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Z)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Z )( 
            IReactors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Z0)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Z0 )( 
            IReactors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Z0)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Z0 )( 
            IReactors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Z1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Z1 )( 
            IReactors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Z1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Z1 )( 
            IReactors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Z2)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Z2 )( 
            IReactors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Z2)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Z2 )( 
            IReactors * This,
            /* [in] */ VARIANT Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_AllNames)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_AllNames )( 
            IReactors * This,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Bus1)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Bus1 )( 
            IReactors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Bus1)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Bus1 )( 
            IReactors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Bus2)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Bus2 )( 
            IReactors * This,
            /* [retval][out] */ BSTR *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Bus2)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Bus2 )( 
            IReactors * This,
            /* [in] */ BSTR Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_IsDelta)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_IsDelta )( 
            IReactors * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_IsDelta)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_IsDelta )( 
            IReactors * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IReactors, get_Phases)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Phases )( 
            IReactors * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IReactors, put_Phases)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Phases )( 
            IReactors * This,
            /* [in] */ long Value);
        
        END_INTERFACE
    } IReactorsVtbl;

    interface IReactors
    {
        CONST_VTBL struct IReactorsVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IReactors_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IReactors_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IReactors_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IReactors_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IReactors_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IReactors_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IReactors_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IReactors_get_First(This,Value)	\
    ( (This)->lpVtbl -> get_First(This,Value) ) 

#define IReactors_get_Next(This,Value)	\
    ( (This)->lpVtbl -> get_Next(This,Value) ) 

#define IReactors_get_Count(This,Value)	\
    ( (This)->lpVtbl -> get_Count(This,Value) ) 

#define IReactors_get_Name(This,Value)	\
    ( (This)->lpVtbl -> get_Name(This,Value) ) 

#define IReactors_put_Name(This,Value)	\
    ( (This)->lpVtbl -> put_Name(This,Value) ) 

#define IReactors_get_kV(This,Value)	\
    ( (This)->lpVtbl -> get_kV(This,Value) ) 

#define IReactors_put_kV(This,Value)	\
    ( (This)->lpVtbl -> put_kV(This,Value) ) 

#define IReactors_get_kvar(This,Value)	\
    ( (This)->lpVtbl -> get_kvar(This,Value) ) 

#define IReactors_put_kvar(This,Value)	\
    ( (This)->lpVtbl -> put_kvar(This,Value) ) 

#define IReactors_get_LCurve(This,Value)	\
    ( (This)->lpVtbl -> get_LCurve(This,Value) ) 

#define IReactors_put_LCurve(This,Value)	\
    ( (This)->lpVtbl -> put_LCurve(This,Value) ) 

#define IReactors_get_lmH(This,Value)	\
    ( (This)->lpVtbl -> get_lmH(This,Value) ) 

#define IReactors_put_lmH(This,Value)	\
    ( (This)->lpVtbl -> put_lmH(This,Value) ) 

#define IReactors_get_Parallel(This,Value)	\
    ( (This)->lpVtbl -> get_Parallel(This,Value) ) 

#define IReactors_put_Parallel(This,Value)	\
    ( (This)->lpVtbl -> put_Parallel(This,Value) ) 

#define IReactors_get_R(This,Value)	\
    ( (This)->lpVtbl -> get_R(This,Value) ) 

#define IReactors_put_R(This,Value)	\
    ( (This)->lpVtbl -> put_R(This,Value) ) 

#define IReactors_get_RCurve(This,Value)	\
    ( (This)->lpVtbl -> get_RCurve(This,Value) ) 

#define IReactors_put_RCurve(This,Value)	\
    ( (This)->lpVtbl -> put_RCurve(This,Value) ) 

#define IReactors_get_Rmatrix(This,Value)	\
    ( (This)->lpVtbl -> get_Rmatrix(This,Value) ) 

#define IReactors_put_Rmatrix(This,Value)	\
    ( (This)->lpVtbl -> put_Rmatrix(This,Value) ) 

#define IReactors_get_Rp(This,Value)	\
    ( (This)->lpVtbl -> get_Rp(This,Value) ) 

#define IReactors_put_Rp(This,Value)	\
    ( (This)->lpVtbl -> put_Rp(This,Value) ) 

#define IReactors_get_x(This,Value)	\
    ( (This)->lpVtbl -> get_x(This,Value) ) 

#define IReactors_put_x(This,Value)	\
    ( (This)->lpVtbl -> put_x(This,Value) ) 

#define IReactors_get_Xmatrix(This,Value)	\
    ( (This)->lpVtbl -> get_Xmatrix(This,Value) ) 

#define IReactors_put_Xmatrix(This,Value)	\
    ( (This)->lpVtbl -> put_Xmatrix(This,Value) ) 

#define IReactors_get_Z(This,Value)	\
    ( (This)->lpVtbl -> get_Z(This,Value) ) 

#define IReactors_put_Z(This,Value)	\
    ( (This)->lpVtbl -> put_Z(This,Value) ) 

#define IReactors_get_Z0(This,Value)	\
    ( (This)->lpVtbl -> get_Z0(This,Value) ) 

#define IReactors_put_Z0(This,Value)	\
    ( (This)->lpVtbl -> put_Z0(This,Value) ) 

#define IReactors_get_Z1(This,Value)	\
    ( (This)->lpVtbl -> get_Z1(This,Value) ) 

#define IReactors_put_Z1(This,Value)	\
    ( (This)->lpVtbl -> put_Z1(This,Value) ) 

#define IReactors_get_Z2(This,Value)	\
    ( (This)->lpVtbl -> get_Z2(This,Value) ) 

#define IReactors_put_Z2(This,Value)	\
    ( (This)->lpVtbl -> put_Z2(This,Value) ) 

#define IReactors_get_AllNames(This,Value)	\
    ( (This)->lpVtbl -> get_AllNames(This,Value) ) 

#define IReactors_get_Bus1(This,Value)	\
    ( (This)->lpVtbl -> get_Bus1(This,Value) ) 

#define IReactors_put_Bus1(This,Value)	\
    ( (This)->lpVtbl -> put_Bus1(This,Value) ) 

#define IReactors_get_Bus2(This,Value)	\
    ( (This)->lpVtbl -> get_Bus2(This,Value) ) 

#define IReactors_put_Bus2(This,Value)	\
    ( (This)->lpVtbl -> put_Bus2(This,Value) ) 

#define IReactors_get_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> get_IsDelta(This,Value) ) 

#define IReactors_put_IsDelta(This,Value)	\
    ( (This)->lpVtbl -> put_IsDelta(This,Value) ) 

#define IReactors_get_Phases(This,Value)	\
    ( (This)->lpVtbl -> get_Phases(This,Value) ) 

#define IReactors_put_Phases(This,Value)	\
    ( (This)->lpVtbl -> put_Phases(This,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IReactors_INTERFACE_DEFINED__ */


#ifndef __IYMatrix_INTERFACE_DEFINED__
#define __IYMatrix_INTERFACE_DEFINED__

/* interface IYMatrix */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IYMatrix;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("17EAEDB2-1F24-11F0-9D8E-C87F5452571C")
    IYMatrix : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE ZeroInjCurr( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetSourceInjCurrents( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetPCInjCurr( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE BuildYMatrixD( 
            /* [in] */ long BuildOps,
            /* [in] */ VARIANT_BOOL AllocateVI) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE AddInAuxCurrents( 
            /* [in] */ long SType) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SolveSystem( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE SetGeneratordQdV( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE CheckConvergence( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SystemYChanged( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SystemYChanged( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_UseAuxCurrents( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_UseAuxCurrents( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_LoadsNeedUpdating( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_LoadsNeedUpdating( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SolutionInitialized( 
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SolutionInitialized( 
            /* [in] */ VARIANT_BOOL Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_SolverOptions( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_SolverOptions( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][propget][id] */ HRESULT STDMETHODCALLTYPE get_Iteration( 
            /* [retval][out] */ long *Value) = 0;
        
        virtual /* [helpstring][propput][id] */ HRESULT STDMETHODCALLTYPE put_Iteration( 
            /* [in] */ long Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE GetCompressedYMatrix( 
            /* [out] */ VARIANT *ColPtr,
            /* [out] */ VARIANT *RowIdxPtr,
            /* [out] */ VARIANT *cVals) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IYMatrixVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IYMatrix * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IYMatrix * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IYMatrix * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IYMatrix * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IYMatrix * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IYMatrix * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IYMatrix * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IYMatrix, ZeroInjCurr)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *ZeroInjCurr )( 
            IYMatrix * This);
        
        DECLSPEC_XFGVIRT(IYMatrix, GetSourceInjCurrents)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetSourceInjCurrents )( 
            IYMatrix * This);
        
        DECLSPEC_XFGVIRT(IYMatrix, GetPCInjCurr)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetPCInjCurr )( 
            IYMatrix * This);
        
        DECLSPEC_XFGVIRT(IYMatrix, BuildYMatrixD)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *BuildYMatrixD )( 
            IYMatrix * This,
            /* [in] */ long BuildOps,
            /* [in] */ VARIANT_BOOL AllocateVI);
        
        DECLSPEC_XFGVIRT(IYMatrix, AddInAuxCurrents)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *AddInAuxCurrents )( 
            IYMatrix * This,
            /* [in] */ long SType);
        
        DECLSPEC_XFGVIRT(IYMatrix, SolveSystem)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SolveSystem )( 
            IYMatrix * This);
        
        DECLSPEC_XFGVIRT(IYMatrix, SetGeneratordQdV)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *SetGeneratordQdV )( 
            IYMatrix * This);
        
        DECLSPEC_XFGVIRT(IYMatrix, CheckConvergence)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *CheckConvergence )( 
            IYMatrix * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, get_SystemYChanged)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SystemYChanged )( 
            IYMatrix * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, put_SystemYChanged)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SystemYChanged )( 
            IYMatrix * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, get_UseAuxCurrents)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_UseAuxCurrents )( 
            IYMatrix * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, put_UseAuxCurrents)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_UseAuxCurrents )( 
            IYMatrix * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, get_LoadsNeedUpdating)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_LoadsNeedUpdating )( 
            IYMatrix * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, put_LoadsNeedUpdating)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_LoadsNeedUpdating )( 
            IYMatrix * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, get_SolutionInitialized)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SolutionInitialized )( 
            IYMatrix * This,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, put_SolutionInitialized)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SolutionInitialized )( 
            IYMatrix * This,
            /* [in] */ VARIANT_BOOL Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, get_SolverOptions)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_SolverOptions )( 
            IYMatrix * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, put_SolverOptions)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_SolverOptions )( 
            IYMatrix * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, get_Iteration)
        /* [helpstring][propget][id] */ HRESULT ( STDMETHODCALLTYPE *get_Iteration )( 
            IYMatrix * This,
            /* [retval][out] */ long *Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, put_Iteration)
        /* [helpstring][propput][id] */ HRESULT ( STDMETHODCALLTYPE *put_Iteration )( 
            IYMatrix * This,
            /* [in] */ long Value);
        
        DECLSPEC_XFGVIRT(IYMatrix, GetCompressedYMatrix)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *GetCompressedYMatrix )( 
            IYMatrix * This,
            /* [out] */ VARIANT *ColPtr,
            /* [out] */ VARIANT *RowIdxPtr,
            /* [out] */ VARIANT *cVals);
        
        END_INTERFACE
    } IYMatrixVtbl;

    interface IYMatrix
    {
        CONST_VTBL struct IYMatrixVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IYMatrix_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IYMatrix_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IYMatrix_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IYMatrix_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IYMatrix_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IYMatrix_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IYMatrix_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IYMatrix_ZeroInjCurr(This)	\
    ( (This)->lpVtbl -> ZeroInjCurr(This) ) 

#define IYMatrix_GetSourceInjCurrents(This)	\
    ( (This)->lpVtbl -> GetSourceInjCurrents(This) ) 

#define IYMatrix_GetPCInjCurr(This)	\
    ( (This)->lpVtbl -> GetPCInjCurr(This) ) 

#define IYMatrix_BuildYMatrixD(This,BuildOps,AllocateVI)	\
    ( (This)->lpVtbl -> BuildYMatrixD(This,BuildOps,AllocateVI) ) 

#define IYMatrix_AddInAuxCurrents(This,SType)	\
    ( (This)->lpVtbl -> AddInAuxCurrents(This,SType) ) 

#define IYMatrix_SolveSystem(This)	\
    ( (This)->lpVtbl -> SolveSystem(This) ) 

#define IYMatrix_SetGeneratordQdV(This)	\
    ( (This)->lpVtbl -> SetGeneratordQdV(This) ) 

#define IYMatrix_CheckConvergence(This,Value)	\
    ( (This)->lpVtbl -> CheckConvergence(This,Value) ) 

#define IYMatrix_get_SystemYChanged(This,Value)	\
    ( (This)->lpVtbl -> get_SystemYChanged(This,Value) ) 

#define IYMatrix_put_SystemYChanged(This,Value)	\
    ( (This)->lpVtbl -> put_SystemYChanged(This,Value) ) 

#define IYMatrix_get_UseAuxCurrents(This,Value)	\
    ( (This)->lpVtbl -> get_UseAuxCurrents(This,Value) ) 

#define IYMatrix_put_UseAuxCurrents(This,Value)	\
    ( (This)->lpVtbl -> put_UseAuxCurrents(This,Value) ) 

#define IYMatrix_get_LoadsNeedUpdating(This,Value)	\
    ( (This)->lpVtbl -> get_LoadsNeedUpdating(This,Value) ) 

#define IYMatrix_put_LoadsNeedUpdating(This,Value)	\
    ( (This)->lpVtbl -> put_LoadsNeedUpdating(This,Value) ) 

#define IYMatrix_get_SolutionInitialized(This,Value)	\
    ( (This)->lpVtbl -> get_SolutionInitialized(This,Value) ) 

#define IYMatrix_put_SolutionInitialized(This,Value)	\
    ( (This)->lpVtbl -> put_SolutionInitialized(This,Value) ) 

#define IYMatrix_get_SolverOptions(This,Value)	\
    ( (This)->lpVtbl -> get_SolverOptions(This,Value) ) 

#define IYMatrix_put_SolverOptions(This,Value)	\
    ( (This)->lpVtbl -> put_SolverOptions(This,Value) ) 

#define IYMatrix_get_Iteration(This,Value)	\
    ( (This)->lpVtbl -> get_Iteration(This,Value) ) 

#define IYMatrix_put_Iteration(This,Value)	\
    ( (This)->lpVtbl -> put_Iteration(This,Value) ) 

#define IYMatrix_GetCompressedYMatrix(This,ColPtr,RowIdxPtr,cVals)	\
    ( (This)->lpVtbl -> GetCompressedYMatrix(This,ColPtr,RowIdxPtr,cVals) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IYMatrix_INTERFACE_DEFINED__ */


#ifndef __IZIP_INTERFACE_DEFINED__
#define __IZIP_INTERFACE_DEFINED__

/* interface IZIP */
/* [object][oleautomation][dual][hidden][helpstring][version][uuid] */ 


EXTERN_C const IID IID_IZIP;

#if defined(__cplusplus) && !defined(CINTERFACE)
    
    MIDL_INTERFACE("AEFFF57E-1D5F-11F0-BC79-C87F5452571C")
    IZIP : public IDispatch
    {
    public:
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Open( 
            /* [in] */ BSTR FileName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Close( void) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Redirect( 
            /* [in] */ BSTR FileName) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Extract( 
            /* [in] */ BSTR FileName,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE List( 
            /* [in] */ BSTR regexp,
            /* [retval][out] */ VARIANT *Value) = 0;
        
        virtual /* [helpstring][id] */ HRESULT STDMETHODCALLTYPE Contains( 
            /* [in] */ BSTR Name,
            /* [retval][out] */ VARIANT_BOOL *Value) = 0;
        
    };
    
    
#else 	/* C style interface */

    typedef struct IZIPVtbl
    {
        BEGIN_INTERFACE
        
        DECLSPEC_XFGVIRT(IUnknown, QueryInterface)
        HRESULT ( STDMETHODCALLTYPE *QueryInterface )( 
            IZIP * This,
            /* [in] */ REFIID riid,
            /* [annotation][iid_is][out] */ 
            _COM_Outptr_  void **ppvObject);
        
        DECLSPEC_XFGVIRT(IUnknown, AddRef)
        ULONG ( STDMETHODCALLTYPE *AddRef )( 
            IZIP * This);
        
        DECLSPEC_XFGVIRT(IUnknown, Release)
        ULONG ( STDMETHODCALLTYPE *Release )( 
            IZIP * This);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfoCount)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfoCount )( 
            IZIP * This,
            /* [out] */ UINT *pctinfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetTypeInfo)
        HRESULT ( STDMETHODCALLTYPE *GetTypeInfo )( 
            IZIP * This,
            /* [in] */ UINT iTInfo,
            /* [in] */ LCID lcid,
            /* [out] */ ITypeInfo **ppTInfo);
        
        DECLSPEC_XFGVIRT(IDispatch, GetIDsOfNames)
        HRESULT ( STDMETHODCALLTYPE *GetIDsOfNames )( 
            IZIP * This,
            /* [in] */ REFIID riid,
            /* [size_is][in] */ LPOLESTR *rgszNames,
            /* [range][in] */ UINT cNames,
            /* [in] */ LCID lcid,
            /* [size_is][out] */ DISPID *rgDispId);
        
        DECLSPEC_XFGVIRT(IDispatch, Invoke)
        /* [local] */ HRESULT ( STDMETHODCALLTYPE *Invoke )( 
            IZIP * This,
            /* [annotation][in] */ 
            _In_  DISPID dispIdMember,
            /* [annotation][in] */ 
            _In_  REFIID riid,
            /* [annotation][in] */ 
            _In_  LCID lcid,
            /* [annotation][in] */ 
            _In_  WORD wFlags,
            /* [annotation][out][in] */ 
            _In_  DISPPARAMS *pDispParams,
            /* [annotation][out] */ 
            _Out_opt_  VARIANT *pVarResult,
            /* [annotation][out] */ 
            _Out_opt_  EXCEPINFO *pExcepInfo,
            /* [annotation][out] */ 
            _Out_opt_  UINT *puArgErr);
        
        DECLSPEC_XFGVIRT(IZIP, Open)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Open )( 
            IZIP * This,
            /* [in] */ BSTR FileName);
        
        DECLSPEC_XFGVIRT(IZIP, Close)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Close )( 
            IZIP * This);
        
        DECLSPEC_XFGVIRT(IZIP, Redirect)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Redirect )( 
            IZIP * This,
            /* [in] */ BSTR FileName);
        
        DECLSPEC_XFGVIRT(IZIP, Extract)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Extract )( 
            IZIP * This,
            /* [in] */ BSTR FileName,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IZIP, List)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *List )( 
            IZIP * This,
            /* [in] */ BSTR regexp,
            /* [retval][out] */ VARIANT *Value);
        
        DECLSPEC_XFGVIRT(IZIP, Contains)
        /* [helpstring][id] */ HRESULT ( STDMETHODCALLTYPE *Contains )( 
            IZIP * This,
            /* [in] */ BSTR Name,
            /* [retval][out] */ VARIANT_BOOL *Value);
        
        END_INTERFACE
    } IZIPVtbl;

    interface IZIP
    {
        CONST_VTBL struct IZIPVtbl *lpVtbl;
    };

    

#ifdef COBJMACROS


#define IZIP_QueryInterface(This,riid,ppvObject)	\
    ( (This)->lpVtbl -> QueryInterface(This,riid,ppvObject) ) 

#define IZIP_AddRef(This)	\
    ( (This)->lpVtbl -> AddRef(This) ) 

#define IZIP_Release(This)	\
    ( (This)->lpVtbl -> Release(This) ) 


#define IZIP_GetTypeInfoCount(This,pctinfo)	\
    ( (This)->lpVtbl -> GetTypeInfoCount(This,pctinfo) ) 

#define IZIP_GetTypeInfo(This,iTInfo,lcid,ppTInfo)	\
    ( (This)->lpVtbl -> GetTypeInfo(This,iTInfo,lcid,ppTInfo) ) 

#define IZIP_GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId)	\
    ( (This)->lpVtbl -> GetIDsOfNames(This,riid,rgszNames,cNames,lcid,rgDispId) ) 

#define IZIP_Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr)	\
    ( (This)->lpVtbl -> Invoke(This,dispIdMember,riid,lcid,wFlags,pDispParams,pVarResult,pExcepInfo,puArgErr) ) 


#define IZIP_Open(This,FileName)	\
    ( (This)->lpVtbl -> Open(This,FileName) ) 

#define IZIP_Close(This)	\
    ( (This)->lpVtbl -> Close(This) ) 

#define IZIP_Redirect(This,FileName)	\
    ( (This)->lpVtbl -> Redirect(This,FileName) ) 

#define IZIP_Extract(This,FileName,Value)	\
    ( (This)->lpVtbl -> Extract(This,FileName,Value) ) 

#define IZIP_List(This,regexp,Value)	\
    ( (This)->lpVtbl -> List(This,regexp,Value) ) 

#define IZIP_Contains(This,Name,Value)	\
    ( (This)->lpVtbl -> Contains(This,Name,Value) ) 

#endif /* COBJMACROS */


#endif 	/* C style interface */




#endif 	/* __IZIP_INTERFACE_DEFINED__ */


EXTERN_C const CLSID CLSID_Text;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B198-18C2-11F0-A417-C87F5452571C")
Text;
#endif

EXTERN_C const CLSID CLSID_DSSProperty;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B1DE-18C2-11F0-A417-C87F5452571C")
DSSProperty;
#endif

EXTERN_C const CLSID CLSID_CktElement;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B242-18C2-11F0-A417-C87F5452571C")
CktElement;
#endif

EXTERN_C const CLSID CLSID_Error;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B27E-18C2-11F0-A417-C87F5452571C")
Error;
#endif

EXTERN_C const CLSID CLSID_Circuit;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B30A-18C2-11F0-A417-C87F5452571C")
Circuit;
#endif

EXTERN_C const CLSID CLSID_Bus;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B350-18C2-11F0-A417-C87F5452571C")
Bus;
#endif

EXTERN_C const CLSID CLSID_DSS;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B382-18C2-11F0-A417-C87F5452571C")
DSS;
#endif

EXTERN_C const CLSID CLSID_Solution;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B3E6-18C2-11F0-A417-C87F5452571C")
Solution;
#endif

EXTERN_C const CLSID CLSID_Monitors;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B418-18C2-11F0-A417-C87F5452571C")
Monitors;
#endif

EXTERN_C const CLSID CLSID_Meters;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B47C-18C2-11F0-A417-C87F5452571C")
Meters;
#endif

EXTERN_C const CLSID CLSID_Generators;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B4B8-18C2-11F0-A417-C87F5452571C")
Generators;
#endif

EXTERN_C const CLSID CLSID_DSSProgress;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B4E0-18C2-11F0-A417-C87F5452571C")
DSSProgress;
#endif

EXTERN_C const CLSID CLSID_Settings;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B53A-18C2-11F0-A417-C87F5452571C")
Settings;
#endif

EXTERN_C const CLSID CLSID_Lines;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B594-18C2-11F0-A417-C87F5452571C")
Lines;
#endif

EXTERN_C const CLSID CLSID_CtrlQueue;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B5C6-18C2-11F0-A417-C87F5452571C")
CtrlQueue;
#endif

EXTERN_C const CLSID CLSID_Loads;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B634-18C2-11F0-A417-C87F5452571C")
Loads;
#endif

EXTERN_C const CLSID CLSID_DSSElement;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B65C-18C2-11F0-A417-C87F5452571C")
DSSElement;
#endif

EXTERN_C const CLSID CLSID_ActiveClass;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B68E-18C2-11F0-A417-C87F5452571C")
ActiveClass;
#endif

EXTERN_C const CLSID CLSID_Capacitors;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B6CA-18C2-11F0-A417-C87F5452571C")
Capacitors;
#endif

EXTERN_C const CLSID CLSID_Transformers;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B7BA-18C2-11F0-A417-C87F5452571C")
Transformers;
#endif

EXTERN_C const CLSID CLSID_SwtControls;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B7CE-18C2-11F0-A417-C87F5452571C")
SwtControls;
#endif

EXTERN_C const CLSID CLSID_CapControls;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B7E2-18C2-11F0-A417-C87F5452571C")
CapControls;
#endif

EXTERN_C const CLSID CLSID_RegControls;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B80A-18C2-11F0-A417-C87F5452571C")
RegControls;
#endif

EXTERN_C const CLSID CLSID_Topology;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B83C-18C2-11F0-A417-C87F5452571C")
Topology;
#endif

EXTERN_C const CLSID CLSID_DSS_Executive;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B878-18C2-11F0-A417-C87F5452571C")
DSS_Executive;
#endif

EXTERN_C const CLSID CLSID_Sensors;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B8FA-18C2-11F0-A417-C87F5452571C")
Sensors;
#endif

EXTERN_C const CLSID CLSID_XYCurves;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B940-18C2-11F0-A417-C87F5452571C")
XYCurves;
#endif

EXTERN_C const CLSID CLSID_PDElements;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B97C-18C2-11F0-A417-C87F5452571C")
PDElements;
#endif

EXTERN_C const CLSID CLSID_Reclosers;

#ifdef __cplusplus

class DECLSPEC_UUID("C924B9EA-18C2-11F0-A417-C87F5452571C")
Reclosers;
#endif

EXTERN_C const CLSID CLSID_Relays;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BA26-18C2-11F0-A417-C87F5452571C")
Relays;
#endif

EXTERN_C const CLSID CLSID_CmathLib;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BA58-18C2-11F0-A417-C87F5452571C")
CmathLib;
#endif

EXTERN_C const CLSID CLSID_Parser;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BA9E-18C2-11F0-A417-C87F5452571C")
Parser;
#endif

EXTERN_C const CLSID CLSID_LoadShapes;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BAE4-18C2-11F0-A417-C87F5452571C")
LoadShapes;
#endif

EXTERN_C const CLSID CLSID_Fuses;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BB34-18C2-11F0-A417-C87F5452571C")
Fuses;
#endif

EXTERN_C const CLSID CLSID_ISources;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BB66-18C2-11F0-A417-C87F5452571C")
ISources;
#endif

EXTERN_C const CLSID CLSID_DSSimComs;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BB98-18C2-11F0-A417-C87F5452571C")
DSSimComs;
#endif

EXTERN_C const CLSID CLSID_PVSystems;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BBFC-18C2-11F0-A417-C87F5452571C")
PVSystems;
#endif

EXTERN_C const CLSID CLSID_Vsources;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BC2E-18C2-11F0-A417-C87F5452571C")
Vsources;
#endif

EXTERN_C const CLSID CLSID_Parallel;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BC60-18C2-11F0-A417-C87F5452571C")
Parallel;
#endif

EXTERN_C const CLSID CLSID_LineCodes;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BCA6-18C2-11F0-A417-C87F5452571C")
LineCodes;
#endif

EXTERN_C const CLSID CLSID_GICSources;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BCE2-18C2-11F0-A417-C87F5452571C")
GICSources;
#endif

EXTERN_C const CLSID CLSID_ReduceCkt;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BD14-18C2-11F0-A417-C87F5452571C")
ReduceCkt;
#endif

EXTERN_C const CLSID CLSID_Storages;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BD78-18C2-11F0-A417-C87F5452571C")
Storages;
#endif

EXTERN_C const CLSID CLSID_WindGens;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BDC8-18C2-11F0-A417-C87F5452571C")
WindGens;
#endif

EXTERN_C const CLSID CLSID_Reactors;

#ifdef __cplusplus

class DECLSPEC_UUID("C924BE18-18C2-11F0-A417-C87F5452571C")
Reactors;
#endif

EXTERN_C const CLSID CLSID_ZIP;

#ifdef __cplusplus

class DECLSPEC_UUID("8A3493D0-1D5F-11F0-BC79-C87F5452571C")
ZIP;
#endif

EXTERN_C const CLSID CLSID_YMatrix;

#ifdef __cplusplus

class DECLSPEC_UUID("1C561F66-1F24-11F0-9D8E-C87F5452571C")
YMatrix;
#endif
#endif /* __DSSExtensions_LIBRARY_DEFINED__ */

/* Additional Prototypes for ALL interfaces */

/* end of Additional Prototypes */

#ifdef __cplusplus
}
#endif

#endif


