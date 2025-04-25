// SPDX-FileCopyrightText: (C) 2024-2025 Paulo Meira & contributors to DSS-Extensions
// SPDX-License-Identifier: LGPL-3.0-only

typedef struct {
    int resType;
    int argType;
    int collectionIdx;
    int funcIdxDim1;
    int funcIdxDim2;
    size_t c_funcOffset;
    void *func;
    size_t attrOffset;
    char const* fname;
} FastDSSFuncInfo;

typedef struct {
    int funcIdxCount;
    int funcIdxFirst;
    int funcIdxNext;
    int funcIdxSetIdx;
    int funcIdxGetIdx;
    int elementType;
    char const* collectionName;
} FastDSSCollectionInfo;

enum {
    fastdss_types_void = 0,
    fastdss_types_u16,
    fastdss_types_f32,
    fastdss_types_f64,
    fastdss_types_f64_f64_i32,
    fastdss_types_gr_f64s,
    fastdss_types_gr_i32s,
    fastdss_types_gr_i8s,
    fastdss_types_gr_z128,
    fastdss_types_gr_z128s,
    fastdss_types_i32,
    fastdss_types_i32_i32,
    fastdss_types_str,
    fastdss_types_strs,
    fastdss_types_z64,
};

enum FastDSSCollection {
    Collection_Buses = -4,
    Collection_ActiveClass = -3,
    Collection_PDElements = -2,
    Collection_None = -1,
    Collection_CktElement = 0,
    Collection_CapControls = 1,
    Collection_Capacitors = 2,
    Collection_CNData = 3,
    Collection_Fuses = 4,
    Collection_Generators = 5,
    Collection_GICSources = 6,
    Collection_ISources = 7,
    Collection_Lines = 8,
    Collection_Loads = 9,
    Collection_LoadShapes = 10,
    Collection_LineCodes = 11,
    Collection_LineGeometries = 12,
    Collection_LineSpacings = 13,
    Collection_Meters = 14,
    Collection_Monitors = 15,
    Collection_PVSystems = 16,
    Collection_RegControls = 17,
    Collection_Reclosers = 18,
    Collection_Relays = 19,
    Collection_Reactors = 20,
    Collection_SwtControls = 21,
    Collection_Sensors = 22,
    Collection_Storages = 23,
    Collection_Transformers = 24,
    Collection_TSData = 25,
    Collection_Vsources = 26,
    Collection_WireData = 27,
    Collection_XYCurves = 28,
    Collection_MAX = 29,
};
