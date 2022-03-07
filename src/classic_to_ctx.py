#!/usr/bin/env python3

'''
A very basic script to adapt the classic DSS C-API code to
the DSSContext version. 

Tasks achieved by this script:

- Modify functions to include the TDSSContext as a parameter.
- Replace DSSPrime with that parameter.
- Prepend the "DSS" parameter to API calls (e.g. in the GR functions).
- Prepend function names with "ctx_".
- Generate an .inc file with the names of the modified functions.
- Generate an .inc file with the files to be used in the "uses" 
  clause of the project.
- Generate a partial C header with the modified declarations.

This script is called in the build scripts automatically.

In a possible future, we could to the reverse, committing the ctx 
files instead:
- Remove the "ctx_" prefix.
- Remove "DSS: TDSSContext" parameters.
- Rename "DSS" to "DSSPrime" in the source code.
or
- Generate wrapper files that call the "ctx_" functions
  with the "DSSPrime" parameter.
'''

from glob import glob
import os, sys, re

if os.path.exists('CAPI'):
    print('ERROR: run this script from the main DSS C-API source folder.', file=sys.stderr)
    exit(-1)

# Start the list with the CAPI_Utils.pas functions and
# the context handling ones
funcnames = [
    'ctx_DSS_ResetStringBuffer',
    'ctx_DSS_DisposeGRData',
    'ctx_DSS_GetGRPointers',
    'ctx_DSS_GR_DataPtr_PDouble',
    'ctx_DSS_GR_DataPtr_PInteger',
    'ctx_DSS_GR_DataPtr_PByte',
    'ctx_DSS_GR_CountPtr_PDouble',
    'ctx_DSS_GR_CountPtr_PInteger',
    'ctx_DSS_GR_CountPtr_PByte',
    
    'ctx_New',
    'ctx_Dispose',
    'ctx_Get_Prime',
    'ctx_Set_Prime',
]

usefns = [
    "CAPI_Context in 'src/CAPI/CAPI_Context.pas'"
]

def skip(fun):
    prefixes = [' DSS_Dispose_', ' DSS_Get_PAnsiChar(', ' Obj_', 'Batch_', ' DSS_ExtractSchema']
    for p in prefixes:
        if p in fun:
            return True

    return False

for fn in glob('src/CAPI/*.pas'):
    bn = os.path.basename(fn)
    if bn in ['CAPI_Utils.pas', 'CAPI_Types.pas', 'CAPI_Metadata.pas', 'CAPI_Context.pas', 'CAPI_Obj.pas']:
        continue
        
    bnctx = bn.replace('CAPI_', 'CAPICtx_')
    
    os.makedirs('build/generated/', exist_ok=True)
    
    with open(fn, 'r') as fi, open('build/generated/' + bnctx, 'w') as fo:
        src = fi.read()

        #unitname = bn.split('.')[0]
        unitname = src.split(';', 1)[0].strip().split(' ')[1]
        ctxunitname = unitname.replace('CAPI_', 'CAPICtx_')
        
        usefns.append(f"{ctxunitname} in 'build/generated/{bnctx}'")
        
        
        iface, _ = src.split('implementation', 1)
        funcs = [x.split('//')[0].strip() for x in iface.split('\n') if x.lower().startswith('function') or x.lower().startswith('procedure')]
        file_funcnames = []
        file_ftypes = []
        for func in funcs:
            assert '(' in func, (bn, func)
            file_ftypes.append(func.split(' ', 1)[0])

            try:
                funcname = func.split()[1].split('(')[0]
            except:
                try:
                    funcname = func.split()[1].split(':')[0]
                except:
                    funcname = func.split()[1].split(';')[0]

            src = src.replace(func, func.replace('()', '(DSS: TDSSContext)'))
            src = src.replace(func, func.replace('(', '(DSS: TDSSContext; '))
            src = src.replace(funcname + '(', funcname + '(DSS, ')
            src = src.replace(funcname + '(DSS, )', funcname + '(DSS)')
            src = src.replace(funcname + '(DSS, DSS:', funcname + '(DSS:')
            src = src.replace(' ' + funcname + '(', ' ctx_' + funcname + '(')
            file_funcnames.append('ctx_' + funcname)

        funcnames += file_funcnames

        # This block is required to use the correct "actor" for the PM implementation
        impls = src.split('implementation', 1)[1].split('\nend;\n')
        assert '\nend; ' not in src, fn
        fstarts = [ftype + ' ' + funcname + '(' for ftype, funcname in zip(file_ftypes, file_funcnames)]
        n = '\n'
        file_used_funcs = set()
        if 'CAPI_Parallel' not in fn:
            for impl in impls:
                for fstart, ftype, funcname in zip(fstarts, file_ftypes, file_funcnames):
                    if fstart not in impl:
                        continue

                    if not '_GR(' in fstart:
                        new_impl, sub_count = re.subn(
                            rf'{re.escape(fstart)}(.*?){n}begin{n}(.*?)', 
                            rf'{fstart}\1{n}begin{n}    DSSPrime := DSSPrime.ActiveChild;{n}\2',
                            impl, 
                            1,
                            flags=re.DOTALL|re.IGNORECASE
                        )
                        assert sub_count == 1
                        assert fstart not in file_used_funcs, fstart
                        src = src.replace(impl, new_impl)
                        file_used_funcs.add(fstart)
                    break
            
            missing = (set(fstart for fstart in fstarts if not '_GR(' in fstart) - file_used_funcs)
            assert not missing, missing

        src = src.replace(f'unit {unitname};', f'unit {ctxunitname};')
        # src = src.replace('ctx_ctx_', 'ctx_')
        # src = src.replace('ctx_ctx_', 'ctx_')
        src = src.replace('DSSPrime', 'DSS')
    
        fo.write('// WARNING:\n')
        fo.write('// This file is automatically generated from the CAPI folder\n')
        fo.write('// by the classic_to_ctx.py script. DO NOT EDIT MANUALLY!\n')
        fo.write('// If you need to edit this, remember to disable running\n')
        fo.write('// classic_to_ctx.py in the build scripts.\n')
        
        fo.write('// \n')
        fo.write(src)

        
with open('build/generated/ctx_functions.inc', 'w') as fo:
    fo.write('    ' + ',\n    '.join(sorted(set(funcnames))))
    
with open('build/generated/ctx_files.inc', 'w') as fo:
    fo.write('    ' + ',\n    '.join(sorted(usefns)))
    
with open('include/dss_capi.h', 'r') as fi:
    header = fi.read()
    
with open('include/dss_capi_ctx.h', 'w') as fo:    
    fo.write('/*\n')
    fo.write('    REMINDER:\n')
    fo.write('    This file is automatically generated from dss_capi.h\n')
    fo.write('    by the classic_to_ctx.py script. DO NOT EDIT MANUALLY!\n')
    fo.write('    If you need to edit this, remember to disable running\n')
    fo.write('    classic_to_ctx.py in the build scripts.\n')
    fo.write('*/\n\n')

    # Enums and other definitions are used from the main header
    
    functions = header.split('/* Functions start here */\n')[1].split('#ifdef __cplusplus')[0].split('\n')
    
    functions = [
        re.sub(r'(    DSS_CAPI_DLL [^ ]+[ \*]*)([a-zA-Z0-9_]+)\(', r'\1ctx_\2(void* ctx, ', function) 
        for function in functions
        if not skip(function)
    ]
    functions = '\n'.join(functions).replace('(void* ctx, void)', '(void* ctx)')
    
    fo.write('''#ifndef DSS_CAPI_CTX_H
#define DSS_CAPI_CTX_H

#include "./dss_capi.h"

#ifdef __cplusplus
extern "C" {
#endif

    /*
    Create a new DSS engine context. 
    */
    DSS_CAPI_DLL void* ctx_New(void);

    /*
    Dispose an existing DSS engine context. 
    
    Pass a pointer to the variable, which will be zeroed on success.
    */
    DSS_CAPI_DLL void ctx_Dispose(void *ctx);

    /*
    Returns the prime (default) instance of the DSS engine.
    
    This engine is created by default. This instance is used 
    by the classic API.
    */
    DSS_CAPI_DLL void* ctx_Get_Prime(void);

    /*
    Replaces the existing prime DSS engine context, returning
    the previous instance pointer. 
    Returns NULL if the given context is already set as prime.
    
    A prime engine is created by default when the library is initialized. 
    The user is responsible for handling the potential disposal of the 
    previous prime instance returned by ctx_Set_Prime, if required.
    */
    DSS_CAPI_DLL void *ctx_Set_Prime(void *ctx);

''')
    fo.write(functions)
    fo.write('''
#ifdef __cplusplus
} // extern "C"
#endif
#endif
''')

