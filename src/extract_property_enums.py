#!/usr/bin/env python3

'''
Extract property enums to generate include/dss_enums.h
'''

from glob import glob
import os, sys, re, copy
from io import StringIO

def extract(root='.', write=False):
    if os.path.exists(os.path.join(root, 'CAPI')):
        print('ERROR: run this script from the main DSS C-API source folder.', file=sys.stderr)
        exit(-1)

    enums = {}
    cls_parents = {}
    org_names = {}
    concrete = set()

    for fn in glob(os.path.join(root, 'src/*/*.pas')):
        bn = os.path.basename(fn)
        if bn.startswith('CAPI_'):
            continue
            
        text = open(fn, 'r').read()
        text = re.split(r'^\s*implementation\s*$', text, flags=re.IGNORECASE)[0]
        fn = fn[4:]

        # Remove comments
        text = re.sub(r'\s//.*', '', text)
        text = re.sub(r'\{.*?\}', '', text, flags=re.DOTALL)
        # Find and extract the enum info
        for m0 in re.finditer(r'T([a-z0-9\_]+)Prop = \((.*?)\);', text, flags=re.IGNORECASE|re.DOTALL):
            cls = m0.group(1)
            enum = []
            v = -1
            for m1 in re.finditer('\s*([a-z0-9\_]+)\s*=*\s*(\d+)*.*?', m0.group(2), flags=re.IGNORECASE):
                k, vnew = m1.groups()
                if vnew is not None:
                    v = int(vnew)
                else:
                    v = v + 1

                if k == 'INVALID':
                    continue

                enum.append((k, v))

            # save for later
            cls = cls.lower()
            if cls == 'cktelement':
                cls = 'dss' + cls
            
            enums[cls] = [x[0] for x in sorted(enum, key=lambda v: v[1])]
            concrete.add(cls)

        # Find the class parents
        for m0 in re.finditer(r'^\s*T([a-z0-9\_]+((Obj)|(Element)|(Elem)|(Object)))\s*=\s*class\(T(.*?)\)', text, flags=re.IGNORECASE|re.MULTILINE):
            cls, parent = m0.group(1), m0.group(7)

            if cls.lower() in ['solutionobj', 'cimbankobject', 'cimoplimitobject']:
                # Ignore non-exposed classes
                continue

            if cls.endswith('Obj'):
                cls = cls[:-3]
            
            org_names[cls.lower()] = cls # preserve original case
            if parent.endswith('Obj'):
                parent = parent[:-3]
            
            cls = cls.lower()
            cls_parents[cls] = parent.lower()

            if parent.lower() in concrete:
                concrete.remove(parent.lower())

        
    # Walk the tree until all get to TObject
    cls_parents0 = copy.deepcopy(cls_parents)
    changed = True
    while changed:
        changed = False
        for cls, parent in list(cls_parents.items()):
            if parent == 'object':
                continue

            changed = True
            if parent in enums:
                if cls not in enums:
                    enums[cls] = enums[parent]
                else:
                    enums[cls].extend(enums[parent])

            cls_parents[cls] = cls_parents[parent]
    
    # Write the output
    if write:
        fo = open('include/dss_enums.h', 'w')
    else:
        fo = StringIO()

    fo.write('''// This file is generated. Avoid manual edition.
    // NOTE: The numbers are specific to DSS C-API and can change across versions.

    #ifndef DSS_CAPI_ENUMS_H
    #define DSS_CAPI_ENUMS_H

    #include "./dss_capi.h"

    #ifdef __cplusplus
    extern "C" {
    #endif

    ''')

    # override the actual implementation names
    org_names['transf'] = 'Transformer' 
    org_names['dsscktelement'] = 'CktElement'
    org_names['tcc_curve'] = 'TCCCurve'
    for cls in ['pvsystem', 'invcontrol', 'storage', 'storagecontroller']:
        org_names[cls + '2'] = org_names[cls]
        org_names[cls] = 'Legacy' + org_names[cls]

    concrete.add('wiredata') # it's empty

    for cls, enum in sorted(enums.items(), key=lambda v: v[0]):
        # skip base and abstract classes
        if cls not in concrete:
            continue

        cls = org_names[cls].replace('curve', 'Curve').replace('source', 'Source').replace('system', 'System')
        fo.write(f'    enum {cls}Property {{\n')
        for idx, k in enumerate(enum):
            if k.startswith('__'):
                k = k[2:]
            k.replace('__', '_')

            fo.write(f'        {cls}_{k} = {idx + 1},\n')
            
        fo.write(f'        {cls}_PropertyCount = {idx + 1}\n')

        fo.write('    };\n\n')


    fo.write('''
    #ifdef __cplusplus
    } // extern "C"
    #endif
    #endif
    ''')

    fo.close()

    return dict(
        org_names=org_names,
        enums=enums,
        concrete=concrete,
        cls_parents=cls_parents0
    )

if __name__ == '__main__':
    result = extract(write=True)
    print(len(result['enums']), 'enums processed')
    