from collections import defaultdict
from copy import deepcopy
import re
from pprint import pprint

import matplotlib
matplotlib.use('tkAGG')
import networkx as nx
from networkx.drawing.nx_agraph import graphviz_layout
from matplotlib import pyplot as plt
from lxml import etree
from cycler import cycler
import numpy as np

cls = 'Load'

#print(etree.tostring(edit_method).decode())
def get_label(node):
    name = node.get('name')
    if name:
        return f'{node.tag.lower()}:\n{name}'
    else:
        return node.tag.lower()

def plot(what, paths=(), pos=None):
    G = nx.DiGraph()
    labels = {}
    it = what.getiterator()
    next(it)
    for node in it:
        #print(node)
        parent = node.getparent()
        
        labels[parent] = get_label(parent)
        labels[node] = get_label(node)
        
        G.add_edge(parent, node)

    if pos is None:
        pos = graphviz_layout(G, prog='dot')
    plt.figure()
    ax = plt.axes([0, 0, 1, 1])
    nx.draw_networkx_edges(G, pos, alpha=0.3, arrows=False)#G, pos, labels=labels, arrows=False, node_size=0, font_size=6, alpha=0.5)
    cycle = cycler(color=['r', 'g', 'b', 'y', 'orange', 'c', 'm'])()
    for edgelist in paths:
        nx.draw_networkx_edges(G, pos, edgelist=edgelist, arrows=False, edge_color=next(cycle)['color'])
        
    #nx.draw_networkx_labels(G, pos, labels=labels, font_size=6)
    ax.set_axis_off()
    ax.autoscale()

    #plt.show()
    return pos

def indent(elem, level=0):
    i = "\n" + level*"  "
    if len(elem):
        if not elem.text or not elem.text.strip():
            elem.text = i + " "
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
        for elem in elem:
            indent(elem, level+1)
        if not elem.tail or not elem.tail.strip():
            elem.tail = i
    else:
        if level and (not elem.tail or not elem.tail.strip()):
            elem.tail = i
            
    return elem

def printxml(elem, f=None):
    s = etree.tostring(indent(deepcopy(elem))).decode()
    if f:
        f.write(s)
        return
        
    print(s)


class SideEffectsVisitor:
    def visit_generic(self):
        pass
        
    def visit_expression(self):
        return
        
    def visit_call(self):
        return
        
    def visit_if(self):
        return
        
    def visit_assign(self):
        return

    def visit_expression(self):
        return

    def visit_case(self):
        pass

    
# data = open(f'{cls}.pas.ast.xml.org', 'r').read()
# data = re.sub(r' begin_line="\d+"', '', data)
# data = re.sub(r' begin_col="\d+"', '', data)
# data = re.sub(r' end_line="\d+"', '', data)
# data = re.sub(r' end_col="\d+"', '', data)
# data = re.sub(r' line="\d+"' , '', data)
# data = re.sub(r' col="\d+"', '', data)
# with open(f'{cls}.pas.ast.xml', 'w') as f:
    # f.write(data)

parser = etree.XMLParser(remove_blank_text=True)
doc = etree.parse(f'{cls}.pas.ast.xml', parser)

# Replace redundant call->call
it = doc.getiterator()
for node in it:
    if node.tag == 'CALL':
        ch = list(node)
        if len(ch) == 1 and ch[0].tag == 'CALL':
            node.getparent().replace(node, ch[0])

methods = doc.xpath('/UNIT/IMPLEMENTATION/METHOD')
properties = doc.xpath('/UNIT/INTERFACE//PROPERTY')
impl, = doc.xpath('/UNIT/IMPLEMENTATION')
fields = doc.xpath(f'/UNIT/INTERFACE/TYPESECTION/TYPEDECL[@name="T{cls}Obj"]//FIELD')


#print(methods)

cls_vars = {}
lower_cls_vars = {}
lower_cls_var_to_canonical = {}
for f in fields:
    # print(f)
    fname, ftype = f
    cls_vars[fname.get('value')] = ftype.get('name') or ftype.get('type')
    lower_cls_vars[fname.get('value').lower()] = ftype.get('name') or ftype.get('type')
    lower_cls_var_to_canonical[fname.get('value').lower()] = fname.get('value')

#pprint(cls_vars)


lower_methods = {
    m.get('name').split('.')[1].lower(): m for m in methods
}
methods = {
    m.get('name').split('.')[1]: m for m in methods
}

def get_name(x):
    if x is not None: 
        return x.get('name')
        
    return None
    
def get_method(name):
    if name:
        m = lower_methods.get(name.lower())
        if m is not None: 
            return m#.get('name').split('.')[1]

    return name

properties_get = {}
properties_set = {}
local_temps = {}
    
for p in properties:
    TYPE, = p.xpath('TYPE')
    try:
        READ, = p.xpath('READ/IDENTIFIER')
    except:
        READ = None

    try:
        WRITE, = p.xpath('WRITE/IDENTIFIER')
    except:
        WRITE = None
        
    name = p.get('name')
    #print(p, name, get_name(TYPE), get_name(READ), get_name(WRITE))
    GET = get_method(get_name(READ))
    SET = get_method(get_name(WRITE))
    properties_get[name] = GET
    properties_set[name] = SET
    
    # print(name, GET, SET)
    # try:
        # print(etree.tostring(SET).decode())
    # except:
        # pass


for method_name, method in lower_methods.items():
    if method_name in (f'T{cls}.MakeLine', f'T{cls}.Destroy', f'T{cls}.NewObject', f'T{cls}Obj.Destroy', f'T{cls}Obj.GetPropertyValue'):
        impl.remove(method)
        continue
    #print(name)

for prop in properties:
    name = prop.get('name')
    #print(prop.getparent().getparent().getparent().get('name'), name)
        
edit_method, = doc.xpath(f'/UNIT/IMPLEMENTATION/METHOD[@name="T{cls}.Edit"]')
local_vars = {'Result': edit_method.xpath(f'RETURNTYPE/TYPE')[0].get('name')}
for f in edit_method.xpath(f'VARIABLES/VARIABLE'):
    fname, ftype = f
    local_vars[fname.get('value')] = ftype.get('name') or ftype.get('type')
    


def inline_set_property(ident_name, tmp_name, where):
    method = deepcopy(properties_set[ident_name])
    par_name = method.xpath('PARAMETERS/PARAMETER/NAME')[0].get('value')
    statements, = method.xpath('STATEMENTS')
    
    def replace(node, org, new):
        if node.tag == 'IDENTIFIER' and node.get('name') == org:
            print('replacing in inline property setter')
            node.set('name', new)
    
    it = statements.getiterator()
    for node in it:
        #print(node, node.tag == 'IDENTIFIER', node.get('name') == par_name)
        replace(node, par_name, tmp_name)
    # print(method, par_name, etree.tostring(statements).decode())
    # print()
    # printxml(where)
    # print()
    
    pos = len(where)
    for st in statements:
        where.insert(pos, st)
        pos += 1
        
    # print()
    # printxml(where)
    
    # exit()
    
    # print(etree.tostring(where).decode())
    # print()
    # print(method, par_name, etree.tostring(where0).decode())


def inline_method(method, where, call_params):
    par_names = method.xpath('PARAMETERS/PARAMETER/NAME')
    #assert len(par_names) <= 1, par_names
    method = deepcopy(method)
    statements, = method.xpath('STATEMENTS')
    
    for param_index in range(len(par_names)):
        par_name = method.xpath('PARAMETERS/PARAMETER/NAME')[param_index].get('value')
        tmp_name = f'tmp_{par_name}_{len(local_temps)}'
        local_temps[tmp_name] = call_params #ident_name
    
        def replace(node, org, new):
            if node.tag == 'IDENTIFIER' and node.get('name') == org:
                print('replacing in inline method')
                node.set('name', new)
        
        it = statements.getiterator()
        for node in it:
            # print(node, node.tag == 'IDENTIFIER', node.get('name') == par_name)
            replace(node, par_name, tmp_name)
    
    where.getparent().replace(where, statements)
    
# print(deepcopy(edit_method))
# print((edit_method))

# plot(edit_method)
# plt.savefig(f'{cls}_original.png', dpi=200)
# plt.close()

changes = True
iterations = 0
while changes:
    changes = False
    it = edit_method.getiterator()
    next(it)
    iterations += 1
    for node in it:
        if node.tag == 'ASSIGN':
            ident = node.xpath('LHS/IDENTIFIER')
            if not ident:
                print('TODO')#etree.tostring(node).decode())
                continue
            
            ident, = ident
            ident_name = ident.get('name')
            kind = ''
            if ident_name in properties_set:
                kind = 'PROPERTY'
            elif ident_name.lower() in lower_cls_vars:
                kind = 'CLS_VARIABLE'
            elif ident_name in local_vars:
                kind = 'LOCAL_VARIABLE'
                
            #print(ident_name, kind)

            if kind == 'PROPERTY' and not isinstance(properties_set[ident_name], (bytes, str)):
                tmp_name = f'tmp_{ident_name}_{len(local_temps)}'
                local_temps[tmp_name] = []#ident_name
                ident.set('name', tmp_name)
                #print(node.getparent())
                #printxml(node.getparent())
                #exit()
                inline_set_property(ident_name, tmp_name, node.getparent())
                changes = True
                
            elif kind == 'PROPERTY':
                #print('!', ident_name, properties_set[ident_name])
                ident.set('name', properties_set[ident_name])
                changes = True
                
        elif node.tag == 'CALL':
            ident = list(node)[0]
            ident_name = ident.get('name')
            #print('+++++++++++++', ident, ident_name)
            if ident.tag != 'IDENTIFIER':
                #print('!!!', ident)
                continue
            
            if ident_name.lower() not in lower_methods:
                #print('> Not a class method, skipping:', ident_name)
                continue

            assert node.getparent().tag != 'ASSIGNMENT'
            
            #print('###', ident_name, node, node.getparent(), node.getparent().getparent())
            
            params = node.xpath('EXPRESSIONS/EXPRESSION')
            #print(':::::::', ident_name, params)
            
            inline_method(lower_methods[ident_name.lower()], node, params)
            changes = True
        
print('Iterations:', iterations)            
pos = plot(edit_method)
#plt.savefig(f'{cls}_expandida.png', dpi=200)
plt.close()
#plt.show()
#exit()

it = edit_method.getiterator()
next(it)

assignment_nodes = defaultdict(list)
var_dependencies = defaultdict(set)

for node in it:
    if node.tag == 'ASSIGN':
        ident = node.xpath('LHS/IDENTIFIER')
        if not ident:
            print('TODO')#etree.tostring(node).decode())
            continue
        
        ident, = ident
        ident_name = ident.get('name')
        if ident_name.lower() in lower_cls_vars:
            #print('FOUND ASSIGNMENT', ident_name)
            assignment_nodes[ident_name.lower()].append(node)
            
            rhs_ids = node.xpath('RHS//IDENTIFIER')
            names = [x.get('name') for x in rhs_ids]
            # names_2 = [x for x in names if x.lower() not in lower_cls_vars and x not in local_temps]
            names_vars = [x.lower() for x in names if x.lower() in lower_cls_vars]
            #assert len(rhs) == 1
            #print(names, names_2)
            var_dependencies[ident_name.lower()] |= set(names_vars)
            
            names_tmps = [x for x in names if x in local_temps]
            for tmp in names_tmps:
                exprs = local_temps[tmp]
                for expr in exprs:
                    ids = node.xpath('RHS//IDENTIFIER')
                    names = [x.get('name') for x in ids]
                    # names_2 = [x for x in names if x.lower() not in lower_cls_vars and x not in local_temps]
                    names_vars = [x.lower() for x in names if x.lower() in lower_cls_vars]
            
            var_dependencies[ident_name.lower()] |= set(names_vars)
            
            #print(etree.tostring(rhs).decode())
            #var_dependencies

changed = True
while changed:
    changed = False
    
    for v, deps in list(var_dependencies.items()):
        for d in list(deps):
            org = set(var_dependencies[v])
            var_dependencies[v] |= var_dependencies[d]
            if not changed:
                changed = var_dependencies[v] != org

var_dependencies2 = {}
for v, deps in list(var_dependencies.items()):
    var_dependencies2[lower_cls_var_to_canonical[v]] = [
        lower_cls_var_to_canonical[x] for x in sorted(deps)
    ]

pprint(var_dependencies2)

Gdep = nx.DiGraph()
for v, deps in var_dependencies2.items():
    for d in deps:
        Gdep.add_edge(d, v)

print(len(Gdep))
print(len(cls_vars))
for x in cls_vars:
    if x not in Gdep:
        print(x)
#pos = graphviz_layout(Gdep, prog='dot')
#nx.draw(Gdep, pos)
# nx.draw_circular(Gdep)
# nx.draw_shell(Gdep, labels={v:v for v in var_dependencies2.keys()})
# data = []
# for n in list(Gdep):
    # data.append((n, Gdep.in_degree(n), Gdep.out_degree(n)))

# nodes, ind, outd = zip(*data)

# plt.figure()
# width = 0.35
# x = np.array(range(len(nodes)))
# plt.bar(x - width/2, ind, width, label='Número de dependências')
# plt.bar(x + width/2, outd, width, label='Número de dependentes')
# ax = plt.gca()
# ax.set_xticks(x)
# ax.set_xticklabels(nodes, rotation=90)
# plt.legend()
# plt.grid()
# plt.tight_layout()
# #nx.draw_shell(Gdep, with_labels=True)
# plt.show()
    

# pprint(local_temps)

exit()
for node in edit_method.xpath('//CALL/DOT/IDENTIFIER[@name="ParseAsVector"]'):
    gparent = node.getparent().getparent()
    expr = list(list(gparent)[1])[1]
    ident_name = expr[0].get('name' )
    assignment_nodes[ident_name].append(node)
    if ident_name.lower() in lower_cls_vars:
        #print('FOUND ASSIGNMENT', ident_name)
        assignment_nodes[ident_name.lower()].append(gparent)
    
#variable = 'kVABase'
with open(f'{cls}_custom.xml', 'w') as f:
    printxml(edit_method, f)

change_counts = []



for variable in list(lower_cls_vars):
    paths = []
    nodes = assignment_nodes[variable]
    for node in nodes:
        path = []
        while node is not None:
            parent = node.getparent()
            if parent is None:
                break
                
            path.append((parent, node))
            node = parent
            if node == edit_method: 
                break
            
        paths.append(path[::-1])
        
    #print(paths)
    variable = lower_cls_var_to_canonical[variable]
    change_counts.append((variable, len(paths)))
    if not len(paths):
        print('No path for variable', variable)
        continue

    #print(variable, len(paths))
    # plot(edit_method, paths, pos=pos)
    # plt.text(0, 0, f'{variable} ({len(paths)})')
    # plt.savefig(f'{cls}_varmod_{variable}.png', dpi=200)
    # plt.close()
    
#plt.show()

pprint(sorted(change_counts, key=lambda x: x[1]))
'''
[('FkWhDays', 0),
 ('FCFactor', 0),
 ('FPhaseCurr', 0),
 ('HarmAng', 0),
 ('HarmMag', 0),
 ('LastYear', 0),
 ('LoadFundamental', 0),
 ('LoadSolutionCount', 0),
 ('OpenLoadSolutionCount', 0),
 ('YPrimOpenCond', 0),
 ('EEN_Factor', 0),
 ('UE_Factor', 0),
 ('FkVAAllocationFactor', 1),
 ('FConnectedkVA', 1),
 ('FkWh', 1),
 ('varBase', 1),
 ('Yeq', 1),
 ('YQFixed', 1),
 ('FpuXHarm', 1),
 ('FXRHarmRatio', 1),
 ('FpuMean', 1),
 ('FpuStdDev', 1),
 ('Vmaxpu', 1),
 ('VminEmerg', 1),
 ('VminNormal', 1),
 ('Vminpu', 1),
 ('VLowpu', 1),
 ('ILow', 1),
 ('I95', 1),
 ('IBase', 1),
 ('M95', 1),
 ('M95I', 1),
 ('FnZIPV', 1),
 ('DailyShapeObj', 1),
 ('DutyShapeObj', 1),
 ('GrowthShape', 1),
 ('GrowthShapeObj', 1),
 ('HasBeenAllocated', 1),
 ('kVLoadBase', 1),
 ('LoadClass', 1),
 ('NumCustomers', 1),
 ('Rneut', 1),
 ('Xneut', 1),
 ('CVRshape', 1),
 ('CVRShapeObj', 1),
 ('ZIPV', 1),
 ('puSeriesRL', 1),
 ('RelWeighting', 1),
 ('FLoadModel', 1),
 ('PFChanged', 2),
 ('VBase105', 2),
 ('VBase95', 2),
 ('VBaseLow', 2),
 ('WNominal', 2),
 ('Yeq105', 2),
 ('Yeq105I', 2),
 ('Yeq95', 2),
 ('FCVRwattFactor', 2),
 ('FCVRvarFactor', 2),
 ('PFSpecified', 2),
 ('DailyShape', 2),
 ('DutyShape', 2),
 ('YearlyShape', 2),
 ('YearlyShapeObj', 2),
 ('FAllocationFactor', 3),
 ('FAvgkW', 3),
 ('Yneut', 3),
 ('ExemptFromLDCurve', 3),
 ('Fixed', 3),
 ('PFNominal', 3),
 ('Connection', 4),
 ('kVABase', 4),
 ('varNominal', 5),
 ('RandomMult', 6),
 ('VBase', 9),
 ('ShapeIsActual', 11),
 ('kWBase', 11),
 ('LoadSpecType', 12),
 ('ShapeFactor', 20),
 ('kvarBase', 22),
 ('LastGrowthFactor', 36)]
 '''


