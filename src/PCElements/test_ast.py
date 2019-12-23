from lxml import etree
from collections import Counter

xml = open('Load_Edit.pas.ast.xml', 'r').read()

root = etree.fromstring(xml)
tree = etree.ElementTree(root)
 
for tag in root.iter():
    path = tree.getpath(tag)
    path = path.replace('/', '    ')
    spaces = Counter(path)
    tag_name = path.split()[-1].split('[')[0]
    tag_name = ' ' * (spaces[' '] - 4) + tag_name
    print(tag_name)