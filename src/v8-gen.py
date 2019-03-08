'''
Generate most of the DSS C-API v8 files from v7.
This script only needs to be run when there are changes in the C-API v7 files. 
Note that the output needs to be validated before commiting it.
'''
from glob import glob
import re

def replace(line, a, b):
    # print(a,b)
    lline = line.lower().strip()
    line0 = line.split('//')[0]
    if lline.startswith('function') or lline.startswith('procedure') or lline.startswith('//'):
        return line
        
    new_line = re.sub(r"([^a-z0-9_']+)" + re.escape(a) + r'([^a-z0-9_]+)', r'\1' + b + r'\2', line, flags=re.IGNORECASE)
    
    new_line0 = new_line.split('//')[0]
    
    if new_line0 == line0:
        return line
        
    return new_line
    
    #return re.sub(re.escape(a), re.escape(b), text, flags=re.IGNORECASE)

def v7_to_v8(fn0):
    if 'CAPI_DSSMain' in fn0 or 'CAPI_Solution' in fn0:
        return
        
    fn = fn0.replace('v7', 'v8', 1)
    print(fn)
    with open(fn0, 'r', newline='\n') as fin, open(fn, 'w', newline='\n') as fout:
        if 'CAPI_Parser' in fn:
            fout.write(fin.read())
            return

        for text in fin:
            if text.strip() in ('{V8-ONLY>', '<V8-ONLY}'):
                continue
        
            for k in ['AuxParser', 'DataDirectory', 'EventStrings', 'YprimInvalid', 'Parser', 'ActiveCircuit', 'DIFilesAreOpen', 'DSSClassList', 'ClassNames', 'ActiveDSSClass', 'ActiveDSSObject', 'LastClassReferenced', 'Losses']:
                text = replace(text, '{}'.format(k), '{}[ActiveActor]'.format(k))

    #        text = re.sub(r'ActiveCircuit[\.\)]', 'ActiveCircuit[ActiveActor].', text, flags=re.IGNORECASE) # why not "If ActiveCircuit[ActiveActor] <> Nil"?
            
            text = replace(text, 'ActiveCircuit.Solution', 'ActiveCircuit[ActiveActor].Solution')
            text = replace(text, 'ComputeCapacity()', 'ComputeCapacity(ActiveActor)')
            text = replace(text, 'ComputeCapacity ', 'ComputeCapacity(ActiveActor) ')
            
            text = replace(text, 'RecalcElementData()', 'RecalcElementData(ActiveActor)')
            text = replace(text, 'RecalcElementData;', 'RecalcElementData(ActiveActor);')
            text = replace(text, 'RecalcElementData ;', 'RecalcElementData(ActiveActor);')

            functions = ['SolveSnap', 'GetPCInjCurr', 'GetSourceInjCurrents', 'ZeroInjCurr', 'Solve', 'Sample_DoControlActions', 'Check_Fault_Status', 'SolveDirect', 'DoPflowSolution', 'SolveCircuit', 'SnapShotInit', 'CheckControls', 'SampleControlDevices', 'DoControlActions', 'OpenAllDIFiles', 'CloseAllDIFiles', 'DoZscRefresh', 'DoAllActions', 'ComputeCapacity', 'Edit', 'ProgressHide', 'InitProgressForm', 'GetWindingCurrentsResult', 'SampleAll', 'SaveAll', 'AddStep', 'SubtractStep', 'EndOfTimeStepCleanup', 'UpdateAll', 'ResetAll', 'ResetIt', 'TakeSample', 'PostProcessAll', 'PostProcess', 'SaveRegisters']
            if 'CAPI_Sensors' in fn:
                functions.remove('ResetIt')
                
            for k in functions:
                text = replace(text, '{}'.format(k), '{}(ActiveActor)'.format(k))
            
            text = replace(text, 'States[i]', 'States[i, ActiveActor]')
            text = replace(text, 'States[k]', 'States[k, ActiveActor]')
            text = replace(text, 'Closed[0]', 'Closed[0, ActiveActor]')
            text = replace(text, 'Closed [0]', 'Closed[0, ActiveActor]')
            text = replace(text, 'Closed[i]', 'Closed[i, ActiveActor]')
            text = replace(text, 'Closed[Phs]', 'Closed[Phs, ActiveActor]')
            text = replace(text, 'Power[1]', 'Power[1, ActiveActor]')
            text = replace(text, 'PresentTap[elem.ActiveWinding]', 'PresentTap[elem.ActiveWinding, ActiveActor]')
            text = text.replace('ProxyHdl: Integer)', 'ProxyHdl: Integer; ActorID: Integer)')
            text = text.replace('ProxyHdl:Integer)', 'ProxyHdl: Integer; ActorID: Integer)')
            
            for k in [
                "ProgressFormCaption(' '",
                'ControlQueue.Delete(ActionHandle',
                'BuildYMatrix(BuildOps, AllocateV',
                'BuildYMatrix(BuildOption, FALSE',
                'BuildYMatrix(BuildOption, TRUE',
                'ProgressCaption (Value',
                'ShowPctProgress (Value',
                'ShowPctProgress (0',
                'ShowPctProgress(0',
                'GetPhaseLosses(NValues, cBuffer',
                'GetPhasePower(cBuffer',
                'TranslateToCSV(True',
                'CalcReliabilityIndices(AssumeRestoration',
                'AddInAuxCurrents(SType',
                'SolveSystem(NodeV',
                'GetCurrents(cBuffer',
                'GetAllWindingCurrents(TempCurrentBuffer',
                'GetWindingVoltages(elem.ActiveWinding, TempVoltageBuffer',
                'Hour, Seconds, ActionCode, DeviceHandle, COMControlProxyObj'
            ]:
                text = replace(text, '{})'.format(k), '{}, ActiveActor)'.format(k))
            
            
            for k in ['PVSystem', 'CNData', 'LineSpacing', 'Monitor', 'EnergyMeter', 'Storage', 'XYCurve', 'WireData', 'TSData', 'LoadShape', 'Sensor', 'GICsource', 'VSource', 'Transformer', 'SwtControl', 'GrowthShape', 'LineGeometry', 'LineCode', 'Line', 'ISource', 'Generator', 'Fuse', 'Capacitor', 'CapControl', 'Reactor', 'RegControl', 'Load', 'Recloser', 'Relay']:
                text = replace(text, '{}Class'.format(k), '{}Class[ActiveActor]'.format(k))
                
            text = replace(text, 'Result := NumCircuits;', 'Result := ActiveCircuit[ActiveActor].NumCircuits;')
            text = replace(text, 'CSVFileName', 'Get_FileName(ActiveActor)')
            text = replace(text, 'ShowPctProgress', '// ShowPctProgress')
            text = replace(text, 'DSSExecutive', 'DSSExecutive[ActiveActor]')
        
            fout.write(text)
            

for fn in glob('v7/*.pas'):
    v7_to_v8(fn)
    