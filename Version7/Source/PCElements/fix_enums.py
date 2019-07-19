import re

LoadProp = dict(
    INVALID = 0,
    phases = 1,
    bus1 = 2,
    kV = 3,
    kW = 4,
    pf = 5,
    model = 6,
    yearly = 7,
    daily = 8,
    duty = 9, 
    growth = 10, 
    conn = 11, 
    kvar = 12, 
    Rneut = 13, # IF entered -, assume open
    Xneut = 14, 
    status = 15, # fixed or variable
    cls = 16, # integer
    Vminpu = 17, # Min pu voltage for which model applies
    Vmaxpu = 18, # Max pu voltage for which model applies
    Vminnorm = 19, # Min pu voltage normal load
    Vminemerg = 20, # Min pu voltage emergency rating
    xfkVA = 21, # Service transformer rated kVA
    allocationfactor = 22, # allocation factor  for xfkVA
    kVA = 23, # specify load in kVA and PF
    pctmean = 24, # per cent default mean
    pctstddev = 25, # per cent default standard deviation
    CVRwatts = 26, # Percent watts reduction per 1% reduction in voltage from nominal
    CVRvars = 27, # Percent vars reduction per 1% reduction in voltage from nominal
    kwh = 28, # kwh billing
    kwhdays = 29, # kwh billing period (24-hr days)
    Cfactor = 30, # multiplier from kWh avg to peak kW
    CVRcurve = 31, # name of curve to use for yearly CVR simulations
    NumCust = 32, # Number of customers, this load
    ZIPV = 33, # array of 7 coefficients
    pctSeriesRL = 34, # pct of Load that is series R-L
    RelWeight = 35, # Weighting factor for reliability
    Vlowpu = 36, # Below this value resort to constant Z model = Yeq
    puXharm = 37, # pu Reactance for Harmonics, if specifies
    XRharm = 38 # X/R at fundamental for series R-L model for hamonics
)

#TODO: use dss.enums.LoadsModels directly
LoadModels = dict(
    ConstPQ = 1,
    ConstZ = 2,
    Motor = 3,
    CVR = 4,
    ConstI = 5,
    ConstPFixedQ = 6,
    ConstPFixedX = 7,
    ZIPV = 8
)


ReactorProp = dict(
    INVALID = 0,   
    bus1 = 1,
    bus2 = 2, 
    phases = 3, 
    kvar = 4, 
    kv = 5, 
    conn = 6, 
    Rmatrix = 7, 
    Xmatrix = 8, 
    Parallel = 9, 
    R = 10, 
    X = 11, 
    Rp = 12, 
    Z1 = 13, 
    Z2 = 14, 
    Z0 = 15, 
    Z = 16, 
    RCurve = 17, 
    LCurve = 18, 
    LmH = 19
)

ReactorConnection = dict(
    Wye = 0, # wye, star, line-neutral connection
    Delta = 1 # delta, line-line connection
);


rLoadProp = {str(x[1]): x[0] for x in LoadProp.items()}
rLoadModels = {str(x[1]): x[0] for x in LoadModels.items()}

rReactorProp = {str(x[1]): x[0] for x in ReactorProp.items()}

fn = 'Z:/dss/dss_capi/Version7/Source/PCElements/Load.pas'
fn = 'Z:/dss/dss_capi/Version7/Source/PDElements/Reactor.pas'

if fn.endswith('Load.pas'):
    cls = 'Load'
    props = LoadProp
    rprops = rLoadProp
elif fn.endswith('Reactor.pas'):
    cls = 'Reactor'
    props = ReactorProp
    rprops = rReactorProp


text = open(fn, 'r').read()
with open(fn, 'w', newline='\n') as o:
# if True:

    # We can safely replace these two
    text = re.sub(
        r'PropertyHelp\[(\d+)\]', 
        lambda x: f'PropertyHelp[ord(T{cls}Prop.{rprops[x.group(1)]})]',
        text
    )
    text = re.sub(
        r'PropertyValue\[(\d+)\]', 
        lambda x: f'PropertyValue[ord(T{cls}Prop.{rprops[x.group(1)]})]',
        text
    )
    
    # DumpProperties?
    
    m = re.search(f'(function T{cls}Obj.GetPropertyValue.*?^end;)', text, flags=re.MULTILINE|re.DOTALL)
    sample = m.group(1)
    fixed_sample = sample
    fixed_sample = re.sub(
        r'(\s+)(\d+), (\d+):', 
        lambda x: f'{x.group(1)}T{cls}Prop.{rprops[x.group(2)]}, T{cls}Prop.{rprops[x.group(3)]}:' if x.group(2) in rprops and x.group(3) in rprops else x.group(0),
        fixed_sample
    ).replace('case Index of', f'case T{cls}Prop(Index) of')
    fixed_sample = re.sub(
        r'(\s+)(\d+)\.\.(\d+):', 
        lambda x: f'{x.group(1)}T{cls}Prop.{rprops[x.group(2)]}..T{cls}Prop.{rprops[x.group(3)]}:' if x.group(2) in rprops and x.group(3) in rprops else x.group(0),
        fixed_sample
    ).replace('case Index of', f'case T{cls}Prop(Index) of')
    fixed_sample = re.sub(
        r'(\s+)(\d+):', 
        lambda x: f'{x.group(1)}T{cls}Prop.{rprops[x.group(2)]}:' if x.group(2) in rprops else x.group(0),
        fixed_sample
    )
    text = text.replace(sample, fixed_sample)
    
    m = re.search(f'(function T{cls}\.Edit: Integer;.*?^end;)', text, flags=re.MULTILINE|re.DOTALL)
    sample = m.group(1)
    fixed_sample = sample
    fixed_sample = re.sub(
        r'(\s+)(\d+), (\d+):', 
        lambda x: f'{x.group(1)}T{cls}Prop.{rprops[x.group(2)]}, T{cls}Prop.{rprops[x.group(3)]}:' if x.group(2) in rprops and x.group(3) in rprops else x.group(0),
        fixed_sample
    )
    fixed_sample = re.sub(
        r'(\s+)(\d+)\.\.(\d+):', 
        lambda x: f'{x.group(1)}T{cls}Prop.{rprops[x.group(2)]}..T{cls}Prop.{rprops[x.group(3)]}:' if x.group(2) in rprops and x.group(3) in rprops else x.group(0),
        fixed_sample
    )
    fixed_sample = re.sub(
        r'(\s+)(\d+):', 
        lambda x: f'{x.group(1)}T{cls}Prop.{rprops[x.group(2)]}:' if x.group(2) in rprops else x.group(0),
        fixed_sample
    )
    text = text.replace(sample, fixed_sample)
    
    text = text.replace('case ParamPointer of', f'case T{cls}Prop(ParamPointer) of')
    
    # print(fixed_sample)
                
                
    
    
    o.write(text)
