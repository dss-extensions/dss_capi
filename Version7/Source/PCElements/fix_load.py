import re

props = dict(
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


rprops = {str(x[1]): x[0] for x in props.items()}
rLoadModels = {str(x[1]): x[0] for x in props.items()}

text = open('Load.pas', 'r').read()
with open('Load.pas', 'w', newline='\n') as o:
    # We can safely replace these two
    text = re.sub(
        r'PropertyHelp\[(\d+)\]', 
        lambda x: f'PropertyHelp[ord(props.{rprops[x.group(1)]})]',
        text
    )
    text = re.sub(
        r'PropertyValue\[(\d+)\]', 
        lambda x: f'PropertyValue[ord(props.{rprops[x.group(1)]})]',
        text
    )
    
    # DumpProperties?
    
    m = re.search('(function TLoadObj.GetPropertyValue.*?^end;)', text, flags=re.MULTILINE|re.DOTALL)
    sample = m.group(1)
    fixed_sample = re.sub(
        r'(\s+)(\d+):', 
        lambda x: f'{x.group(1)}props.{rprops[x.group(2)]}:' if x.group(2) in rprops else x.group(0),
        sample
    ).replace('case Index of', 'case props(Index) of')
    text = text.replace(sample, fixed_sample)
    
    
    m = re.search('(function TLoad\.Edit: Integer;.*?^end;)', text, flags=re.MULTILINE|re.DOTALL)
    sample = m.group(1)
    fixed_sample = re.sub(
        r'(\s+)(\d+):', 
        lambda x: f'{x.group(1)}props.{rprops[x.group(2)]}:' if x.group(2) in rprops else x.group(0),
        sample
    )
    
    text = text.replace(sample, fixed_sample)
    text = text.replace('case ParamPointer of', 'case props(ParamPointer) of')
    
    print(fixed_sample)
    
    o.write(text)

#TODO: explicit enum for Solution.Mode
