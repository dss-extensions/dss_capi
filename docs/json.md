# JSON in AltDSS

- JSON Export: **new** ***experimental*** flag `DSSJSONOptions_State` to include most of the state of the circuit. This flag might be split into multiple flags to allow better selection of the exported data. Currently, if exports the state of the circuit elements and buses. Another flaf `DSSJSONOptions_Reliability` can be used to include the reliability results on buses and PD elements.
