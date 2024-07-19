// Minimal sample for altdss.hpp, Obj/Alt API
#include <iostream>
#include "altdss_obj.hpp"
int main()
{
    using std::cout;
    using std::endl;
    using dss::obj::Load;
    using dss::obj::LoadBatch;
    using dss::obj::LineGeometry;

    dss::APIUtil util;

    Text_Set_Command("redirect '../../../electricdss-tst/Version8/Distrib/IEEETestCases/NEVTestCase/NEVMASTER.DSS'");

    util.check_for_error();
    Load load(&util, 1);
    LineGeometry line_geo(&util, "quadcircuit");

    cout <<
        load.name() << endl <<
        load.bus1() << endl <<
        load.kW() << endl <<
        load.kvar() << endl <<
    endl;

    load.kW(1500).kvar(200).bus1("blabla.1.2");
    cout << load.name() << " " << load.kW() << " " << load.kvar() << endl << endl;

    LoadBatch(&util).kW() *= 5;

    cout << load.name() << " " << load.kW() << " " << load.kvar() << endl << endl;

    cout <<
        line_geo.name() << endl <<
        line_geo.nconds() << endl <<
        line_geo.x() << endl <<
    endl;

    auto wires = line_geo.wires_obj();
    for (auto &w: wires)
    {
        cout << w.name() << ": " << w.radius() << " " << w.radunits_str() << endl;
    }

    LoadBatch new_batch = util.create<LoadBatch>("myload", 25);
    new_batch.kW(1200.0).kvar(900.0).model(Load::LoadModel::ZIPV).end_edit();

    return 0;
}
