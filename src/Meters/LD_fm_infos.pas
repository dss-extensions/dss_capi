unit LD_fm_infos;

{
  ----------------------------------------------------------
  Copyright (c) 2017, Electric Power Research Institute, Inc.
  Added by Ying.
  ----------------------------------------------------------

  Definition of Fmonitor (virtue leader) Public Data Record
}

interface

uses
    UComplex;
      //PointerList;

type

   {Fmonitor public data/state variable structure}
    TLD_fm_infos = packed record
   //properties for Nodes
         // highest voltage node
        ndnum_hghst: Integer;
        b_ctrl_hghst: Boolean; //can contribute more to the high volt problem
        volt_hghst: Double;    //low volt in pu
        volt_hgh_lmt: Double;   //low limit in pu
        Pinjec_hghst: Double;  //net P injection on this node

         // lowest voltage node
        ndnum_lwst: Integer;
        b_ctrl_lwst: Boolean; //can contribute more to the high volt problem
        volt_lwst: Double;  //low volt in pu
        volt_lw_lmt: Double; //low limit in pu
        Pinjec_lwst: Double; // net P injection on this node

         // overview information
        volt_avg: Double;
        total_pg: Double; //total generation of this cluster
        total_pl: Double; //total load of this cluster

        b_Curt_Ctrl: Boolean;
    end;

implementation

end.
