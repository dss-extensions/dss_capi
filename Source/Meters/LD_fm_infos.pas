unit LD_fm_infos;

{
  ----------------------------------------------------------
  Copyright (c) 2017, Electric Power Research Institute, Inc.
  Added by Ying.
  ----------------------------------------------------------

  Definition of Fmonitor (virtue leader) Public Data Record
}

interface

Uses
      UComplex;
      //PointerList;

TYPE

   {Fmonitor public data/state variable structure}
   TLD_fm_infos = packed Record
   //properties for Nodes
         // highest voltage node
         ndnum_hghst : integer;
         b_ctrl_hghst : boolean; //can contribute more to the high volt problem
         volt_hghst : double;    //low volt in pu
         volt_hgh_lmt :double;   //low limit in pu
         Pinjec_hghst : double;  //net P injection on this node

         // lowest voltage node
         ndnum_lwst : integer;
         b_ctrl_lwst : boolean; //can contribute more to the high volt problem
         volt_lwst : double;  //low volt in pu
         volt_lw_lmt :double; //low limit in pu
         Pinjec_lwst : double; // net P injection on this node

         // overview information
         volt_avg : double;
         total_pg : double; //total generation of this cluster
         total_pl : double; //total load of this cluster

         b_Curt_Ctrl : boolean;
    End;

implementation

end.
