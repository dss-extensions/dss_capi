///
/// CppIndMach012: A C++ implementation of IndMach012 as a generator user-model
/// Prepared by Paulo Meira for DSS-Extensions
///
/// For a similar but simpler implementation in Python, including a comparison,
/// please see the notebook on Google Colab:
///
/// https://colab.research.google.com/github/dss-extensions/DSS-Python/blob/master/docs/examples/UserModels/PyIndMach012/README.ipynb
///
/// Or check the latest rendered version at:
///
/// https://dss-extensions.org/DSS-Python/examples/UserModels/PyIndMach012/README.html
///
/// This implementation uses Eigen https://eigen.tuxfamily.org/ (for matrix
/// ops), and the header provided in DSS C-API (for the DSS user-model
/// structures). Since the different versions of OpenDSS and AltDSS have some
/// changes throughout the years, there are a few preprocessor definitions to
/// toggle when using a specific version. The CMake configuration provided
/// already builds dedicated DLLs/SOs for four variations (AltDSS/DSS C-API,
/// OpenDSS version 7.x, OpenDSS versions 8.x and 9.x, and OpenDSS version 10.x).
///
/// This targets C++17.
///
/// -> If you use this, please cite the repository or the Git commit on your 
///    work. This is distributed under the same license as the rest of DSS 
///    C-API (see the LICENSE file in the repository root). This is original 
///    work from DSS-Extensions.

#include "dss_GenUserModel.h" // for user-model structures and function declarations
#include "altdss/capi/enums.h" // For SolveModes enum

#ifdef _MSC_VER
#define _USE_MATH_DEFINES
#endif

#include <cstdint>
#include <cmath>
#include <cctype>
#include <vector>
#include <complex>
#include <string>
#include <string_view>
#include <charconv>
#include <Eigen/Dense>

using std::vector;
using std::string;
using namespace std::complex_literals;
using std::abs;
using std::conj;
using std::arg;
using std::string_view;
using Eigen::Map;
using Eigen::Matrix3cd;
using Eigen::Vector3cd;

typedef std::complex<double> complex;

double sqr(const double x)
{
    return x * x;
}

void to_double(string_view &s, double &res)
{
    // We can ignore errors since there is no proper way to report them.
    // If you're debugging, you could add a check here.
    std::from_chars(s.data(), s.data() + s.size(), res);
}

#pragma region "C++ user-model implementation for IndMach012"
struct TIndMach012
{
    TGeneratorVars *gen;
    TDynamicsRec *dyn;
    TDSSCallBacks *callbacks;

    // Static and const data
    static vector<TIndMach012*> instances;
    static TIndMach012* activeInstance;
    static Matrix3cd Ap2s;
    static Matrix3cd As2p;

    enum class ParseStatus {
        Name,
        Value
    };

    enum class Param: int32_t {
        INVALID,
        H,
        D,
        puRs,
        puXs,
        puRr,
        puXr,
        puXm,
        Slip,
        MaxSlip,
        SlipOption
    };

    enum class Var: int32_t {
        Slip,
        puRs,
        puXs,
        puRr,
        puXr,
        puXm,
        MaxSlip,
        Is1,
        Is2,
        Ir1,
        Ir2,
        E1_pu,
        StatorLosses,
        RotorLosses,
        ShaftPower_hp,
        PowerFactor,
        Efficiency_pct,
        NUM_VARS
    };
    const static int32_t NUM_VARS = int32_t(Var::NUM_VARS);

    const static string outputVarNames[NUM_VARS];
    // State variables and deltas
    complex E1=0, dE1_dt=0, E1n=0, dE1n_dt=0;
    complex E2=0, dE2_dt=0, E2n=0, dE2n_dt=0;

    // Input data
    double H = 0.02;
    double D = 0.02;
    double puRs = 0.0053;
    double puXs = 0.106;
    double puRr = 0.007;
    double puXr = 0.12;
    double puXm = 4.0;
    double slip = 0.007;
    double MaxSlip = 0.1;
    string slipOption = "variable";

    double Xopen, Xp, T0p;
    complex Zsp, Zr, Zs, Zm;
    bool fixedSlip;

    // General state
    double S1, S2, dS_dP;
    complex V1, V2, Is1, Is2, Ir1, Ir2, Power;
    Vector3cd V012, I012;
    bool firstIteration;

    static int32_t New(TGeneratorVars* GenData, TDynamicsRec* DynaData, TDSSCallBacks* CallBacks)
    {
        if (instances.empty())
        {
            // Initialize matrices
            const complex a = std::exp(1i * 2. * M_PI / 3.);
            const complex a_3 = a / 3.;
            const complex aa = std::exp(1i * 4. * M_PI / 3.);
            const complex aa_3 = aa / 3.;
            const complex one = 1.; 
            const complex one_3 = 1./3.; 
            Ap2s <<
                one_3, one_3, one_3,
                one_3, a_3, aa_3,
                one_3, aa_3, a_3;
            As2p << 
                one, one, one,
                one, aa, a,
                one, a, aa;
        }
        
        activeInstance = new TIndMach012(GenData, DynaData, CallBacks);
        instances.push_back(activeInstance);
        return static_cast<int32_t>(instances.size());
    }

    void copyState()
    {
        E1n = E1;
        dE1n_dt = dE1_dt;
        E2n = E2;
        dE2n_dt = dE2_dt;
    }

    /// Initialize state variables (dynamics mode), equivalent to
    /// TIndMach012Obj.InitStateVars
    void InitStateVars(Map<Vector3cd> &Vabc, Map<Vector3cd> &Iabc)
    {
        V012.noalias() = Ap2s * Vabc;
        I012.noalias() = Ap2s * Iabc;
        
        // The following is done in TIndMach012Obj.InitModel:
        // Compute Voltage behind transient reactance and set derivatives to zero
        E1 = V012[1] - I012[1] * Zsp;
        dE1_dt = 0;
        E2 = V012[2] - I012[2] * Zsp;
        dE2_dt = 0;

        // Copy the current state to the previous state
        copyState();
        
        // Initial rotor speed
        gen->Speed = -S1 * gen->w0;
        gen->dSpeed = 0;
        gen->Theta = arg(E1); // overwrite Theta
        gen->dTheta = 0;
    }

    /// Equivalent to TIndMach012Obj.Integrate
    void Integrate()
    {
        if (dyn->IterationFlag == 0)
        {
            // First iteration of new time step, copy the previous state
            // to be used in the integration process
            copyState();
        }
        
        // Derivative of E
        dE1_dt = (1i * -gen->w0 * S1 * E1) - ((E1 - 1i * (Xopen - Xp) * Is1) / T0p);
        dE2_dt = (1i * -gen->w0 * S2 * E2) - ((E2 - 1i * (Xopen - Xp) * Is2) / T0p);
        
        // Trapezoidal Integration
        double h_2 = dyn->h * 0.5;
        E1 = E1n + h_2 * (dE1_dt + dE1n_dt);
        E2 = E2n + h_2 * (dE2_dt + dE2n_dt);
    }

    /// Propagate changes from the input parameters to the model.
    /// Equivalent to part of TIndMach012Obj.RecalcElementData
    void Update()
    {
        setLocalSlip(slip);
        
        // make generator speed agree
        gen->Speed = -S1 * gen->w0;
        gen->dSpeed = 0.0;
    
        fixedSlip = (!slipOption.empty()) && (slipOption[0] == 'F' || slipOption[0] == 'f');
        firstIteration = true;

        double
            ZBase = 1000.0 * (sqr(gen->kVGeneratorBase) / gen->kVArating),
            Rs = puRs * ZBase,
            Xs = puXs * ZBase,
            Rr = puRr * ZBase,
            Xr = puXr * ZBase,
            Xm = puXm * ZBase;
        
        Zs = {Rs, Xs};
        Zm = {0, Xm};
        Zr = {Rr, Xr};
        
        Xopen = Xs + Xm;
        Xp  = Xs + (Xr * Xm) / (Xr + Xm);
        Zsp = complex(Rs, Xp);
        T0p = (Xr + Xm) / (gen->w0 * Rr);
        
        // Init dS_dP based on rated slip and rated voltage
        V1 = complex(gen->kVGeneratorBase * 1000.0 / sqrt(3));
        if (S1 != 0)
        {
            pfModelCurrent(V1, S1, Is1, Ir1);
        }
        
        dS_dP = S1 / (V1 * conj(Is1)).real();
        
        Is1 = 0;
        V1 = 0;
        Is2 = 0;
        V2 = 0;
    }

    /// Calculate the new model state. Vabc is used as an
    /// input, while Iabc is the output used in OpenDSS.
    void Calc(Map<Vector3cd> &Vabc, Map<Vector3cd> &Iabc)
    {
        V012.noalias() = Ap2s * Vabc;
        I012.noalias() = Ap2s * Iabc;
        
        if (dyn->SolutionMode == SolveModes_Dynamic)
        {
            CalcDynamic(V012, I012);
        }
        else
        {
            CalcPowerFlow(V012, I012);
        }

        Iabc.noalias() = As2p * I012;
        
        // Keep a copy of the total power
        Power = (Vabc.array() * Iabc.conjugate().array()).sum();
    }

    /// Equivalent to TIndMach012Obj.CalcDynamic
    void CalcDynamic(Vector3cd &V012, Vector3cd &I012)
    {
        V1 = V012[1];
        V2 = V012[2];

        // In dynamics mode, slip is allowed to vary
        
        // Gets slip from shaft speed
        setLocalSlip(-gen->Speed / gen->w0);
        
        // The stator and rotor currents from the Pascal code are 
        // computed in TIndMach012Obj.Get_DynamicModelCurrent

        // Stator current
        Is1 = (V1 - E1) / Zsp;
        Is2 = (V2 - E2) / Zsp;

        // Rotor current
        Ir1 = Is1 - (V1 - Is1 * Zsp) / Zm;
        Ir2 = Is2 - (V2 - Is2 * Zsp) / Zm;
        
        I012[0] = 0;
        I012[1] = Is1;
        I012[2] = Is2;
    }

    /// Equivalent to TIndMach012Obj.CalcPFlow
    void CalcPowerFlow(Vector3cd &V012, Vector3cd &I012)
    {
        V1 = V012[1];
        V2 = V012[2];
        
        if (firstIteration)
        {
            // Initialize Is1
            pfModelCurrent(V1, S1, Is1, Ir1);
        }

        // If fixed slip option set, then use the value set by the user
        if (!fixedSlip)
        {
            double P_Error = gen->PNominalPerPhase - (V1 * conj(Is1)).real();
            
            // make new guess at slip
            setLocalSlip(S1 + dS_dP * P_Error);
        }
        
        pfModelCurrent(V1, S1, Is1, Ir1);
        pfModelCurrent(V2, S2, Is2, Ir2);
        
        I012[0] = 0;
        I012[1] = Is1;
        I012[2] = Is2;
    }

    /// Equivalent to TIndMach012Obj.Get_PFlowModelCurrent
    void pfModelCurrent(complex V, double s, complex &Istator, complex &Irotor)
    {
        double RL;
        if (s != 0.0)
        {
            RL = Zr.real() * (1 - s) / s;
        }
        else
        {
            RL = Zr.real() * 1.0e6;
        }
        complex Zrotor = RL + Zr;
        complex Zmotor = Zs + (Zrotor * Zm) / (Zrotor + Zm);
        
        Istator = V / Zmotor;
        Irotor = Istator - (V - Zs * Istator) / Zm;
    }

    void setLocalSlip(double value)
    {
        S1 = value;
        if (dyn->SolutionMode != SolveModes_Dynamic)
        {
            // Put limits on the slip unless dynamics
            if (abs(S1) > MaxSlip)
            {
                S1 = copysign(MaxSlip, S1);
            }
        }
        S2 = 2 - S1;
    }

    void editParam(Param param, string_view value)
    {
        if (value.empty())
        {
            return;
        }
        switch (param)
        {
            case Param::H:
                to_double(value, H);
                return;
            case Param::D:
                to_double(value, D);
                return;
            case Param::puRs:
                to_double(value, puRs);
                return;
            case Param::puXs:
                to_double(value, puXs);
                return;
            case Param::puRr:
                to_double(value, puRr);
                return;
            case Param::puXr:
                to_double(value, puXr);
                return;
            case Param::puXm:
                to_double(value, puXm);
                return;
            case Param::Slip:
                to_double(value, slip);
                return;
            case Param::MaxSlip:
                to_double(value, MaxSlip);
                return;
            case Param::SlipOption:
                slipOption = value;
                return;
            default:
                return;
        }
    }

    /// Our Edit function expects pairs of identifiers and values (e.g. 
    /// "x=1.1 y=1.2"), separated by spaces or commas.
    /// Neither parentheses nor OpenDSS RPN expressions are allowed.
    /// Note that this could be simplified by using an established format, 
    /// allowing us to reuse an existing parser. For example, a JSON-encoded 
    /// parameter string could be useful, etc. We don't use that here to 
    /// avoid adding dependencies.
    ///
    /// Note that the callbacks for some DSS parser functions are also 
    /// available, but not required. The relevant callback functions are
    /// LoadParser, NextParam, GetIntValue, GetDblValue, and GetStrValue.
    ///
    /// See also the `edit` method in 
    /// https://github.com/dss-extensions/DSS-Python/blob/master/dss/UserModels/bases.py
    void Edit(const string_view &editStr)
    {
        string tmp;
        ParseStatus status = ParseStatus::Name;
        string_view::const_iterator it = editStr.begin();
        const string_view::const_iterator it_end = editStr.end();
        Param param = Param::INVALID;

        auto isSpace = [](char ch) -> bool { 
            return (ch == ' ' || ch == '\t' || ch == ','); 
        };

        // Skip leading spaces
        while (it != it_end && isSpace(*it))
        {
            ++it;
        }
        string_view::const_iterator partBegin = it;
        string_view::const_iterator partEnd = it;
        while (it != it_end)
        {
            if (status == ParseStatus::Name)
            {
                partEnd = it;
                ++it;
                if (*partEnd == '=')
                {
                    // Grab the param name
                    tmp = string(partBegin, partEnd);

                    // Set change the status and the marker iterator
                    status = ParseStatus::Value;
                    // Skip leading spaces for the value
                    while (it != it_end && isSpace(*it))
                    {
                        ++it;
                    }
                    partBegin = it;

                    for (auto &ch: tmp)
                    {
                        ch = std::tolower(ch);
                    }
                    // Initially mark it as an invalid name, so we can ignored later if not found.
                    param = Param::INVALID;
                    // Since we have a small list of params, let's just compare the strings.
                    // This shouldn't be in the hot path, but there are obvious alternatives.
                    if (tmp == "h")
                    {
                        param = Param::H;
                        continue;
                    }
                    if (tmp == "d")
                    {
                        param = Param::H;
                        continue;
                    }
                    if (tmp[0] == 'p')
                    {
                        if (tmp == "purs")
                        {
                            param = Param::puRs;
                            continue;
                        }
                        if (tmp == "puxs")
                        {
                            param = Param::puXs;
                            continue;
                        }
                        if (tmp == "purr")
                        {
                            param = Param::puRr;
                            continue;
                        }
                        if (tmp == "puxr")
                        {
                            param = Param::puXr;
                            continue;
                        }
                        if (tmp == "puxm")
                        {
                            param = Param::puXm;
                            continue;
                        }
                    }
                    if (tmp == "slip")
                    {
                        param = Param::Slip;
                        continue;
                    }
                    if (tmp == "maxslip")
                    {
                        param = Param::Slip;
                        continue;
                    }
                    if (tmp == "slipoption")
                    {
                        param = Param::SlipOption;
                        continue;
                    }
                }
                continue;
            }
            else if (status == ParseStatus::Value)
            {
                partEnd = it;
                ++it;
                if (isSpace(*partEnd) || it == it_end) // either a separator or the end of the string
                {
                    if (it == it_end)
                    {
                        partEnd = it_end;
                    }

                    // Set the param value
                    if (partBegin != partEnd)
                    {
                        editParam(param, string_view(&*partBegin, partEnd - partBegin));
                    }
                    // Set change the status and the marker iterator
                    status = ParseStatus::Name;
                    // Skip spaces for the next param, if any
                    while (it != it_end && isSpace(*it))
                    {
                        ++it;
                    }
                    partBegin = it;
                }
            }
        }
    }

    // The following are functions to emulate the model outputs from 
    // the Pascal version of built-in IndMach012 
    
    double E1_pu()
    {
        return sqrt(3.0) * abs(E1) / (1000 * gen->kVGeneratorBase);
    }

    double RotorLosses()
    {
        return 3 * (sqr(Ir1.real()) + sqr(Ir1.imag()) + sqr(Ir2.real()) + sqr(Ir2.imag())) * Zr.real();
    }

    double StatorLosses()
    {
        return 3 * (sqr(Is1.real()) + sqr(Is1.imag()) + sqr(Is2.real()) + sqr(Is2.imag())) * Zs.real();
    }

    double PowerFactor()
    {
        return copysign(Power.real() / abs(Power), Power.imag());
    }

    double Efficiency_pct()
    {
        double eff = (1 - (StatorLosses() + RotorLosses()) / Power.real()) * 100;
        if (eff < 0)
            return 0;
        if (eff > 100)
            return 100;
        return eff;
    }

    double ShaftPower_hp()
    {
        return (3.0/746) * (sqr(abs(Ir1)) * (1 - S1) / S1 + sqr(abs(Ir2)) * (1 - S2)/S2) * Zr.real();
    }

    void SetVariable(Var idx, double value)
    {
        switch (idx)
        {
            case Var::Slip: S1 = value; return;
            case Var::puRs: puRs = value; return;
            case Var::puXs: puXs = value; return;
            case Var::puRr: puRr = value; return;
            case Var::puXr: puXr = value; return;
            case Var::puXm: puXm = value; return;
            case Var::MaxSlip: MaxSlip = value; return;
            // The rest are read-only
            default: return;
        }
    }

    double GetVariable(Var idx)
    {
        switch (idx)
        {
            case Var::Slip: return S1;
            case Var::puRs: return puRs;
            case Var::puXs: return puXs;
            case Var::puRr: return puRr;
            case Var::puXr: return puXr;
            case Var::puXm: return puXm;
            case Var::MaxSlip: return MaxSlip;
            case Var::Is1: return abs(Is1);
            case Var::Is2: return abs(Is2);
            case Var::Ir1: return abs(Ir1);
            case Var::Ir2: return abs(Ir2);
            case Var::E1_pu: return E1_pu();
            case Var::StatorLosses: return StatorLosses();
            case Var::RotorLosses: return RotorLosses();
            case Var::ShaftPower_hp: return ShaftPower_hp();
            case Var::PowerFactor: return PowerFactor();
            case Var::Efficiency_pct: return Efficiency_pct();
            default: return -1;
        }
    }

private:
    TIndMach012(TGeneratorVars* GenData, TDynamicsRec* DynaData, TDSSCallBacks* CallBacks):
        gen(GenData),
        dyn(DynaData),
        callbacks(CallBacks)
    {
    }
};
#pragma endregion
#pragma region "Wrapper functions"

DSS_MODEL_DLL(int32_t) New(struct TGeneratorVars* GenData, struct TDynamicsRec* DynaData, struct TDSSCallBacks* CallBacks)
{
    return TIndMach012::New(GenData, DynaData, CallBacks);
}

DSS_MODEL_DLL(void) Delete(int32_t *ID)
{
    if (*ID <= 0 || static_cast<size_t>(*ID) >= TIndMach012::instances.size())
        return;

    delete TIndMach012::instances[static_cast<size_t>(*ID - 1)];
    TIndMach012::instances[static_cast<size_t>(*ID - 1)] = nullptr;
}

DSS_MODEL_DLL(int32_t) Select(int32_t *ID)
{
    if (*ID <= 0 || static_cast<size_t>(*ID) > TIndMach012::instances.size())
        return -1;

    TIndMach012::activeInstance = TIndMach012::instances[static_cast<size_t>(*ID - 1)];
    return *ID;
}

DSS_MODEL_DLL(void) Init(double *V, double *I)
{
    if (TIndMach012::activeInstance == nullptr || TIndMach012::activeInstance->gen->NumPhases != 3)
        return;

    Map<Vector3cd> mV{reinterpret_cast<complex*>(V)};
    Map<Vector3cd> mI{reinterpret_cast<complex*>(I)};
    TIndMach012::activeInstance->InitStateVars(mV, mI);
}

DSS_MODEL_DLL(void) Calc(double *V, double *I)
{
    if (TIndMach012::activeInstance == nullptr || TIndMach012::activeInstance->gen->NumPhases != 3)
        return;
    
    Map<Vector3cd> mV{reinterpret_cast<complex*>(V)};
    Map<Vector3cd> mI{reinterpret_cast<complex*>(I)};
    TIndMach012::activeInstance->Calc(mV, mI);
}

DSS_MODEL_DLL(void) Integrate(void)
{
    if (TIndMach012::activeInstance == nullptr || TIndMach012::activeInstance->gen->NumPhases != 3)
        return;

    TIndMach012::activeInstance->Integrate();
}

DSS_MODEL_DLL(void) Edit(char *EditStr, uint32_t MaxLen)
{
    if (TIndMach012::activeInstance == nullptr)
        return;

    string_view sv{EditStr, MaxLen};
    TIndMach012::activeInstance->Edit(sv);
}

DSS_MODEL_DLL(void) UpdateModel(void)
{
    if (TIndMach012::activeInstance == nullptr)
        return;

    TIndMach012::activeInstance->Update();
}

DSS_MODEL_DLL(int32_t) NumVars(void)
{
    return TIndMach012::NUM_VARS;
}

DSS_MODEL_DLL(void) GetAllVars(double *vars)
{
    if (TIndMach012::activeInstance == nullptr)
        return;

    for (int32_t n = 0; n < TIndMach012::NUM_VARS; ++n)
    {
        vars[n] = TIndMach012::activeInstance->GetVariable(static_cast<TIndMach012::Var>(n));
    }
}

DSS_MODEL_DLL(double) GetVariable(int32_t *i)
{
    if (TIndMach012::activeInstance == nullptr || (*i <= 0 || *i > TIndMach012::NUM_VARS))
        return -1;

    return TIndMach012::activeInstance->GetVariable(static_cast<TIndMach012::Var>(*i));
}

DSS_MODEL_DLL(void) SetVariable(int32_t *i, double *value)
{
    if (TIndMach012::activeInstance == nullptr || (*i <= 0 || *i > TIndMach012::NUM_VARS))
        return;

    TIndMach012::activeInstance->SetVariable(static_cast<TIndMach012::Var>(*i), *value);
}

DSS_MODEL_DLL(void) GetVarName(int32_t *i, char *VarName, uint32_t MaxLen)
{
    if (TIndMach012::activeInstance == nullptr || (*i <= 0 || *i > TIndMach012::NUM_VARS))
    {
        VarName[0] = 0;
        return;
    }

    const string &name = TIndMach012::outputVarNames[*i - 1];
    uint32_t N = std::min(MaxLen - 1, static_cast<uint32_t>(name.size()));
    uint32_t n;
    for (n = 0; n < N; ++n)
    {
        VarName[n] = name[n];
    }
    VarName[n] = 0;
}

DSS_MODEL_DLL(void) Save(void)
{
    // Nothing to do, unused in current versions of OpenDSS
}

DSS_MODEL_DLL(void) Restore(void)
{
    // Nothing to do, unused in current versions of OpenDSS
}

#pragma endregion

TIndMach012* TIndMach012::activeInstance = nullptr;
vector<TIndMach012*> TIndMach012::instances;
Matrix3cd TIndMach012::Ap2s;
Matrix3cd TIndMach012::As2p;
const string TIndMach012::outputVarNames[TIndMach012::NUM_VARS] = {
    "Slip", // The current slip (`slip` is the DSS input param)
    
    // There don"t need to be in the output (they're constant) but are listed 
    // in IndMach012.pas -- most likely to debug
    "puRs",
    "puXs",
    "puRr",
    "puXr",
    "puXm",
    "MaxSlip",
    
    // complex variables like these are exported as their absolute values
    "Is1", 
    "Is2",
    "Ir1",
    "Ir2",
    
    // Some properties to mimic the Pascal version
    "E1_pu",
    "StatorLosses",
    "RotorLosses",
    "ShaftPower_hp",
    "PowerFactor",
    "Efficiency_pct"
};
