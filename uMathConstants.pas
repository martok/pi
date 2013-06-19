unit uMathConstants;

interface

uses
  uMathIntf;

type
  TConstantDef = record
    LongName: string;
    Value: Number;
    Uni,
      Comment: string;
  end;

const
  cPi: Number = 3.1415926535897932384626433832795028841972; // required for tests
  MathematicalConstants: array[0..8] of TConstantDef = (
    (LongName: 'Pi'; Value: 3.1415926535897932384626433832795028841972),
    (LongName: 'Tau'; Value: 6.28318530717958647692528676655900576839433879875021),
    (LongName: 'E'; Value: 2.7182818284590452353602874713526624977572),
    (LongName: 'Degree'; Value: 0.0174532925199432957692369076848861271344),
    (LongName: 'GoldenRatio'; Value: 1.6180339887498948482045868343656381177203),
    (LongName: 'EulerGamma'; Value: 0.57721566490153286060651209008240243104216),
    (LongName: 'Catalan'; Value: 0.91596559417721901505460351493238411077415),
    (LongName: 'Glaisher'; Value: 1.2824271291006226368753425688697917277677),
    (LongName: 'Khinchin'; Value: 2.6854520010653064453097148354817956938204)
    );
  PhysicalConstants: array[0..37] of TConstantDef = (
    //Auswahl nach: Formelsammlung, PAETEC. 4. Auflage, Berlin, 2004
    //Quelle dort: CODATA
    //Erweitert: Wolfram Research Mathematica, PhysicalConstants package (Auswahl)
    //Measurement
    (LongName: 'AbsoluteZero'; Value: - 273.15; Uni: '°C'; Comment: 'for conversion purposes'),
    (LongName: 'AtomicUnit'; Value: 1.660539E-27; Uni: 'kg'),
    (LongName: 'SpeedOfLight'; Value: 2.99792458E8; Uni: 'm/s'),
    (LongName: 'SpeedOfSound'; Value: 343.051; Uni: 'm/s'),
    (LongName: 'Planck'; Value: 6.626069E-34; Uni: 'J/s'),
    (LongName: 'PlanckMass'; Value: 2.177E-8; Uni: 'kg'),
    (LongName: 'Rydberg'; Value: 1.097373E7; Uni: 'm^-1'),
    //Electromagnetics
    (LongName: 'ElectricalFieldConstant'; Value: 8.854187818E-12; Uni: 'A s/V m'),
    (LongName: 'MagneticFieldConstant'; Value: 1.256637061E-6; Uni: 'V s/A m'),
    //Thermodynamics
    (LongName: 'TriplePointWater'; Value: 273.16; Uni: 'K'),
    (LongName: 'StefanBoltzmann'; Value: 5.670400E-8; Uni: 'W/m^2 K^4'),
    (LongName: 'MolarGasConstant'; Value: 8.314472; Uni: 'J/K mol'),
    (LongName: 'WiensDisplacement'; Value: 2.8977685E-3; Uni: 'm K'),
    //Astronomy
    (LongName: 'GravitationConstant'; Value: 6.673E-11; Uni: 'm^3 / kg s^2'),
    (LongName: 'EarthSolarConstant'; Value: 1.366E3; Uni: 'W/m^2'),
    (LongName: 'EarthApparentGravity'; Value: 9.80665; Uni: 'm/s^2'),
    (LongName: 'EarthRadius'; Value: 6.371E6; Uni: 'm'),
    (LongName: 'EarthMass'; Value: 5.9742E24; Uni: 'kg'),
    (LongName: 'SolRadius'; Value: 6.955E8; Uni: 'm'),
    (LongName: 'SolMass'; Value: 1.988435E30; Uni: 'kg'),
    (LongName: 'SolLuminosity'; Value: 3.846E26; Uni: 'W'),
    (LongName: 'AstronomicalUnit'; Value: 149.6E9; Uni: 'm'),
    (LongName: 'Lightyear'; Value: 9.4605E15; Uni: 'm'),
    (LongName: 'Parsec'; Value: 3.08568E16; Uni: 'm'),
    //Chemistry
    (LongName: 'Avogadro'; Value: 6.022142E32; Uni: 'mol^-1'),
    (LongName: 'Boltzmann'; Value: 1.380650E-23; Uni: 'J/K'; Comment: 'MolarGasConstant/Avogadro'),
    (LongName: 'MolarVolume'; Value: 22.414E-3; Uni: 'm^3/mol'),
    (LongName: 'Faraday'; Value: 9.648534E4; Uni: 'A s/mol'; Comment: 'Avogadro*ElectronCharge'),
    (LongName: 'Loschmidt'; Value: 2.686778E25; Uni: 'm^-3'; Comment: 'Avogadro/MolarVolume'),
    (LongName: 'StandardPressure'; Value: 101325; Uni: 'Pa'),
    (LongName: 'StandardTemperature'; Value: 273.15; Uni: 'K'),
    //Paricles
    (LongName: 'ElectronComptonWavelength'; Value: 2.426310303E-12; Uni: 'm'; Comment: 'Planck/(ElectronMass*SpeedOfLight)'),
    (LongName: 'ElectronCharge'; Value: 1.60217646E-19; Uni: 'C'),
    (LongName: 'ElectronMass'; Value: 9.10938188E-31; Uni: 'kg'),
    (LongName: 'NeutronComptonWavelength'; Value: 1.319590943E-15; Uni: 'm'; Comment: 'Planck/(NeutronMass*SpeedOfLight)'),
    (LongName: 'NeutronMass'; Value: 1.67492716E-27; Uni: 'kg'),
    (LongName: 'ProtonComptonWavelength'; Value: 1.321409898E-15; Uni: 'm'; Comment: 'Planck/(ProtonMass*SpeedOfLight)'),
    (LongName: 'ProtonMass'; Value: 1.67262158E-27; Uni: 'kg')
    );


implementation

end.
