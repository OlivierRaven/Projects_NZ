! Sediment parameters for Lake Tutira				                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       
!									                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      
!	-----------------------------------------------------------------------------!								                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
INORGANIC PARTICLE constants						                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              
!	-----------------------------------------------------------------------------!								                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        
! Density of suspended solid particles                                          !
 0.26500E+04           deS (kg/m^3)      : SSOL1                                !
 0.26500E+04           deS (kg/m^3)      : SSOL2                                !
 0.26500E+04                                                                    !
 0.26500E+04                            				        !
 0.26500E+04                                				        !
 0.26500E+04                                 					!
! Diameter of suspended solids groups                                           !
 1.000E-06             diaSS (m)         : SSOL1                                !
 3.000E-06             diaSS (m)         : SSOL2                                !
 3.000E-06                                                                      !
 3.000E-06                                                                      !
 3.000E-06                                                                      !
 3.000E-06                                                                      !
! Specific attenuation coefficient                                              !
 0.050E+00              KeSS (mg^-1Lm^-1) : SSOL1                               !
 0.050E+00              KeSS (mg^-1Lm^-1) : SSOL2                               !
 0.050E+00                                                                      !
 0.050E+00                                                                      !
 0.050E+00                                                                      !
 0.050E+00                                                                      !
! Critical shear stress                                                         !
 0.05000E+00            tcs (N/m2)        : SSOL1                               !
 0.05000E+00            tcs (N/m2)        : SSOL2                               !
 0.03000E+00                                                                    !
 0.03000E+00                                                                    !
 0.03000E+00                                                                    !  
 0.03000E+00                                                                    !
! -----------------------------------------------------------------------------!
STATIC	SEDIMENT	constants												                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
! -----------------------------------------------------------------------------!														                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  
! Theta(sed)           : Temp multiplier of sediment fluxes                    !
     1.06                                                                      !
!-- SEDIMENT OXYGEN DEMAND
     0.50E-00
     1.00E+00
!-- NUTRIENT FLUXES                                                             !
! PO4 sediment flux parameters                                                  ! 
     0.00500            SmpPO4 (g/m2/day) : Release rate of PO4                  !
     1.00000           KDOS-PO4 (g/m^3)  : Controls sed release of PO4 via O    !
! NH4 sediment flux parameters                                                  !
     0.01000           SmpNH4 (g/m2/day) : Release rate of NH4                !
     1.000           KDOS-NH4 (g/m^3)  : Controls sed release of NH4 via O      !
! NO3 sediment flux parameters                                                  ! 
     0.00000           SmpNO3 (g/m2/day) : Release rate of NO3                  !
     3.50000           KDOS-NO3 (g/m^3)  : Controls sed release of NO3 via O    !
! Si sediment flux parameters                                                   !
     0.03850           SmpSi (g/m2/day)  : Release rate of Si                   !
     0.50000           KDOS-Si (g/m^3)   : Controls sed release of Si via O     !
! DOC sediment flux parameters                                                  !
     0.09200           SmpdocL (g/m2/day): Release rate of DOCL                 ! 
     0.00100           SmpdocR (g/m2/day): Release rate of DOCR                 !
     0.50000           KDOS-doc (g/m^3)  : Controls sed release of DOC via O    !
! DOP sediment flux parameters                                                  !
     0.00000           SmpdopL (g/m2/day): Release rate of DOPL                   !
     0.00001           SmpdopR (g/m2/day): Release rate of DOPR                 !
     0.10000           KDOS-dop (g/m^3)  : Controls sed release of DOP via O    !
! DON sediment flux parameters                                                  !
     0.0000               SmpdonL (g/m2/day): Release rate of DONL                !
     0.00001           SmpdonR (g/m2/day): Release rate of DONR                 !
     0.50000           KDOS-don (g/m^3)  : Controls sed release of DON via O    !
!--	METAL	ETC	FLUXES											                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 
!	Fe	Sediment	flux											                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
  0.440000000000000     
  4.000000000000000E-002
!	Mn	Sediment	flux											                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
  8.599999999999999E-002
  0.400000000000000     
!	Su	Sediment	flux											                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
  8.599999999999999E-002
  0.400000000000000     
!	Al	Sediment	flux											                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
  1.200000000000000E-002
   350.000000000000     
!	Zn	Sediment	flux										                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    
  1.200000000000000E-002
   350.000000000000     
!--- SEDIMENT COMPOSITION											                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            
     0.050             sedOrganicFrac    : Fraction of sediment that is organics	!
     0.100             sedPorosity       : Sediment Porosity (porewater fraction)	!
     0.000             resusRate (g/m2/d): Composite resuspension rate 			!
     5.0E+08           resusKT	(g)      : Half-sat for resus dependence on sed mass	!



! -----------------------------------------------------------------------------!
   DYNAMIC SEDIMENT constants
! -----------------------------------------------------------------------------!
!-- Sediment Physical Properties   --------------------------------------------!
 1.00           SURFACE (X=0) VALUE OF THE BIODIFFUSION [in units of cm^2/yr](DB0)
 1              CHOICE OF BIOTURBATION: 0=GAUSSIAN DECREASE 1=2-LAYER MODE (IMIX)
 3.00           If IMIX=0 : HALF DEPTH FOR THE GAUSSIAN MIXING [cm] (XS)
 0.00           If IMIX=1 : DEPTH FOR START OF DB DECREASE [cm] (X1)
 1.00           If IMIX=1 : DEPTH WHERE DB(X) IS ZERO,[cm] X2 > X1 !! (X2)
 0              IRRIGATION ?? YES = 1  NO=0 (IRRG)
 5.00           COEFFICIENT OF IRRIGATION [1/yr] (ALPHA0)
 3.00           MAXIMUM DEPTH OF IRRIGATION [cm]  (XIRRIG)
 0.00           ADVECTIVE FLUX FROM BELOW [cm/yr] + from below, - from top (ventflow)
 0.0015         SEDIMENT ACCUMULATION: BURIAL VELOCITY AS X -> infinity [cm/yr) (W00) Hempel Wert
 0.95           POROSITY at X=0 (P0)
 0.71           POROSITY at INFINITY (P00)
 0.22           If P0<>P00 : POROSITY DEPTH-ATTENUATION COEFFICIENT (BP)
 3              Type of TORTUOSITY Calculation 1: Archie 2: Burger-Frieke 3: Weissberg  (TORTEQ)
 2.14           POROSITY EXPONENT FOR TORTUOSITY CORRECTION (Archies Law) (AN)
 3.79           POROSITY EXPONENT FOR TORTUOSITY CORRECTION (Burger-Frieke) (Aa)
 2.02           POROSITY EXPONENT FOR TORTUOSITY CORRECTION (Weissberg) (Ab)
10.00           MAXIMUM DEPTH FOR CALCULATIONS [cm] (XL)
20              NUMBER OF LAYERS TO BE INCLUDED (MAXNPTS)
!-- Reaction Rates   ----------------------------------------------------------!
20.000          Rate of Labile DOC consumption [1/yr]
20.000          Rate of Labile DON consumption [1/yr]
20.000          Rate of Labile DOP consumption [1/yr]
 2.000          Rate of Labile POC hydrolysis to DOCL [1/yr]
 2.000          Rate of Labile PON hydrolysis to DONL [1/yr]
 2.000          Rate of Labile POP hydrolysis to DOPL [1/yr]
 0.550          Rate of Refrac DOC breakdown to DOCL [1/yr]
 0.550          Rate of Refrac DON breakdown to DONL [1/yr]
 0.550          Rate of Refrac DOP breakdown to DOPL [1/yr]
 0.200          Rate of Refrac POC breakdown to DOCR [1/yr]
 0.200          Rate of Refrac PON breakdown to DONR [1/yr]
 0.200          Rate of Refrac POP breakdown to DOPR [1/yr]
 0.0001        Rate of Very Refrac POC breakdown to DOCR [1/yr]
 0.0001        Rate of Very Refrac PON breakdown to DONR [1/yr]
 0.0001        Rate of Very Refrac POP breakdown to DOPR [1/yr]
 1.50E+04       RATE CONSTANT FOR NH4 OXIDATION BY O2 (KNH4OX)
 1.00E+04       RATE CONSTANT FOR TH2S OXID. BY NO3 (KNO3TS)
 6.00E+04       RATE CONSTANT FOR H2S OXIDATION BY O2 (KSOX)
 2.00E+06       RATE CONSTANT FOR MnII OXIDATION BY O2 (KMNOX)
 0.00           RATE CONSTANT FOR MnII OXIDATION BY NO3 (KMNNO3)
 2.00E+06       RATE CONSTANT FOR FeII OXIDATION BY O2 (KFEOX)
 2.20E+04       RATE CONSTANT FOR FeS OXIDATION BY O2 (KFESOX)
 0.00           RATE CONSTANT FOR solid FeII #1 OXIDATION BY O2 (KFe1OX)
 0.00           RATE CONSTANT FOR solid FeII #2 OXIDATION BY O2 (KFe2OX)
 0.00           RATE CONSTANT FOR solid FeII #1 OXIDATION BY NO3 (KFe1NO3)
 0.00           RATE CONSTANT FOR solid FeII #2 OXIDATION BY NO3 (KFe2NO3)
 0.00           RATE CONSTANT FOR FeS OXIDATION BY MnO2 (KFESMN4)
 0.00           RATE CONSTANT FOR FeS OXIDATION BY FeOH3 (KFESFE3)
 0.00           RATE CONSTANT FOR FeS PPT FROM Fe2+ (KFESPPT)
 8.00E+03       RATE CONSTANT FOR TH2S OXIDATION WITH FEOH3 (KTSFE3)
 0.00           RATE CONSTANT FOR PYRITE FORMATION FROM FeS (KPYRIT)
10.00           RATE CONSTANT FOR FeII  OXIDATION WITH MnO2 (KMNFE)
 2.00E+04       RATE CONSTANT FOR TH2S  OXIDATION WITH MnO2 (KMNO2TS)
 1.00E+07       RATE CONSTANT FOR CH4   OXIDATION WITH O2 (KCH4OX)
10.00           RATE CONSTANT FOR CH4   OXIDATION WITH SO4 (KCH4SO4)
 8.0            MONOD CONSTANT FOR O2   REDUCTION [mM] (FOR OXIDANT DEPENDENCE) (KO2)
 3.0            MONOD CONSTANT FOR NO3  REDUCTION [mM] (KNO3)
10.00           MONOD CONSTANT FOR MnO2 REDUCTION [mM] (KMN)
10.00           MONOD CONSTANT FOR FeOH REDUCTION [mM] (KFE)
10.00           MONOD CONSTANT FOR SO4  REDUCTION [mM] (KSO4)
 0.00           ADSORPTION CONSTANT FOR NH4 (KANH4)
 0.00           ADSORPTION CONSTANT FOR PO4 (KAPO4)
!-- Initial Conditions ---------------------------------------------------------!
1               BOUNDARY COND FOR THE ORG MATTER AT X=0: 1=KNOWN CONC 2=PRESCRIBED FLUX (IBC2) (has to be 2 when running dynamic)
1               Calculate a steady state for start conditions 0=N,1=Y
1.0             Scale diffusive flux bewtween sed & water
0.00100         % by weight of Very Refractory Organic Matter (POMVR) 
EX_I            Initialisation method for OM (EX_I = expon; LI_I = linear; CO_I = const)
1.0 0.2         EX_I initialisation params (first param is surface conc second param is slope)
