 module AbilityId (AbilityId(..), toEnum, fromEnum, isBuildAbility) where

import Data.List (isPrefixOf)

data AbilityId =
   Invalid Int  -- 0
 | Smart  -- 1,      Target: Unit, Point.
 | Attack  -- 3674,   Target: Unit, Point.
 | AttackAttack  -- 23,     Target: Unit, Point.
 | AttackAttackbuilding  -- 2048,   Target: Unit, Point.
 | AttackRedirect  -- 1682,   Target: Unit, Point.
 | BehaviorBuildingattackoff  -- 2082,   Target: None.
 | BehaviorBuildingattackon  -- 2081,   Target: None.
 | BehaviorCloakoff  -- 3677,   Target: None.
 | BehaviorCloakoffBanshee  -- 393,    Target: None.
 | BehaviorCloakoffGhost  -- 383,    Target: None.
 | BehaviorCloakon  -- 3676,   Target: None.
 | BehaviorCloakonBanshee  -- 392,    Target: None.
 | BehaviorCloakonGhost  -- 382,    Target: None.
 | BehaviorGeneratecreepoff  -- 1693,   Target: None.
 | BehaviorGeneratecreepon  -- 1692,   Target: None.
 | BehaviorHoldfireoff  -- 3689,   Target: None.
 | BehaviorHoldfireoffLurker  -- 2552,   Target: None.
 | BehaviorHoldfireon  -- 3688,   Target: None.
 | BehaviorHoldfireonGhost  -- 36,     Target: None.
 | BehaviorHoldfireonLurker  -- 2550,   Target: None.
 | BehaviorPulsarbeamoff  -- 2376,   Target: None.
 | BehaviorPulsarbeamon  -- 2375,   Target: None.
 | BuildArmory  -- 331,    Target: Point.
 | BuildAssimilator  -- 882,    Target: Unit.
 | BuildBanelingnest  -- 1162,   Target: Point.
 | BuildBarracks  -- 321,    Target: Point.
 | BuildBunker  -- 324,    Target: Point.
 | BuildCommandcenter  -- 318,    Target: Point.
 | BuildCreeptumor  -- 3691,   Target: Point.
 | BuildCreeptumorQueen  -- 1694,   Target: Point.
 | BuildCreeptumorTumor  -- 1733,   Target: Point.
 | BuildCyberneticscore  -- 894,    Target: Point.
 | BuildDarkshrine  -- 891,    Target: Point.
 | BuildEngineeringbay  -- 322,    Target: Point.
 | BuildEvolutionchamber  -- 1156,   Target: Point.
 | BuildExtractor  -- 1154,   Target: Unit.
 | BuildFactory  -- 328,    Target: Point.
 | BuildFleetbeacon  -- 885,    Target: Point.
 | BuildForge  -- 884,    Target: Point.
 | BuildFusioncore  -- 333,    Target: Point.
 | BuildGateway  -- 883,    Target: Point.
 | BuildGhostacademy  -- 327,    Target: Point.
 | BuildHatchery  -- 1152,   Target: Point.
 | BuildHydraliskden  -- 1157,   Target: Point.
 | BuildInfestationpit  -- 1160,   Target: Point.
 | BuildInterceptors  -- 1042,   Target: None.
 | BuildMissileturret  -- 323,    Target: Point.
 | BuildNexus  -- 880,    Target: Point.
 | BuildNuke  -- 710,    Target: None.
 | BuildNydusnetwork  -- 1161,   Target: Point.
 | BuildNydusworm  -- 1768,   Target: Point.
 | BuildPhotoncannon  -- 887,    Target: Point.
 | BuildPylon  -- 881,    Target: Point.
 | BuildReactor  -- 3683,   Target: None.
 | BuildReactorBarracks  -- 422,    Target: None.
 | BuildReactorFactory  -- 455,    Target: None.
 | BuildReactorStarport  -- 488,    Target: None.
 | BuildRefinery  -- 320,    Target: Unit.
 | BuildRoachwarren  -- 1165,   Target: Point.
 | BuildRoboticsbay  -- 892,    Target: Point.
 | BuildRoboticsfacility  -- 893,    Target: Point.
 | BuildSensortower  -- 326,    Target: Point.
 | BuildShieldbattery  -- 895,    Target: Point.
 | BuildSpawningpool  -- 1155,   Target: Point.
 | BuildSpinecrawler  -- 1166,   Target: Point.
 | BuildSpire  -- 1158,   Target: Point.
 | BuildSporecrawler  -- 1167,   Target: Point.
 | BuildStargate  -- 889,    Target: Point.
 | BuildStarport  -- 329,    Target: Point.
 | BuildStasistrap  -- 2505,   Target: Point.
 | BuildSupplydepot  -- 319,    Target: Point.
 | BuildTechlab  -- 3682,   Target: None.
 | BuildTechlabBarracks  -- 421,    Target: None.
 | BuildTechlabFactory  -- 454,    Target: None.
 | BuildTechlabStarport  -- 487,    Target: None.
 | BuildTemplararchive  -- 890,    Target: Point.
 | BuildTwilightcouncil  -- 886,    Target: Point.
 | BuildUltraliskcavern  -- 1159,   Target: Point.
 | Burrowdown  -- 3661,   Target: None.
 | BurrowdownBaneling  -- 1374,   Target: None.
 | BurrowdownDrone  -- 1378,   Target: None.
 | BurrowdownHydralisk  -- 1382,   Target: None.
 | BurrowdownInfestor  -- 1444,   Target: None.
 | BurrowdownLurker  -- 2108,   Target: None.
 | BurrowdownQueen  -- 1433,   Target: None.
 | BurrowdownRavager  -- 2340,   Target: None.
 | BurrowdownRoach  -- 1386,   Target: None.
 | BurrowdownSwarmhost  -- 2014,   Target: None.
 | BurrowdownWidowmine  -- 2095,   Target: None.
 | BurrowdownZergling  -- 1390,   Target: None.
 | Burrowup  -- 3662,   Target: None.
 | BurrowupBaneling  -- 1376,   Target: None.
 | BurrowupDrone  -- 1380,   Target: None.
 | BurrowupHydralisk  -- 1384,   Target: None.
 | BurrowupInfestor  -- 1446,   Target: None.
 | BurrowupLurker  -- 2110,   Target: None.
 | BurrowupQueen  -- 1435,   Target: None.
 | BurrowupRavager  -- 2342,   Target: None.
 | BurrowupRoach  -- 1388,   Target: None.
 | BurrowupSwarmhost  -- 2016,   Target: None.
 | BurrowupWidowmine  -- 2097,   Target: None.
 | BurrowupZergling  -- 1392,   Target: None.
 | Cancel  -- 3659,   Target: None.
 | CancelslotAddon  -- 313,    Target: None.
 | CancelslotQueue1  -- 305,    Target: None.
 | CancelslotQueue5  -- 307,    Target: None.
 | CancelslotQueuecanceltoselection  -- 309,    Target: None.
 | CancelslotQueuepassive  -- 1832,   Target: None.
 | CancelAdeptphaseshift  -- 2594,   Target: None.
 | CancelAdeptshadephaseshift  -- 2596,   Target: None.
 | CancelBarracksaddon  -- 451,    Target: None.
 | CancelBuildinprogress  -- 314,    Target: None.
 | CancelCreeptumor  -- 1763,   Target: None.
 | CancelFactoryaddon  -- 484,    Target: None.
 | CancelGravitonbeam  -- 174,    Target: None.
 | CancelLast  -- 3671,   Target: None.
 | CancelMorphbroodlord  -- 1373,   Target: None.
 | CancelMorphlair  -- 1217,   Target: None.
 | CancelMorphlurker  -- 2333,   Target: None.
 | CancelMorphlurkerden  -- 2113,   Target: None.
 | CancelMorphmothership  -- 1848,   Target: None.
 | CancelMorphorbital  -- 1517,   Target: None.
 | CancelMorphoverlordtransport  -- 2709,   Target: None.
 | CancelMorphoverseer  -- 1449,   Target: None.
 | CancelMorphplanetaryfortress  -- 1451,   Target: None.
 | CancelMorphravager  -- 2331,   Target: None.
 | CancelQueue1  -- 304,    Target: None.
 | CancelQueue5  -- 306,    Target: None.
 | CancelQueueaddon  -- 312,    Target: None.
 | CancelQueuecanceltoselection  -- 308,    Target: None.
 | CancelQueuepasive  -- 1831,   Target: None.
 | CancelQueuepassivecanceltoselection  -- 1833,   Target: None.
 | CancelSpinecrawlerroot  -- 1730,   Target: None.
 | CancelStarportaddon  -- 517,    Target: None.
 | EffectAbduct  -- 2067,   Target: Unit.
 | EffectAdeptphaseshift  -- 2544,   Target: Point.
 | EffectAutoturret  -- 1764,   Target: Point.
 | EffectBlindingcloud  -- 2063,   Target: Point.
 | EffectBlink  -- 3687,   Target: Point.
 | EffectBlinkStalker  -- 1442,   Target: Point.
 | EffectCalldownmule  -- 171,    Target: Unit, Point.
 | EffectCausticspray  -- 2324,   Target: Unit.
 | EffectCharge  -- 1819,   Target: Unit.
 | EffectChronoboost  -- 3755,    Target: Unit.
 | EffectContaminate  -- 1825,   Target: Unit.
 | EffectCorrosivebile  -- 2338,   Target: Point.
 | EffectEmp  -- 1628,   Target: Point.
 | EffectExplode  -- 42,     Target: None.
 | EffectFeedback  -- 140,    Target: Unit.
 | EffectForcefield  -- 1526,   Target: Point.
 | EffectFungalgrowth  -- 74,     Target: Point.
 | EffectGhostsnipe  -- 2714,   Target: Unit.
 | EffectGravitonbeam  -- 173,    Target: Unit.
 | EffectGuardianshield  -- 76,     Target: None.
 | EffectHeal  -- 386,    Target: Unit.
 | EffectHunterseekermissile  -- 169,    Target: Unit.
 | EffectImmortalbarrier  -- 2328,   Target: None.
 | EffectInfestedterrans  -- 247,    Target: Point.
 | EffectInjectlarva  -- 251,    Target: Unit.
 | EffectKd8charge  -- 2588,   Target: Unit, Point.
 | EffectLockon  -- 2350,   Target: Unit.
 | EffectLocustswoop  -- 2387,   Target: Point.
 | EffectMassrecall  -- 3686,   Target: Unit.
 | EffectMassrecallMothership  -- 2368,   Target: Unit.
 | EffectMassrecallMothershipcore  -- 1974,   Target: Unit.
 | EffectMedivacigniteafterburners  -- 2116,   Target: None.
 | EffectNeuralparasite  -- 249,    Target: Unit.
 | EffectNukecalldown  -- 1622,   Target: Point.
 | EffectOraclerevelation  -- 2146,   Target: Point.
 | EffectParasiticbomb  -- 2542,   Target: Unit.
 | EffectPhotonovercharge  -- 2162,   Target: Unit.
 | EffectPointdefensedrone  -- 144,    Target: Point.
 | EffectPsistorm  -- 1036,   Target: Point.
 | EffectPurificationnova  -- 2346,   Target: Point.
 | EffectRepair  -- 3685,   Target: Unit.
 | EffectRepairMule  -- 78,     Target: Unit.
 | EffectRepairScv  -- 316,    Target: Unit.
 | EffectRestore  -- 3765,       Target: Unit.
 | EffectSalvage  -- 32,     Target: None.
 | EffectScan  -- 399,    Target: Point.
 | EffectShadowstride  -- 2700,   Target: Point.
 | EffectSpawnchangeling  -- 181,    Target: None.
 | EffectSpawnlocusts  -- 2704,   Target: Point.
 | EffectSpray  -- 3684,   Target: Point.
 | EffectSprayProtoss  -- 30,     Target: Point.
 | EffectSprayTerran  -- 26,     Target: Point.
 | EffectSprayZerg  -- 28,     Target: Point.
 | EffectStim  -- 3675,   Target: None.
 | EffectStimMarauder  -- 253,    Target: None.
 | EffectStimMarine  -- 380,    Target: None.
 | EffectStimMarineRedirect  -- 1683,   Target: None.
 | EffectSupplydrop  -- 255,    Target: Unit.
 | EffectTacticaljump  -- 2358,   Target: Point.
 | EffectTempestdisruptionblast  -- 2698,   Target: Point.
 | EffectTimewarp  -- 2244,   Target: Point.
 | EffectTransfusion  -- 1664,   Target: Unit.
 | EffectViperconsume  -- 2073,   Target: Unit.
 | EffectVoidrayprismaticalignment  -- 2393,   Target: None.
 | EffectWidowmineattack  -- 2099,   Target: Unit.
 | EffectYamatogun  -- 401,    Target: Unit.
 | HallucinationAdept  -- 2391,   Target: None.
 | HallucinationArchon  -- 146,    Target: None.
 | HallucinationColossus  -- 148,    Target: None.
 | HallucinationDisruptor  -- 2389,   Target: None.
 | HallucinationHightemplar  -- 150,    Target: None.
 | HallucinationImmortal  -- 152,    Target: None.
 | HallucinationOracle  -- 2114,   Target: None.
 | HallucinationPhoenix  -- 154,    Target: None.
 | HallucinationProbe  -- 156,    Target: None.
 | HallucinationStalker  -- 158,    Target: None.
 | HallucinationVoidray  -- 160,    Target: None.
 | HallucinationWarpprism  -- 162,    Target: None.
 | HallucinationZealot  -- 164,    Target: None.
 | Halt  -- 3660,   Target: None.
 | HaltBuilding  -- 315,    Target: None.
 | HaltTerranbuild  -- 348,    Target: None.
 | HarvestGather  -- 3666,   Target: Unit.
 | HarvestGatherDrone  -- 1183,   Target: Unit.
 | HarvestGatherProbe  -- 298,    Target: Unit.
 | HarvestGatherScv  -- 295,    Target: Unit.
 | HarvestReturn  -- 3667,   Target: None.
 | HarvestReturnDrone  -- 1184,   Target: None.
 | HarvestReturnMule  -- 167,    Target: None.
 | HarvestReturnProbe  -- 299,    Target: None.
 | HarvestReturnScv  -- 296,    Target: None.
 | Holdposition  -- 18,     Target: None.
 | Land  -- 3678,   Target: Point.
 | LandBarracks  -- 554,    Target: Point.
 | LandCommandcenter  -- 419,    Target: Point.
 | LandFactory  -- 520,    Target: Point.
 | LandOrbitalcommand  -- 1524,   Target: Point.
 | LandStarport  -- 522,    Target: Point.
 | Lift  -- 3679,   Target: None.
 | LiftBarracks  -- 452,    Target: None.
 | LiftCommandcenter  -- 417,    Target: None.
 | LiftFactory  -- 485,    Target: None.
 | LiftOrbitalcommand  -- 1522,   Target: None.
 | LiftStarport  -- 518,    Target: None.
 | Load  -- 3668,   Target: Unit.
 | Loadall  -- 3663,   Target: None.
 | LoadallCommandcenter  -- 416,    Target: None.
 | LoadBunker  -- 407,    Target: Unit.
 | LoadMedivac  -- 394,    Target: Unit.
 | MorphArchon  -- 1766,   Target: None.
 | MorphBroodlord  -- 1372,   Target: None.
 | MorphGateway  -- 1520,   Target: None.
 | MorphGreaterspire  -- 1220,   Target: None.
 | MorphHellbat  -- 1998,   Target: None.
 | MorphHellion  -- 1978,   Target: None.
 | MorphHive  -- 1218,   Target: None.
 | MorphLair  -- 1216,   Target: None.
 | MorphLiberatoraamode  -- 2560,   Target: None.
 | MorphLiberatoragmode  -- 2558,   Target: Point.
 | MorphLurker  -- 2332,   Target: None.
 | MorphLurkerden  -- 2112,   Target: None.
 | MorphMothership  -- 1847,   Target: None.
 | MorphOrbitalcommand  -- 1516,   Target: None.
 | MorphOverlordtransport  -- 2708,   Target: None.
 | MorphOverseer  -- 1448,   Target: None.
 | MorphPlanetaryfortress  -- 1450,   Target: None.
 | MorphRavager  -- 2330,   Target: None.
 | MorphRoot  -- 3680,   Target: Point.
 | MorphSiegemode  -- 388,    Target: None.
 | MorphSpinecrawlerroot  -- 1729,   Target: Point.
 | MorphSpinecrawleruproot  -- 1725,   Target: None.
 | MorphSporecrawlerroot  -- 1731,   Target: Point.
 | MorphSporecrawleruproot  -- 1727,   Target: None.
 | MorphSupplydepotLower  -- 556,    Target: None.
 | MorphSupplydepotRaise  -- 558,    Target: None.
 | MorphThorexplosivemode  -- 2364,   Target: None.
 | MorphThorhighimpactmode  -- 2362,   Target: None.
 | MorphUnsiege  -- 390,    Target: None.
 | MorphUproot  -- 3681,   Target: None.
 | MorphVikingassaultmode  -- 403,    Target: None.
 | MorphVikingfightermode  -- 405,    Target: None.
 | MorphWarpgate  -- 1518,   Target: None.
 | MorphWarpprismphasingmode  -- 1528,   Target: None.
 | MorphWarpprismtransportmode  -- 1530,   Target: None.
 | Move  -- 3794,     Target: Unit, Point.
 | Patrol  -- 3795,     Target: Unit, Point.
 | RallyBuilding  -- 195,    Target: Unit, Point.
 | RallyCommandcenter  -- 203,    Target: Unit, Point.
 | RallyHatcheryUnits  -- 211,    Target: Unit, Point.
 | RallyHatcheryWorkers  -- 212,    Target: Unit, Point.
 | RallyMorphingUnit  -- 199,    Target: Unit, Point.
 | RallyNexus  -- 207,    Target: Unit, Point.
 | RallyUnits  -- 3673,   Target: Unit, Point.
 | RallyWorkers  -- 3690,   Target: Unit, Point.
 | ResearchAdeptresonatingglaives  -- 1594,   Target: None.
 | ResearchAdvancedballistics  -- 805,    Target: None.
 | ResearchBansheecloakingfield  -- 790,    Target: None.
 | ResearchBansheehyperflightrotors  -- 799,    Target: None.
 | ResearchBattlecruiserweaponrefit  -- 1532,   Target: None.
 | ResearchBlink  -- 1593,   Target: None.
 | ResearchBurrow  -- 1225,   Target: None.
 | ResearchCentrifugalhooks  -- 1482,   Target: None.
 | ResearchCharge  -- 1592,   Target: None.
 | ResearchChitinousplating  -- 265,    Target: None.
 | ResearchCombatshield  -- 731,    Target: None.
 | ResearchConcussiveshells  -- 732,    Target: None.
 | ResearchCyclonelockondamage  -- 769
 | ResearchDrillingclaws  -- 764,    Target: None.
 | ResearchEnhancedmunitions  -- 806,    Target: None.
 | ResearchExtendedthermallance  -- 1097,   Target: None.
 | ResearchGlialregeneration  -- 216,    Target: None.
 | ResearchGraviticbooster  -- 1093,   Target: None.
 | ResearchGraviticdrive  -- 1094,   Target: None.
 | ResearchGroovedspines  -- 1282,   Target: None.
 | ResearchHighcapacityfueltanks  -- 804,    Target: None.
 | ResearchHisecautotracking  -- 650,    Target: None.
 | ResearchInfernalpreigniter  -- 761,    Target: None.
 | ResearchInterceptorgravitoncatapult  -- 44,     Target: None.
 | ResearchMagfieldlaunchers  -- 766,    Target: None.
 | ResearchMuscularaugments  -- 1283,   Target: None.
 | ResearchNeosteelframe  -- 655,    Target: None.
 | ResearchNeuralparasite  -- 1455,   Target: None.
 | ResearchPathogenglands  -- 1454,   Target: None.
 | ResearchPersonalcloaking  -- 820,    Target: None.
 | ResearchPhoenixanionpulsecrystals  -- 46,     Target: None.
 | ResearchPneumatizedcarapace  -- 1223,   Target: None.
 | ResearchProtossairarmor  -- 3692,   Target: None.
 | ResearchProtossairarmorlevel1  -- 1565,   Target: None.
 | ResearchProtossairarmorlevel2  -- 1566,   Target: None.
 | ResearchProtossairarmorlevel3  -- 1567,   Target: None.
 | ResearchProtossairweapons  -- 3693,   Target: None.
 | ResearchProtossairweaponslevel1  -- 1562,   Target: None.
 | ResearchProtossairweaponslevel2  -- 1563,   Target: None.
 | ResearchProtossairweaponslevel3  -- 1564,   Target: None.
 | ResearchProtossgroundarmor  -- 3694,   Target: None.
 | ResearchProtossgroundarmorlevel1  -- 1065,   Target: None.
 | ResearchProtossgroundarmorlevel2  -- 1066,   Target: None.
 | ResearchProtossgroundarmorlevel3  -- 1067,   Target: None.
 | ResearchProtossgroundweapons  -- 3695,   Target: None.
 | ResearchProtossgroundweaponslevel1  -- 1062,   Target: None.
 | ResearchProtossgroundweaponslevel2  -- 1063,   Target: None.
 | ResearchProtossgroundweaponslevel3  -- 1064,   Target: None.
 | ResearchProtossshields  -- 3696,   Target: None.
 | ResearchProtossshieldslevel1  -- 1068,   Target: None.
 | ResearchProtossshieldslevel2  -- 1069,   Target: None.
 | ResearchProtossshieldslevel3  -- 1070,   Target: None.
 | ResearchPsistorm  -- 1126,   Target: None.
 | ResearchRapidfirelaunchers  -- 768,   Target: None.
 | ResearchRavencorvidreactor  -- 793,    Target: None.
 | ResearchRavenrecalibratedexplosives  -- 803,    Target: None.
 | ResearchShadowstrike  -- 2720,   Target: None.
 | ResearchSmartservos  -- 766,  Target: None.
 | ResearchStimpack  -- 730,    Target: None.
 | ResearchTerraninfantryarmor  -- 3697,   Target: None.
 | ResearchTerraninfantryarmorlevel1  -- 656,    Target: None.
 | ResearchTerraninfantryarmorlevel2  -- 657,    Target: None.
 | ResearchTerraninfantryarmorlevel3  -- 658,    Target: None.
 | ResearchTerraninfantryweapons  -- 3698,   Target: None.
 | ResearchTerraninfantryweaponslevel1  -- 652,    Target: None.
 | ResearchTerraninfantryweaponslevel2  -- 653,    Target: None.
 | ResearchTerraninfantryweaponslevel3  -- 654,    Target: None.
 | ResearchTerranshipweapons  -- 3699,   Target: None.
 | ResearchTerranshipweaponslevel1  -- 861,    Target: None.
 | ResearchTerranshipweaponslevel2  -- 862,    Target: None.
 | ResearchTerranshipweaponslevel3  -- 863,    Target: None.
 | ResearchTerranstructurearmorupgrade  -- 651,    Target: None.
 | ResearchTerranvehicleandshipplating  -- 3700,   Target: None.
 | ResearchTerranvehicleandshipplatinglevel1  -- 864,    Target: None.
 | ResearchTerranvehicleandshipplatinglevel2  -- 865,    Target: None.
 | ResearchTerranvehicleandshipplatinglevel3  -- 866,    Target: None.
 | ResearchTerranvehicleweapons  -- 3701,   Target: None.
 | ResearchTerranvehicleweaponslevel1  -- 855,    Target: None.
 | ResearchTerranvehicleweaponslevel2  -- 856,    Target: None.
 | ResearchTerranvehicleweaponslevel3  -- 857,    Target: None.
 | ResearchTunnelingclaws  -- 217,    Target: None.
 | ResearchWarpgate  -- 1568,   Target: None.
 | ResearchZergflyerarmor  -- 3702,   Target: None.
 | ResearchZergflyerarmorlevel1  -- 1315,   Target: None.
 | ResearchZergflyerarmorlevel2  -- 1316,   Target: None.
 | ResearchZergflyerarmorlevel3  -- 1317,   Target: None.
 | ResearchZergflyerattack  -- 3703,   Target: None.
 | ResearchZergflyerattacklevel1  -- 1312,   Target: None.
 | ResearchZergflyerattacklevel2  -- 1313,   Target: None.
 | ResearchZergflyerattacklevel3  -- 1314,   Target: None.
 | ResearchZerggroundarmor  -- 3704,   Target: None.
 | ResearchZerggroundarmorlevel1  -- 1189,   Target: None.
 | ResearchZerggroundarmorlevel2  -- 1190,   Target: None.
 | ResearchZerggroundarmorlevel3  -- 1191,   Target: None.
 | ResearchZerglingadrenalglands  -- 1252,   Target: None.
 | ResearchZerglingmetabolicboost  -- 1253,   Target: None.
 | ResearchZergmeleeweapons  -- 3705,   Target: None.
 | ResearchZergmeleeweaponslevel1  -- 1186,   Target: None.
 | ResearchZergmeleeweaponslevel2  -- 1187,   Target: None.
 | ResearchZergmeleeweaponslevel3  -- 1188,   Target: None.
 | ResearchZergmissileweapons  -- 3706,   Target: None.
 | ResearchZergmissileweaponslevel1  -- 1192,   Target: None.
 | ResearchZergmissileweaponslevel2  -- 1193,   Target: None.
 | ResearchZergmissileweaponslevel3  -- 1194,   Target: None.
 | ScanMove  -- 19,     Target: Unit, Point.
 | Stop  -- 3665,   Target: None.
 | StopBuilding  -- 2057,   Target: None.
 | StopCheer  -- 6,      Target: None.
 | StopDance  -- 7,      Target: None.
 | StopRedirect  -- 1691,   Target: None.
 | StopStop  -- 4,      Target: None.
 | TrainwarpAdept  -- 1419,   Target: Point.
 | TrainwarpDarktemplar  -- 1417,   Target: Point.
 | TrainwarpHightemplar  -- 1416,   Target: Point.
 | TrainwarpSentry  -- 1418,   Target: Point.
 | TrainwarpStalker  -- 1414,   Target: Point.
 | TrainwarpZealot  -- 1413,   Target: Point.
 | TrainAdept  -- 922,    Target: None.
 | TrainBaneling  -- 80,     Target: None.
 | TrainBanshee  -- 621,    Target: None.
 | TrainBattlecruiser  -- 623,    Target: None.
 | TrainCarrier  -- 948,    Target: None.
 | TrainColossus  -- 978,    Target: None.
 | TrainCorruptor  -- 1353,   Target: None.
 | TrainCyclone  -- 597,    Target: None.
 | TrainDarktemplar  -- 920,    Target: None.
 | TrainDisruptor  -- 994,    Target: None.
 | TrainDrone  -- 1342,   Target: None.
 | TrainGhost  -- 562,    Target: None.
 | TrainHellbat  -- 596,    Target: None.
 | TrainHellion  -- 595,    Target: None.
 | TrainHightemplar  -- 919,    Target: None.
 | TrainHydralisk  -- 1345,   Target: None.
 | TrainImmortal  -- 979,    Target: None.
 | TrainInfestor  -- 1352,   Target: None.
 | TrainLiberator  -- 626,    Target: None.
 | TrainMarauder  -- 563,    Target: None.
 | TrainMarine  -- 560,    Target: None.
 | TrainMedivac  -- 620,    Target: None.
 | TrainMothership  -- 110,   Target: None.
 | TrainMothershipcore  -- 1853,   Target: None.
 | TrainMutalisk  -- 1346,   Target: None.
 | TrainObserver  -- 977,    Target: None.
 | TrainOracle  -- 954,    Target: None.
 | TrainOverlord  -- 1344,   Target: None.
 | TrainPhoenix  -- 946,    Target: None.
 | TrainProbe  -- 1006,   Target: None.
 | TrainQueen  -- 1632,   Target: None.
 | TrainRaven  -- 622,    Target: None.
 | TrainReaper  -- 561,    Target: None.
 | TrainRoach  -- 1351,   Target: None.
 | TrainScv  -- 524,    Target: None.
 | TrainSentry  -- 921,    Target: None.
 | TrainSiegetank  -- 591,    Target: None.
 | TrainStalker  -- 917,    Target: None.
 | TrainSwarmhost  -- 1356,   Target: None.
 | TrainTempest  -- 955,    Target: None.
 | TrainThor  -- 594,    Target: None.
 | TrainUltralisk  -- 1348,   Target: None.
 | TrainVikingfighter  -- 624,    Target: None.
 | TrainViper  -- 1354,   Target: None.
 | TrainVoidray  -- 950,    Target: None.
 | TrainWarpprism  -- 976,    Target: None.
 | TrainWidowmine  -- 614,    Target: None.
 | TrainZealot  -- 916,    Target: None.
 | TrainZergling  -- 1343,   Target: None.
 | Unloadall  -- 3664,   Target: None.
 | Unloadallat  -- 3669,   Target: Unit, Point.
 | UnloadallatMedivac  -- 396,    Target: Unit, Point.
 | UnloadallatOverlord  -- 1408,   Target: Unit, Point.
 | UnloadallatWarpprism  -- 913,    Target: Unit, Point.
 | UnloadallBunker  -- 408,    Target: None.
 | UnloadallCommandcenter  -- 413,    Target: None.
 | UnloadallNydasnetwork  -- 1438,   Target: None.
 | UnloadallNydusworm  -- 2371,   Target: None.
 | UnloadunitBunker  -- 410,    Target: None.
 | UnloadunitCommandcenter  -- 415,    Target: None.
 | UnloadunitMedivac  -- 397,    Target: None.
 | UnloadunitNydasnetwork  -- 1440,   Target: None.
 | UnloadunitOverlord  -- 1409,   Target: None.
 | UnloadunitWarpprism  -- 914,    Target: None.
 deriving (Show, Eq, Read)

instance Enum AbilityId where

  --fromEnum :: AbilityId -> Int
  fromEnum x = case x of
    Invalid _ -> 0
    Smart -> 1
    Attack -> 3674
    AttackAttack -> 23
    AttackAttackbuilding -> 2048
    AttackRedirect -> 1682
    BehaviorBuildingattackoff -> 2082
    BehaviorBuildingattackon -> 2081
    BehaviorCloakoff -> 3677
    BehaviorCloakoffBanshee -> 393
    BehaviorCloakoffGhost -> 383
    BehaviorCloakon -> 3676
    BehaviorCloakonBanshee -> 392
    BehaviorCloakonGhost -> 382
    BehaviorGeneratecreepoff -> 1693
    BehaviorGeneratecreepon -> 1692
    BehaviorHoldfireoff -> 3689
    BehaviorHoldfireoffLurker -> 2552
    BehaviorHoldfireon -> 3688
    BehaviorHoldfireonGhost -> 36
    BehaviorHoldfireonLurker -> 2550
    BehaviorPulsarbeamoff -> 2376
    BehaviorPulsarbeamon -> 2375
    BuildArmory -> 331
    BuildAssimilator -> 882
    BuildBanelingnest -> 1162
    BuildBarracks -> 321
    BuildBunker -> 324
    BuildCommandcenter -> 318
    BuildCreeptumor -> 3691
    BuildCreeptumorQueen -> 1694
    BuildCreeptumorTumor -> 1733
    BuildCyberneticscore -> 894
    BuildDarkshrine -> 891
    BuildEngineeringbay -> 322
    BuildEvolutionchamber -> 1156
    BuildExtractor -> 1154
    BuildFactory -> 328
    BuildFleetbeacon -> 885
    BuildForge -> 884
    BuildFusioncore -> 333
    BuildGateway -> 883
    BuildGhostacademy -> 327
    BuildHatchery -> 1152
    BuildHydraliskden -> 1157
    BuildInfestationpit -> 1160
    BuildInterceptors -> 1042
    BuildMissileturret -> 323
    BuildNexus -> 880
    BuildNuke -> 710
    BuildNydusnetwork -> 1161
    BuildNydusworm -> 1768
    BuildPhotoncannon -> 887
    BuildPylon -> 881
    BuildReactor -> 3683
    BuildReactorBarracks -> 422
    BuildReactorFactory -> 455
    BuildReactorStarport -> 488
    BuildRefinery -> 320
    BuildRoachwarren -> 1165
    BuildRoboticsbay -> 892
    BuildRoboticsfacility -> 893
    BuildSensortower -> 326
    BuildShieldbattery -> 895
    BuildSpawningpool -> 1155
    BuildSpinecrawler -> 1166
    BuildSpire -> 1158
    BuildSporecrawler -> 1167
    BuildStargate -> 889
    BuildStarport -> 329
    BuildStasistrap -> 2505
    BuildSupplydepot -> 319
    BuildTechlab -> 3682
    BuildTechlabBarracks -> 421
    BuildTechlabFactory -> 454
    BuildTechlabStarport -> 487
    BuildTemplararchive -> 890
    BuildTwilightcouncil -> 886
    BuildUltraliskcavern -> 1159
    Burrowdown -> 3661
    BurrowdownBaneling -> 1374
    BurrowdownDrone -> 1378
    BurrowdownHydralisk -> 1382
    BurrowdownInfestor -> 1444
    BurrowdownLurker -> 2108
    BurrowdownQueen -> 1433
    BurrowdownRavager -> 2340
    BurrowdownRoach -> 1386
    BurrowdownSwarmhost -> 2014
    BurrowdownWidowmine -> 2095
    BurrowdownZergling -> 1390
    Burrowup -> 3662
    BurrowupBaneling -> 1376
    BurrowupDrone -> 1380
    BurrowupHydralisk -> 1384
    BurrowupInfestor -> 1446
    BurrowupLurker -> 2110
    BurrowupQueen -> 1435
    BurrowupRavager -> 2342
    BurrowupRoach -> 1388
    BurrowupSwarmhost -> 2016
    BurrowupWidowmine -> 2097
    BurrowupZergling -> 1392
    Cancel -> 3659
    CancelslotAddon -> 313
    CancelslotQueue1 -> 305
    CancelslotQueue5 -> 307
    CancelslotQueuecanceltoselection -> 309
    CancelslotQueuepassive -> 1832
    CancelAdeptphaseshift -> 2594
    CancelAdeptshadephaseshift -> 2596
    CancelBarracksaddon -> 451
    CancelBuildinprogress -> 314
    CancelCreeptumor -> 1763
    CancelFactoryaddon -> 484
    CancelGravitonbeam -> 174
    CancelLast -> 3671
    CancelMorphbroodlord -> 1373
    CancelMorphlair -> 1217
    CancelMorphlurker -> 2333
    CancelMorphlurkerden -> 2113
    CancelMorphmothership -> 1848
    CancelMorphorbital -> 1517
    CancelMorphoverlordtransport -> 2709
    CancelMorphoverseer -> 1449
    CancelMorphplanetaryfortress -> 1451
    CancelMorphravager -> 2331
    CancelQueue1 -> 304
    CancelQueue5 -> 306
    CancelQueueaddon -> 312
    CancelQueuecanceltoselection -> 308
    CancelQueuepasive -> 1831
    CancelQueuepassivecanceltoselection -> 1833
    CancelSpinecrawlerroot -> 1730
    CancelStarportaddon -> 517
    EffectAbduct -> 2067
    EffectAdeptphaseshift -> 2544
    EffectAutoturret -> 1764
    EffectBlindingcloud -> 2063
    EffectBlink -> 3687
    EffectBlinkStalker -> 1442
    EffectCalldownmule -> 171
    EffectCausticspray -> 2324
    EffectCharge -> 1819
    EffectChronoboost -> 3755
    EffectContaminate -> 1825
    EffectCorrosivebile -> 2338
    EffectEmp -> 1628
    EffectExplode -> 42
    EffectFeedback -> 140
    EffectForcefield -> 1526
    EffectFungalgrowth -> 74
    EffectGhostsnipe -> 2714
    EffectGravitonbeam -> 173
    EffectGuardianshield -> 76
    EffectHeal -> 386
    EffectHunterseekermissile -> 169
    EffectImmortalbarrier -> 2328
    EffectInfestedterrans -> 247
    EffectInjectlarva -> 251
    EffectKd8charge -> 2588
    EffectLockon -> 2350
    EffectLocustswoop -> 2387
    EffectMassrecall -> 3686
    EffectMassrecallMothership -> 2368
    EffectMassrecallMothershipcore -> 1974
    EffectMedivacigniteafterburners -> 2116
    EffectNeuralparasite -> 249
    EffectNukecalldown -> 1622
    EffectOraclerevelation -> 2146
    EffectParasiticbomb -> 2542
    EffectPhotonovercharge -> 2162
    EffectPointdefensedrone -> 144
    EffectPsistorm -> 1036
    EffectPurificationnova -> 2346
    EffectRepair -> 3685
    EffectRepairMule -> 78
    EffectRepairScv -> 316
    EffectRestore -> 3765
    EffectSalvage -> 32
    EffectScan -> 399
    EffectShadowstride -> 2700
    EffectSpawnchangeling -> 181
    EffectSpawnlocusts -> 2704
    EffectSpray -> 3684
    EffectSprayProtoss -> 30
    EffectSprayTerran -> 26
    EffectSprayZerg -> 28
    EffectStim -> 3675
    EffectStimMarauder -> 253
    EffectStimMarine -> 380
    EffectStimMarineRedirect -> 1683
    EffectSupplydrop -> 255
    EffectTacticaljump -> 2358
    EffectTempestdisruptionblast -> 2698
    EffectTimewarp -> 2244
    EffectTransfusion -> 1664
    EffectViperconsume -> 2073
    EffectVoidrayprismaticalignment -> 2393
    EffectWidowmineattack -> 2099
    EffectYamatogun -> 401
    HallucinationAdept -> 2391
    HallucinationArchon -> 146
    HallucinationColossus -> 148
    HallucinationDisruptor -> 2389
    HallucinationHightemplar -> 150
    HallucinationImmortal -> 152
    HallucinationOracle -> 2114
    HallucinationPhoenix -> 154
    HallucinationProbe -> 156
    HallucinationStalker -> 158
    HallucinationVoidray -> 160
    HallucinationWarpprism -> 162
    HallucinationZealot -> 164
    Halt -> 3660
    HaltBuilding -> 315
    HaltTerranbuild -> 348
    HarvestGather -> 3666
    HarvestGatherDrone -> 1183
    HarvestGatherProbe -> 298
    HarvestGatherScv -> 295
    HarvestReturn -> 3667
    HarvestReturnDrone -> 1184
    HarvestReturnMule -> 167
    HarvestReturnProbe -> 299
    HarvestReturnScv -> 296
    Holdposition -> 18
    Land -> 3678
    LandBarracks -> 554
    LandCommandcenter -> 419
    LandFactory -> 520
    LandOrbitalcommand -> 1524
    LandStarport -> 522
    Lift -> 3679
    LiftBarracks -> 452
    LiftCommandcenter -> 417
    LiftFactory -> 485
    LiftOrbitalcommand -> 1522
    LiftStarport -> 518
    Load -> 3668
    Loadall -> 3663
    LoadallCommandcenter -> 416
    LoadBunker -> 407
    LoadMedivac -> 394
    MorphArchon -> 1766
    MorphBroodlord -> 1372
    MorphGateway -> 1520
    MorphGreaterspire -> 1220
    MorphHellbat -> 1998
    MorphHellion -> 1978
    MorphHive -> 1218
    MorphLair -> 1216
    MorphLiberatoraamode -> 2560
    MorphLiberatoragmode -> 2558
    MorphLurker -> 2332
    MorphLurkerden -> 2112
    MorphMothership -> 1847
    MorphOrbitalcommand -> 1516
    MorphOverlordtransport -> 2708
    MorphOverseer -> 1448
    MorphPlanetaryfortress -> 1450
    MorphRavager -> 2330
    MorphRoot -> 3680
    MorphSiegemode -> 388
    MorphSpinecrawlerroot -> 1729
    MorphSpinecrawleruproot -> 1725
    MorphSporecrawlerroot -> 1731
    MorphSporecrawleruproot -> 1727
    MorphSupplydepotLower -> 556
    MorphSupplydepotRaise -> 558
    MorphThorexplosivemode -> 2364
    MorphThorhighimpactmode -> 2362
    MorphUnsiege -> 390
    MorphUproot -> 3681
    MorphVikingassaultmode -> 403
    MorphVikingfightermode -> 405
    MorphWarpgate -> 1518
    MorphWarpprismphasingmode -> 1528
    MorphWarpprismtransportmode -> 1530
    Move -> 3794
    Patrol -> 3795
    RallyBuilding -> 195
    RallyCommandcenter -> 203
    RallyHatcheryUnits -> 211
    RallyHatcheryWorkers -> 212
    RallyMorphingUnit -> 199
    RallyNexus -> 207
    RallyUnits -> 3673
    RallyWorkers -> 3690
    ResearchAdeptresonatingglaives -> 1594
    ResearchAdvancedballistics -> 805
    ResearchBansheecloakingfield -> 790
    ResearchBansheehyperflightrotors -> 799
    ResearchBattlecruiserweaponrefit -> 1532
    ResearchBlink -> 1593
    ResearchBurrow -> 1225
    ResearchCentrifugalhooks -> 1482
    ResearchCharge -> 1592
    ResearchChitinousplating -> 265
    ResearchCombatshield -> 731
    ResearchConcussiveshells -> 732
    ResearchCyclonelockondamage -> 769
    ResearchDrillingclaws -> 764
    ResearchEnhancedmunitions -> 806
    ResearchExtendedthermallance -> 1097
    ResearchGlialregeneration -> 216
    ResearchGraviticbooster -> 1093
    ResearchGraviticdrive -> 1094
    ResearchGroovedspines -> 1282
    ResearchHighcapacityfueltanks -> 804
    ResearchHisecautotracking -> 650
    ResearchInfernalpreigniter -> 761
    ResearchInterceptorgravitoncatapult -> 44
    ResearchMagfieldlaunchers -> 766
    ResearchMuscularaugments -> 1283
    ResearchNeosteelframe -> 655
    ResearchNeuralparasite -> 1455
    ResearchPathogenglands -> 1454
    ResearchPersonalcloaking -> 820
    ResearchPhoenixanionpulsecrystals -> 46
    ResearchPneumatizedcarapace -> 1223
    ResearchProtossairarmor -> 3692
    ResearchProtossairarmorlevel1 -> 1565
    ResearchProtossairarmorlevel2 -> 1566
    ResearchProtossairarmorlevel3 -> 1567
    ResearchProtossairweapons -> 3693
    ResearchProtossairweaponslevel1 -> 1562
    ResearchProtossairweaponslevel2 -> 1563
    ResearchProtossairweaponslevel3 -> 1564
    ResearchProtossgroundarmor -> 3694
    ResearchProtossgroundarmorlevel1 -> 1065
    ResearchProtossgroundarmorlevel2 -> 1066
    ResearchProtossgroundarmorlevel3 -> 1067
    ResearchProtossgroundweapons -> 3695
    ResearchProtossgroundweaponslevel1 -> 1062
    ResearchProtossgroundweaponslevel2 -> 1063
    ResearchProtossgroundweaponslevel3 -> 1064
    ResearchProtossshields -> 3696
    ResearchProtossshieldslevel1 -> 1068
    ResearchProtossshieldslevel2 -> 1069
    ResearchProtossshieldslevel3 -> 1070
    ResearchPsistorm -> 1126
    ResearchRapidfirelaunchers -> 768
    ResearchRavencorvidreactor -> 793
    ResearchRavenrecalibratedexplosives -> 803
    ResearchShadowstrike -> 2720
    ResearchSmartservos -> 766
    ResearchStimpack -> 730
    ResearchTerraninfantryarmor -> 3697
    ResearchTerraninfantryarmorlevel1 -> 656
    ResearchTerraninfantryarmorlevel2 -> 657
    ResearchTerraninfantryarmorlevel3 -> 658
    ResearchTerraninfantryweapons -> 3698
    ResearchTerraninfantryweaponslevel1 -> 652
    ResearchTerraninfantryweaponslevel2 -> 653
    ResearchTerraninfantryweaponslevel3 -> 654
    ResearchTerranshipweapons -> 3699
    ResearchTerranshipweaponslevel1 -> 861
    ResearchTerranshipweaponslevel2 -> 862
    ResearchTerranshipweaponslevel3 -> 863
    ResearchTerranstructurearmorupgrade -> 651
    ResearchTerranvehicleandshipplating -> 3700
    ResearchTerranvehicleandshipplatinglevel1 -> 864
    ResearchTerranvehicleandshipplatinglevel2 -> 865
    ResearchTerranvehicleandshipplatinglevel3 -> 866
    ResearchTerranvehicleweapons -> 3701
    ResearchTerranvehicleweaponslevel1 -> 855
    ResearchTerranvehicleweaponslevel2 -> 856
    ResearchTerranvehicleweaponslevel3 -> 857
    ResearchTunnelingclaws -> 217
    ResearchWarpgate -> 1568
    ResearchZergflyerarmor -> 3702
    ResearchZergflyerarmorlevel1 -> 1315
    ResearchZergflyerarmorlevel2 -> 1316
    ResearchZergflyerarmorlevel3 -> 1317
    ResearchZergflyerattack -> 3703
    ResearchZergflyerattacklevel1 -> 1312
    ResearchZergflyerattacklevel2 -> 1313
    ResearchZergflyerattacklevel3 -> 1314
    ResearchZerggroundarmor -> 3704
    ResearchZerggroundarmorlevel1 -> 1189
    ResearchZerggroundarmorlevel2 -> 1190
    ResearchZerggroundarmorlevel3 -> 1191
    ResearchZerglingadrenalglands -> 1252
    ResearchZerglingmetabolicboost -> 1253
    ResearchZergmeleeweapons -> 3705
    ResearchZergmeleeweaponslevel1 -> 1186
    ResearchZergmeleeweaponslevel2 -> 1187
    ResearchZergmeleeweaponslevel3 -> 1188
    ResearchZergmissileweapons -> 3706
    ResearchZergmissileweaponslevel1 -> 1192
    ResearchZergmissileweaponslevel2 -> 1193
    ResearchZergmissileweaponslevel3 -> 1194
    ScanMove -> 19
    Stop -> 3665
    StopBuilding -> 2057
    StopCheer -> 6
    StopDance -> 7
    StopRedirect -> 1691
    StopStop -> 4
    TrainwarpAdept -> 1419
    TrainwarpDarktemplar -> 1417
    TrainwarpHightemplar -> 1416
    TrainwarpSentry -> 1418
    TrainwarpStalker -> 1414
    TrainwarpZealot -> 1413
    TrainAdept -> 922
    TrainBaneling -> 80
    TrainBanshee -> 621
    TrainBattlecruiser -> 623
    TrainCarrier -> 948
    TrainColossus -> 978
    TrainCorruptor -> 1353
    TrainCyclone -> 597
    TrainDarktemplar -> 920
    TrainDisruptor -> 994
    TrainDrone -> 1342
    TrainGhost -> 562
    TrainHellbat -> 596
    TrainHellion -> 595
    TrainHightemplar -> 919
    TrainHydralisk -> 1345
    TrainImmortal -> 979
    TrainInfestor -> 1352
    TrainLiberator -> 626
    TrainMarauder -> 563
    TrainMarine -> 560
    TrainMedivac -> 620
    TrainMothership -> 110
    TrainMothershipcore -> 1853
    TrainMutalisk -> 1346
    TrainObserver -> 977
    TrainOracle -> 954
    TrainOverlord -> 1344
    TrainPhoenix -> 946
    TrainProbe -> 1006
    TrainQueen -> 1632
    TrainRaven -> 622
    TrainReaper -> 561
    TrainRoach -> 1351
    TrainScv -> 524
    TrainSentry -> 921
    TrainSiegetank -> 591
    TrainStalker -> 917
    TrainSwarmhost -> 1356
    TrainTempest -> 955
    TrainThor -> 594
    TrainUltralisk -> 1348
    TrainVikingfighter -> 624
    TrainViper -> 1354
    TrainVoidray -> 950
    TrainWarpprism -> 976
    TrainWidowmine -> 614
    TrainZealot -> 916
    TrainZergling -> 1343
    Unloadall -> 3664
    Unloadallat -> 3669
    UnloadallatMedivac -> 396
    UnloadallatOverlord -> 1408
    UnloadallatWarpprism -> 913
    UnloadallBunker -> 408
    UnloadallCommandcenter -> 413
    UnloadallNydasnetwork -> 1438
    UnloadallNydusworm -> 2371
    UnloadunitBunker -> 410
    UnloadunitCommandcenter -> 415
    UnloadunitMedivac -> 397
    UnloadunitNydasnetwork -> 1440
    UnloadunitOverlord -> 1409
    UnloadunitWarpprism -> 914

  --toEnum :: Int -> AbilityId
  toEnum x = case x of
    0 -> Invalid 0
    1 -> Smart
    3674 -> Attack
    23 -> AttackAttack
    2048 -> AttackAttackbuilding
    1682 -> AttackRedirect
    2082 -> BehaviorBuildingattackoff
    2081 -> BehaviorBuildingattackon
    3677 -> BehaviorCloakoff
    393 -> BehaviorCloakoffBanshee
    383 -> BehaviorCloakoffGhost
    3676 -> BehaviorCloakon
    392 -> BehaviorCloakonBanshee
    382 -> BehaviorCloakonGhost
    1693 -> BehaviorGeneratecreepoff
    1692 -> BehaviorGeneratecreepon
    3689 -> BehaviorHoldfireoff
    2552 -> BehaviorHoldfireoffLurker
    3688 -> BehaviorHoldfireon
    36 -> BehaviorHoldfireonGhost
    2550 -> BehaviorHoldfireonLurker
    2376 -> BehaviorPulsarbeamoff
    2375 -> BehaviorPulsarbeamon
    331 -> BuildArmory
    882 -> BuildAssimilator
    1162 -> BuildBanelingnest
    321 -> BuildBarracks
    324 -> BuildBunker
    318 -> BuildCommandcenter
    3691 -> BuildCreeptumor
    1694 -> BuildCreeptumorQueen
    1733 -> BuildCreeptumorTumor
    894 -> BuildCyberneticscore
    891 -> BuildDarkshrine
    322 -> BuildEngineeringbay
    1156 -> BuildEvolutionchamber
    1154 -> BuildExtractor
    328 -> BuildFactory
    885 -> BuildFleetbeacon
    884 -> BuildForge
    333 -> BuildFusioncore
    883 -> BuildGateway
    327 -> BuildGhostacademy
    1152 -> BuildHatchery
    1157 -> BuildHydraliskden
    1160 -> BuildInfestationpit
    1042 -> BuildInterceptors
    323 -> BuildMissileturret
    880 -> BuildNexus
    710 -> BuildNuke
    1161 -> BuildNydusnetwork
    1768 -> BuildNydusworm
    887 -> BuildPhotoncannon
    881 -> BuildPylon
    3683 -> BuildReactor
    422 -> BuildReactorBarracks
    455 -> BuildReactorFactory
    488 -> BuildReactorStarport
    320 -> BuildRefinery
    1165 -> BuildRoachwarren
    892 -> BuildRoboticsbay
    893 -> BuildRoboticsfacility
    326 -> BuildSensortower
    895 -> BuildShieldbattery
    1155 -> BuildSpawningpool
    1166 -> BuildSpinecrawler
    1158 -> BuildSpire
    1167 -> BuildSporecrawler
    889 -> BuildStargate
    329 -> BuildStarport
    2505 -> BuildStasistrap
    319 -> BuildSupplydepot
    3682 -> BuildTechlab
    421 -> BuildTechlabBarracks
    454 -> BuildTechlabFactory
    487 -> BuildTechlabStarport
    890 -> BuildTemplararchive
    886 -> BuildTwilightcouncil
    1159 -> BuildUltraliskcavern
    3661 -> Burrowdown
    1374 -> BurrowdownBaneling
    1378 -> BurrowdownDrone
    1382 -> BurrowdownHydralisk
    1444 -> BurrowdownInfestor
    2108 -> BurrowdownLurker
    1433 -> BurrowdownQueen
    2340 -> BurrowdownRavager
    1386 -> BurrowdownRoach
    2014 -> BurrowdownSwarmhost
    2095 -> BurrowdownWidowmine
    1390 -> BurrowdownZergling
    3662 -> Burrowup
    1376 -> BurrowupBaneling
    1380 -> BurrowupDrone
    1384 -> BurrowupHydralisk
    1446 -> BurrowupInfestor
    2110 -> BurrowupLurker
    1435 -> BurrowupQueen
    2342 -> BurrowupRavager
    1388 -> BurrowupRoach
    2016 -> BurrowupSwarmhost
    2097 -> BurrowupWidowmine
    1392 -> BurrowupZergling
    3659 -> Cancel
    313 -> CancelslotAddon
    305 -> CancelslotQueue1
    307 -> CancelslotQueue5
    309 -> CancelslotQueuecanceltoselection
    1832 -> CancelslotQueuepassive
    2594 -> CancelAdeptphaseshift
    2596 -> CancelAdeptshadephaseshift
    451 -> CancelBarracksaddon
    314 -> CancelBuildinprogress
    1763 -> CancelCreeptumor
    484 -> CancelFactoryaddon
    174 -> CancelGravitonbeam
    3671 -> CancelLast
    1373 -> CancelMorphbroodlord
    1217 -> CancelMorphlair
    2333 -> CancelMorphlurker
    2113 -> CancelMorphlurkerden
    1848 -> CancelMorphmothership
    1517 -> CancelMorphorbital
    2709 -> CancelMorphoverlordtransport
    1449 -> CancelMorphoverseer
    1451 -> CancelMorphplanetaryfortress
    2331 -> CancelMorphravager
    304 -> CancelQueue1
    306 -> CancelQueue5
    312 -> CancelQueueaddon
    308 -> CancelQueuecanceltoselection
    1831 -> CancelQueuepasive
    1833 -> CancelQueuepassivecanceltoselection
    1730 -> CancelSpinecrawlerroot
    517 -> CancelStarportaddon
    2067 -> EffectAbduct
    2544 -> EffectAdeptphaseshift
    1764 -> EffectAutoturret
    2063 -> EffectBlindingcloud
    3687 -> EffectBlink
    1442 -> EffectBlinkStalker
    171 -> EffectCalldownmule
    2324 -> EffectCausticspray
    1819 -> EffectCharge
    3755 -> EffectChronoboost
    1825 -> EffectContaminate
    2338 -> EffectCorrosivebile
    1628 -> EffectEmp
    42 -> EffectExplode
    140 -> EffectFeedback
    1526 -> EffectForcefield
    74 -> EffectFungalgrowth
    2714 -> EffectGhostsnipe
    173 -> EffectGravitonbeam
    76 -> EffectGuardianshield
    386 -> EffectHeal
    169 -> EffectHunterseekermissile
    2328 -> EffectImmortalbarrier
    247 -> EffectInfestedterrans
    251 -> EffectInjectlarva
    2588 -> EffectKd8charge
    2350 -> EffectLockon
    2387 -> EffectLocustswoop
    3686 -> EffectMassrecall
    2368 -> EffectMassrecallMothership
    1974 -> EffectMassrecallMothershipcore
    2116 -> EffectMedivacigniteafterburners
    249 -> EffectNeuralparasite
    1622 -> EffectNukecalldown
    2146 -> EffectOraclerevelation
    2542 -> EffectParasiticbomb
    2162 -> EffectPhotonovercharge
    144 -> EffectPointdefensedrone
    1036 -> EffectPsistorm
    2346 -> EffectPurificationnova
    3685 -> EffectRepair
    78 -> EffectRepairMule
    316 -> EffectRepairScv
    3765 -> EffectRestore
    32 -> EffectSalvage
    399 -> EffectScan
    2700 -> EffectShadowstride
    181 -> EffectSpawnchangeling
    2704 -> EffectSpawnlocusts
    3684 -> EffectSpray
    30 -> EffectSprayProtoss
    26 -> EffectSprayTerran
    28 -> EffectSprayZerg
    3675 -> EffectStim
    253 -> EffectStimMarauder
    380 -> EffectStimMarine
    1683 -> EffectStimMarineRedirect
    255 -> EffectSupplydrop
    2358 -> EffectTacticaljump
    2698 -> EffectTempestdisruptionblast
    2244 -> EffectTimewarp
    1664 -> EffectTransfusion
    2073 -> EffectViperconsume
    2393 -> EffectVoidrayprismaticalignment
    2099 -> EffectWidowmineattack
    401 -> EffectYamatogun
    2391 -> HallucinationAdept
    146 -> HallucinationArchon
    148 -> HallucinationColossus
    2389 -> HallucinationDisruptor
    150 -> HallucinationHightemplar
    152 -> HallucinationImmortal
    2114 -> HallucinationOracle
    154 -> HallucinationPhoenix
    156 -> HallucinationProbe
    158 -> HallucinationStalker
    160 -> HallucinationVoidray
    162 -> HallucinationWarpprism
    164 -> HallucinationZealot
    3660 -> Halt
    315 -> HaltBuilding
    348 -> HaltTerranbuild
    3666 -> HarvestGather
    1183 -> HarvestGatherDrone
    298 -> HarvestGatherProbe
    295 -> HarvestGatherScv
    3667 -> HarvestReturn
    1184 -> HarvestReturnDrone
    167 -> HarvestReturnMule
    299 -> HarvestReturnProbe
    296 -> HarvestReturnScv
    18 -> Holdposition
    3678 -> Land
    554 -> LandBarracks
    419 -> LandCommandcenter
    520 -> LandFactory
    1524 -> LandOrbitalcommand
    522 -> LandStarport
    3679 -> Lift
    452 -> LiftBarracks
    417 -> LiftCommandcenter
    485 -> LiftFactory
    1522 -> LiftOrbitalcommand
    518 -> LiftStarport
    3668 -> Load
    3663 -> Loadall
    416 -> LoadallCommandcenter
    407 -> LoadBunker
    394 -> LoadMedivac
    1766 -> MorphArchon
    1372 -> MorphBroodlord
    1520 -> MorphGateway
    1220 -> MorphGreaterspire
    1998 -> MorphHellbat
    1978 -> MorphHellion
    1218 -> MorphHive
    1216 -> MorphLair
    2560 -> MorphLiberatoraamode
    2558 -> MorphLiberatoragmode
    2332 -> MorphLurker
    2112 -> MorphLurkerden
    1847 -> MorphMothership
    1516 -> MorphOrbitalcommand
    2708 -> MorphOverlordtransport
    1448 -> MorphOverseer
    1450 -> MorphPlanetaryfortress
    2330 -> MorphRavager
    3680 -> MorphRoot
    388 -> MorphSiegemode
    1729 -> MorphSpinecrawlerroot
    1725 -> MorphSpinecrawleruproot
    1731 -> MorphSporecrawlerroot
    1727 -> MorphSporecrawleruproot
    556 -> MorphSupplydepotLower
    558 -> MorphSupplydepotRaise
    2364 -> MorphThorexplosivemode
    2362 -> MorphThorhighimpactmode
    390 -> MorphUnsiege
    3681 -> MorphUproot
    403 -> MorphVikingassaultmode
    405 -> MorphVikingfightermode
    1518 -> MorphWarpgate
    1528 -> MorphWarpprismphasingmode
    1530 -> MorphWarpprismtransportmode
    3794 -> Move
    3795 -> Patrol
    195 -> RallyBuilding
    203 -> RallyCommandcenter
    211 -> RallyHatcheryUnits
    212 -> RallyHatcheryWorkers
    199 -> RallyMorphingUnit
    207 -> RallyNexus
    3673 -> RallyUnits
    3690 -> RallyWorkers
    1594 -> ResearchAdeptresonatingglaives
    805 -> ResearchAdvancedballistics
    790 -> ResearchBansheecloakingfield
    799 -> ResearchBansheehyperflightrotors
    1532 -> ResearchBattlecruiserweaponrefit
    1593 -> ResearchBlink
    1225 -> ResearchBurrow
    1482 -> ResearchCentrifugalhooks
    1592 -> ResearchCharge
    265 -> ResearchChitinousplating
    731 -> ResearchCombatshield
    732 -> ResearchConcussiveshells
    769 -> ResearchCyclonelockondamage
    764 -> ResearchDrillingclaws
    806 -> ResearchEnhancedmunitions
    1097 -> ResearchExtendedthermallance
    216 -> ResearchGlialregeneration
    1093 -> ResearchGraviticbooster
    1094 -> ResearchGraviticdrive
    1282 -> ResearchGroovedspines
    804 -> ResearchHighcapacityfueltanks
    650 -> ResearchHisecautotracking
    761 -> ResearchInfernalpreigniter
    44 -> ResearchInterceptorgravitoncatapult
    766 -> ResearchMagfieldlaunchers
    1283 -> ResearchMuscularaugments
    655 -> ResearchNeosteelframe
    1455 -> ResearchNeuralparasite
    1454 -> ResearchPathogenglands
    820 -> ResearchPersonalcloaking
    46 -> ResearchPhoenixanionpulsecrystals
    1223 -> ResearchPneumatizedcarapace
    3692 -> ResearchProtossairarmor
    1565 -> ResearchProtossairarmorlevel1
    1566 -> ResearchProtossairarmorlevel2
    1567 -> ResearchProtossairarmorlevel3
    3693 -> ResearchProtossairweapons
    1562 -> ResearchProtossairweaponslevel1
    1563 -> ResearchProtossairweaponslevel2
    1564 -> ResearchProtossairweaponslevel3
    3694 -> ResearchProtossgroundarmor
    1065 -> ResearchProtossgroundarmorlevel1
    1066 -> ResearchProtossgroundarmorlevel2
    1067 -> ResearchProtossgroundarmorlevel3
    3695 -> ResearchProtossgroundweapons
    1062 -> ResearchProtossgroundweaponslevel1
    1063 -> ResearchProtossgroundweaponslevel2
    1064 -> ResearchProtossgroundweaponslevel3
    3696 -> ResearchProtossshields
    1068 -> ResearchProtossshieldslevel1
    1069 -> ResearchProtossshieldslevel2
    1070 -> ResearchProtossshieldslevel3
    1126 -> ResearchPsistorm
    768 -> ResearchRapidfirelaunchers
    793 -> ResearchRavencorvidreactor
    803 -> ResearchRavenrecalibratedexplosives
    2720 -> ResearchShadowstrike
    766 -> ResearchSmartservos
    730 -> ResearchStimpack
    3697 -> ResearchTerraninfantryarmor
    656 -> ResearchTerraninfantryarmorlevel1
    657 -> ResearchTerraninfantryarmorlevel2
    658 -> ResearchTerraninfantryarmorlevel3
    3698 -> ResearchTerraninfantryweapons
    652 -> ResearchTerraninfantryweaponslevel1
    653 -> ResearchTerraninfantryweaponslevel2
    654 -> ResearchTerraninfantryweaponslevel3
    3699 -> ResearchTerranshipweapons
    861 -> ResearchTerranshipweaponslevel1
    862 -> ResearchTerranshipweaponslevel2
    863 -> ResearchTerranshipweaponslevel3
    651 -> ResearchTerranstructurearmorupgrade
    3700 -> ResearchTerranvehicleandshipplating
    864 -> ResearchTerranvehicleandshipplatinglevel1
    865 -> ResearchTerranvehicleandshipplatinglevel2
    866 -> ResearchTerranvehicleandshipplatinglevel3
    3701 -> ResearchTerranvehicleweapons
    855 -> ResearchTerranvehicleweaponslevel1
    856 -> ResearchTerranvehicleweaponslevel2
    857 -> ResearchTerranvehicleweaponslevel3
    217 -> ResearchTunnelingclaws
    1568 -> ResearchWarpgate
    3702 -> ResearchZergflyerarmor
    1315 -> ResearchZergflyerarmorlevel1
    1316 -> ResearchZergflyerarmorlevel2
    1317 -> ResearchZergflyerarmorlevel3
    3703 -> ResearchZergflyerattack
    1312 -> ResearchZergflyerattacklevel1
    1313 -> ResearchZergflyerattacklevel2
    1314 -> ResearchZergflyerattacklevel3
    3704 -> ResearchZerggroundarmor
    1189 -> ResearchZerggroundarmorlevel1
    1190 -> ResearchZerggroundarmorlevel2
    1191 -> ResearchZerggroundarmorlevel3
    1252 -> ResearchZerglingadrenalglands
    1253 -> ResearchZerglingmetabolicboost
    3705 -> ResearchZergmeleeweapons
    1186 -> ResearchZergmeleeweaponslevel1
    1187 -> ResearchZergmeleeweaponslevel2
    1188 -> ResearchZergmeleeweaponslevel3
    3706 -> ResearchZergmissileweapons
    1192 -> ResearchZergmissileweaponslevel1
    1193 -> ResearchZergmissileweaponslevel2
    1194 -> ResearchZergmissileweaponslevel3
    19 -> ScanMove
    3665 -> Stop
    2057 -> StopBuilding
    6 -> StopCheer
    7 -> StopDance
    1691 -> StopRedirect
    4 -> StopStop
    1419 -> TrainwarpAdept
    1417 -> TrainwarpDarktemplar
    1416 -> TrainwarpHightemplar
    1418 -> TrainwarpSentry
    1414 -> TrainwarpStalker
    1413 -> TrainwarpZealot
    922 -> TrainAdept
    80 -> TrainBaneling
    621 -> TrainBanshee
    623 -> TrainBattlecruiser
    948 -> TrainCarrier
    978 -> TrainColossus
    1353 -> TrainCorruptor
    597 -> TrainCyclone
    920 -> TrainDarktemplar
    994 -> TrainDisruptor
    1342 -> TrainDrone
    562 -> TrainGhost
    596 -> TrainHellbat
    595 -> TrainHellion
    919 -> TrainHightemplar
    1345 -> TrainHydralisk
    979 -> TrainImmortal
    1352 -> TrainInfestor
    626 -> TrainLiberator
    563 -> TrainMarauder
    560 -> TrainMarine
    620 -> TrainMedivac
    110 -> TrainMothership
    1853 -> TrainMothershipcore
    1346 -> TrainMutalisk
    977 -> TrainObserver
    954 -> TrainOracle
    1344 -> TrainOverlord
    946 -> TrainPhoenix
    1006 -> TrainProbe
    1632 -> TrainQueen
    622 -> TrainRaven
    561 -> TrainReaper
    1351 -> TrainRoach
    524 -> TrainScv
    921 -> TrainSentry
    591 -> TrainSiegetank
    917 -> TrainStalker
    1356 -> TrainSwarmhost
    955 -> TrainTempest
    594 -> TrainThor
    1348 -> TrainUltralisk
    624 -> TrainVikingfighter
    1354 -> TrainViper
    950 -> TrainVoidray
    976 -> TrainWarpprism
    614 -> TrainWidowmine
    916 -> TrainZealot
    1343 -> TrainZergling
    3664 -> Unloadall
    3669 -> Unloadallat
    396 -> UnloadallatMedivac
    1408 -> UnloadallatOverlord
    913 -> UnloadallatWarpprism
    408 -> UnloadallBunker
    413 -> UnloadallCommandcenter
    1438 -> UnloadallNydasnetwork
    2371 -> UnloadallNydusworm
    410 -> UnloadunitBunker
    415 -> UnloadunitCommandcenter
    397 -> UnloadunitMedivac
    1440 -> UnloadunitNydasnetwork
    1409 -> UnloadunitOverlord
    914 -> UnloadunitWarpprism
    _ -> Invalid x

isBuildAbility:: AbilityId -> Bool
isBuildAbility x = "Build" `isPrefixOf` show x

